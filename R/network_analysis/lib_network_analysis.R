get_network <- function(type = "theme", prob = TRUE, filter_method = FALSE, blindspot = FALSE, country.threshold = 30, percentile.threshold = 0.90, country_filter = FALSE, countries = NULL, theme_filter = FALSE, themes = NULL){

	ensure_mat <- function(network_results){
		if ((country_filter | theme_filter) & !is.matrix(network_results)){
			if (country_filter & length(countries) == 1){
				network_results <- t(as.matrix(network_results))
				rownames(network_results) <- countries
				if (ncol(network_results) == length(themes)) {
					colnames(network_results) <- themes
				}
			} else if (theme_filter & length(themes) == 1){
				network_results <- as.matrix(network_results)
				colnames(network_results) <- themes
			}
		}
		return(network_results)
	}

	type_list <- c("methods", "NSF_general", "NSF_specific", "spatial scale", "theme", "water budget")
	stopifnot(type %in% type_list)
	consolidated_results <- readRDS(paste0("consolidated_results_", type, ".Rds"))
	docTopics <- consolidated_results %>% filter(country != "Irrelevant") %>% select(-c("year","country"))

	predCountryMembership <- readRDS("predCountryMembership.Rds")
	colnames(predCountryMembership) <- gsub("prob.", "", colnames(predCountryMembership))
	predCountryMembership <- predCountryMembership[which(consolidated_results$country != "Irrelevant"), ]
	
	country <- consolidated_results$country
	country <- as.character(country[country != "Irrelevant"])

	if (prob){
		network_results <- t(predCountryMembership) %*% as.matrix(docTopics)
	} else {
		df_country <- createDummyFeatures(country)
		network_results <- t(as.matrix(df_country)) %*% as.matrix(docTopics)
	}

	rownames(network_results) <- gsub("Costa.Rica", "Costa Rica", rownames(network_results))
	rownames(network_results) <- gsub("El.Salvador", "El Salvador", rownames(network_results))

	if (!theme_filter){
		if (filter_method & type == "theme"){
			topic_names <- read.csv("./data/topic_names.csv")
			method_topics <- as.character(topic_names$theme[grepl("methods", topic_names$description)])
			network_results <- network_results[, which(!colnames(network_results) %in% method_topics)]
		}
	}
	if (country_filter & !is.null(countries)){
		network_results <- network_results[rownames(network_results) %in% countries, ]
		network_results <- ensure_mat(network_results)
	}
	if (theme_filter & !is.null(themes)){
		network_results <- network_results[, colnames(network_results) %in% themes]
		network_results <- ensure_mat(network_results)
	}

	if(blindspot){
		network_results <- 1 / network_results
	}

	if (!country_filter){
		tab <- table(country)
		keepCountries <- names(tab)[which(tab >= country.threshold)]
		network_results <- network_results[rownames(network_results) %in% keepCountries, ]
	}

	if(!theme_filter){
		network_results <- apply(network_results, 1, function(row){
			row[row <= quantile(row, percentile.threshold)] <- 0
			return(row)
		})
		network_results <- t(network_results)
	}
	network_results <- ensure_mat(network_results)

	network_results <- network_results[, which(colSums(network_results) != 0)]
	network_results <- ensure_mat(network_results)

	network_results <- network_results[order(rownames(network_results), decreasing = TRUE), ]
	network_results <- ensure_mat(network_results)
	dimnames(network_results) <- list(orig = rownames(network_results), dest = colnames(network_results))
	# countryTopics <- rowSums(network_results)
	# topicsCountry <- colSums(network_results)
	return(network_results)
}

VizSpots <- function(m, scaled = FALSE, cluster_color = TRUE, NSF_general_color = TRUE, type = "theme", sort_topic = TRUE){
	countryColors <- readRDS("countryColors.Rds")
	N <- nrow(countryColors)
	ind <- match(rownames(m), countryColors$country_name)
	countryColors <- as.matrix(countryColors[ind, 1:3])
	
	if (cluster_color){
		l_phcs <- readRDS("./data/l_phcs.Rds")
		cluster_descriptors <- ggplot_build(l_phcs[[1]][[3]])$data[[2]][, c("label", "group", "colour")]
		cluster_descriptors <- cluster_descriptors[which(cluster_descriptors$label %in% rownames(m)), ]
		cluster_descriptors <- cluster_descriptors[order(as.character(cluster_descriptors$label), decreasing = TRUE), ]
	}

	if (NSF_general_color & type == "NSF_specific"){
		topic_names <- read.csv("./data/topic_names.csv")
		lvls <- as.numeric(topic_names$NSF_general[match(colnames(m), topic_names$NSF_specific)])
		if (sort_topic){
			m <- m[, order(lvls)]
			lvls <- as.numeric(topic_names$NSF_general[match(colnames(m), topic_names$NSF_specific)])
		}
		NSF_general_colors <- RColorBrewer::brewer.pal(5, "Set1")[lvls]
	}
	
	grid.col <- rainbow(N)[ind]
	grid.col <- c(grid.col, rep("#c2c2c2", ncol(m)))
	circos.clear()
	par(mar = rep(0, 4), cex=.75)
	circos.par(start.degree = -90)
	chordDiagram(x = m, directional = 1, 
		transparency = 0.3,
		grid.col = grid.col,
		link.sort = TRUE,
		link.decreasing = TRUE,
		symmetric = FALSE,
		diffHeight = 0,
		scale = scaled,
		annotationTrack = NULL,
		preAllocateTracks = list(
								list(track.height = 0.03, track.margin = c(0, 0)),
								list(track.height = 2.25 * circos.par("track.height")),
	                            list(track.height = 0.02, track.margin = c(0, 0)),
	                            list(track.height = 0.02, track.margin = c(0, 0)),
	                            list(track.height = 0.02, track.margin = c(0, 0))
	                            ),
		big.gap = 30)
	for (i in seq(nrow(m))){
		highlight.sector(rownames(m)[i], track.index = 3, col = countryColors[i, 1])
		highlight.sector(rownames(m)[i], track.index = 4, col = countryColors[i, 2])
		highlight.sector(rownames(m)[i], track.index = 5, col = countryColors[i, 3])
		if (cluster_color) highlight.sector(rownames(m)[i], track.index = 1, col = cluster_descriptors$colour[i])
	}
	if (NSF_general_color & type == "NSF_specific"){
		for (i in seq(ncol(m))){	
			highlight.sector(colnames(m)[i], track.index = 3, col = NSF_general_colors[i])
			highlight.sector(colnames(m)[i], track.index = 4, col = NSF_general_colors[i])
			highlight.sector(colnames(m)[i], track.index = 5, col = NSF_general_colors[i])
		}
	} else {
		for (i in seq(ncol(m))){
			highlight.sector(colnames(m)[i], track.index = 3, col = "#c2c2c2")
			highlight.sector(colnames(m)[i], track.index = 4, col = "#c2c2c2")
			highlight.sector(colnames(m)[i], track.index = 5, col = "#c2c2c2")
		}
	}	
	# circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
	#   xlim = get.cell.meta.data("xlim")
	#   ylim = get.cell.meta.data("ylim")
	#   sector.name = get.cell.meta.data("sector.index")
	#   circos.text(mean(xlim), mean(ylim), sector.name, facing = "clockwise", niceFacing = TRUE, text.vjust = "6mm")
	#   # circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 1)
	# }, bg.border = NA)
	top_topics <- data.frame(NSF_general = topic_names$NSF_general[match(colnames(m), topic_names$NSF_specific)],
			NSF_specific = colnames(m),
			volume = colSums(m)
		)
	top_topics <- top_topics %>% 
		# group_by(NSF_general) %>%
		filter(volume >= quantile(volume, 0.75))
	for (NSF_spe in top_topics$NSF_specific){
		highlight.sector(NSF_spe,
			track.index = 2,
			text = NSF_spe,
			col = NA, 
			border = NA, 
			facing = "clockwise",
			niceFacing = TRUE
			)
	}	
	for (country in row.names(m)){
		highlight.sector(country,
			track.index = 2,
			text = country,
			cex = 1.5,
			col = NA, 
			border = NA, 
			facing = "clockwise",
			niceFacing = TRUE			)
	}
	for (i in seq_along(levels(topic_names$NSF_general))){
		NSF_gen <- levels(topic_names$NSF_general)[i]
		ind <- which(colnames(m) %in% topic_names$NSF_specific[topic_names$NSF_general == NSF_gen])
		highlight.sector(colnames(m)[ind],
			track.index = c(1,2),
			text = NSF_gen,
			col = NA, 
			border = RColorBrewer::brewer.pal(5, "Set1")[i], 
			facing = "bending.inside",
			niceFacing = TRUE,
			text.vjust = "30mm",
			cex = 1.5
			)
	}
}


