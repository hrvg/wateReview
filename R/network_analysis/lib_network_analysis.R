get_network <- function(type = "theme", prob = TRUE, filter_method = FALSE, blindspot = FALSE, country.threshold = 30, percentile.threshold = 0.90){
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

	dimnames(network_results) <- list(orig = rownames(network_results), dest = colnames(network_results))
	countryTopics <- rowSums(network_results)
	topicsCountry <- colSums(network_results)
	
	if (filter_method & type == "theme"){
		topic_names <- read.csv("./data/topic_names.csv")
		method_topics <- as.character(topic_names$theme[grepl("methods", topic_names$description)])
		network_results <- network_results[, which(!colnames(network_results) %in% method_topics)]
	}

	if(blindspot){
		network_results <- 1 / network_results
	}

	tab <- table(country)
	keepCountries <- names(tab)[which(tab >= country.threshold)]

	network_results <- network_results[rownames(network_results) %in% keepCountries, ]
	network_results <- apply(network_results, 1, function(row){
		row[row <= quantile(row, percentile.threshold)] <- 0
		return(row)
	})
	network_results <- t(network_results)
	network_results <- network_results[, which(colSums(network_results) != 0)]

	network_results <- network_results[order(rownames(network_results), decreasing = TRUE), ]
	rownames(network_results) <- gsub("Costa.Rica", "Costa Rica", rownames(network_results))
	rownames(network_results) <- gsub("El.Salvador", "El Salvador", rownames(network_results))
	return(network_results)
}

VizSpots <- function(m, scaled = FALSE){
	countryColors <- readRDS("countryColors.Rds")
	countryColors <- as.matrix(countryColors[match(rownames(m), countryColors$country_name), 1:3])
	
	l_phcs <- readRDS("./data/l_phcs.Rds")
	cluster_descriptors <- ggplot_build(l_phcs[[1]][[3]])$data[[2]][, c("label", "group", "colour")]
	cluster_descriptors <- cluster_descriptors[which(cluster_descriptors$label %in% rownames(m)), ]
	cluster_descriptors <- cluster_descriptors[order(as.character(cluster_descriptors$label), decreasing = TRUE), ]
	
	grid.col <- rev(pal_igv("default")(nrow(m)))
	grid.col <- rainbow(nrow(m))
	grid.col <- c(grid.col, rep("#c2c2c2", ncol(m)))
	circos.clear()
	par(mar = rep(0, 4), cex=.75)
	circos.par(start.degree = -90)
	chordDiagram(x = m, directional = 1, 
		transparency = 0.2,
		grid.col = grid.col,
		link.sort = TRUE,
		link.decreasing = TRUE,
		symmetric = FALSE,
		diffHeight = 0,
		scale = scaled,
		annotationTrack = NULL,
		preAllocateTracks = list(
								list(track.height = 0.03, track.margin = c(0, 0)),
								list(track.height = circos.par("track.height")),
	                            list(track.height = 0.02, track.margin = c(0, 0)),
	                            list(track.height = 0.02, track.margin = c(0, 0)),
	                            list(track.height = 0.02, track.margin = c(0, 0))
	                            ),
		big.gap = 45)
	circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
	  xlim = get.cell.meta.data("xlim")
	  ylim = get.cell.meta.data("ylim")
	  sector.name = get.cell.meta.data("sector.index")
	  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
	  # circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 1)
	}, bg.border = NA)
	for (i in seq(nrow(m))){
		highlight.sector(rownames(m)[i], track.index = 3, col = countryColors[i, 1])
		highlight.sector(rownames(m)[i], track.index = 4, col = countryColors[i, 2])
		highlight.sector(rownames(m)[i], track.index = 5, col = countryColors[i, 3])
		highlight.sector(rownames(m)[i], track.index = 1, col = cluster_descriptors$colour[i])
	}
	for (i in seq(ncol(m))){
		highlight.sector(colnames(m)[i], track.index = 3, col = "#c2c2c2")
		highlight.sector(colnames(m)[i], track.index = 4, col = "#c2c2c2")
		highlight.sector(colnames(m)[i], track.index = 5, col = "#c2c2c2")
	}
}