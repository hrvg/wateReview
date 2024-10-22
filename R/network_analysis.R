#' Creates the adjencency matrix of a bi-partite network: country to topics
#' @param type one of "methods", "NSF_general", "NSF_specific", "spatial scale", "theme", "water budget"
#' @param prob logical, define if the country labels are to be treated as probabilities
#' @param filter_method logical, controls if methods topics are removed
#' @param blindspot logical, controls if the network shows blindspots, if so the network is inverted
#' @param country.threshold numeric, minimum number of documents per country
#' @param percentile.threshold numeric, controls at which percentile the adjacency matrix is filtered for visualization
#' @param country_filter logical, controls if countries should be filtered, see `countries`
#' @param countries character, filter for the provided countries
#' @param theme_filter logical, controls if themes should be filtered, see `themes`
#' @param themes character, filter for the provided themes
#' @return a matrix
#' @export
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
	consolidated_results <- readRDS(system.file("extdata", paste0("consolidated_results_", type, ".Rds"), package = "wateReview"))
	docTopics <- consolidated_results %>% dplyr::filter(country != "Irrelevant") %>% dplyr::select(-c("year","country"))

	predCountryMembership <- readRDS(system.file("extdata", "predCountryMembership.Rds", package = "wateReview"))
	colnames(predCountryMembership) <- gsub("prob.", "", colnames(predCountryMembership))
	predCountryMembership <- predCountryMembership[which(consolidated_results$country != "Irrelevant"), ]
	
	country <- consolidated_results$country
	country <- as.character(country[country != "Irrelevant"])

	if (prob){
		network_results <- t(predCountryMembership) %*% as.matrix(docTopics)
	} else {
		df_country <- mlr::createDummyFeatures(country)
		network_results <- t(as.matrix(df_country)) %*% as.matrix(docTopics)
	}

	rownames(network_results) <- gsub("Costa.Rica", "Costa Rica", rownames(network_results))
	rownames(network_results) <- gsub("El.Salvador", "El Salvador", rownames(network_results))

	if (!theme_filter){
		if (filter_method & type == "theme"){
			topic_names <- read.csv(system.file("extdata", "topic_names.csv", package = "wateReview"))
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
		keepCountries <- gsub("Costa.Rica", "Costa Rica", keepCountries)
		keepCountries <- gsub("El.Salvador", "El Salvador", keepCountries)
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

#' Produces a chord diagram visualization with country cluster colors and topics categories
#' @param m matrix, weighted adjacency matrix with rows indicating origin and columns indicating destination
#' @param scaled logical, default to FALSE
#' @param cluster_color logical, default to TRUE
#' @param NSF_general_color logical, default to TRUE
#' @param type character, default to "theme"
#' @param sort_topic logical, default to TRUE
#' @param topic_threshold numeric, a percentile to cut the display of topic names
#' @param reorder_cluster logical, default to FALSE, controls if the origin is re-ordered by cluster
#' @return a chord diagram figure
#' @export
#' @import circlize
#' @importFrom magrittr %>%
VizSpots <- function(m, scaled = FALSE, cluster_color = TRUE, NSF_general_color = TRUE, type = "theme", sort_topic = TRUE, topic_threshold = 0.75, reorder_cluster = FALSE){
	countryColors <- readRDS(system.file("extdata", "countryColors.Rds", package = "wateReview"))
	N <- nrow(countryColors)
	ind <- match(rownames(m), countryColors$country_name)
	countryColors <- as.matrix(countryColors[ind, 1:3])

	if (cluster_color){
		l_phcs <- readRDS(system.file("extdata", "l_phcs.Rds", package = "wateReview"))
		cluster_descriptors <- ggplot2::ggplot_build(l_phcs[[1]][[3]])$data[[2]][, c("label", "group", "colour")]
		cluster_descriptors <- cluster_descriptors[which(cluster_descriptors$label %in% rownames(m)), ]
		cluster_descriptors <- cluster_descriptors[order(as.character(cluster_descriptors$label), decreasing = TRUE), ]
		if (reorder_cluster){
			cluster_descriptors$group[cluster_descriptors$group == 1] <- 3 # fixing the random ordering from statistical clustering
			ind_reorder <- order(cluster_descriptors$group)
			m <- m[ind_reorder, ]
			countryColors <- countryColors[ind_reorder, ]
			cluster_descriptors <- cluster_descriptors[ind_reorder, ]
		}
	}

	if (NSF_general_color & type == "NSF_specific"){
		topic_names <- read.csv(system.file("extdata", "topic_names.csv", package = "wateReview"))
		lvls <- as.numeric(topic_names$NSF_general[match(colnames(m), topic_names$NSF_specific)])
		if (sort_topic){
			m <- m[, order(lvls)]
			lvls <- as.numeric(topic_names$NSF_general[match(colnames(m), topic_names$NSF_specific)])
		}
		NSF_general_colors <- RColorBrewer::brewer.pal(5, "Set1")[lvls]
	}
	
	grid.col <- inlmisc::GetColors(N, scheme = "smooth rainbow", reverse = TRUE, bias = 0.9, start = 0.1, end = .9)[ind]
	grid.col <- c(grid.col, rep("#c2c2c2", ncol(m)))
	circos.clear()
	par(mar = rep(0, 4), cex=.75)
	circos.par(start.degree = -90)
	chordDiagram(x = m, directional = 1, 
		transparency = 0.1,
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
		dplyr::filter(volume >= quantile(volume, topic_threshold))
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

#' Creates a weighted adjacency matrix of probability of citation between countries
#' @param source_ids source_ids
#' @param citation_network an edge data.frame
#' @return a weighted adjacency matrix of probability of citation between countries
make_countryNetwork <- function(source_ids, citation_network){
	consolidated_results <- readRDS("consolidated_results_topic_name.Rds")
	consolidated_results$source_ids <- source_ids

	consolidated_network <- consolidated_results[which(consolidated_results$country != "Irrelevant"), ]
	consolidated_network <- consolidated_network[which(consolidated_network$source_ids %in% citation_network$citing), ]

	consolidated_predCountryMembership <- predCountryMembership[which(consolidated_results$country != "Irrelevant"), ]
	consolidated_predCountryMembership <- consolidated_predCountryMembership[which(consolidated_network$source_ids %in% citation_network$citing), ]

	relevant_network <- citation_network[citation_network$citing %in% consolidated_network$source_ids, ]
	relevant_network <- relevant_network[relevant_network$cited %in% consolidated_network$source_ids, ]
	relevant_network$citing_country <- consolidated_network$country[match(relevant_network$citing, consolidated_network$source_ids)]
	relevant_network$cited_country <- consolidated_network$country[match(relevant_network$cited, consolidated_network$source_ids)]
	relevant_network <- relevant_network[relevant_network$cited_country %in% relevant_network$citing_country, ]
	relevant_network$citing_country <- as.factor(as.character(relevant_network$citing_country))
	relevant_network$cited_country <- factor(as.character(relevant_network$cited_country), levels = levels(relevant_network$citing_country))

	# weighted adjacency matrix with country as vertex and number of citations as weight
	g <- graph_from_data_frame(relevant_network[, 1:2])
	adj <- as.matrix(get.adjacency(g))

	consolidated_predCountryMembership$source_ids <- consolidated_network$source_ids
	countryNetwork <- consolidated_predCountryMembership[which(consolidated_network$source_ids %in% colnames(adj)), ]
	countryNetwork <- countryNetwork[match(colnames(adj), countryNetwork$source_ids), ]
	countryNetwork$source_ids <- NULL

	network_results <- t(as.matrix(countryNetwork)) %*% as.matrix(adj) %*% as.matrix(countryNetwork)

	m <- network_results
	rownames(m) <- gsub("Costa.Rica", "Costa Rica", rownames(m))
	rownames(m) <- gsub("El.Salvador", "El Salvador", rownames(m))
	colnames(m) <- rownames(m)
	saveRDS(m, "countryNetwork.Rds")
	return(m)
}

#' Creates a weighted adjacency matrix of probability of citation between topics
#' @param source_ids source_ids
#' @param citation_network an edge data.frame
#' @return a weighted adjacency matrix of probability of citation between topics
make_topicNetwork <- function(source_ids, citation_network, type = "NSF_specific", percentile.threshold = 0.90){
	type_list <- c("methods", "NSF_general", "NSF_specific", "spatial scale", "theme", "water budget")
	stopifnot(type %in% type_list)
	consolidated_results <- readRDS(paste0("consolidated_results_", type, ".Rds"))
	consolidated_results$source_ids <- source_ids

	consolidated_network <- consolidated_results[which(consolidated_results$country != "Irrelevant"), ]
	consolidated_network <- consolidated_network[which(consolidated_network$source_ids %in% citation_network$citing), ]

	consolidated_predCountryMembership <- predCountryMembership[which(consolidated_results$country != "Irrelevant"), ]
	consolidated_predCountryMembership <- consolidated_predCountryMembership[which(consolidated_network$source_ids %in% citation_network$citing), ]

	relevant_network <- citation_network[citation_network$citing %in% consolidated_network$source_ids, ]
	relevant_network <- relevant_network[relevant_network$cited %in% consolidated_network$source_ids, ]

	relevant_network$citing_country <- consolidated_network$country[match(relevant_network$citing, consolidated_network$source_ids)]
	relevant_network$cited_country <- consolidated_network$country[match(relevant_network$cited, consolidated_network$source_ids)]
	relevant_network <- relevant_network[relevant_network$cited_country %in% relevant_network$citing_country, ]
	relevant_network$citing_country <- as.factor(as.character(relevant_network$citing_country))
	relevant_network$cited_country <- factor(as.character(relevant_network$cited_country), levels = levels(relevant_network$citing_country))

	# df <- as.matrix(get.adjacency(graph.data.frame(relevant_network[, 3:4])))
	g <- graph_from_data_frame(relevant_network[, 1:2])
	adj <- as.matrix(get.adjacency(g))

	topicNetwork <- consolidated_network[which(consolidated_network$source_ids %in% colnames(adj)), ]
	topicNetwork <- topicNetwork[match(colnames(adj), topicNetwork$source_ids), ]
	docTopics <- dplyr::select(topicNetwork, -c("year", "source_ids", "country"))

	network_results <- t(as.matrix(docTopics)) %*% as.matrix(adj) %*% as.matrix(docTopics)

	m <- network_results
	saveRDS(m, paste0("rawTopicNetwork_", type, ".Rds"))
	m <- apply(m, 1, function(row){
		row[row <= quantile(row, percentile.threshold)] <- 0
		return(row)
	})
	m <- t(m)
	saveRDS(m, paste0("topicNetwork_", type, ".Rds"))
}


rgb2hex <- function(r,g,b) sprintf('#%s',paste(as.hexmode(c(r,g,b)),collapse = ''))

get.topColors <- function(iso, n = 3){
	source <- paste0("../country-flags/png100px/", iso, ".png")
	img <- readPNG(source, native = FALSE, info = FALSE)
	red <- c(img[,, 1] * 255)
	green <- c(img[,, 2] * 255)
	blue <- c(img[,, 3] * 255)
	img <- cbind(red, green, blue)
	colors <- apply(img, MARGIN = 1, function(row) rgb2hex(row[1], row[2], row[3]))
	topColors <- rev(names(tail(sort(table(colors)), n)))
	return(topColors)
}

#' Normalize an adjacency matrix by its rows (i.e., "from")
#' @param adj a matrix
#' @return a normalized matrix
#' @export
normalize_adj_matrix <- function(adj){
	rSums <- rowSums(adj)
	normalized_adj_matrix <- sweep(adj, MARGIN = 1, rSums, "/")
	return(normalized_adj_matrix)
}