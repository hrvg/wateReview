# lib
library("mlr")
library("circlize")

# matrix multiplication
consolidated_results <- readRDS("consolidated_results.Rds")
consolidated_results <- consolidated_results[which(consolidated_results$country != "Irrelevant"), ]
docTopics <- consolidated_results[, seq(62)]
country <- consolidated_results$country
country <- as.character(country[country != "Irrelevant"])
df_country <- createDummyFeatures(country)
network_results <- t(as.matrix(df_country)) %*% as.matrix(docTopics)
dimnames(network_results) <- list(orig = rownames(network_results), dest = colnames(network_results))
countryTopics <- rowSums(network_results)
topicsCountry <- colSums(network_results)

# normalized_network_results <- sweep(network_results, 1, countryTopics, "/")
# dist_theme_KL <- philentropy::distance(normalized_network_results, method = "kullback-leibler")
# rownames(dist_theme_KL) <- rownames(normalized_network_results)
# colnames(dist_theme_KL) <- rownames(normalized_network_results)
# distance <- as.dist(dist_theme_KL)
# p_dist <- fviz_dist(distance, gradient = list(low = "steelblue",  high = "white"), order = TRUE)

m <- network_results
# filter country with less than 30 papers
indCountry <- which(table(country) >= 30)
m <- m[indCountry, ]
m <- apply(m, 1, function(row){
	row[row <= quantile(row, 0.90)] <- 0
	return(row)
})
m <- t(m)
m <- m[, which(colSums(m) != 0)]
rownames(m) <- gsub("Costa.Rica", "Costa Rica", rownames(m))
rownames(m) <- gsub("El.Salvador", "El Salvador", rownames(m))





topic_names <- read.csv("./data/topic_names.csv")
countryColors <- readRDS("countryColors.Rds")[indCountry, ][order(rownames(m), decreasing = TRUE), ]
m <- m[order(rownames(m), decreasing = TRUE), ]

# grid.col <- apply(countryColors, 1, function(row) DescTools::MixColor(row[1], row[2]))
library(RColorBrewer)
library(ggsci)
grid.col <- rev(pal_igv("default")(nrow(m)))
grid.col <- rainbow(nrow(m))
grid.col <- c(grid.col, rep("#c2c2c2", ncol(m)))

# get theme_df
l_phcs <- readRDS("./data/l_phcs.Rds")
library(ggplot2)
cluster_descriptors <- ggplot_build(l_phcs[[1]][[3]])$data[[2]][, c("label", "group", "colour")]
cluster_descriptors <- cluster_descriptors[which(cluster_descriptors$label %in% rownames(m)), ]
cluster_descriptors <- cluster_descriptors[order(as.character(cluster_descriptors$label), decreasing = TRUE), ]

# border_mat <- lapply(countryColors[, 2], function(col) rep(col, ncol(m)))
# border_mat <- do.call(rbind, border_mat)
# colnames(border_mat) <- colnames(m)
# rownames(border_mat) <- rownames(m)
# lwd_mat <- matrix(2, nrow = nrow(m), ncol = ncol(m))
# colnames(lwd_mat) <- colnames(m)
# rownames(lwd_mat) <- rownames(m)

# visualisation
dev.new()
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
	scale = TRUE,
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

