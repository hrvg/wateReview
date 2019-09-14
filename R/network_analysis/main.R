# lib
library("mlr")
library("circlize")

# matrix multiplication
consolidated_results <- readRDS("consolidated_results.Rds")
consolidated_results <- consolidated_results[which(consolidated_results$country != "Irrelevant"), ]
docTopics <- consolidated_results[, seq(62)]
country <- consolidated_results$country
country <- as.character(country[country != "Irrelevant"])
country <- createDummyFeatures(country)
network_results <- t(as.matrix(country)) %*% as.matrix(docTopics)
dimnames(network_results) <- list(orig = rownames(network_results), dest = colnames(network_results))
countryTopics <- rowSums(network_results)
topicsCountry <- colSums(network_results)

normalized_network_results <- sweep(network_results, 1, countryTopics, "/")
dist_theme_KL <- philentropy::distance(normalized_network_results, method = "kullback-leibler")
rownames(dist_theme_KL) <- rownames(normalized_network_results)
colnames(dist_theme_KL) <- rownames(normalized_network_results)
distance <- as.dist(dist_theme_KL)
p_dist <- fviz_dist(distance, gradient = list(low = "steelblue",  high = "white"), order = TRUE)


m <- network_results
m <- apply(m, 1, function(row){
	row[row <= quantile(row, 0.90)] <- 0
	return(row)
})
m <- t(m)

# visualisation
dev.new()
circos.clear()
par(mar = rep(0, 4), cex=.75)
circos.par(start.degree = -90)
chordDiagram(x = m, directional = 1, 
	transparency = .5,
	symmetric = FALSE,
	annotationTrack = "grid", preAllocateTracks = 1,
	big.gap = 20)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)