# loading citation network
citation_network <- readRDS("citingDf.Rds")
source_ids <- readRDS("source_ids.Rds")

# load paper id
in_corpus_file <- "in_corpus.Rds"
in_corpus <- readRDS(in_corpus_file)

EndNoteIdcorpus <- unname(sapply(in_corpus$pdfs, substr, start = 1, stop = 10))
titleDocs <- readLines("/media/hguillon/hrvg/research/data/latin_america/water-management/topic-model/data/info.dat")
EndNoteIdLDA <- unname(sapply(titleDocs, substr, start = 1, stop = 10))

in_corpus_LDA <- in_corpus[which(EndNoteIdcorpus %in% EndNoteIdLDA), ]
EndNoteIdcorpus_LDA <- EndNoteIdcorpus[which(EndNoteIdcorpus %in% EndNoteIdLDA)]
source_ids_LDA <- source_ids[which(EndNoteIdcorpus %in% EndNoteIdLDA)]

# loading consolidated results
missing <- which(! EndNoteIdLDA %in% EndNoteIdcorpus)
consolidated_results <- readRDS("consolidated_results.Rds")
consolidated_results$ID <- EndNoteIdLDA[-missing]
consolidated_results$source_ids <- source_ids_LDA

consolidated_network <- consolidated_results[which(consolidated_results$country != "Irrelevant"), ]
consolidated_network <- consolidated_network[which(consolidated_network$source_ids %in% citation_network$citing), ]
relevant_network <- citation_network[citation_network$citing %in% consolidated_network$source_ids, ]
relevant_network <- relevant_network[relevant_network$cited %in% consolidated_network$source_ids, ]


relevant_network$citing_country <- consolidated_network$country[match(relevant_network$citing, consolidated_network$source_ids)]
relevant_network$cited_country <- consolidated_network$country[match(relevant_network$cited, consolidated_network$source_ids)]
relevant_network <- relevant_network[relevant_network$cited_country %in% relevant_network$citing_country, ]
relevant_network$citing_country <- as.factor(as.character(relevant_network$citing_country))
relevant_network$cited_country <- factor(as.character(relevant_network$cited_country), levels = levels(relevant_network$citing_country))

counts <- lapply(levels(relevant_network$citing_country), function(c){
	df <- relevant_network[relevant_network$citing_country == c, ]
	return(table(df$cited_country))
})
counts <- do.call(rbind, counts)
rownames(counts) <- levels(relevant_network$citing_country)


library("circlize")

countryColors <- readRDS("countryColors.Rds")[levels(consolidated_results$country)[levels(consolidated_results$country) != "Irrelevant"] %in% colnames(counts), ]

m <- counts
rownames(m) <- gsub("Costa.Rica", "Costa Rica", rownames(m))
rownames(m) <- gsub("El.Salvador", "El Salvador", rownames(m))
colnames(m) <- rownames(m)
grid.col <- rev(rainbow(ncol(m)))

# visualisation
dev.new()
circos.clear()
par(mar = rep(0, 4), cex=1)
circos.par(start.degree = 0)
chordDiagram(x = m, directional = 1, 
	grid.col = grid.col,
	transparency = 0.2,
	link.sort = TRUE,
	self.link = 2,
	link.decreasing = TRUE,
	symmetric = FALSE,
	diffHeight = 0,
	annotationTrack = NULL,
	preAllocateTracks = list(
							list(track.height = circos.par("track.height")),
                            list(track.height = 0.02, track.margin = c(0, 0)),
                            list(track.height = 0.02, track.margin = c(0, 0)),
                            list(track.height = 0.02, track.margin = c(0, 0))
                            ),
	big.gap = 45)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  # circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 1)
}, bg.border = NA)
for (i in seq(nrow(m))){
	highlight.sector(rownames(m)[i], track.index = 2, col = countryColors[i, 1])
	highlight.sector(rownames(m)[i], track.index = 3, col = countryColors[i, 2])
	highlight.sector(rownames(m)[i], track.index = 4, col = countryColors[i, 3])
}

# 
docTopics <- consolidated_results[, seq(62)]