### libraries ###
library("mlr")
library("OpenML")
library("NLP")
library("tm")
library("data.table")
library("mldr")
library("dplyr")
library("igraph")

### utils ###
import::here(.from = "./R/utils/lib_webscrapping.R",
  get.EndNoteIdcorpus,
  get.EndNoteIdLDA,
  QA.EndNoteIdCorpusLDA,
  align.dataWithEndNoteIdLDA,
  align.dataWithEndNoteIdcorpus,
  order.data
)


import::here(.from = "./R/network_analysis/lib_network_analysis.R", 
  make.countryNetwork,
  make.topicNetwork
 )

# data loading
englishCorpus_file <- "F:/hguillon/research/exploitation/R/latin_america/data/english_corpus.Rds"
englishCorpus <- readRDS(englishCorpus_file)

in_corpus_file <- "in_corpus.Rds"
in_corpus <- readRDS(in_corpus_file)
source_ids <- readRDS("source_ids.Rds") # aligned with corpus

citation_network <- readRDS("citingDf.Rds")

# get document IDs
EndNoteIdcorpus <- get.EndNoteIdcorpus(in_corpus)
EndNoteIdLDA <- get.EndNoteIdLDA(englishCorpus)
QA.EndNoteIdCorpusLDA(EndNoteIdLDA, EndNoteIdcorpus)

# align databases 
in_corpus <- align.dataWithEndNoteIdcorpus(in_corpus, EndNoteIdcorpus, EndNoteIdLDA)
source_ids <- align.dataWithEndNoteIdcorpus(source_ids, EndNoteIdcorpus, EndNoteIdLDA)

englishCorpus <- align.dataWithEndNoteIdLDA(englishCorpus, EndNoteIdLDA, EndNoteIdcorpus)

EndNoteIdLDA <- align.dataWithEndNoteIdLDA(EndNoteIdLDA, EndNoteIdLDA, EndNoteIdcorpus)
EndNoteIdcorpus <- align.dataWithEndNoteIdcorpus(EndNoteIdcorpus, EndNoteIdcorpus, EndNoteIdLDA)

QA.EndNoteIdCorpusLDA(EndNoteIdLDA, EndNoteIdcorpus)

# order them according to LDA database
source_ids <- order.data(source_ids, EndNoteIdLDA, EndNoteIdcorpus)
in_corpus <- order.data(in_corpus, EndNoteIdLDA, EndNoteIdcorpus)

predCountryMembership <- readRDS("predCountryMembership.Rds")
colnames(predCountryMembership) <- gsub("prob.", "", colnames(predCountryMembership))

# make the networks
countryNetwork <- make.countryNetwork(source_ids, citation_network)

type_list <- c("methods", "NSF_general", "NSF_specific", "spatial scale", "theme", "water budget")
for (tt in type_list) make.topicNetwork(source_ids, citation_network, type = tt, percentile.threshold = 0.90)



# # visualisation for country
# library("circlize")
# countryColors <- readRDS("countryColors.Rds")
# grid.col <- rev(rainbow(ncol(m)))
# dev.new()
# circos.clear()
# par(mar = rep(0, 4), cex=1)
# circos.par(start.degree = 0)
# chordDiagram(x = m, directional = 1, 
# 	grid.col = grid.col,
# 	transparency = 0.2,
# 	link.sort = TRUE,
# 	self.link = 2,
# 	scale = TRUE,
# 	link.decreasing = TRUE,
# 	symmetric = FALSE,
# 	diffHeight = 0,
# 	annotationTrack = NULL,
# 	preAllocateTracks = list(
# 							list(track.height = circos.par("track.height")),
#                             list(track.height = 0.02, track.margin = c(0, 0)),
#                             list(track.height = 0.02, track.margin = c(0, 0)),
#                             list(track.height = 0.02, track.margin = c(0, 0))
#                             ),
# 	big.gap = 45)
# circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
#   xlim = get.cell.meta.data("xlim")
#   ylim = get.cell.meta.data("ylim")
#   sector.name = get.cell.meta.data("sector.index")
#   circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
#   # circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 1)
# }, bg.border = NA)
# for (i in seq(nrow(m))){
# 	highlight.sector(rownames(m)[i], track.index = 2, col = countryColors[i, 1])
# 	highlight.sector(rownames(m)[i], track.index = 3, col = countryColors[i, 2])
# 	highlight.sector(rownames(m)[i], track.index = 4, col = countryColors[i, 3])
# }

# # 

# # visualisation
# grid.col <- rev(rainbow(ncol(m)))

# dev.new()
# circos.clear()
# par(mar = rep(0, 4), cex=1)
# circos.par(start.degree = 0)
# chordDiagram(x = m, directional = 1, 
# 	grid.col = grid.col,
# 	transparency = 0.2,
# 	link.sort = TRUE,
# 	self.link = 2,
# 	link.decreasing = TRUE,
# 	symmetric = FALSE,
# 	diffHeight = .05,
# 	annotationTrack = NULL,
# 	preAllocateTracks = list(
# 							list(track.height = circos.par("track.height")),
#                             list(track.height = 0.02, track.margin = c(0, 0))
#                             ),
# 	big.gap = 45)
# circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
#   xlim = get.cell.meta.data("xlim")
#   ylim = get.cell.meta.data("ylim")
#   sector.name = get.cell.meta.data("sector.index")
#   circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
#   # circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 1)
# }, bg.border = NA)
# for (i in seq(ncol(m))){
# 	highlight.sector(colnames(m)[i], track.index = 2, col = grid.col[i])
# }