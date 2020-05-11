### libraries ###
library("circlize")
library("cowplot")
library("factoextra")
library("forcats")
library("ggplot2")
library("reshape2")
library("oce")
library("RColorBrewer")
library("dplyr")
library("ggpubr")
library("plotly")

### functions ###
import::here(.from = "./R/network_analysis/lib_network_analysis.R", 
  get_network,
  VizSpots)

out.dir <- "/media/hguillon/hrvg/research/production/figure/latin_america/chord_diagrams"

pdf(file.path(out.dir, "all.pdf"), width = 9, height = 9)
for (pct in c(0, 25, 50, 75, 95)){
	m <- get_network(type = "NSF_specific", prob = TRUE, filter_method = FALSE, blindspot = FALSE, country.threshold = 30, percentile.threshold = pct / 100)
	for (tpt in c(0.5, 0.75)){
		# pdf(file.path(out.dir, paste0(pct, "-", tpt, ".pdf")), width = 9, height = 9)
		VizSpots(m, FALSE, cluster_color = TRUE, NSF_general_color = TRUE, type = "NSF_specific", topic_threshold = tpt)

	}
}
dev.off()