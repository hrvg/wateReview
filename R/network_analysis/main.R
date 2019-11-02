# lib
library("mlr")
library("circlize")
library("ggplot2")
library("RColorBrewer")
library("ggsci")
library("dplyr")

import::here(.from = "./R/network_analysis/lib_network_analysis.R", 
	get_network,
	VizSpots)

# bright spots
m <- get_network(type = "theme", prob = TRUE, filter_method = TRUE, blindspot = FALSE)
# blind spots
m2 <- get_network(type = "theme", prob = TRUE, filter_method = TRUE, blindspot = TRUE)

# visualisation
VizSpots(m)
VizSpots(m2)