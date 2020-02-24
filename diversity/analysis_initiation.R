setwd("~/R_code/LAC")
source("./diversity/lib_lac.R")

################### libraries #########################
library(dplyr)
library(vegan)
library(broom)
library(reshape2)
library(ggpubr)
library(data.table)
library(wesanderson)
library(ggplot2)
library(philentropy)
library(scales)
library(ggplotify)
library(plotly)
library(tidyr)
library(tidyverse)
library(ggrepel)

################### raw files #########################
general <- readRDS("./consolidated_results_NSF_general.Rds")
specific <- readRDS("./consolidated_results_NSF_specific.Rds") # 45 themes
methods <- readRDS("./consolidated_results_methods.Rds")
budget <- readRDS("./consolidated_results_water budget.Rds")
theme <- readRDS("./consolidated_results_theme.Rds")