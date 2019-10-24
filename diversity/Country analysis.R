################### libraries #########################
library(dplyr)
library(vegan)
library(broom)
library(reshape2)
library(ggpubr)
library(data.table)


################### files #########################
general <- readRDS("./consolidated_results_NSF_general.Rds")
specific <- readRDS("./consolidated_results_NSF_specific.Rds")
theme <- readRDS("./consolidated_results_theme.Rds")
methods <- readRDS("./consolidated_results_methods.Rds")
budget <- readRDS("./consolidated_results_water budget.Rds")


################### functions #########################
subset
diversity of papers
place in LAC




################### analysis #########################

# subset by country
general <- subset(general,country == "Mexico")
specific <- subset(specific,country == "Mexico")
theme <- subset(theme,country == "Mexico")
methods <- subset(methods,country == "Mexico")
budget <- subset(budget,country == "Mexico")


# 4 320 papers in mexico

# analyze breakdown of methods and water budget
methods <- methods %>%
  select(-c("year","country"))

methods <- as.data.frame(colSums(methods))

plot(methods)

