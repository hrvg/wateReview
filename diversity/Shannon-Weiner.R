################### ideas #########################
# calculate diversity index 

# community = location:
# each paper, all papers in each country, all papers in country clusters, all papers in LAC

# species = topics: 
# NSF gemeral, NSF specific, theme, theme subsets, specific theme
# species population = probability or sum of probability


# GOAL:
output <- as.data.frame(unique(res$country))
output <- output %>%
  rename(community = `unique(res$country)`) %>%
  mutate(nsfgeneral = 0) %>%
  mutate(nsfspecific = 0) %>%
  mutate(theme = 0) %>%
  mutate(themesubset = 0) %>%
  mutate(themespecific = 0)

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



################### functions #########################

diversity_country <- function(df) { 
  df <- df %>%
    select(-c("year"))
  
  df <- melt(df, 
            id.vars = c("country"))
  
  df <- aggregate(df$value, by=list(df$country,df$variable), FUN=sum)
  
  df <- df %>%
    rename(country = Group.1, sepcies = Group.2, population = x) # species is country*topic
  
  df <- group_by(df, country)
  df <- do(df, tidy(diversity(.$population)))
  
  return(df)
}

  
remove_year_country <- function(df) {
  df <- df %>%
    filter(country != "Irrelevant") %>%
    select(-c("year","country"))
  
  return(df)
  
}

remove_irrelevant <- function(df) {
  df <- df %>%
    filter(country != "Irrelevant")
  
  return(df)
  
}

diversity_LAC <- function(df) { 
  df <- colSums(df)
  df <- diversity(df)
  
  return(df)
}


################### analysis #########################

# calculate diversity by paper / for all of LAC
clean <- remove_year_country(theme) #  specify which (species group)
diversity_paper <- diversity(clean)
diversity_LAC <- diversity_LAC(clean)

# calculate diversity by country
general2 <- remove_irrelevant(general)
specific2 <- remove_irrelevant(specific)
theme2 <- remove_irrelevant(theme)

general2 <-  diversity_country(general2)
specific2 <- diversity_country(specific2)
theme2 <- diversity_country(theme2)

diversity_by_country<-cbind(general2, specific2,theme2)
diversity_by_country <- diversity_by_country %>%
  select(-c("country1","country2")) %>%
  rename(NSFgeneral = x, NSFspecific = x1, theme = x2)

fwrite(diversity_by_country, file = "./diversity/diversity by country.csv")

################### graphs #########################

diversity_by_country_graph <- melt(diversity_by_country, 
           id.vars = c("country"))

# theme
theme_graphdf <- subset(diversity_by_country_graph, variable == "theme")
theme_graph <- ggdotchart(theme_graphdf, x = "country", y = "value", #add color = cluster
           add = "segments", sorting = "descending", rotate = TRUE, title = "theme") +
  geom_hline(yintercept = diversity_LAC, linetype = 2, color = "lightgray")

# NSF specific  
specific_graphdf <- subset(diversity_by_country_graph, variable == "NSFspecific")
specific_graph <- ggdotchart(specific_graphdf, x = "country", y = "value", #add color = cluster
                    add = "segments", sorting = "descending", rotate = TRUE, title = "specific") +
  geom_hline(yintercept = diversity_LAC, linetype = 2, color = "lightgray")

# NSF general  
general_graphdf <- subset(diversity_by_country_graph, variable == "NSFgeneral")
general_graph <- ggdotchart(general_graphdf, x = "country", y = "value", #add color = cluster
                       add = "segments", sorting = "descending", rotate = TRUE, title = "general") +
  geom_hline(yintercept = diversity_LAC, linetype = 2, color = "lightgray")

theme_graph
specific_graph
general_graph

to do : irrelevant is still there??!