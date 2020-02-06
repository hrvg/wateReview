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
library(wesanderson)


################### files #########################
general <- readRDS("./consolidated_results_NSF_general.Rds")
specific <- readRDS("./consolidated_results_NSF_specific.Rds")
theme <- readRDS("./consolidated_results_theme.Rds")
methods <- readRDS("./consolidated_results_methods.Rds")
budget <- readRDS("./consolidated_results_water budget.Rds")



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

remove_year <- function(df) {
  df <- df %>%
    select(-c("year"))
  
  return(df)
  
}

remove_country <- function(df) {
  df <- df %>%
    select(-c("country"))
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

sum_country <- function(df) {
  
  sumdf <- data.frame(value=apply(df,2,sum))
  sumdf$key=rownames(sumdf)
  
  return(sumdf)
  
}

sum_country_by_year <- function(df) {
  
  df<- aggregate(df$x, by=list(df$Group.2), FUN=sum)
  
  
  return(df)
  
}

################### diversity analysis #########################

# calculate diversity by paper / for all of LAC
clean <- remove_year_country(general) #  specify which (species group)
diversity_LA <- diversity_LAC(clean)
diversity_paper <- diversity(clean)

# check - diveristy function from vegan package works
check <- as.data.frame(colSums(clean))
sum <- sum(check)
check$pi <- check$`colSums(clean)`/sum
check$lnpi <- log(check$pi)
check$pilnpi <- check$pi * check$lnpi
H <- -sum(check$pilnpi)
  
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

################### diversity graphs #########################

diversity_by_country_graph <- melt(diversity_by_country, 
           id.vars = c("country"))

# theme
theme_graphdf <- subset(diversity_by_country_graph, variable == "theme")
theme_graph <- ggdotchart(theme_graphdf, x = "country", y = "value", #add color = cluster
           add = "segments", sorting = "descending", rotate = TRUE, title = "theme") +
  geom_hline(yintercept = diversity_LA, linetype = 2, color = "lightgray")


# NSF specific  
specific_graphdf <- subset(diversity_by_country_graph, variable == "NSFspecific")
specific_graph <- ggdotchart(specific_graphdf, x = "country", y = "value", #add color = cluster
                    add = "segments", sorting = "descending", rotate = TRUE, title = "specific")
# NSF general  
general_graphdf <- subset(diversity_by_country_graph, variable == "NSFgeneral")
general_graph <- ggdotchart(general_graphdf, x = "country", y = "value", #add color = cluster
                       add = "segments", sorting = "descending", rotate = TRUE, title = "general")
theme_graph
specific_graph
general_graph


ggscatter(diversity_by_country, x = "NSFgeneral", y = "theme", color = "country")



################### corpus analysis  #########################
general_corpus <- remove_year_country(general) 
general_corpus <- sum_country(general_corpus)

theme_corpus <- remove_year_country(theme) 
theme_corpus <- sum_country(theme_corpus)
################### corpus analysis graphs #########################

pie(general_corpus$value, labels = general_country2$key, main="Distribution of water research
based on NSF general categories - corpus")


################### country level analysis #########################

country_pick <- "Mexico"

theme_country <- subset(theme, country == country_pick)
theme_country2 <- remove_year_country(theme_country)
theme_country2 <- sum_country(theme_country2)
theme_country3 <- remove_country(theme_country)
theme_country3 <- melt(theme_country3,
                       id.vars = "year")
theme_country3 <- sum_country_by_year(theme_country3)
theme_country3 <- subset(theme_country3, Group.1 != 2018)
theme_country3$cum  <- cumsum(theme_country3$x)


budget_country <- subset(budget, country == country_pick)
budget_country2 <- remove_year_country(budget_country)
budget_country2 <- sum_country(budget_country2)

methods_country <- subset(methods, country == country_pick)
methods_country2 <- remove_year_country(methods_country)
methods_country2 <- sum_country(methods_country2)

general_country <- subset(general, country == country_pick)
general_country2 <- remove_year_country(general_country)
general_country2 <- sum_country(general_country2)

################### country level graphs #########################


ggdotchart(theme_country2, 
           x= "key", y = "value", 
           rotate = TRUE, add = "segments", sorting = "descending", 
           title = "theme")

ggdotchart(budget_country2, 
           x= "key", y = "value", 
           rotate = TRUE, add = "segments", sorting = "descending", 
           title = "water budget components")

ggdotchart(methods_country2, 
           x= "key", y = "value", 
           rotate = TRUE, add = "segments", sorting = "descending", 
           title = "methods")

ggline(theme_country3, x = "Group.1", y = "x") # annual 
ggline(theme_country3, x = "Group.1", y = "cum") # cummulative
ggplot(theme_country3) +
  geom_line(aes(x = Group.1, y = x), linetype = "dashed") + 
  geom_line(aes(x = Group.1, y = cum)) +
  theme_pubr() +
  labs(y = "Number of papers on water in Mexico", x=NULL)


pie(general_country2$value, labels = general_country2$key, main="Distribution of water research
based on NSF general categories")

# ggpie(general_country2, "value", label = "key",
      #fill = "group",
      #palette = scale_fill_manual(values = wes_palette(n=5, name ="Darjeeling1")))


################### topic level analysis #########################

topic_pick <- "climate change impacts"

theme_topic <- remove_irrelevant(theme) # define correct data frame

theme_topic <- melt(theme_topic,
              id.vars = c("year","country"))

theme_topic <- subset(theme_topic, variable == topic_pick)


# sum by country
theme_topic <- theme_topic %>%
  select(-c("year","variable"))
theme_topic<- aggregate(theme_topic$value, by=list(theme_topic$country), FUN=sum)

# compare to corpus
theme_topic2 <- cbind(theme_topic,theme_graphdf)
theme_topic2$perc <- theme_topic2$value / theme_topic2$x

# sum by year
df2 <- theme_topic %>%
  select(-c("country","variable"))
df2<- aggregate(df2$value, by=list(df2$year), FUN=sum)


# graph

ggdotchart(df, 
           x= "Group.1", y = "x", 
           rotate = TRUE, add = "segments", sorting = "descending", 
           title = topic_pick)

ggplot(df2, aes(x = Group.1, y = x)) + 
  geom_area() +
  theme_pubr() +
  labs(title = topic_pick)


# combine
risk_country <- df
risk_yar <- df2

climate_country <- df
climate_year <- df2

df <-cbind(risk_country, climate_country)
names(df)[2]<-"risk assessment"
names(df)[4]<-"climate change impacts"

df2 <-cbind(risk_yar, climate_year)
names(df2)[2]<-"risk assessment"
names(df2)[4]<-"climate change impacts"


df[3] <- NULL
df2[3] <- NULL

df <- melt(df,
           id.vars = "Group.1")

df2 <- melt(df2,
           id.vars = "Group.1")

################### topic level graphs #########################

ggdotchart(df, 
           x= "Group.1", y = "value", color = "variable", 
           rotate = TRUE, add = "segments", sorting = "descending", 
           title = topic_pick)

ggplot(df2, aes(x = Group.1, y = value, color = variable)) + 
  geom_area() +
  theme_pubr() +
  labs(title = topic_pick)

################### chronological analysis #########################
library(viridis)
library(hrbrthemes)

general <- readRDS("./consolidated_results_NSF_general.Rds")
general <- subset(general, select = -c(country))
general_long <- gather(general, topic, prop, 'physical sciences':'social sciences', factor_key = TRUE)
pivot <- with(general_long, tapply(prop, list(year, topic), sum))
pivot <- pivot[!(row.names(pivot) %in% 2018),]
years <- row.names(pivot)
rownames(pivot) <- NULL
pivot <- cbind.data.frame(years, pivot)
pivot$years = strtoi(pivot$years)
pivot <- complete(pivot, 
                  years = full_seq(years, period = 1), 
                  fill = list(`social sciences` = 0, `physical sciences` = 0, `life sciences` = 0, `mathematics & statistics`=0,engineering=0))
pivot_long <- gather(pivot, topic, prop, 'physical sciences':'social sciences', factor_key = TRUE)
ggplot(pivot_long, aes(x=years, y=prop, fill=topic)) + 
  geom_area(alpha=0.6 , size=.5, colour="white") +
  scale_y_continuous(trans='pseudo_log') +
  scale_fill_viridis(discrete = T) +
  ggtitle("Topics over time")

general <- readRDS("./consolidated_results_NSF_general.Rds")
general <- general[6:7]
general <- remove_irrelevant(general)
pivot <- with(general, tapply(country, list(year, country), length))
years <- row.names(pivot)
rownames(pivot) <- NULL
pivot <- cbind.data.frame(years, pivot)
pivot <- pivot[!(pivot$years == 2018),]
pivot$years = strtoi(pivot$years)
pivot <- complete(pivot, 
                  years = full_seq(years, period = 1))
pivot_long <- gather(pivot, country, prop, Argentina:Venezuela, factor_key = TRUE)
ggplot(pivot_long, aes(x=years, y=prop, fill=country)) + 
  geom_area(alpha=0.6 , size=.5, colour="white") +
  scale_y_continuous(trans='pseudo_log') +
  scale_fill_viridis(discrete = T) +
  ggtitle("Topics over time")

                              
