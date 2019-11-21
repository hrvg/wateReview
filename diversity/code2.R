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
diversity_LAC <- function(df) { 
  df <- colSums(df)
  df <- diversity(df)
  
  return(df)
}
col_sums <- function(df) {
  
  df <- colSums(df)
  df$key=rownames(df)
  
  return(sumdf)
  
}


###################  1) subject analysis  #########################

## define data frame
df <- remove_year(theme) 
df <- remove_irrelevant(df)
df <- melt(df, id.vars = "country")

## count # papers per country
sums <- aggregate(df$value, by=list(df$country,df$variable), FUN=sum)
names(sums) = c("country","topic","sum")

country_sums <- aggregate(sums$sum, by=list(sums$country), FUN=sum) # no. papers per country
names(country_sums) = c("country","no.papers")

remove <- as.data.frame(country_sums$country[country_sums$no.papers < 30]) # list countries w/ < 30 papers
keep <- as.data.frame(country_sums$country[country_sums$no.papers > 30])

## subset countries with > 30 papers
df<- subset(df, df$country %in% keep$`country_sums$country[country_sums$no.papers > 30]`)

sums <-aggregate(df$value, by=list(df$country,df$variable), FUN=sum)
names(sums) = c("country","topic","sum")

## calculate country + topic sums
country_sums <- aggregate(sums$sum, by=list(sums$country), FUN=sum) # no. papers per country
names(country_sums) = c("country","no.papers")

topic_sums <- aggregate(sums$sum, by=list(sums$topic), FUN=sum)
names(topic_sums) = c("topic","sum")




## topic
topic_pick <- "island and extreme weather"
topic_total <- topic_sums$sum[topic_sums$topic == topic_pick]

topic_subset <- subset(sums, topic == topic_pick)
topic_subset <- cbind(topic_subset, country_sums)
topic_subset[4] <- NULL
topic_subset$perc <- topic_subset$sum / topic_subset$no.papers * 100


ggdotchart(topic_subset, 
           x= "country", y = "perc", 
           rotate = TRUE, add = "segments", sorting = "descending", 
           title = topic_pick) + 
          xlab(NULL) +
  ylab("Percent of research on topic")


## country
country_pick <- "Mexico"
country_total <- country_sums$no.papers[country_sums$country == country_pick]

country_subset <- subset(sums, country == country_pick)
country_subset <- cbind(country_subset, topic_sums)
country_subset[4] <- NULL
colnames(country_subset)[4] <- "total"
country_subset$perc <- country_subset$sum / country_subset$total * 100
country_subset$rel <- country_subset$sum / country_total



ggdotchart(country_subset, 
           x= "topic", y = "rel", 
           rotate = TRUE, add = "segments", sorting = "descending", 
           title = country_pick) + 
  xlab(NULL) +
  ylab("Probability research is on each topic")






###################  2) time analysis  #########################

## topic
df2 <- remove_country(budget)
df2 <- melt(df2, id.vars = "year")

sums2 <-aggregate(df2$value, by=list(df2$year,df2$variable), FUN=sum)
names(sums2) = c("year","topic","sum")

topic_subset2 <- subset(sums2, topic == topic_pick)
topic_subset2$cum <- cumsum(topic_subset2$sum)


ggplot(topic_subset2) +
  geom_line(aes(x = year, y = sum), linetype = "dashed") + 
  geom_line(aes(x = year, y = cum)) +
  theme_pubr() +
  labs(y = "Number of papers on reservoirs", x=NULL)


## country
df3 <- remove_irrelevant(theme)
df3 <- melt(df3, id.vars = c("year","country"))
df3 <- subset(df3, df3$country %in% keep$`country_sums$country[country_sums$no.papers > 30]`)

sums3 <-aggregate(df3$value, by=list(df3$year,df3$variable,df3$country), FUN=sum)
names(sums3) = c("year","topic","country","sum")

country_subset2 <- subset(sums3, country == country_pick)
country_subset2 <- aggregate(country_subset2$sum, by = list(country_subset2$year), FUN = sum)


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



###################  pie charts #########################
df3 <- remove_year_country(general)
df3 <- col_sums(df2)

pie(df3$value, labels = df3$key, main="Distribution of water research
    based on NSF general categories")

general_subset <- subset(general, country == country_pick)
df4 <- remove_year_country(general_subset)
df4 <- col_sums(df3)

pie(df4$value, labels = df4$key, main="Distribution of water research
    based on NSF general categories: Mexico")

df5 <- cbind(df3,df4)
names(df4) <- c("total","key","country")
df5 <- melt(df5)

ggbarplot(df5, x = "key", y = "value", fill ="variable" )


