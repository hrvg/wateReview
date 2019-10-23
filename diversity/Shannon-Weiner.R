library(dplyr)
library(vegan)
library(broom)

# calculate diversity index 

# community = location:
# each paper, all papers in each country, all papers in country clusters, all papers in LAC

# species = topics: 
# NSF gemeral, NSF specific, theme, theme subsets, specific theme
# species population = probability or sum of probability

res <- readRDS("~/Downloads/consolidated_results.Rds")

theme <- res %>%
  filter(country != "Irrelevant") %>%
  select(-c("year"))

themeSubset <- res %>%
  filter(country != "Irrelevant") %>%
  select(-c("year",
            "amazon floods",
            "canopy interception",
            "coastal ecology",
            "desalination",
            "fuel",
            "glaciers",
            "island and extreme weather",
            "marine science",
            "oceans",
            "salt water"))


# GOAL:
output <- as.data.frame(unique(res$country))
output <- output %>%
  rename(community = `unique(res$country)`) %>%
  mutate(nsfgeneral = 0) %>%
  mutate(nsfspecific = 0) %>%
  mutate(theme = 0) %>%
  mutate(themesubset = 0) %>%
  mutate(themespecific = 0)
  
  
# A. diversity of each paper
a <- theme %>%
  select(-c("country"))
a2 <- diversity(a)

boxplot(a2)
densityplot(a2)

# B. diversity in LAC
b <- as.data.frame(colSums(a))
b2 <- diversity(b)



# C. diversity per country
c <- melt(theme, 
          id.vars = c("country"))

c <- aggregate(c$value, by=list(c$country,c$variable), FUN=sum)

c <- c %>%
  rename(country = Group.1, sepcies = Group.2, population = x) # species is country*topic

c <- group_by(c, country)
c <- do(c, tidy(diversity(.$population)))

plot(c)
densityplot(c$x)

