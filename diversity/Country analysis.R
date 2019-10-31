################### analysis #########################

# subset by country
general <- subset(general,country == "Mexico")
specific <- subset(specific,country == "Mexico")
theme <- subset(theme,country == "Mexico")
methods <- subset(methods,country == "Mexico")
budget <- subset(budget,country == "Mexico")


# top/bottom

theme_sum <- top_bottom(theme)


theme <- theme %>%
  select(-c("year","country"))

sumtheme <- data.frame(value=apply(theme,2,sum))
sumtheme$key=rownames(sumtheme)
ggdotchart(sumtheme, x= "key", y = "value", rotate = TRUE, add = "segments", sorting = "descending")




methods <- methods %>%
  select(-c("year","country"))

summethods <- data.frame(value=apply(methods,2,sum))
summethods$key=rownames(summethods)
ggdotchart(summethods, x= "key", y = "value", rotate = TRUE, add = "segments", sorting = "descending")



budget <- budget %>%
  select(-c("year","country"))

sumbudget <- data.frame(value=apply(budget,2,sum))
sumbudget$key=rownames(sumbudget)
ggdotchart(sumbudget, x= "key", y = "value", rotate = TRUE, add = "segments", sorting = "descending")

