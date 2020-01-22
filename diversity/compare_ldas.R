library(dplyr)
library(reshape2)
library(ggpubr)
library(tidyr)
library(ggplot2)

######## load data #############
en <- read.csv("./topic_names_en.csv")
es <- read.csv("./topic_names_es.csv")
pt <- read.csv("./topic_names_pt.csv", na.strings=c("","NA"))

en$lang <- "en"
es$lang <- "es"
pt$lang <- "pt"

en2 <- melt(en,
           id.vars = c("topic_id","lang"))
es2 <- melt(es,
            id.vars = c("topic_id","lang"))
pt2 <- melt(pt,
            id.vars = c("topic_id","lang"))


x <- rbind(en2,es2,pt2)
x <- na.omit(x)

######## filter #############
x2 <- x %>%
  filter(variable == "NSF_specific") %>%
  group_by(value, lang) %>%
  tally


######## dot chart #############
ggbarplot(x2, x = "value", y = "n", 
           fill = "lang",
          color = "white",
           palette = c("#00AFBB", "#FC4E07",  "#E7B800"),
           rotate = TRUE)

######## heat map #############

# need to order based on engligh numbers
lvls <- as.character(x2$value[x2$lang=="en"])[order(x2$n[x2$lang=="en"])]
x2$value <- factor(x2$value, levels = lvls)
x2 <- na.omit(x2)


a <- ggplot(data = x2, aes(x = lang, y = value)) +
  geom_tile(aes(fill = n, width=0.9, height=0.9)) # edit for specific vs. general
a
a <- a + 
  scale_fill_gradient(low = "lightsteelblue1", high = "lightsteelblue4") +
  labs(title= "LDA coverage",
       y="NSF specific categories",
       x = "LDA for each language")+
  theme_pubr() 

a

plotly::ggplotly(a)
