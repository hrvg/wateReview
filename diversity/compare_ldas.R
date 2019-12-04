library(dplyr)
library(reshape2)
library(ggpubr)

en <- read.csv("./data/topic_names_en.csv")
es <- read.csv("./data/topic_names_es.csv")
pt <- read.csv("./data/topic_names_pt.csv", na.strings=c("","NA"))

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


x2 <- x %>%
  filter(variable == "NSF_specific") %>%
  group_by(value, lang) %>%
  tally
  

ggdotchart(x2, x = "value", y = "n", 
           color = "lang",
           palette = c("#00AFBB", "#FC4E07",  "#E7B800"),
           #group = "lang", 
           add = "segments",
           add.params = list(color = "lightgray", size = 1),
           rotate = TRUE)

ggbarplot(x2, x = "value", y = "n", 
           fill = "lang",
          color = "white",
           palette = c("#00AFBB", "#FC4E07",  "#E7B800"),
           rotate = TRUE)
