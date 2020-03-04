library(dplyr)
library(reshape2)
library(ggpubr)
library(tidyr)
library(ggplot2)
library(forcats)

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


# need to order based on engligh numbers + number of overall topics
x2$nlang <- x2 %>% 
  group_by(value) %>% 
  group_map(~ rep(length(table(.x$lang)), length(table(.x$lang)))) %>% 
  unlist()

lvls <- as.character(x2$value[x2$lang=="en"])[order(x2$n[x2$lang=="en"])]

x2$value <- factor(x2$value, levels = lvls)

x2 <- na.omit(x2) # missing 1 spanish?

x2$lang <- factor(x2$lang, labels = c("English", "Spanish", "Portuguese"))

######## heat map #############

a <- ggplot(data = x2, aes(x = lang, y = fct_reorder(value, nlang))) +
  geom_tile(aes(fill = nlang, width=0.9, height=0.9)) +
  geom_text(aes(label = n))
a

a + 
  scale_fill_gradient(low = "grey89",
                      high = "grey50") +
  labs(title= "LDA coverage",
       y="NSF specific categories",
       x = "LDA for each language") +
  theme_pubr() +  
  theme(axis.text.y = element_text(face = c('plain','plain','plain', 'plain', 'plain', 'plain', 'plain',
                                            'plain', 'bold', 'plain', 'plain', 'plain',
                                            'plain', 'plain', 'plain', 'plain', 'plain',
                                            'plain', 'plain', 'plain', 'plain', 'plain',
                                            'plain', 'bold', 'plain', 'plain', 'plain',
                                            'bold', 'bold', 'plain', 'bold', 'bold', 
                                            'bold', 'plain', 'bold','plain', 'plain', # 6-10
                                            'bold', 'bold', 'bold','bold','bold' # 1-5
                                            ))) +
  rremove("legend")






# plotly::ggplotly(a)

######## dot chart #############
ggbarplot(x2, x = "value", y = "n", 
          fill = "lang",
          color = "white",
          palette = c("#00AFBB", "#FC4E07",  "#E7B800"),
          rotate = TRUE)
