######## methods + budget #########


budget <- read.csv("./diversity/csvs/waterbudgetdistance.csv")
methods <- read.csv("./diversity/csvs/methodsdistance.csv")

methods.base <- ggplot(methods,aes(topic_distance,country_distance, label = topic)) +
  geom_text_repel() +
  geom_point() +
  theme_pubr() +
  labs(x = "Normality across documents",y = "Normality across countries")

budget.base <- ggplot(budget,aes(topic_distance,country_distance, label = topic)) +
  geom_text_repel() +
  geom_point() +
  theme_pubr() +
  labs(x = "Normality across documents",y = "Normality across countries")

# interactive graphs
# b <- ggplot(budget, aes(y = country_distance, x = topic_distance, group = topic)) + geom_point() + theme_pubr()
# ggplotly(b, tooltip = c('topic'))


# combine into 1

methods.edit <- 
  methods.base +
  #grids() +
  coord_fixed() +
  ylim(0.55,0.95) +
  xlim(0.25,0.65) +
  labs(title = "Research methods")

budget.edit <- 
  budget.base +
  #grids() +
  ylim(0.55,0.95) +
  xlim(0.25,0.65) +
  coord_fixed() +
  labs(title = "Components of water budget")


joined <- ggarrange(budget.edit, methods.edit,
                    ncol = 2, nrow = 1)


# add guide - need to go through 1st part of pca analysis

reservoir.edit <- 
  reservoir +
  rremove("legend") +
  clean_theme() +
  labs(title = "Far from normal")

rivers.edit <-
  rivers +
  rremove("legend") +
  ylim(0,0.8) +
  clean_theme() +
  labs(title = "Close to normal")

text <- paste("Normality describes how far a topic's distribution is from the standard normal distribution.", sep = " ")
text.p <- ggparagraph(text = text, face = "italic", size = 16, color = "black")
reference <- ggarrange(text.p, reservoir.edit, rivers.edit,
                        ncol = 3, nrow = 1,
                       widths = c(1,1,1))

ggarrange(reference, joined,
          ncol = 1, nrow = 2,
          heights = c(1,5))
# SAVE AS jpeg 1050x700


######## themes ##########


theme <- read.csv("./diversity/csvs/themesdistance.csv")


ggscatter(theme, y = "country_distance", x = "topic_distance",
                    label = "topic",
                    label.rectangle = TRUE,
                    repel = TRUE,
                    xlab = "Normality across documents",
                    ylab = "Normality across countries") 

# interactive graphs
b <- ggplot(theme, aes(y = country_distance, x = topic_distance, group = topic)) + geom_point() + theme_pubr()
ggplotly(b, tooltip = c('topic'))




######## saving individual pieces ##########

methods.edit
budget.edit
reservoir.edit
rivers.edit
