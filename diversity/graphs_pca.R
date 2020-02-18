budget <- read.csv("./diversity/csvs/waterbudgetdistance.csv")
methods <- read.csv("./diversity/csvs/methodsdistance.csv")

                    
budget <- ggscatter(budget, y = "country_distance", x = "topic_distance",
                    label = "topic",
                    label.rectangle = TRUE,
                    repel = TRUE,
                    xlab = "Normality across documents",
                    ylab = "Normality across countries") 

methods <- ggscatter(methods, y = "country_distance", x = "topic_distance",
                    label = "topic",
                    label.rectangle = TRUE,
                    repel = TRUE,
                    xlab = "Normality across documents",
                    ylab = "Normality across countries") 



budget
methods

# interactive graphs
# b <- ggplot(budget, aes(y = country_distance, x = topic_distance, group = topic)) + geom_point() + theme_pubr()
# ggplotly(b, tooltip = c('topic'))


# combine into 1

methods.edit <- 
  methods +
  #theme(axis.text = element_blank()) +
  ylim(.58,.92) +
  xlim(.25,.63) +
  labs(title = "Research methods")

budget.edit <- 
  budget +
  #theme(axis.text = element_blank()) +
  ylim(.58,.92) +
  xlim(.25,.63) +
  labs(title = "Components of water budget")

joined <- ggarrange(budget.edit, methods.edit,
                    ncol = 2, nrow = 1)




# add guide - need to go through 1st part of pca analysis

reservoir <- get_JSd_country('reservoirs', plot = T) 
rivers <- get_JSd_country('rivers', plot = T)


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
                        ncol = 3, nrow = 1)

ggarrange(reference, joined,
          ncol = 1, nrow = 2,
          heights = c(1,5))
# SAVE AS jpeg 1000x900
