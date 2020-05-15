budget <- read.csv("./diversity/csvs/waterbudgetdistance2.csv") 
methods <- read.csv("./diversity/csvs/methodsdistance2.csv") 
theme <- read.csv("./diversity/csvs/themesdistance2.csv")
general <- read.csv("./diversity/csvs/generaldistance2.csv")
specific <- read.csv("./diversity/csvs/specificdistance2.csv")


######## methods #########
methods.base <- ggplot(methods,aes(topic_distance,country_distance, label = topic)) +
  geom_text_repel() +
  geom_point() +
  theme_pubr() +
  labs(x = "Normality across documents",y = "Normality across countries")

methods.edit <- 
  methods.base +
  #grids() +
  coord_fixed() +
  ylim(0.53,0.93) +
  xlim(0.29,0.67) +
  coord_fixed() +
  labs(title = "Research methods")

######## budget #########
budget.base <- ggplot(budget,aes(topic_distance,country_distance, label = topic)) +
  geom_text_repel() +
  geom_point() +
  theme_pubr() +
  labs(x = "Normality across documents",y = "Normality across countries")

budget.edit <- 
  budget.base +
  #grids() +
  ylim(0.53,0.93) +
  xlim(0.29,0.67) +
  coord_fixed() +
  labs(title = "Components of water budget")

######## themes ##########
ggscatter(theme, y = "country_distance", x = "topic_distance",
                    label = "topic",
                    label.rectangle = TRUE,
                    repel = TRUE,
                    xlab = "Normality across documents",
                    ylab = "Normality across countries") 

themes.base <- ggplot(theme,aes(topic_distance,country_distance, label = topic)) +
  geom_text_repel() +
  geom_point() +
  theme_pubr() +
  labs(x = "Normality across documents",y = "Normality across countries")

themes.edit <- 
  themes.base +
  #grids() +
  coord_fixed() +
  labs(title = "Themes")


######## general ##########
general.base <- ggplot(general,aes(topic_distance,country_distance, label = topic)) +
  geom_text_repel() +
  geom_point() +
  theme_pubr() +
  labs(x = "Normality across documents",y = "Normality across countries")

general.edit <- 
  general.base +
  #grids() +
  coord_fixed() +
  labs(title = "General topics")

######## specific ##########
ggscatter(specific, y = "country_distance", x = "topic_distance",
         label = "topic",
         label.rectangle = TRUE,
         repel = TRUE,
         xlab = "Normality across documents",
         ylab = "Normality across countries") 

specific.base <- ggplot(specific,aes(topic_distance,country_distance, label = topic)) +
  geom_text_repel() +
  geom_point() +
  theme_pubr() +
  labs(x = "Normality across documents",y = "Normality across countries")

specific.edit <- 
  specific.base +
  coord_fixed() +
  labs(title = "Specific  topics")




######## interactie graph ######## 

b <- ggplot(theme, aes(y = country_distance, x = topic_distance, group = topic)) + geom_point() + theme_pubr()
ggplotly(b, tooltip = c('topic'))


######## topics for guide to normality ######## 
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


######## saving individual pieces ##########

methods.edit
budget.edit
# reservoir.edit
# rivers.edit
themes.edit
general.edit
specific.edit
