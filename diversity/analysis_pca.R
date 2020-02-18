################### pseudocode #########################

# start with 1 topic
# calculate distance from normal distribution for 2 data frames
## 1 - % of research devoted to each topic within a country
## 2- probability of a topic in each document 


# subset main df to countries with > 30 papers
# rescale value
# graph density distribution of corpus data + a normal distribution
# extract info both graphs - compare "y"s
# calculate distance - sqrt of JSD

################### code #########################

# define subset
probs <- reduce_docs_for_JSd(budget)
# probs <- probs %>% mutate(topic = replace(topic, topic == "groundwater flow", "groundwater"))


############### across countries ###########

# calculate distance from normal for % of research done in each country

sums <-aggregate(probs$value, by=list(probs$country,probs$topic), FUN=sum) # re - summarize after removing countries w/ < 30 papers
names(sums) = c("country","topic","sum")
df <- sums %>%
  group_by(country) %>%
  mutate(prop = sum/sum(sum)) %>% # calculate proportion of research each country spends on each topic
  group_by(topic) %>%
  mutate(scaled = scale(prop)) %>% # scale by topic
  ungroup() %>%
  select(c("topic","scaled"))

country_distance<- sapply(unique(df$topic), get_JSd_country)
names(country_distance) <- unique(df$topic)


country_distance <- as.data.frame(country_distance)
country_distance$topic <- rownames(country_distance)



############### across documents ###########

# calculate distance from normal for all documents
# MEDIAN OF JSD for EACH topic by 12 COUNTRIES

df <- probs %>%
  mutate(countrytopic = paste(country,topic)) %>%
  group_by(countrytopic) %>%
  mutate(scaled = scale(value)) %>%
  select(c("countrytopic","scaled")) # need to scale somehow?
  

topic_distance <- sapply(unique(df$countrytopic), get_JSd_corpus)
names(topic_distance) <- unique(df$countrytopic)
topic_distance <- as.data.frame(topic_distance)
topic_distance$countrytopic <- rownames(topic_distance)

topic_distance <- topic_distance %>%
  mutate(topic = word(countrytopic, 2, -1)) %>%
  group_by(topic) %>%
  mutate(topic_distance = median(topic_distance)) %>%
  select(c("topic","topic_distance")) %>%
  distinct()



################### graph #########################
distance <- merge(country_distance, topic_distance, by = "topic")

budget <- ggscatter(distance, y = "country_distance", x = "topic_distance",
          label = "topic",
          label.rectangle = TRUE,
          repel = TRUE,
          xlab = "Normality across topics",
          ylab = "Normality across countries")  # SAVE BUDGET & METHODS SEPARATE




# interactive graphs
# b <- ggplot(distance, aes(y = country_distance, x = topic_distance, group = topic)) + geom_point() + theme_pubr()
# ggplotly(b, tooltip = c('topic'))

# combine into 1

methods.edit <- 
  methods +
  theme(axis.text = element_blank()) +
  ylim(.58,.92) +
  xlim(.25,.63) +
  labs(title = "Research methods")


budget.edit <- 
  budget +
  theme(axis.text = element_blank()) +
  ylim(.58,.92) +
  xlim(.25,.63) +
  labs(title = "Components of water budget")

joined <- ggarrange(budget.edit, methods.edit,
          ncol = 2, nrow = 1)




# add guide

reservoir <- get_JSd_country('reservoirs', plot = T) 
rivers <- get_JSd_country('rivers', plot = T)

reservoir.edit <- 
  reservoir +
  rremove("legend") +
  clean_theme() +
  labs(title = "Less normal")

rivers.edit <-
  rivers +
  rremove("legend") +
  ylim(0,0.8) +
  clean_theme() +
  labs(title = "More normal")

text <- paste("Normality describes how similar a topic's distribution is to 'normal'.", sep = " ")
text.p <- ggparagraph(text = text, face = "italic", size = 16, color = "black")
reference2 <- ggarrange(text.p, reservoir.edit, rivers.edit,
                       ncol = 3, nrow = 1)

ggarrange(reference2, joined,
          ncol = 1, nrow = 2,
          heights = c(1,5))
# SAVE AS jpeg 1000x900
