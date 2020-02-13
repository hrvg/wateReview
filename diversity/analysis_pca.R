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
probs <- reduce_docs_for_JSd(general)

# x axis
# calculate distance from normal for %  of research done in each country

sums <-aggregate(probs$value, by=list(probs$country,probs$topic), FUN=sum) # re - summarize after removing countries w/ < 30 papers
names(sums) = c("country","topic","sum")
df <- sums %>%
  group_by(country) %>%
  mutate(prop = sum/sum(sum)) %>%
  group_by(topic) %>%
  mutate(scaled = scale(prop)) %>%
  ungroup() %>%
  select(c("topic","scaled"))

country_distance<- sapply(unique(df$topic), get_JSd)
names(country_distance) <- unique(df$topic)

# get_JSd('rivers', plot = T)


# y axis
# calculate distance from normal for all documents
df <- probs %>%
  group_by(country) %>%
  mutate(prop = value/sum(value)) %>% # added proportion to account for influence of country
  mutate(scaled= scale(prop)) %>%
  ungroup() %>%
  select(c("topic","scaled"))

topic_distance <- sapply(unique(df$topic), get_JSd)
names(topic_distance) <- unique(df$topic)




################### graph #########################
country_distance <- as.data.frame(country_distance)


topic_distance <- as.data.frame(topic_distance)
distance <- cbind(country_distance, topic_distance)
distance$topic <- rownames(distance)

ggscatter(distance, y= "country_distance", x = "topic_distance",
          label = "topic",
          label.rectangle = TRUE,
          repel = TRUE)

# interactive graphs
# a <- ggplot(distance, aes(x = country_distance, y = topic_distance, group = topic)) + geom_point() + theme_pubr()
# ggplotly(a, tooltip = c('topic'))
