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
probs <- reduce_docs_for_JSd(themes)
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

# graphs for guide
# reservoir <- get_JSd_country('reservoirs', plot = T)
# rivers <- get_JSd_country('rivers', plot = T)
# get_JSd_country('irrigation', plot = T) 

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


# combine and save separate csvs
distance <- merge(country_distance, topic_distance, by = "topic")

write.csv(distance, file = "./diversity/csvs/themesdistance2.csv")
