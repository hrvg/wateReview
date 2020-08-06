# final csvs for database
# SAVE
# write.csv(df,'./diversity/csvs/final-all-countries/diversity-general.csv')

# revised previous code to NOT remove countries with less than 30 papers



################ COUNT ################ 

# define data frame
df <- remove_year(methods) # pick data to work with
df <- remove_irrelevant(df)
df <- melt(df, id.vars = "country")

# count # papers per country
sums <- aggregate(df$value, by=list(df$country,df$variable), FUN=sum)
names(sums) = c("country","topic","sum")

country_sums <- aggregate(sums$sum, by=list(sums$country), FUN=sum) # no. papers per country
names(country_sums) = c("country","no.papers")

# remove <- as.data.frame(country_sums$country[country_sums$no.papers < 30]) # list countries w/ < 30 papers
# keep <- as.data.frame(country_sums$country[country_sums$no.papers > 30])

# summarize country x topic
sums <-aggregate(df$value, by=list(df$country,df$topic), FUN=sum) # re - summarize after removing countries w/ < 30 papers
names(sums) = c("country","topic","sum")
sums <- sums %>%
  group_by(country) %>%
  mutate(prop = sum/sum(sum))


################ DIVERSITY BY COUNTRY ################ 

df <- remove_irrelevant(budget)
df <- diversity_country(df)
write.csv(df,'./diversity/csvs/final-all-countries/diversity-budget.csv')



################  NORMALITY BY TOPIC ################ 


# define subset
probs <- reduce_docs_for_JSd_2(budget) # pick df

############### across countries

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

############### across documents 

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
  na.omit() %>%
  mutate(topic = word(countrytopic, 2, -1)) %>%
  group_by(topic) %>%
  mutate(topic_distance = median(topic_distance)) %>%
  select(c("topic","topic_distance")) %>%
  distinct()


# combine and save separate csvs
distance <- merge(country_distance, topic_distance, by = "topic")

write.csv(distance,'./diversity/csvs/final-all-countries/distance-budget.csv')
