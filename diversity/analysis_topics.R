################### analysis  #########################

## define data frame
df <- remove_year(budget) # pick data to work with
df <- remove_irrelevant(df)
df <- melt(df, id.vars = "country")

## count # papers per country
sums <- aggregate(df$value, by=list(df$country,df$variable), FUN=sum)
names(sums) = c("country","topic","sum")

country_sums <- aggregate(sums$sum, by=list(sums$country), FUN=sum) # no. papers per country
names(country_sums) = c("country","no.papers")

remove <- as.data.frame(country_sums$country[country_sums$no.papers < 30]) # list countries w/ < 30 papers
keep <- as.data.frame(country_sums$country[country_sums$no.papers > 30])

## subset countries with > 30 papers
df<- subset(df, df$country %in% keep$`country_sums$country[country_sums$no.papers > 30]`)
names(df) = c("country","topic","value")
country_sums <- aggregate(sums$sum, by=list(sums$country), FUN=sum) 
names(country_sums) = c("country","no.papers")


## summarize country x topic
sums <-aggregate(df$value, by=list(df$country,df$topic), FUN=sum) # re - summarize after removing countries w/ < 30 papers
names(sums) = c("country","topic","sum")

sums <- sums %>%
  group_by(country) %>%
  mutate(prop = sum/sum(sum))


# SAVE for mapping
# write.csv(sums,'./diversity/csvs/nsfspecific.csv')

## describe tpoics

topic <- df %>% # uses df of individual papers
  select(-c("country")) %>%
  group_by(topic) %>%
  mutate(total = sum(value)) %>%
  mutate(sd = sd(value)) %>%
  mutate(mean = mean(value)) %>%
  mutate(var = var(value)) %>% # Variance measures the dispersion of a set of data points around their mean value
  mutate(cv = sd/mean) %>% # extent of variability in relation to the mean of the population
  mutate(diversity = diversity(value)) %>%
  select(-c("value")) %>%
  distinct()
topic$prop <- topic$total/sum(topic$total)





################### graph  #########################
pca <- ggscatter(topic, x= "prop", y = "diversity",
                 label = "topic",
                 label.rectangle = TRUE,
                 repel = TRUE,
                 add = "reg.line"
)

pca




################### individual topic/country analysis  #########################

## topic
topic_pick <- "water sampling"
topic_total <- topic$total[topic$topic == topic_pick]

topic_subset <- subset(sums, topic == topic_pick)
topic_subset <- merge(topic_subset, country)
topic_subset$perc <- topic_subset$sum / topic_subset$total * 100


ggdotchart(topic_subset, 
           x= "country", y = "perc", 
           rotate = TRUE, add = "segments", sorting = "descending", 
           title = topic_pick) + 
  xlab(NULL) +
  ylab("Percent of research on topic")


## country
country_pick <- "Mexico"
country_total <- country_sums$no.papers[country_sums$country == country_pick]

country_subset <- subset(sums, country == country_pick)
country_subset <- merge(country_subset, topic)
country_subset$perc <- country_subset$sum / country_subset$total * 100



ggdotchart(country_subset, 
           x= "topic", y = "perc", 
           rotate = TRUE, add = "segments", sorting = "descending", 
           title = country_pick) + 
  xlab(NULL) +
  ylab("Probability research is on each topic")