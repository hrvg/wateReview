## topic
df2 <- remove_country(budget)
df2 <- melt(df2, id.vars = "year")

sums2 <-aggregate(df2$value, by=list(df2$year,df2$variable), FUN=sum)
names(sums2) = c("year","topic","sum")

topic_subset2 <- subset(sums2, topic == topic_pick)
topic_subset2$cum <- cumsum(topic_subset2$sum)


ggplot(topic_subset2) +
  geom_line(aes(x = year, y = sum), linetype = "dashed") + 
  geom_line(aes(x = year, y = cum)) +
  theme_pubr() +
  labs(y = "Number of papers on reservoirs", x=NULL)


## country
df3 <- remove_irrelevant(theme)
df3 <- melt(df3, id.vars = c("year","country"))
df3 <- subset(df3, df3$country %in% keep$`country_sums$country[country_sums$no.papers > 30]`)

sums3 <-aggregate(df3$value, by=list(df3$year,df3$variable,df3$country), FUN=sum)
names(sums3) = c("year","topic","country","sum")

country_subset2 <- subset(sums3, country == country_pick)
country_subset2 <- aggregate(country_subset2$sum, by = list(country_subset2$year), FUN = sum)