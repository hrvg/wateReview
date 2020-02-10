################### pie charts #########################
df3 <- remove_year_country(general)
df3 <- col_sums(df2)

pie(df3$value, labels = df3$key, main="Distribution of water research
    based on NSF general categories")

general_subset <- subset(general, country == country_pick)
df4 <- remove_year_country(general_subset)
df4 <- col_sums(df3)

pie(df4$value, labels = df4$key, main="Distribution of water research
    based on NSF general categories: Mexico")

df5 <- cbind(df3,df4)
names(df4) <- c("total","key","country")
df5 <- melt(df5)

ggbarplot(df5, x = "key", y = "value", fill ="variable" )