# calculate diversity by paper / for all of LAC
clean <- remove_year_country(specific) #  specify which (species group)
diversity_LA <- diversity_LAC(clean)
diversity_paper <- diversity(clean)

# check - diveristy function from vegan package works
# check <- as.data.frame(colSums(clean))
# sum <- sum(check)
# check$pi <- check$`colSums(clean)`/sum
# check$lnpi <- log(check$pi)
# check$pilnpi <- check$pi * check$lnpi
# H <- -sum(check$pilnpi)

################### analysis #########################

# calculate diversity by country
general2 <- remove_irrelevant(general)
specific2 <- remove_irrelevant(specific)
budget2 <- remove_irrelevant(budget)

general2 <-  diversity_country(general2)
specific2 <- diversity_country(specific2)
budget2 <- diversity_country(budget2)

diversity_by_country<-cbind(general2, specific2, budget2)
diversity_by_country <- diversity_by_country %>%
  select(-c("country1","country2")) %>%
  rename(NSFgeneral = x, NSFspecific = x1, budget = x2)

# SAVE
write.csv(diversity_by_country, file = "./diversity/csvs/diversity2.csv")

################### graphs #########################

diversity_by_country_graph <- melt(diversity_by_country, 
                                   id.vars = c("country"))

# NSF specific  
specific_graphdf <- subset(diversity_by_country_graph, variable == "NSFspecific")
specific_graph <- ggdotchart(specific_graphdf, x = "country", y = "value", #add color = cluster
                             add = "segments", sorting = "descending", rotate = TRUE, title = "specific")
# NSF general  
general_graphdf <- subset(diversity_by_country_graph, variable == "NSFgeneral")
general_graph <- ggdotchart(general_graphdf, x = "country", y = "value", #add color = cluster
                            add = "segments", sorting = "descending", rotate = TRUE, title = "general")
# budget
budget_graphdf <- subset(diversity_by_country_graph, variable == "budget")
budget_graph <- ggdotchart(budget_graphdf, x = "country", y = "value", #add color = cluster
                             add = "segments", sorting = "descending", rotate = TRUE, title = "specific")


specific_graph
general_graph
budget_graph

ggscatter(diversity_by_country, x = "NSFgeneral", y = "NSFspecific", color = "country")

ggdotchart(diversity_by_country_graph, x = "country", y = "value", color = "variable",
           rotate = TRUE)

ggplot(diversity_by_country,aes(NSFgeneral,NSFspecific, label = country)) +
  geom_text_repel() +
  geom_point() +
  theme_pubr() +
  labs(x = "NSFgeneral",y = "NSFspecific")
