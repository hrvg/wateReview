################### libraries #########################
library(dplyr)
library(vegan)
library(broom)
library(reshape2)
library(viridis)
library(hrbrthemes)
library(ggplot2)
library(tidyr)

################### Topics chronology #########################

general <- readRDS("./consolidated_results_NSF_general.Rds")
general <- subset(general, select = -c(country))
general_long <- gather(general, topic, prop, 'physical sciences':'social sciences', factor_key = TRUE)
pivot <- with (general_long, tapply( prop, list(year, topic), sum))
years <- row.names(pivot)
rownames(pivot) <- NULL
pivot <- cbind.data.frame(years, pivot)
pivot <- pivot[!(pivot$years == 2018),]
pivot$years = strtoi(pivot$years)
pivot <- pivot[(pivot$years > 1979),]
pivot <- complete(pivot, years = full_seq(years, period = 1))
pivot_long <- gather(pivot, topic, prop, 'physical sciences':'social sciences', factor_key = TRUE)

ggplot(pivot_long, aes(x=years, y=prop, fill=topic)) +
  geom_area(alpha=0.6 , size=.5, colour="white") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() +
  ggtitle("Topics over time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title = element_text(hjust = 0))

################### Countries chronology #########################

general <- readRDS("./consolidated_results_NSF_general.Rds")
general <- subset(general, select = c(year, country))
general <- general[general$country != "Irrelevant", ]
general$country <- as.character(general$country)

# arrange country clusters
braz_mex <- c("Mexico", "Brazil")
chile_argen <- c("Chile", "Argentina", "Uruguay", "Suriname", "Guyana", "Belize", "Paraguay", "Costa.Rica", "Cuba", "Panama", "Venezuela")
colom_boliv <- c("Colombia", "Bolivia", "Jamaica", "Nicaragua", "El.Salvador", "Ecuador", "Honduras", "Guatemala", "Peru", "Dominican Republic")

general$country[which(!is.na(match(general$country, braz_mex)))] <- "Braz_Mex"
general$country[which(!is.na(match(general$country, chile_argen)))] <- "Chile_Argen"
general$country[which(!is.na(match(general$country, colom_boliv)))] <- "Colom_Boliv"

# Drop rows with caribbean countries excluded from clusters?
general <- general[!(general$country == "Bahamas" | general$country == "Haiti"),]

pivot <- with(general, tapply(country, list(year, country), length))
years <- row.names(pivot)
rownames(pivot) <- NULL
pivot <- cbind.data.frame(years, pivot)
pivot <- pivot[!(pivot$years == 2018),]
pivot$years = strtoi(pivot$years)
pivot <- pivot[(pivot$years > 1979),]
pivot <- complete(pivot, years = full_seq(years, period = 1))
pivot_long <- gather(pivot, country, count, Braz_Mex:Colom_Boliv, factor_key = TRUE)

ggplot(pivot_long, aes(x=years, y=count, fill=country)) +
  geom_area(alpha=0.6 , size=.5, colour="white") +
  # scale_y_continuous(trans='pseudo_log') +
  scale_fill_viridis(discrete = T) +
  ggtitle("Country clusters over time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title = element_text(hjust = 0))
