#####################
##### LIBRARIES #####
#####################

#################
##### UTILS #####
#################

import::here(.from = "./R/utils/envpath.R", 
	get_rootdir)

#######################
####### HELPERS #######
#######################

##############
#### MAIN ####
##############

root.dir = get_rootdir()

# 1. read csv database

lang <- "spanish"
csv.dir <- paste0("data/latin_america/corpus_csv/", lang, "/")
csv.file <- 'citation_dataframe.csv'

citation.df <- read.csv(file.path(root.dir, csv.dir, csv.file), header = TRUE)


hist(citation.df$Year)

hist(citation.df$Cites)

library(ggplot2)
# counts
p <- ggplot(citation.df, aes(x = Source)) +
  geom_bar()

print(p)

tab <- table(tolower(citation.df$Source))
print(head(tab[order(-tab)], 20))

# 2. get fields Source, Year, Cites