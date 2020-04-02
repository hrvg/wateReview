################### functions #########################

remove_year_country <- function(df) {
  df <- df %>%
    filter(country != "Irrelevant") %>%
    select(-c("year","country"))
  return(df)
}
remove_year <- function(df) {
  df <- df %>%
    select(-c("year"))
  
  return(df)
  
}
remove_country <- function(df) {
  df <- df %>%
    select(-c("country"))
  
  return(df)
  
}
remove_irrelevant <- function(df) {
  df <- df %>%
    filter(country != "Irrelevant")
  
  return(df)
  
}
diversity_country <- function(df) { 
  df <- df %>%
    select(-c("year"))
  
  df <- melt(df, 
             id.vars = c("country"))
  
  df <- aggregate(df$value, by=list(df$country,df$variable), FUN=sum)
  
  df <- df %>%
    rename(country = Group.1, sepcies = Group.2, population = x) # species is country*topic
  
  df <- group_by(df, country)
  df <- do(df, tidy(diversity(.$population)))
  
  return(df)
}
diversity_LAC <- function(df) { 
  df <- colSums(df)
  df <- diversity(df)
  
  return(df)
}
col_sums <- function(df) {
  
  df <- colSums(df)
  df$key=rownames(df)
  
  return(sumdf)
  
}



reduce_docs_for_JSd <- function(df){
  
  a <- remove_year(df) # pick data to work with
  a <- remove_irrelevant(a)
  a <- melt(a, id.vars = "country")

## count # papers per country
  sums <- aggregate(a$value, by=list(a$country,a$variable), FUN=sum)
  names(sums) = c("country","topic","sum")
  country_sums <- aggregate(sums$sum, by=list(sums$country), FUN=sum) # no. papers per country
  names(country_sums) = c("country","no.papers")
  remove <- as.data.frame(country_sums$country[country_sums$no.papers < 30]) # list countries w/ < 30 papers
  keep <- as.data.frame(country_sums$country[country_sums$no.papers > 30])
  names(keep) <- "country"

## subset countries with > 30 papers
  a <- subset(a, a$country %in% keep$country)
  names(a) = c("country","topic","value")

return(a)

}







get_JSd_country <- function(top, plot = FALSE){
  p <- ggplot(subset(df, topic == top)) +
    geom_density(aes(x = scaled, y = ..density.., fill= top)) +
    xlim(c(-5, 5)) +
    stat_function(fun = dnorm, n = 512, args = list(mean = 0, sd = 1)) +
    labs(x = "scaled density", 
         y = "probability of research (rescaled)") +
    theme_pubr()
  p 
  if (plot) return(p)
  
  g <- ggplot_build(p)
  gdata <- g$data[[1]]$y / sum(g$data[[1]]$y) # topic - normalized 
  normdata <- g$data[[2]]$y / sum(g$data[[2]]$y) # standard, normal - normalized
  return(1 - sqrt(as.numeric(JSD(rbind(gdata, normdata)))))
}



get_JSd_corpus <- function(top, plot = FALSE){
  p <- ggplot(subset(df, countrytopic == top)) +
    geom_density(aes(x = scaled, y = ..density.., fill= top)) +
    xlim(c(-5, 5)) +
    stat_function(fun = dnorm, n = 512, args = list(mean = 0, sd = 1)) +
    labs(x = "scaled desntiy", 
         y = "probability of research (rescaled)") +
    theme_pubr()
  p 
  if (plot) return(p)
  
  g <- ggplot_build(p)
  gdata <- g$data[[1]]$y / sum(g$data[[1]]$y)
  normdata <- g$data[[2]]$y / sum(g$data[[2]]$y)
  return(1 - sqrt(as.numeric(JSD(rbind(gdata, normdata)))))
}


