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

