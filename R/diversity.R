#' Remove `year` and `country` columns
#' @param df a data.frame
#' @return a data.frame without the `year` and `country` columns
#' @importFrom magrittr %>%
#' @export
remove_year_country <- function(df) {
  df <- df %>% 
    remove_irrelevant() %>%
    remove_year() %>%
    remove_country()
  return(df)
}

#' Remove `year` column
#' @param df a data.frame
#' @return a data.frame without the `year` column
#' @importFrom magrittr %>%
#' @export
remove_year <- function(df) {
  df <- df %>% dplyr::select(-c("year"))
  return(df)
}

#' Remove `country` column
#' @param df a data.frame
#' @return a data.frame without the `country` column
#' @importFrom magrittr %>%
#' @export
remove_country <- function(df) {
  df <- df %>% dplyr::select(-c("country"))
  return(df)
}

#' Removes `Irrelevant` entries from the `country` column of a data.frame
#' @param df a data.frame
#' @return a data.frame without `country` entries marked as `Irrelevant`
#' @importFrom magrittr %>%
#' @export
remove_irrelevant <- function(df) {
  df <- df %>% dplyr::filter(country != "Irrelevant")
  return(df)
}

#' Calculates a diversity over the entire LAC region
#' @param df a data.frame
#' @return a data.frame
#' @export
diversity_LAC <- function(df) { 
  df <- colSums(df)
  df <- vegan::diversity(df)
  return(df)
}

#' Calculates the diversity
#' @param df a data.frame
#' @importFrom magrittr %>%
#' @return a data.frame
#' @export
diversity_country <- function(df){
  a <- remove_year(df) %>% 
    remove_irrelevant() %>%
    reshape2::melt(id.vars = "country")
  ## count # papers per country
  sums <- aggregate(a$value, by = list(a$country, a$variable), FUN = sum)
  names(sums) <- c("country","topic","sum")
  # no. papers per country
  country_sums <- aggregate(sums$sum, by = list(sums$country), FUN = sum)
  names(country_sums) <- c("country","no.papers")
  # list countries w/ < 30 papers
  remove <- as.data.frame(country_sums$country[country_sums$no.papers < 30])
  keep <- as.data.frame(country_sums$country[country_sums$no.papers > 30])
  names(keep) <- "country"
  
  ## subset countries with > 30 papers
  a <- subset(a, a$country %in% keep$country)
  a <- aggregate(a$value, by=list(a$country,a$variable), FUN=sum)
  a <- a %>% dplyr::rename(country = Group.1, species = Group.2, population = x) # species is country*topic
  a <- dplyr::group_by(a, country)
  a <- dplyr::do(a, broom::tidy(vegan::diversity(.$population)))
  return(a)
}

#' Reduce the documents before calculating the Jensen-Shannon distance
#' @param df a data.frame
#' @importFrom magrittr %>%
#' @return a data.frame with columns `country`, `topic` and `value`
#' @export
reduce_docs_for_JSd <- function(df){
  a <- remove_year(df) %>% 
    remove_irrelevant() %>%
    reshape2::melt(id.vars = "country")
  ## count # papers per country
  sums <- aggregate(a$value, by = list(a$country, a$variable), FUN = sum)
  names(sums) <- c("country", "topic", "sum")
  # no. papers per country
  country_sums <- aggregate(sums$sum, by = list(sums$country), FUN=sum)
  names(country_sums) = c("country","no.papers")
  # list countries w/ < 30 papers
  remove <- as.data.frame(country_sums$country[country_sums$no.papers < 30])
  keep <- as.data.frame(country_sums$country[country_sums$no.papers > 30])
  names(keep) <- "country"
  ## subset countries with > 30 papers
  a <- subset(a, a$country %in% keep$country)
  names(a) = c("country","topic","value")
  return(a)
}

#' Calculates the Jensen-Shannon distance for countries
#' @param top one possible value for the `topic` column of the data.frame
#' @param .df a data.frame
#' @param .plot logical, if `TRUE` returns a plot, if not returns the Jensen-Shannon distance 
#' @return the Jensen-Shannon distance or a `ggplot` object depending on `.plot`
#' @import ggplot2
#' @export
get_JSd_country <- function(top, .df = df, .plot = FALSE){
  p <- ggplot(subset(.df, topic == top)) +
    geom_density(aes(x = scaled, y = ..density.., fill= top)) +
    xlim(c(-5, 5)) +
    stat_function(fun = dnorm, n = 512, args = list(mean = 0, sd = 1)) +
    labs(x = "scaled density", 
         y = "probability of research (rescaled)") +
    ggpubr::theme_pubr()
  p 
  if (.plot) return(p)
  g <- ggplot_build(p)
  gdata <- g$data[[1]]$y / sum(g$data[[1]]$y) # topic - normalized 
  normdata <- g$data[[2]]$y / sum(g$data[[2]]$y) # standard, normal - normalized
  return(1 - sqrt(as.numeric(philentropy::JSD(rbind(gdata, normdata)))))
}

#' Calculates the Jensen-Shannon distance for countries
#' @param top one possible value for the `topic` column of the data.frame
#' @param .df a data.frame
#' @param .plot logical, if `TRUE` returns a plot, if not returns the Jensen-Shannon distance 
#' @return the Jensen-Shannon distance or a `ggplot` object depending on `.plot`
#' @import ggplot2
#' @export
get_JSd_corpus <- function(top, .df =df, .plot = FALSE){
  p <- ggplot(subset(.df, countrytopic == top)) +
    geom_density(aes(x = scaled, y = ..density.., fill= top)) +
    xlim(c(-5, 5)) +
    stat_function(fun = dnorm, n = 512, args = list(mean = 0, sd = 1)) +
    labs(x = "scaled desntiy", 
         y = "probability of research (rescaled)") +
    ggpubr::theme_pubr()
  p 
  if (.plot) return(p)
  g <- ggplot_build(p)
  gdata <- g$data[[1]]$y / sum(g$data[[1]]$y)
  normdata <- g$data[[2]]$y / sum(g$data[[2]]$y)
  return(1 - sqrt(as.numeric(philentropy::JSD(rbind(gdata, normdata)))))
}


#' Get the Jensen-Shannon distance across countries
#' @param probs probabilities
#' @return data.frame
#' @importFrom magrittr %>%
#' @export
get_country_distance <- function(probs){
  sums <- aggregate(probs$value, by = list(probs$country, probs$topic), FUN = sum)
  names(sums) <- c("country","topic","sum")
  df <- sums %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(prop = sum / sum(sum)) %>% # calculate proportion of research each country spends on each topic
    dplyr::group_by(topic) %>%
    dplyr::mutate(scaled = scale(prop)) %>% # scale by topic
    dplyr::ungroup() %>%
    dplyr::select(c("topic","scaled"))
  country_distance <- sapply(unique(df$topic), get_JSd_country)
  names(country_distance) <- unique(df$topic)
  country_distance <- as.data.frame(country_distance)
  country_distance$topic <- rownames(country_distance)
  return(country_distance)
}

#' Get the Jensen-Shannon distance across topics
#' @param probs probabilities
#' @return data.frame
#' @importFrom magrittr %>%
#' @export
get_topic_distance <- function(probs){
  df <- probs %>%
    dplyr::mutate(countrytopic = paste(country,topic)) %>%
    dplyr::group_by(countrytopic) %>%
    dplyr::mutate(scaled = scale(value)) %>%
    dplyr::select(c("countrytopic","scaled"))   

  topic_distance <- sapply(unique(df$countrytopic), get_JSd_corpus)
  names(topic_distance) <- unique(df$countrytopic)
  topic_distance <- as.data.frame(topic_distance)
  topic_distance$countrytopic <- rownames(topic_distance)

  topic_distance <- topic_distance %>%
    dplyr::mutate(topic = word(countrytopic, 2, -1)) %>%
    dplyr::group_by(topic) %>%
    dplyr::mutate(topic_distance = median(topic_distance)) %>%
    dplyr::select(c("topic","topic_distance")) %>%
    dplyr::distinct()
  return(topic_distance)
}