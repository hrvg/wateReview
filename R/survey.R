#' Read the survey data
#' @param fname file.path to the survey data, expecting a `.csv` file
#' @return a data.frame
#' @importFrom magrittr %>%
#' @export
read_survey_df <- function(fname = "Water Management in Latin America_edit.csv"){
  df <- read.csv(file.path(fname))
  df <- df[-(1:2),] # remove rows with just text
  # edit column names
  df <- df %>% dplyr::rename(Lat = LocationLatitude, Lon = LocationLongitude, Position = Q1, Affiliation = Q2, Years = Q4, Discipline = Q14, Country_current = Q5, Country_current_other = Q5_46_TEXT, Country_born = Q6, Country_born_other = Q6_47_TEXT, Country_research = Q8, Country_research_other = Q8_46_TEXT, Publications = Q11, Journals = Q12, Journal_motivation = Q15, Funding = Q13, Interdisc_interest = Q16_1, Interdisc_level = Q16_2, Equal_resource_distribution = Q17_1, Sufficient_climate_funds = Q17_2, Future_contact = Q18, Comments = Q16, Comments_translation = Comments.translation)
  # change variable types
  df$Lat <- as.numeric(levels(df$Lat))[df$Lat]
  df$Lon <- as.numeric(levels(df$Lon))[df$Lon]
  df$Years <- as.numeric(levels(df$Years))[df$Years]
  df$Publications <- as.numeric(levels(df$Publications))[df$Publications]
  return(df)
}

#' Filter columns of a data.frame
#' @param df a data.frame
#' @param colname_list character, the names of the column(s) to filter
#' @return a data.frame with the column(s) present in `colname_list`
#' @export
filter_columns <- function(df, colname_list = c("Country_current", "Years")){df[, colnames(df) %in% colname_list]}


#' Filter rows of a data.frame by country
#' @param .df a data.frame
#' @param country character, country to search for
#' @param country_type character, name of the column in which to search for `country`
#' @return a data.frame filtered by rows containing `country`
#' @export
filter_by_country <- function(.df = df, country = "Brazil", country_type = "Country_current"){.df[.df[[country_type]]==country, ]}

#' Manually melt a data.frame
#' @param df_country a country data.frame
#' @param country_type character, name of the column for the different types of `country`
#' @return a data.frame
#' @export
melt_df_country <- function(df_country, country_type = "Country_current"){
  df_country <- df_country[, colnames(df_country) != country_type]
  column_names <- colnames(df_country)
  l.df_country <- lapply(seq(ncol(df_country)), function(j) data.frame(variable = rep(column_names[j], nrow(df_country)), value = as.character(df_country[, j])))
  df_country <- do.call(rbind, l.df_country)
  return(df_country)
}

#' Plot each variables in a melted data.frame in a faceted `ggplot`
#' @param df_country a melted data.frame with a `variable` column
#' @param title_text character, to be used as title for the graph
#' @import ggplot2
#' @export
plot_df_country <- function(df_country, title_text = "Brazil" ){
  df_country_fact <- df_country[!df_country$variable %in% c("Years", "Publications"), ]
  df_country_num <- df_country[df_country$variable %in% c("Years", "Publications"), ]
  if(nrow(df_country_fact) > 1){
    p_factor <- ggplot(df_country_fact, aes(x = value)) +
    theme_minimal() +
    geom_bar() +
    coord_flip() +
    labs(title = title_text) +
    facet_wrap(variable ~ ., scales = "free", ncol = 2)
    print(p_factor)
  }
  if (nrow(df_country_num) > 1){
    df_country_num$value <- as.numeric(df_country_num$value)
    p_numeric <- ggplot(df_country_num, aes(x = value)) +
    theme_minimal() +
    geom_histogram() +
    coord_flip() +
    labs(title = title_text) +
    facet_wrap(variable ~ ., scales = "free", ncol = 2)
    print(p_numeric)
  } 
}

#' Extract the list of possible `country`
#' @param df a data.frame
#' @param country_type character, name of the column for the different types of `country`
#' @return character, list of unique values for the `country_type`
#' @export
get_countries <- function(df, country_type = "Country_current"){as.character(sort(unique(df[[country_type]])))}

