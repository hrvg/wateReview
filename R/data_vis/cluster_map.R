library(raster)
library(tmap)

#################
### FUNCTIONS ###
#################

read_countries_database <- function(db_path = "/Users/noellepatterson/apps/Other/sturdy-umbrella/R/data_vis/data/countries_database.csv"){
  countries_database <- read.csv(db_path, header = TRUE)
  countries_database$COUNTRY <- as.character(countries_database$COUNTRY)
  countries_database$Country_clusters <- as.factor(countries_database$Country_clusters)
  return(countries_database)
}

read_countries_shapefile <- function(map_file = "/Users/noellepatterson/apps/Other/sturdy-umbrella/R/data_vis/data/LAC"){
  countries_shapefile <- shapefile(map_file)
  return(countries_shapefile)
}

join_database_shapefile <- function(countries_database, countries_shapefile){
  countries_shapefile <- countries_shapefile[which(countries_shapefile$COUNTRY %in% countries_database$COUNTRY), ]
  countries_shapefile <- countries_shapefile[match(countries_database$COUNTRY, countries_shapefile$COUNTRY), ]
  countries_shapefile@data <- countries_database
  return(countries_shapefile)	
}

make_map <- function(attribute, title = NULL, colors){
  if (is.null(title)){
    title <- attribute
  }
  c_map <- tm_shape(countries_shapefile) +
    tm_fill(col = attribute, title = title, palette = colors) +
    tm_borders(lwd = 1, col = "black")
    # tm_compass(type = "4star", position = c("right", "top")) + #add compass and scale bar
    # tm_scale_bar(breaks = c(0, 100, 200), size = 3, position = c("right", "bottom"))+
    # tm_layout(bg.color = "lightblue")
  print(c_map)
}
#################
###    MAIN   ###
#################
countries_shapefile <- join_database_shapefile(read_countries_database(), read_countries_shapefile())
colors <- c("#F8766D", "#00BFC4", "#7CAE00")
make_map("Country_clusters", "title", colors)

