### library ###
library("raster")
library("sf")


# list files
# get shp only
# read shp
data_dir <- "F:/hguillon/research/exploitation/R/latin_america/data/natearth_data/"
lf_full <- list.files(file.path(data_dir), full.names = TRUE, recursive = TRUE)
lf <- list.files(file.path(data_dir), full.names = FALSE, recursive = TRUE)
lf_ext <- sapply(lf, tools::file_ext)
lf_full <- lf_full[lf_ext == "shp"]
lf <- lf[lf_ext == "shp"]
l.shp <- lapply(lf_full, read_sf)

# filter for area of study
LAC <- shapefile("../latin_america_SQ_internship/data/LAC.shp")
LAC <- read_sf("../latin_america_SQ_internship/data/LAC.shp")

file <- '../latin_america_SQ_internship/data/countries_database.csv'
countries_database <- read.csv(file, header = TRUE)

l.shp_cropped <- lapply(l.shp, function(shp) st_intersection(shp, LAC))
l.shp_cropped <- lapply(l.shp_cropped, function(shp)  as(shp, "Spatial"))


tmp <- sapply(l.shp_cropped, function(shp){
print(table(countries_database$COUNTRY %in% shp$COUNTRY))
print(levels(countries_database$COUNTRY)[which(!countries_database$COUNTRY %in% shp$COUNTRY)])
})

country_dic <- countries_database$COUNTRY
states_provinces_dic <- unique(na.omit(l.shp_cropped[[1]]$name))
regions_dic <- unique(na.omit(l.shp_cropped[[2]]$name))
glaciers_dic <- unique(na.omit(l.shp_cropped[[3]]$name))

# filter for non-unique river names and city names
popplaces_dic <- na.omit(l.shp_cropped[[4]]$NAME)
rivers_dic <- na.omit(l.shp_cropped[[5]]$name)
popplaces_dic <- unique(popplaces_dic)[table(popplaces_dic) == 1]
rivers_dic <- unique(rivers_dic)[table(rivers_dic) == 1]

dictionnaries <- list(country_dic, states_provinces_dic, regions_dic, glaciers_dic, popplaces_dic, rivers_dic)
dictionnaries <- list(country_dic, states_provinces_dic, regions_dic, glaciers_dic, rivers_dic)
sapply(dictionnaries, length)

dictionnaries <- unlist(dictionnaries)
dictionnaries <- sapply(dictionnaries, tolower)

library("quanteda")

token_dic <- tokens(dictionnaries, 
	remove_punct = TRUE,
	remove_symbols = TRUE,
	remove_numbers = TRUE,
	remove_separators = TRUE,
	remove_hyphens = TRUE,
	remove_url = TRUE)

stemmed <- tokens_wordstem(token_dic)
tkns <- tokens_select(stemmed, min_nchar = 4)
tkns <- unique(unlist(tkns))
saveRDS(tkns, "./data/geographical_tokens.Rds")