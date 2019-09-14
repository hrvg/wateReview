#################
### LIBRARIES ###
#################

library(raster)
library(tmap)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(caret)
library(NbClust)
library(Amelia)
library(oce)
library(clusterSim)

#################
### FUNCTIONS ###
#################

source("R/rescale0_1.R")

read_countries_database <- function(file = './data/countries_database.csv'){
	countries_database <- read.csv(file, header = TRUE)
	countries_database$COUNTRY <- as.character(countries_database$COUNTRY)
	return(countries_database)
}

read_countries_shapefile <- function(file = './data/LAC.shp'){
	countries_shapefile <- shapefile(file)
	return(countries_shapefile)
}

join_database_shapefile <- function(countries_database, countries_shapefile){
	countries_shapefile <- countries_shapefile[which(countries_shapefile$COUNTRY %in% countries_database$COUNTRY), ]
	countries_shapefile <- countries_shapefile[match(countries_database$COUNTRY, countries_shapefile$COUNTRY), ]
	countries_shapefile@data <- countries_database
	return(countries_shapefile)	
}

make_map <- function(attribute, title = NULL){
  if (is.null(title)){
    title <- attribute
  }
  c_map <- tm_shape(countries_shapefile) +
    tm_fill(col = attribute, title = title) +
    tm_borders(lwd = 1)+
    tm_compass(type = "4star", position = c("right", "top")) + #add compass and scale bar
    # tm_scale_bar(breaks = c(0, 100, 200), size = 3, position = c("right", "bottom"))+
    tm_layout(bg.color = "lightblue")
  print(c_map)
}

make_hist <- function(attribute_name){
  df <- data.frame(attribute = countries_shapefile@data[[attribute_name]])
  p <- ggplot(df, aes(x = attribute)) +
    theme_minimal() +
    geom_histogram() +
    labs(title = attribute_name)
  print(p)
}

make_corrmatrix <-function(data_type = "country_descriptors",corr_type = "spearman", rescale = FALSE){
  if (data_type == "country_descriptors"){
    ind <- 2:42
  } else if(data_type == "all"){
    ind <- -1
  }
  mat <- as.matrix(countries_shapefile@data[, ind])
  if (rescale){
    mat <- rescale0_1(mat)
    colnames(mat) <- colnames(countries_shapefile@data[, ind])
    rownames(mat) <- rownames(countries_shapefile@data[, ind])
  }
  correlations <- Hmisc::rcorr(mat, type = corr_type)
  print(correlations)
  corrplot(correlations$r, type = "upper", title = paste(corr_type, ifelse(rescale, "rescaled", "")))
  #corrplot.mixed(correlations$r, lower="number", upper="ellipse")
  return(data.frame(mat))
}

transform_data <- function(data, methods = c("center", "scale", "BoxCox")){
    preProc <- preProcess(data, method = methods)
    print(preProc)
    preprocessed.data <- predict(preProc, data)
    return(preprocessed.data)
}

make_dendrogram <- function(rescaled_data, num_clusters = 3, check_na = FALSE){
    if(check_na){
      ind <- apply(rescaled_data, MARGIN = 2, FUN = function(col) !any(is.na(col)))
      dist_mat <-dist(rescaled_data[, ind])
    } else {
      dist_mat <-dist(rescaled_data)
    }
    data_cluster <- hclust(dist_mat, method = "ward.D2")
    plot(data_cluster)
    rect.hclust(data_cluster, k=num_clusters, border = "green")
    # clust_num <- NbClust(rescaled_data[, ind], distance="euclidean", min.nc=3, max.nc=10, method="ward.D2")
    # print(table(clust_num$Best.n[1,]))
}

count_nas <- function(x) sum(is.na(x))

############
### MAIN ###
############

## LOADING DATA ##

countries_shapefile <- join_database_shapefile(read_countries_database(), read_countries_shapefile())

## MAPPING ATTRIBUTES

# for (attribute in names(countries_shapefile)){
#   make_map(attribute)
# }

make_map(sample(names(countries_shapefile), 1))

## HISTOGRAMS

# for (attribute in names(countries_shapefile)[-1]){
#   make_hist(attribute)

make_hist(sample(names(countries_shapefile), 1))


## RESCALING DATA

dev.new()
rescaled_data <- make_corrmatrix(corr_type = "spearman", rescale = TRUE)  
rownames(rescaled_data) <- countries_shapefile$COUNTRY

## HIERARCHICAL CLUSTERING

# check if some data are missing
missmap(rescaled_data)


### COUNTING MISSING VALUES TO SELECT THE BEST SET OF COUNTRY DESCRIPTORS / COUNTRIES

# columns
count_nas_col <- apply(rescaled_data, MARGIN = 2, FUN = count_nas) 
# keeping the lapply / apply examples
# lapply(seq(ncol(rescaled_data)), function(j) count_nas(rescaled_data[, j]))

# lines
count_nas_row <- apply(rescaled_data, MARGIN = 1, FUN = count_nas) 
# keeping the lapply / apply examples
# lapply(seq(nrow(rescaled_data)), function(i) count_nas(rescaled_data[i, ]))
names(count_nas_row) <- countries_shapefile$COUNTRY

# building the for loop from the pseudo-code
# for (na_threshold in sort(unique(count_nas_row), decreasing = TRUE)){
#     filtered_data <- rescaled_data[count_nas_row <= na_threshold, ]
#     count_nas_col <- apply(filtered_data, MARGIN = 2, FUN = count_nas) # columns
# }

# from the for loop to lapply
na_thresholds <- sort(unique(count_nas_row), decreasing = TRUE)

list_count_nas_col <- lapply(na_thresholds, function(na_threshold){
    filtered_data <- rescaled_data[count_nas_row <= na_threshold, ]
    current_count_nas_col <- apply(filtered_data, MARGIN = 2, FUN = count_nas) # columns
    return(current_count_nas_col)
})
count_nas_col_df <- do.call(cbind, list_count_nas_col)

list_country_left <- sapply(na_thresholds, function(na_threshold){
    filtered_data <- rescaled_data[count_nas_row <= na_threshold, ]
    return(nrow(filtered_data))
})

dev.new()
par(mfrow = c(2, 1))
oce::imagep(t(count_nas_col_df), col = oce.colorsViridis(), decimate = FALSE, xlab = "NA threshold", ylab = "Country descriptors", main = "", cex = 1)
# oce::imagep(as.matrix(list_country_left), col = oce.colorsViridis(), decimate = FALSE, xlab = "NA threshold", ylab = "Country descriptors", main = "", cex = 1)
plot(list_country_left)

### FILTERING DATA
# based on the visualization and short discussion we filter the country descriptors
drops <- c("variability", "surface_withdrawl", "ground_withdrawl", "SPI..2017.", "SPI_basic_human", "SPI_found", "SPI_opp")
rescaled_data <- rescaled_data[, !colnames(rescaled_data) %in% drops]
count_nas_row <- apply(rescaled_data, MARGIN = 1, FUN = count_nas) # lines
rownames(rescaled_data) <- countries_shapefile$COUNTRY
# countries left
rownames(rescaled_data[count_nas_row <= 0, ])
# countries removed
setdiff(countries_shapefile$COUNTRY, rownames(rescaled_data[count_nas_row <= 0, ]))
filtered_data <- rescaled_data[count_nas_row <= 0, ]


### K-MEANS CLUSTERING TO SELECT THE IDEAL NUMBER OF CLUSTER

# How to Wrap
# 1. give a name to function: 
# good_descriptive_name <- function
# 2. add arguments to the function: 
# good_descriptive_name <- function(something){}
# 3. what does the function do?: 
# good_descriptive_name <- function(something){
#   do_something 
# }
# 4. what should the function return?:
# good_descriptive_name <- function(something){
#   do_something 
# return(something)
# }

## K-means clustering
# Create scree plot to determine the correct number of groups
## Adapted from Belize Lane by Colin Byrne, UC Davis Postdoc, 2017
max_clust <- nrow(filtered_data) - 1
seed <- 233
wss <- (nrow(filtered_data)-1)*sum(apply(filtered_data, 2, var))
for (i in 2:max_clust) {
  set.seed(seed)
  wss[i] <- sum(kmeans(filtered_data, centers=i)$withinss)
}
# Run the kmeans clustering for a chosen number of clusters
set.num <- 4
clust_k <- set.num #indicate number of clusters
k_means <- kmeans(filtered_data, clust_k, nstart=12, iter.max=1000) 
db <- index.DB(filtered_data, k_means$cluster)
# Print Davies-Bouldin clustering index to assess cluster strength
cat("Davies-Bouldin =", db$DB, "\n") 

## Plot K-means
# Scree plot
dev.new()
plot(1:max_clust, wss, type="b", xlab="number of Clusters", 
    ylab="Within groups sum of squares")

### DENDROGRAM
dev.new()
make_dendrogram(filtered_data, num_clusters = 5)