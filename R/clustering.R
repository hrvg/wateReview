#' Read the country database file
#' @param file character, a filename to fetch into `extdata`
#' @return a data.frame
#' @export
read_countries_database <- function(file = 'countries_database_final.csv'){
	countries_database <- read.csv(system.file("extdata", file, package = "wateReview"), header = TRUE)
	countries_database$COUNTRY <- as.character(countries_database$COUNTRY)
	return(countries_database)
}


#' Read the country shapefile
#' @param file character, a filename to fetch into `extdata`
#' @return a SpatialPolygonsDataFrame
#' @export
read_countries_shapefile <- function(file = 'LAC.shp'){
	countries_shapefile <- raster::shapefile(system.file("extdata", file, package = "wateReview"))
	return(countries_shapefile)
}

#' Assign the data from the country database to the country shapefile
#' @param countries_shapefile a raster::shapefile file with a `COUNTRY` attribute
#' @param countries_database a data.frame with the country database and with a `COUNTRY` column
#' @return a SpatialPolygonsDataFrame with the country database adjoined
#' @export
join_database_shapefile <- function(countries_database, countries_shapefile){
	countries_shapefile <- countries_shapefile[which(countries_shapefile$COUNTRY %in% countries_database$COUNTRY), ]
	countries_shapefile <- countries_shapefile[match(countries_database$COUNTRY, countries_shapefile$COUNTRY), ]
	countries_shapefile@data <- countries_database
	return(countries_shapefile)	
}

#' Makes a map for a give attribute
#' @param attribute character, name of an attribute to map
#' @param title optional, default to `NULL`, costum title for the map
#' @import tmap
#' @return a tmap object
#' @export
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
  # print(c_map)
  return(c_map)
}


#' Makes a density plot of a selected attribute
#' @param attribute_name character, name of an attribute to create a density plot
#' @param shp a SpatialPolygonsDataFrame with attributes
#' @return a `ggplot2` object
#' @import ggplot2
#' @export
make_hist <- function(attribute_name, shp = countries_shapefile){
  df <- data.frame(attribute = shp@data[[attribute_name]])
  p <- ggplot(df, aes(x = attribute)) +
    theme_minimal() +
    geom_density() +
    labs(title = attribute_name)
  return(p)
}

#' Select the type of data to display
#' @param data_type character, of one "country_descriptors", "survey_results", "survey_results_pct", "all"; default to "country_descriptors"
#' @return a matrix
#' @export 
select_data <- function(data_type = "country_descriptors"){
    if (data_type == "country_descriptors"){
      ind <- 2:46
    } else if(data_type == "survey_results"){
      ind <- 47:78
    } else if(data_type == "survey_results_pct"){
      ind <- 79:110  
    } else if(data_type == "all"){
      ind <- -1
    }
    mat <- as.matrix(countries_shapefile@data[, ind])
    mat <- scale(mat)
    mat <- data.frame(mat)
    colnames(mat) <- colnames(countries_shapefile@data[, ind])
    rownames(mat) <- countries_shapefile$COUNTRY
    if (data_type != "country_descriptors"){
      mat <- na.omit(mat)
    }
    return(mat)
}

#' Make a correlation matrix
#' @param data_type character, of one "country_descriptors", "survey_results", "survey_results_pct", "all"; default to "country_descriptors"
#' @param corr_type character, one of "spearman", "pearson"
#' @param rescale logical, default to `FALSE`, controls if the data have to be rescaled
#' @param .plot logical, controls if the correlation plot is outputed
#' @return a correlation matrix
#' @export
make_corrmatrix <-function(data_type = "country_descriptors", corr_type = "spearman", rescale = FALSE, .plot = TRUE){
  if (data_type == "country_descriptors"){
    ind <- 2:46
  } else if(data_type == "all"){
    ind <- -1
  }
  mat <- as.matrix(countries_shapefile@data[, ind])
  if (rescale){
    mat <- scale(mat)
    colnames(mat) <- colnames(countries_shapefile@data[, ind])
    rownames(mat) <- rownames(countries_shapefile@data[, ind])
  }
  correlations <- Hmisc::rcorr(mat, type = corr_type)
  if (.plot){
    corrplot::corrplot(correlations$r, type = "upper", title = paste(corr_type, ifelse(rescale, "rescaled", "")))
  }
  rescaled_data <- data.frame(mat)
  rownames(rescaled_data) <- countries_shapefile$COUNTRY
  return(rescaled_data)
}


#' Transform the data with centering, scaling and Box-Cox transformations
#' @param data a data.frame
#' @param methods character, methods for the `caret::preProcess` functions
#' @return a data.frame with transformed data
#' @export
transform_data <- function(data, methods = c("center", "scale", "BoxCox")){
    preProc <- caret::preProcess(data, method = methods)
    print(preProc)
    preprocessed.data <- caret::predict(preProc, data)
    return(preprocessed.data)
}


#' Make a dendrogram
#' @param rescaled_data a data.frame with data
#' @param num_clusters numeric, number of clusters
#' @param check_na logical, default to `FALSE`, controls if the data have to be checked for `NA`
#' @return a dendrogram
#' @import factoextra
make_dendrogram <- function(rescaled_data, num_clusters = 3, check_na = FALSE){
    if(check_na){
      ind <- apply(rescaled_data, MARGIN = 2, FUN = function(col) !any(is.na(col)))
      dist_mat <- dist(rescaled_data[, ind])
    } else {
      dist_mat <- dist(rescaled_data)
    }
    data_cluster <- hclust(dist_mat, method = "ward.D2")
    plot(data_cluster)
    rect.hclust(data_cluster, k=num_clusters, border = "green")
    # clust_num <- NbClust(rescaled_data[, ind], distance="euclidean", min.nc=3, max.nc=10, method="ward.D2")
    # print(table(clust_num$Best.n[1,]))
}

#' Count the number of `NA`
#' @param x vector of data
#' @return numeric, number of `NA` found
#' @export
count_nas <- function(x) sum(is.na(x))

#' Display the optimum number of cluster for a given clustering method
#' @param df a data.frame
#' @param method character, one of "kmeans", "hierarchical"
#' @param kmax numeric, the maximum number of cluster to test
#' @param gap logical, controls if the gap statistics have to be computed, default to `FALSE`
#' @return a `gridExtra` object with `ggplot2` plots
#' @import factoextra
#' @export
get_optimk <- function(df, method = "kmeans", kmax = 10, gap = FALSE){
    par_list <- switch(method,
      "kmeans" = list(FUN = kmeans, clMethod = "kmeans"),
      "hierarchical" = list(FUN = hcut, clMethod = "hierarchical")
    )
    p_wss <- fviz_nbclust(df, par_list$FUN, method = "wss", k.max = kmax)
    p_sil <- fviz_nbclust(df, par_list$FUN, method = "silhouette", k.max = kmax)
    if (gap){
      gap_stat <- clusGap(df, FUN = par_list$FUN, nstart = 25, K.max = kmax, B = 50)
      p_gap <- fviz_gap_stat(gap_stat) 
      return(gridExtra::grid.arrange(grobs = list(p_wss, p_sil, p_gap)))
    } else {
      return(gridExtra::grid.arrange(grobs = list(p_wss, p_sil)))
    }
}

#' Evaluates the cluster stability
#' @param df a data.frame
#' @param method character, one of "kmeans", "hierarchical"
#' @param kmax numeric, the maximum number of cluster to test
#' @return a named list with two elements: `stab` the result from the stability function `clValid::clValid` and `optim_df` a data.frame with optimum values following the one-standard-deviation rule
#' @export 
clStab <- function(df, method = "kmeans", kmax = 10){
    par_list <- switch(method,
        "kmeans" = list(FUN = kmeans, clMethod = "kmeans"),
        "hierarchical" = list(FUN = hcut, clMethod = "hierarchical")
      )
    stab <- clValid::clValid(df, nClust = 2:kmax, clMethods = par_list$clMethod, 
                    validation = "stability")
    sd_measures <- apply(stab@measures, 1, sd)
    min_measures <- apply(stab@measures, 1, min)
    one_sd <- t(sapply(seq_along(sd_measures), function(i) min_measures[i] + sd_measures[i] > stab@measures[i, ,]))
    optim_df <- data.frame(measure = stab@measNames, cluster = colnames(stab@measures)[apply(one_sd, 1, which.max)])
    return(list(stab = stab, optim_df = optim_df))
}

#' Convenience function for the shinyApp
#' @param data_type a character
#' @return numeric index
#' @export
select_list <- function(data_type){
  which(c("country_descriptors", "survey_results", "survey_results_pct", "lda") == data_type)
}