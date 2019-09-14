# Function will take an input matrix and rescale each variable from 0 to 1
# rescale equation -> (x - min) / (max - min)
## Written by Colin Byrne, UC Davis Postdoc, 2017

rescale0_1 <- function(var_mat) {
	
	# Determine number of variables (columns) and samples (rows)
	var_num <- dim(var_mat)[2]
	sample_num <- dim(var_mat)[1]
	
	rescale_matrix <- matrix(nrow=sample_num, ncol=var_num)
	
	# Loop through each variable and determine
	for (i in 1:var_num) {
		# Determine variable maximum and minimum, NA values currently NOT removed
		var_max <- max(var_mat[,i],na.rm = TRUE)
		var_min <- min(var_mat[,i],na.rm = TRUE)
		
		# Rescale respective variable
		var_rescale <- (var_mat[,i] - var_min) / (var_max - var_min)
		
		# Assign rescaled variable to new rescaled matrix of all variables
		rescale_matrix[,i] <- var_rescale
	}
	
	return(rescale_matrix)
}