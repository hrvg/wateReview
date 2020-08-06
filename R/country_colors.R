rgb2hex <- function(r,g,b) sprintf('#%s',paste(as.hexmode(c(r,g,b)),collapse = ''))

get.topColors <- function(iso, n = 3){
	source <- paste0("../country-flags/png100px/", iso, ".png")
	img <- readPNG(source, native = FALSE, info = FALSE)
	red <- c(img[,, 1] * 255)
	green <- c(img[,, 2] * 255)
	blue <- c(img[,, 3] * 255)
	img <- cbind(red, green, blue)
	colors <- apply(img, MARGIN = 1, function(row) rgb2hex(row[1], row[2], row[3]))
	topColors <- rev(names(tail(sort(table(colors)), n)))
	return(topColors)
}
