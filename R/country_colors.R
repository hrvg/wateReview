#' Convert rgb to hex colors
#' @param r red value
#' @param g green value
#' @param b blue value
#' @return hex code
#' @export
rgb2hex <- function(r,g,b) sprintf('#%s',paste(as.hexmode(c(r,g,b)),collapse = ''))

#' Get the dominant colors from country flags
#' The country flag are extracted from http://hjnilsson.github.io/country-flags/
#' @param iso ISO Alpha-2 code
#' @param n number of colors
#' @return top `n` colors
#' @export
get_topColors <- function(iso, n = 3){
	source <- system.file("extdata", paste0("country-flags/png100px/", iso, ".png"), package = "wateReview")
	img <- png::readPNG(source, native = FALSE, info = FALSE)
	red <- c(img[,, 1] * 255)
	green <- c(img[,, 2] * 255)
	blue <- c(img[,, 3] * 255)
	img <- cbind(red, green, blue)
	colors <- apply(img, MARGIN = 1, function(row) rgb2hex(row[1], row[2], row[3]))
	topColors <- rev(names(tail(sort(table(colors)), n)))
	return(topColors)
}
