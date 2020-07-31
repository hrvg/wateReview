#' Convenience functions to allow for inter-operability between different systems
#' @return a root file.path depending on the computer name
#' @export
get_rootdir <- function(){
	nodename <- Sys.info()['nodename']
	root.dir <- '/home/hguillon'
	if(nodename=='METRO-VH138X3'){root.dir <- 'F:/hguillon/research'}
	if(nodename=='metro-lvh138k-l'){root.dir <- '/home/hguillon/research'}
	return(root.dir)
}