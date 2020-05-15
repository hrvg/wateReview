library(sp)
# part of this code was provided by Arthur Koehl

construct_urls = function(cc)
{
    baseurl = "https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_"
    extensions = c("_1_sp.rds") 
    urls = paste0(baseurl, cc, extensions)
    return(urls)
}

get_attributes_level_one = function(url)
{
    tryCatch({
        download.file(url, destfile="tmp.rds", quiet= TRUE)
        data = readRDS("tmp.rds")
        file.remove("tmp.rds")
        return(data@data[,c(1,2,4,7)])
    }, error = function(e)
    {
	print(e)
	return(data.frame(GID_0=character(), NAME_0=character(), NAME_1=character(), TYPE_1=character()))
    })
}

## load in country codes for latin america
ccs = read.csv("latin-america.csv", stringsAsFactors=FALSE, header=FALSE)

## construct urls to download
urls = lapply(ccs$V2, construct_urls)

## for each url
data = lapply(unlist(urls), get_attributes_level_one)
level1 = do.call("rbind", data)

