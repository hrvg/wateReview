


#library hexa map

# library
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(geogrid)
library(sf)
library(tmap)
library(tmaptools)
#NSF general geo

#read shapefile

lashp<-readOGR(dsn ='C:/Users/rdg/WMLA/LAC_di.shp')


#merge normalize diversity values#Merge a Spatial* object having attributes with a data.frame

normalize_data<- read.csv('C:/Users/rdg/WMLA_rmap/data/diversity_normalize2.csv')

LAC_normalize_diversity <- merge(lashp, normalize_data,  by.x= "COUNTRY", by.y = "country")


#LAC raw map

rawplot <- tm_shape(LAC_normalize_diversity)+
  tm_polygons("NSFgeneral_normalized",
              style="cont", palette= "Blues") + 
  tmap_options(max.categories=50) +
  tm_text("NAME")
plot(rawplot)


#generate HEXA

new_cells_hex <- calculate_grid(shape = lashp, grid_type = "hexagonal", seed = 5)
resulthex <- assign_polygons(LAC_normalize_diversity, new_cells_hex)

#transfer from real space to hexa space
hexplot <- tm_shape(resulthex) + 
  tm_polygons("NSFgeneral_normalized",style="cont",breaks = c(0.50,0.60,0.70,0.80,0.90,1), palette="Blues")+tm_text("NAME")

hexplot 
LAC_nsfgeneral_plot<-hexplot
LAC_nsfgeneral_plot

#savemap
#tmap_save(LAC_nsfgeneral, "LAC_nsfgeneral.png", height=7)



####hexamap NSF_specific###

#transfer from real space to hexa space (without island)
#palette = "RdBu" # show a strong contrast of the values or "Blues"
hex_specific <- tm_shape(resulthex) + 
  tm_polygons("NSf_specific_normalized", style="cont",breaks = c(0.50,0.60,0.70,0.80,0.90,1), palette = "Blues") + tm_text("NAME")
hex_specific 
LAC_NSF_specicif_hexamap<-hex_specific 
LAC_NSF_specicif_hexamap

##hexamp water budget##
LAC_waterbudget_hexamap<- tm_shape(resulthex) + 
  tm_polygons("budget_normalized", style="cont",breaks = c(0.50,0.60,0.70,0.80,0.90,1), palette = "Blues") + tm_text("NAME")

LAC_waterbudget_hexamap



##explore pallete

palette_explorer()



