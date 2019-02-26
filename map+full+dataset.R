# Josua Lutian 
# Jan 18, 2019 
# Make the map from a collated dataset 

library(maps)
library(geosphere)
library(ggplot2)
library (jpeg)
library(tidyverse)
library(data.table)
library (grid)
library (ggsave)
library (gganimate)
library (transformr)

MapData <- read.csv("./PSPBN2018+Jan1819.csv",fill =TRUE)

# background map
earth <- readJPEG("nmap.jpg", native = TRUE)
earth <- rasterGrob(earth, interpolate = TRUE)

#the problem is that there is no destination
#connecting lines 
all_pairs <- as.data.frame(cbind(MapData$lon0,MapData$lat0,MapData$lon1,MapData$lat1, MapData$year))
colnames(all_pairs)=c("lon0","lat0","lon1","lat1","year")
all_pairs <- all_pairs[complete.cases(all_pairs),]

# Calculate the intermediate points 
connect_lines = function(dep_lon,dep_lat,arr_lon,arr_lat,group){
  inter <- gcIntermediate(c(dep_lon, dep_lat), c(arr_lon, arr_lat), n=50, addStartEnd=TRUE, breakAtDateLine=FALSE)             
  inter=data.frame(inter)
  inter["group"] <- NA
  colnames(inter)=c("lon","lat","group")
  diff_of_lon=abs(dep_lon) + abs(arr_lon)
  #they are dividing it into two groups; ito two line 
  if(diff_of_lon > 180){
    inter$group[ which(inter$lon>=0)]=paste(group, "A",sep="")
    inter$group[ which(inter$lon<0)]=paste(group, "B",sep="")
  }else{
    inter$group=group
  }
  return (inter)
}

# add connections; go through the list and make lines
mapping_p = data.frame()
for (i in 1: length(all_pairs$lon0)){
  tmp = connect_lines(all_pairs$lon0[i], all_pairs$lat0[i], all_pairs$lon1[i], all_pairs$lat1[i],i)
  mapping_p = rbind (mapping_p,tmp)
}

#plot this map!!!
map_plot <- ggplot (all_pairs) + 
  transition_time(year) +
  #map area
  annotation_custom(earth, xmin=-180, xmax=180, ymin=-90, ymax=90) + 
  
  #this makes the lines 
  geom_path(aes(lon, lat, group =  group), alpha = 0.1, size = 0.5, color = "pink", data = mapping_p)+
  
  #departure points
  geom_point(data = all_pairs,aes(lon0, lat0), alpha = 0.1, size = 0.7, colour = "green") +
  
  # arrival points 
  geom_point(data = all_pairs,aes(lon1, lat1), alpha = 0.1, size = 0.7, colour = "green")+
  
  #you have to edit it so that it actually goes through each year 
  transition_layers(layer_length = 1, transition_length = 1, from_blank = FALSE) + 
  
  #takes away all of the regular graph materials 
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         panel.background = element_blank(),
         axis.title = element_blank(),
         axis.text = element_blank(),
         axis.ticks.length = unit(0, "cm"),
         plot.margin=unit(c(-4.7,-4.7,-4.7,-4.7),"cm"),
         legend.position = "none")+
  xlim(-180,180) +
  ylim(-90,90) +
  labs(x=NULL, y=NULL)+
  coord_equal()
#saving the file 
#p <-animate(map_plot, renderer = av_renderer(vfilter = 'fade=in:5:20:color=yellow'))
p <- animate (map_plot)
anim_save("try.gif")
#newFileName = paste ("collaboration123",".png", sep = "") 
#ggsave(newFileName, width = 36, height = 18, units = "in", dpi = 100)



