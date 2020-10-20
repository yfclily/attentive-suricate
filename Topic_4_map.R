# Topic 4: map

library('maptools')
library('rgdal')
library('raster')
library('ggplot2')
library('sf')
library("rnaturalearth")
library("rnaturalearthdata")
library('ggspatial')
library('rgbif')
library('mapr')
library('marmap')
library('RColorBrewer')
library('dplyr')
library('devtools')
library('mapview')
library('fields')
library('ggsflabel')
# devtools::install_github("yutannihilation/ggsflabel") # None

rm(list=ls())# clean all objects in memory

## maptools and rgdal packages
### simple world map
data(wrld_simpl)
plot(wrld_simpl)
plot(wrld_simpl,xlim=c(100,130),ylim=c(-20,50),col='olivedrab3',bg='lightblue')


### GPS: data tracking
par(mfrow=c(1,2))
run1 <- readOGR(dsn="Data/run.gpx",layer="tracks")
plot(run1, main='Line') # my running activity

run2 <- readOGR(dsn="Data/run.gpx",layer="track_points")
plot(run2, main='Points')

dev.off()

### writing spatial data
writeOGR(wrld_simpl,dsn="Data", layer = "world_test", driver = "ESRI Shapefile", overwrite_layer = TRUE)
world_shp <- readOGR(dsn = "Data",layer = "world_test")
plot(world_shp)

## Spatial data types
### Vector based

#### SpatialPointsDataFrame for plotting points
plot(wrld_simpl,xlim=c(115,128) ,ylim=c(19.5,27.5),col='#D2B48C',bg='lightblue') # TW map
coords <- matrix(c(121.537290,120.265541, 25.021335, 22.626524),ncol=2) # NTU and SYS univ. 
coords <- coordinates(coords) # assign values as spatial coordinates
spoints <- SpatialPoints(coords) # create SpatialPoints
df <- data.frame(location=c("NTU","SYS")) # create a dataframe
spointsdf <- SpatialPointsDataFrame(spoints,df) # create a SpatialPointsDataFrame
plot(spointsdf,add=T,col=c('black','black'),pch=19,cex=2.2) # plot it on our map
text(121,24, 'TAIWAN', cex=1)

#### SpatialLinesDataFrame for plotting lines: 
plot(wrld_simpl,xlim=c(-130,-60),ylim=c(45,80),col='#D2B48C',bg='lightblue')
coords <- matrix(c(-110,-102,-102,-110,-110,60,60,49,49,60),ncol=2)
l <- Line(coords)
ls <- Lines(list(l),ID="1")
sls <- SpatialLines(list(ls))
df <- data.frame(province="Saskatchewan")
sldf <- SpatialLinesDataFrame(sls,df)
plot(sldf,add=T,col='#3d2402', cex=2)
text(-114, 55, 'Saskatchewan', srt=90, cex=0.7)
text(-114, 63, 'CANADA', cex=1)

#### SpatialPolygonesDataFrame for plotting polygones
plot(wrld_simpl,xlim=c(-130,-60),ylim=c(45,80),col='#D2B48C',bg='lightblue')
coords <- matrix(c(-110,-102,-102,-110,-110,60,60,49,49,60),ncol=2)
p <- Polygon(coords)
ps <- Polygons(list(p),ID="1")
sps <- SpatialPolygons(list(ps))
df <- data.frame(province="Saskatchewan")
spdf <- SpatialPolygonsDataFrame(sps,df)
plot(spdf,add=T,col='#45220d') 
text(-114, 55, 'Saskatchewan', srt=90, cex=0.7)
text(-114, 63, 'CANADA', cex=1)
text(-103, 46, 'UNITED STATES', cex=1)
text(-40, 78, 'GREENLAND', cex=1)
text(-35, 55, 'Atlantic Ocean', cex=1, col='#071238')


## Map improvement: raster
download.file("https://www.dipintothereef.com/uploads/3/7/3/5/37359245/twn_adm.zip", destfile="Data/taiwan_shape.zip") # download file
unzip('taiwan_shape.zip',exdir="Data/taiwan_shape_unzip") # unzip it 
taiwan <- readOGR(dsn = "Data/taiwan_shape_unzip",layer = "TWN_adm0")
plot (taiwan) # simple naked map
plot (taiwan, axes=T, xlim=c(121,122), ylim=c(24,25.5), bg=colors()[431],col='grey') # add colors

TWN <- getData('GADM', country="TWN", level=1) # data Taiwan
JPN <- getData('GADM', country="JPN", level=1) # data Japan
plot(JPN,axes=T,bg=colors()[431],col='grey')

class(TWN) # SpatialPolygonsDataFrame
TWN$NAME_1 # gives the names of the provinces, incomplete in TW. Try to change level.

plot(TWN,col="grey",xlim=c(119,122.5), ylim=c(21.5,25.5), bg=colors()[431], axes=T)
KAO <- TWN[TWN$NAME_1=="Kaohsiung",]
plot(KAO,col="gray 33",add=TRUE)

plot(TWN,col="grey",xlim=c(121,122), ylim=c(24,25.5), bg=colors()[431], axes=T)
coords <- matrix(cbind(lon=c(121.2,121.6,121.8),lat=c(25,25.1,24.5)),ncol=2)
coords <- coordinates(coords)
spoints <- SpatialPoints(coords)
df <- data.frame(location=c("City 1","City 2","City 3"),pop=c(138644,390095,34562))
spointsdf <- SpatialPointsDataFrame(spoints,df)
scalefactor <- sqrt(spointsdf$pop)/sqrt(max(spointsdf$pop))
plot(spointsdf,add=TRUE,col='black',pch=16,cex=scalefactor*5) 
# add location of NTU
points(121.537290,25.021335, type="p", pch=18, col=2, cex=2)
# add text
text(121.437290,24.921335,"NTU", col='red', font=2)
# add scale
maps::map.scale(x=121.8, y=24.15)
# add north arrow
GISTools::north.arrow(xb=122.25,yb=24.5, len=0.06, lab='N')


## Mapping with ggplot2 and sf
### Getting started: dataset

theme_set(theme_bw()) 
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#### for conversion from st to sf: ####
#### wrld_simpl <- st_as_sf(wrld_simpl) #### 

### Data and basic plot (ggplot and geom_sf)
ggplot(data = world) +
   geom_sf()

ggplot(data = world) +
   geom_sf() +
   coord_sf(expand = FALSE)

ggplot(data = world) +
   geom_sf() +
   coord_sf(expand = FALSE) +
   xlab("Longitude") + ylab("Latitude") +
   ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))

ggplot(data = world) + 
   geom_sf(color = "black", fill = "lightgreen") +
   coord_sf(expand = FALSE) 

ggplot(data = world) +
   geom_sf(aes(fill = pop_est)) +
   coord_sf(expand = FALSE) +
   scale_fill_viridis_c(option = "plasma", trans = "sqrt")

ggplot(data = world) +
   geom_sf() +
   scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
   coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")

ggplot(data = world) +
   geom_sf() +
   coord_sf(xlim = c(118, 128), ylim = c(17, 27), expand = FALSE)

ggplot(data = world) +
   geom_sf() +
   annotation_scale(location = "br", width_hint = 0.4) +
   annotation_north_arrow(location = "br", which_north = "true", 
                          pad_x = unit(0.5, "cm"), pad_y = unit(1, "cm"),
                          style = north_arrow_fancy_orienteering) +
   coord_sf(xlim = c(118, 128), ylim = c(17, 27), expand = FALSE)

#### Add country names
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

ggplot(data = world) +
   geom_sf() +
   geom_text(data= world_points,aes(x=X, y=Y, label=name),
             color = "black", fontface = "bold", check_overlap = FALSE) +
   annotate(geom = "text", x = 124, y = 21, label = "Pacific Ocean", fontface = "italic", color = "#0b3c8a", size = 5) +
   annotate(geom = "text", x = 124.2, y = 24, label = "Ryukyu archipelago", fontface = "italic", color = "#d41919", size = 3) + 
   coord_sf(xlim = c(118, 128), ylim = c(17, 27), expand = FALSE)

### Finalize a map
ggplot(data = world) +
   geom_sf(fill= 'antiquewhite') + 
   geom_text(data= world_points,aes(x=X, y=Y, label=name), color = 'darkblue', fontface = 'bold', check_overlap = FALSE) + 
   annotate(geom = 'text', x = -90, y = 26, label = 'Gulf of Mexico', fontface = 'italic', color = 'grey22', size = 6) +
   annotation_scale(location = 'bl', width_hint = 0.5) + 
   annotation_north_arrow(location = 'bl', which_north = 'true', pad_x = unit(0.75, 'in'), pad_y = unit(0.5, 'in'), style = north_arrow_fancy_orienteering) + 
   coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE) + 
   xlab('Longitude') + ylab('Latitude') + 
   ggtitle('Map of the Gulf of Mexico and the Caribbean Sea') + 
   theme(panel.grid.major = element_line(color = gray(.5), linetype = 'dashed', size = 0.5), panel.background = element_rect(fill = 'aliceblue'))

ggsave("Output/Datamap.pdf")
ggsave("Output/map_web.png", width = 6, height = 6, dpi = "screen")

### Species distribution
gbif.res <- occ_search(scientificName = "Urocissa caerulea", limit = 1200)

map_ggplot(gbif.res) +
   coord_sf(xlim = c(120, 123), ylim = c(21, 26), expand = FALSE)

### Bathymetric map 

TW.bathy <- getNOAA.bathy(lon1=118,lon2=124, lat1=21,lat2=26,resolution=1) # don't put too wide / resolution: 1

blues <- colorRampPalette(c("darkblue", "cyan")) # define palette
greys <- colorRampPalette(c(grey(0.4),grey(0.99))) # define palette
# make the plot
plot.bathy(TW.bathy,
           image=T,
           deepest.isobath=c(-6000,-120,-30,0),
           shallowest.isobath=c(-1000,-60,0,0),
           step=c(2000,60,30,0),
           lwd=c(0.3,1,1,2),
           lty=c(1,1,1,1),
           col=c("grey","black","black","black"), 
           drawlabels=c(T,T,T,F),
           bpal = list(c(0,max(TW.bathy),greys(100)),c(min(TW.bathy),0,blues(100))),
           land=T, xaxs="i"
)

tw.profile <-get.transect(TW.bathy,x1=119.5,y1=23.75, x2=122,y2=23.75, dis=TRUE)
plotProfile(tw.profile) 
#### Not Run: extract a profile Manually
#### manual.profile<-get.transect (TW.bathy, loc=T,dist=T) 
#### plotProfile(manual.profile) 

### Interactive maps
#### Download pop data + map of Taiwan 

df.pop <- read.csv("Data/tw_population.csv", h = T)
df.pop <- data.frame(df.pop)

ggplot(df.pop, aes(x = reorder(County, Population), y = Population/10000, fill = Category)) +
   geom_bar(stat="identity") +
   coord_flip() +
   labs(title = "Taiwan Population", x = "County", y = "Population(萬)")

taiwan.map <- st_read ("Data/GADM/gadm36_TWN_2.shp")

ggplot(data = taiwan.map) +
   geom_sf() +
   labs(title = "Taiwan Map ")

ggplot(data = taiwan.map) +
   geom_sf(aes(fill = NAME_2), show.legend= F) +
   geom_sf_text(aes(label = NAME_2), size = 3) +
   labs(title = "Taiwan Map ")

ggplot(data = taiwan.map) +
   geom_sf(aes(fill = NAME_2)) +
   scale_fill_manual(name = "County",values = colorRampPalette(brewer.pal(8, "Accent"))(22)) +
   labs(title = "Taiwan Map ")

#### Combine the two datasets

my.taiwan.map <- taiwan.map[c("NAME_2", "geometry")]
my.taiwan.map$NAME_2 <- as.character(my.taiwan.map$NAME_2)
my.taiwan.map.data <- left_join(my.taiwan.map, df.pop,by= c("NAME_2" = "County"))

ggplot(data = my.taiwan.map.data) +
   geom_sf(aes(fill = Population))

ggplot(data = my.taiwan.map.data) +
   geom_sf(aes(fill = Population/10000)) +
   scale_fill_distiller(palette = "Spectral", name = "Population(x10,000)") +
   geom_sf_label_repel(aes(label = NAME_2, alpha = 1)) +
   labs(title="Taiwan popualtion map", x ="Longitude", y = "Latitude")

#### Make it fun:
mapview(my.taiwan.map.data["Population"])
# mapview(my.taiwan.map.data["Population"], col.regions = tim.colors(100))

