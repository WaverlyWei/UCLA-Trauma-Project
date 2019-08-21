library(rgdal)
library(maptools)
library(rgeos)
library(readstata13)
library(vimp)
library(SuperLearner)
library(ctmle)
library(ggplot2)
library(ggmap)
library(reshape2)
library(ggthemes)
library(dplyr)
library(data.table)
library(viridis)
library(scatterpie)


options(digits=15)

#sf <-rgdal::readOGR('geo_export_65c83fbf-6403-42af-9b15-5ee5c9e5dcfa.shp')
#class(sf)
#summary(sf)
#plot(sf)

setwd('/Users/waverlywei/Desktop/Violence Project ')
dat <- read.dta13("chronicdz3.dta")

## plot base map 
URL <- "https://github.com/simonkassel/Visualizing_SF_home_prices_R/raw/master/Data/SF_neighborhoods.zip"
download.file(URL, "SF_neighborhoods.zip")
unzip("SF_neighborhoods.zip")
# Read it into R as a spatial polygons data frame & plot
neighb <- readShapePoly("SF_neighborhoods")
plot(neighb)

# define boundaries 
bbox <- neighb@bbox




## ANOTHER METHOD ##
## Use SF-CRIME data as base 
#sf <- read.csv("Case_Data_from_San_Francisco_311__SF311.csv")
#sf_loc <- sf %>% select(Neighborhood,Point) %>% write.csv("sf_loc")
sf_loc <- read.csv("sf_loc")
sf <- sf_loc %>% dplyr::slice(sample(nrow(sf_loc),3000))
sf <- sf %>% dplyr::mutate (neighborhood = tolower(gsub(" ", "", Neighborhood, fixed = TRUE))) 
sf <- sf[!duplicated(sf$neighborhood), ]
merged <- left_join(dat,sf) 
merged$Point <- as.character(merged$Point) 

merged$Point <- lapply(merged$Point, function(x) gsub("[()]", "",x))
merged$Point <-lapply(merged$Point, function(x) unlist(strsplit(x,split = ",")))
merged$lat <- lapply(merged$Point, function(x) x[1])
merged$long <- lapply(merged$Point, function(x) x[2])
merged$lat <- merged %>% select(lat) %>% unlist() %>% as.numeric()
merged$long <- merged %>% select(long) %>% unlist() %>% as.numeric()
## This could work but with several missing values 

## NOW: plot onto map 
sf <- get_stamenmap(bbox = c(left = -122.5164, bottom = 37.7066, right = -122.3554, top = 37.8103), 
                   maptype = c("toner-lite"), zoom = 13)
map <- ggmap(sf)
map

## Let's analyze homicide first


p_dot <- map + 
  geom_point(data = homicide, aes(x = long, y = lat, color = homicide_ayll, size=homicide_ayll)) + 
  scale_colour_gradient(name = 'homicide_ayll', low="blue", high="red") + 
  scale_size(name = 'homicide_ayll', range = c(2,15)) +
  ggtitle("homicide_ayll SF Neighborhood Mapping")
# DOT map works!!
p_dot


# try PIE CHART
#ayll <- merged %>%  select(lat,long,homicide_ayll,totalpop_0to5yrs,
                           #totalpop_10to14yrs,totalpop_15to19yrs,
                           #totalpop_20to24yrs,totalpop_25to34yrs,
                           #totalpop_35to44yrs,totalpop_45to54yrs,
                           #totalpop_55to59yrs,totalpop_60to64yrs,
                           #totalpop_65to74yrs,totalpop_75to84yrs,
                           #totalpop_85plus)

# ============== START making pie chart ============ #

# all the ayll's and lat long 
#ayll <- merged[c(5:24,89:90)]
# only keep disease and homicide 
ayll <- merged[c(5:8,10:11,14:18,20:23, 89:90)]
tot <- rowSums(ayll[,1:15],na.rm = TRUE)
ayll$tot <- tot

#ayll$disease_ayll <- rowSums(ayll[,c(1:4,6:7)])
ayll$ChronicDisease_ayll <- rowSums(ayll[,c(1:14,16)])
#ayll$substance_ayll <- rowSums(ayll[,c(5,8,10:17,19)])
# accident_ayll
#suicide_ayll
# homicide_ayll




pie <- map +
    geom_scatterpie( data = ayll, 
                  aes(long,lat, r = sqrt(tot)/15000),
                  #cols = (names(ayll)[c(9,18,20,24,25)]), 
                  cols = (names(ayll)[c(14,19)]),
                  alpha = 0.5) + ggtitle("AYLL Geospatial Mapping") + 
  theme(plot.title = element_text(hjust = 0.5, size = 25)) 

# pie chart works!!!
pie
# ================= END of Pie ================== # 

## failed to work
p_tile <- map + 
  geom_tile(data = homicide, aes(x = long, y = lat, fill = homicide_ayll, alpha = homicide_ayll)) +
  scale_fill_gradient('Total homicide_ayll', low = 'blue', high = 'red')

p_tile


## try density binned 
## somehow works but bad looking
YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
pred.stat.map <- map %+% homicide + 
  aes(x = long,
      y = lat,
      z = homicide_ayll) +
  stat_summary_2d(fun = mean, 
                 binwidth = c(.05, .05),
                 alpha = 0.5) + 
  scale_fill_gradientn(name = "Median",
                       colours = YlOrBr,
                       space = "Lab") + 
  labs(x = "Longitude",
       y = "Latitude") +
  coord_map()


## another try
colormap <- c("Violet","Blue","Green","Yellow","Red","White")
pred.stat.map.final <- map %+% homicide + 
  aes(x = long,
      y = lat,
      z = homicide_ayll) +
  stat_summary_2d(fun = mean, 
                 binwidth = c(.05, .05),
                 alpha = 1.0) + 
  scale_fill_gradientn(name = "Median",
                       colours = colormap,
                       space = "Lab") + 
  labs(x = "Longitude",
       y = "Latitude") +
  coord_map()
print(pred.stat.map.final)


## try break multipolygon 
sf_2 <- read.csv("SFFind_Neighborhoods.csv")
geom<- as.character(sf_2$the_geom)
geom_list <- lapply(geom,function(x) strsplit(x,",") )
geom_list <- lapply(geom_list,function(x) strsplit(unlist(x)," "))

lon <- c()
lat <- c()
neighborhood <- c()
for (i in 1: length(geom_list)){
    v <- unlist(geom_list[i])
    lon <- c(lon,v[seq(2, length(v), 3)])
    lat <- c(lat,v[seq(3, length(v), 3)])
    neighborhood <- c(neighborhood,rep(as.character(sf_2$name[i]),length(v)))
    
}

cor <- data.frame(cbind(lon,lat,neighborhood))

cor$lon <- as.numeric(as.character(cor$lon))
cor$lat <- as.numeric(as.character(cor$lat))
cor$neighborhood <- as.character(cor$neighborhood)

cor <- cor %>% drop_na()
cor <- cor %>% mutate (neighborhood = tolower(gsub(" ", "", neighborhood, fixed = TRUE))) 
## merge with dat
combined <- left_join(cor,dat) %>% drop_na()

## ===================plot with multipolygon data

## Let's analyze homicide first
ayll <- combined %>%  select(lat,lon,homicide_ayll)
p_dot <- map + 
  geom_point(data = homicide, aes(x = lon, y = lat, color = mhi, size=mhi)) + 
  scale_colour_gradient(name = '# Total homicide_ayll', low="blue", high="red") + 
  scale_size(name = '# Total homicide_ayll', range = c(2,15)) 
# DOT map works!!
p_dot


## ================ready to output !!! ========
# works!!!
map_sf <- get_map('San Francisco', zoom = 12, maptype = 'satellite')
map_sf <- ggmap(map_sf)
####
ayll_map <- map_sf + 
  stat_density2d(data = ayll, aes(x = lon, y = lat, fill = ..density..), geom = 'tile', contour = F, alpha = .5) +
  scale_fill_viridis(option = 'inferno')+
  labs(title = str_c('homicide_ayll SF Neighborhood Mapping'
  ),fill = str_c('homicide_ayll')
  ) 

ayll_map


mhi <- combined %>%  select(lat,lon,mhi)

mhi_map <- map_sf + 
  stat_density2d(data = mhi, aes(x = lon, y = lat, fill = ..density..), geom = 'tile', contour = F, alpha = .5) +
  scale_fill_viridis(option = 'inferno')+
  labs(title = str_c('mhi SF Neighborhood Mapping'
  ),fill = str_c('mhi')
  ) 

mhi_map


pbpl <- combined %>%  select(lat,lon,pbpl)

pbpl_map <- map_sf + 
  stat_density2d(data = pbpl, aes(x = lon, y = lat, fill = pbpl), geom = 'polygon', contour = F, alpha = .5) +
  scale_fill_viridis(option = 'inferno')+
  labs(title = str_c('pbpl SF Neighborhood Mapping'
  ),fill = str_c('pbpl')
  ) 

pbpl_map


