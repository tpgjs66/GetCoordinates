
hh<-read.csv("D:/Projects/github/GetCoordinates/household-gen.txt",sep = ",")

ppcs <-  rgdal::readOGR("D:/Projects/github/GetCoordinates/data/pc6/pc6.shp",
                        layer = "PC6", GDAL1_integer64_policy = TRUE)


pc6land <-  rgdal::readOGR("D:/Projects/github/GetCoordinates/data/pc6/PC6_BBG2012.shp",
                        layer = "PC6_BBG2012", GDAL1_integer64_policy = TRUE)


# Split PC6 to get PC4
ppcs@data$PC4<-substr(ppcs@data$Postcode,1,4)

# Sample ppcs only where households are located
ppcsSample <- subset(ppcs, ppcs@data$PC4 %in% hh$Home)

# Filter residential area only
ppcsSample <- ppcsSample[which(ppcsSample$BG2012 == 20),]

########### Loop for getting household location coordinate #################
### The household file above didn’t have coordinates but it is just 6 ppc.
### Here is a lookup that gives coordiantes and randomly disribute households.

coords <- c()
hhid <- c()
for (i in ppcsSample@data$PC6) {
 
  # Get houehold ID
  hhid <- append(hhid,hh$HHID[hh$Home == i])
  t <- ppcsSample[ppcsSample@data$PC4 == i,]
  
  
  
  # Candiate PC6 to locate household
  polygon <- ppcsSample[ppcsSample@data$PC4 == i,]@polygons
  
  # This is a loop for choosing polygons to locate household
  chosenPolygon <- 1
  for (j in 1:length(polygon)) {
    if (j > 1) {
      if (polygon[[j]]@area > polygon[[j-1]]@area){
        chosenPolygon <- j
      }
    }
  }
  
  
  
  coords <- rbind(coords,spsample(polygon, n = n, type = "random")@coords)
}
hh$Lng <- coords[,1]
hh$Lat <- coords[,2]