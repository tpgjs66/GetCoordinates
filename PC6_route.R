library(sf)
library(sp)
library(leaflet)

## Load household data
hh<-read.csv("D:/Projects/github/GetCoordinates/household-Helmond-gen.txt",sep = ",")

## Load schedule data
sched<-read.csv("D:/Projects/github/GetCoordinates/schedule-Helmond-gen.txt",sep = ",")

sched$SchedID<-do.call(paste, c(sched[c("HHID", "MemID","EpisodeID")], sep = "-"))

## Load ppc zone data
ppcs <- rgdal::readOGR("D:/Projects/github/GetCoordinates/data/ppcs_single.shp",
                       layer = "ppcs_single", GDAL1_integer64_policy = TRUE)

## Load 6 digits PPCS land-use data
# pc6sf <- sf::read_sf("D:/Projects/github/GetCoordinates/data/EindhovenMuni.shp",
#                      layer = "EindhovenMuni")
# pc6sf$PC4 <- substr(pc6sf$PC6,1,4)

pc6sf <-   sf::read_sf("D:/Projects/github/GetCoordinates/data/pc6/PC6_BBG2012.shp",
                           layer = "PC6_BBG2012")
pc6sf$PC4<-substr(pc6sf$PC6,1,4)
 
# pc6sp <- rgdal::readOGR("D:/mygithub/AlbatrossViewer/data/pc6/EindhovenMuni.shp",
#                         layer = "EindhovenMuni", GDAL1_integer64_policy = TRUE)
# pc6sp <- spTransform(pc6sp,  CRS("+proj=longlat +datum=WGS84 +no_defs"))

##########################################################################
############# Loop for getting household PC6 coordinate ##################
### The household file above didn’t have coordinates just 4 ppc area codes.
### Here is a lookup that provides those and randomly disribute households.
##########################################################################


# Sample ppcs only where households are located
ppcsSampleHH <- subset(ppcs, ppcs@data$PC4 %in% hh$Home)

# Add number of household within 4 ppcs
ppcsSampleHH$Household[match(names(table(hh$Home)),
                             ppcsSampleHH$PC4)] <- table(hh$Home)

# Add household density by 4 ppc
ppcsSampleHH$HHDensity <- round(ppcsSampleHH$Household/
                                  (ppcsSampleHH$Shape_Area / 10^6),
                                digits = 2)

coords <- c()
hhid <- c()
hhpc6 <- c()
for (i in ppcsSampleHH@data$PC4) {
  
  # Check if PC6 covers all PC4 polygons
  try(if (!all(is.element(ppcsSampleHH$PC4,unique(pc6sf$PC4)))) 
    stop("PC6 geometry does not contain all polygons in PC4!")
  )
  
  # total household within PC4
  n <- sum(ppcsSampleHH$Household[ppcsSampleHH$PC4 == i], na.rm = TRUE)
  # get household id within PC4
  hhid <- append(hhid,hh$HHID[hh$Home == i])
  # get pc6 where household belongs and residential area
  pc6 <- pc6sf[which(pc6sf$PC4 == i &
                       pc6sf$BG2012_maj == 20),] # 20 for residential area
  
  # get random coordinates within filtered pc6
  coordsTmp <- st_coordinates(st_sample(pc6, size = n, type = "random",
                                        prob = pc6$Aantal_adr, replace = TRUE))
  while (nrow(coordsTmp) < n) {
    coordsTmp <- rbind(coordsTmp,
                       st_coordinates(st_sample(pc6, size = n,
                                                type = "random",
                                                prob = pc6$Aantal_adr,
                                                replace = TRUE)))
  }
  if (n > 1) {
    coordsTmp <- coordsTmp[1:n,]
    rownames(coordsTmp) <- seq(1:nrow(coordsTmp))
    
  }
  coordsTmp <- SpatialPoints(coordsTmp, 
                             proj4string = CRS("+proj=longlat
                                               +datum=WGS84 +no_defs"))
  
  pc6 <- as(pc6, 'Spatial')
  hhpc6 <- append(hhpc6,over(coordsTmp,pc6)$PC6)
  
  coords <- rbind(coords,coordsTmp@coords)
}
if (nrow(coords) > nrow(hh)){
  coords <- coords[1:nrow(hh),]
}
if (length(hhpc6) > nrow(hh)) {
  hhpc6 <- hhpc6[1:nrow(hh)]
}

hh$Lng <- coords[,1]
hh$Lat <- coords[,2]
hh$PC6 <- hhpc6

write.csv(hh,"hh-coords.txt")

##########################################################################

# Sample ppcs only where activities are occured.
ppcsSampleAct <- subset(ppcs, ppcs@data$PC4 %in% sched$DestLoc)

# Add number of activities within 4 ppcs
ppcsSampleAct$NumActs <- table(sched$DestLoc)[match(ppcsSampleAct$PC4,names(table(sched$DestLoc)))]

# Add household density by 4 ppc
ppcsSampleAct$ActDensity <- round(ppcsSampleAct$NumActs/
                                    (ppcsSampleAct$Shape_Area / 10^6),
                                  digits = 2)

##########################################################################
############# Loop for getting activity PC6 coordinate ###################
### The schedule file above didn’t have coordinates just 4 ppc area codes.
### Here is a lookup that provides those and randomly disribute act locs.
##########################################################################

## Land use info
landuse <- function(actType) {
  switch(as.character(actType),
         Home = c(20),
         Work = c(21,22,23,24),
         Business = c(22,23,24),
         BringGet = c(20,21,22,23,24),
         Groceries = c(21),
         NonGroc = c(21),
         Services = c(22,24),
         Leisure = c(23,40,41,42,43,44,75),
         Social = c(20,21,23,24,40,41,42,43,44,75),
         Touring = c(10,11,20,21,22,23,24,40,41,42,43,44,45,75),
         Other = c(10,11,20,21,22,23,24,40,41,42,43,44,45,75))
}

coords <- c()
schedid <- c()
schedpc6 <- c()
for (i in sched$SchedID) {
  print(i)
  destLoc <- sched[which(sched$SchedID == i),]$DestLoc
  actType <- sched[which(sched$SchedID == i),]$ActivityType
  
  pc6 <- pc6sp[which(pc6sp$PC4 == destLoc & pc6sp$BG2012 %in% landuse(actType)),]
  
  # if there is no appropriate land use in PC4
  if (nrow(pc6) == 0) {
    pc6 <- pc6sp[which(pc6sp$PC4 == destLoc),]
    # Assign 4 ppc coordinates if activity location is outside Eindhoven
    while (nrow(pc6) == 0) {
      pc6 <- ppcs[which(ppcs$PC4 == destLoc),]
      destLoc = destLoc + 1
    }
  }
  
  coordsTmp <- spsample(pc6, n = 1, type = "random", prob = pc6$Aantal_adr, iter = 10)
  coords <- rbind(coords,coordsTmp@coords)
  schedpc6 <- append(schedpc6,over(coordsTmp,pc6)$PC6)
  
  # # get pc6 where activity occurs in corresponding land use PC6
  # pc6 <- pc6sf[which(pc6sf$PC4 == destLoc & pc6sf$BG2012 %in% landuse(actType)),]
  # 
  # # if there is no appropriate land use in PC4
  # if (nrow(pc6) == 0) {
  #   pc6 <- pc6sf[which(pc6sf$PC4 == destLoc),]
  #   # get random coordinates within filtered pc6
  #   coordsTmp <- st_coordinates(st_sample(pc6, size = 1, type = "random",
  #                                         prob = pc6$Aantal_adr, replace = TRUE))
  #   while (nrow(coordsTmp) < 1) {
  #     coordsTmp <- rbind(coordsTmp,
  #                        st_coordinates(st_sample(pc6, size = 1,
  #                                                 type = "random",
  #                                                 prob = pc6$Aantal_adr,
  #                                                 replace = TRUE)))
  #   }
  # } else {
  #   # get random coordinates within filtered pc6
  #   coordsTmp <- st_coordinates(st_sample(pc6, size = 1, type = "random",
  #                                         prob = pc6$Aantal_adr, replace = TRUE))
  #   while (nrow(coordsTmp) < 1) {
  #     coordsTmp <- rbind(coordsTmp,
  #                        st_coordinates(st_sample(pc6, size = 1,
  #                                                 type = "random",
  #                                                 prob = pc6$Aantal_adr,
  #                                                 replace = TRUE)))
  #   }
  # }
  
  # coordsTmp <- coordsTmp[1,]
  # coordsTmp <- matrix(coordsTmp,nrow = 1 , ncol =2)
  # 
  # 
  # coordsTmp <- SpatialPoints(coordsTmp, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  # pc6 <- as(pc6, 'Spatial')
  # pc6 <- SpatialPoints(pc6,proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  # schedpc6 <- append(schedpc6,over(coordsTmp,pc6)$PC6)
  # coords <- rbind(coords,coordsTmp@coords)
    
}
# Get Destination PC6 Coordinates
sched$DestLng <- coords[,1]
sched$DestLat <- coords[,2]
sched$DestPC6 <- schedpc6

# Get Origin PC6 Coordinates
for (i in 1:nrow(sched)) {
  if (sched$EpisodeID[i] == 0){
    sched$OrigLng[i] <- NA
    sched$OrigLat[i] <- NA
    sched$OrigPC6[i] <- "Home"
  } else {
    sched$OrigLng[i] <- (sched$DestLng[i-1])
    sched$OrigLat[i] <- (sched$DestLat[i-1])
    sched$OrigPC6[i] <- as.character(sched$DestPC6[i-1])
  }
}

write.csv(sched,"sched-coords.txt")

################################################################################
###############           Routino implementation            ####################
################################################################################

# Read schedule file with coordinates
schedCoords <- read.table("D:/mygithub/AlbatrossViewer/sched-coords.txt",sep = ",",header = T)
# schedCoords <- schedCoords[1:200,]

fileloc <- "/Users/KimSeheon/routino/quickest-all.txt"  #This is the default working directory
setwd("/Users/KimSeheon/routino/")

routeresults <- c()
# colnames <- c('lat', 'lng', 'node', 'type', 'dist', 'dur', 'totdist', 'totdur', 'speed', 'bearing', 'address')
library(tictoc)
tic()
for (i in 1:nrow(schedCoords)) {
  print(i)
  
  lat2 <- schedCoords$DestLat[i]
  lon2 <- schedCoords$DestLng[i]
  
  # Skip the first episode
  if (schedCoords$EpisodeID[i] == 0){
    routeresults[[i]] <- NULL
    next
  }
  lat1 <- schedCoords$OrigLat[i]
  lon1 <- schedCoords$OrigLng[i]
  
  # Assign transport mode to route
  if (schedCoords$Mode[i] == "Car" | schedCoords$Mode[i] == "Car as Passenger") {
    tmode <- "motorcar"
  } else if (schedCoords$Mode[i] == "Walking or Biking") {
    candidate <- c("foot","bicycle")
    tmode <- sample(candidate, prob = c(0.5,0.5), size = 1)
  } else {
    tmode <- "psv" # (Public Service Vehicle - bus, coach)
  }
  
  # Command implementation
  router <- paste("router --transport=", tmode,
                  " --prefix=nl",
                  " --quickest",
                  " --lat1=", lat1,
                  " --lon1=", lon1,
                  " --lat2=",lat2,
                  " --lon2=",lon2,
                  # "--translations=/Users/KimSeheon/routino/routino-translations.xml",
                  # "--profiles=/Users/KimSeheon/routino/xml/routino-profiles.xml",
                  " --output-text-all",
                  # "--output-stdout",
                  " --quiet --dir=/Users/KimSeheon/routino/", sep = "")

  system(router, wait = TRUE)  # Send the routing command

  # Read in the txt instructions to extract the network distance
  routeresults[[i]] <- read.delim(fileloc, header = F, sep = "\t", skip = 6)
  colnames(routeresults[[i]]) <- c('lat', 'lng', 'node', 'type', 'seg.dist', 'seg.dur', 'dist', 'dur', 'speed', 'bearing', 'highway')
  
  ## Pipe implementation
  # routerPipe <- paste("router --transport=", tmode,
  #                 " --prefix=nl",
  #                 " --quickest",
  #                 " --lat1=", lat1,
  #                 " --lon1=", lon1,
  #                 " --lat2=",lat2,
  #                 " --lon2=",lon2,
  #                 # "--translations=/Users/KimSeheon/routino/routino-translations.xml",
  #                 # "--profiles=/Users/KimSeheon/routino/xml/routino-profiles.xml",
  #                 " --output-text-all",
  #                 # "--output-stdout",
  #                 " --quiet --dir=/Users/KimSeheon/routino/", sep = "")
  # 
  # routeresults[[i]] <- read.delim(pipe(routerPipe), skip=6, sep="\t", col.names=colnames)
}
toc()

lines <- c()
index <- c()
for (i in 1:nrow(schedCoords)) {
  if (is.null(routeresults[[i]])) {
    next
  }
  index <- append(index, i)
  lines[i] <- (list(sp::Lines(sp::Line(routeresults[[i]][2:1]),ID = schedCoords$SchedID[[i]])))
}
filtered.lines <- Filter(Negate(is.null), lines)
filtered.lines <- SpatialLines(filtered.lines,proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
filtered.sched <- schedCoords[index,]

routes <- SpatialLinesDataFrame(sl = filtered.lines, data = filtered.sched, match.ID = FALSE)


################################################################################
###############                 Leaflet map                 ####################
################################################################################

m <- leaflet()
m <- addTiles(map=m,group = "OSM (default)")
m <- addProviderTiles(map = m, group = "CartoDB Dark",
                 provider = providers$CartoDB.DarkMatterNoLabels)
labelsPpcs <- sprintf(
  "<strong>PPC: %s</strong><br/>",
  ppcs$PC4
  
) %>% lapply(htmltools::HTML)

# Overlay groups
m <- addPolygons(map = m, data = ppcs,
            group = "4-digit postcode area",
            color = "#444444",
            weight = 1,
            smoothFactor = 0.5,
            opacity = 0.5,
            fill = TRUE,
            fillColor = "#A9F5BC",
            fillOpacity = 0.5,
            label = labelsPpcs,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"),
            highlightOptions = highlightOptions(color = "black",
                                                weight = 4,
                                                bringToFront = TRUE))

## Household location
m <- addCircles(map = m, data = hh,
                group = "Household location",
                lng = ~Lng,
                lat = ~Lat,
                color = "blue",
                radius = 10,
                layerId = ~HHID,
                # radius = 3,
                # icon = icons,
                label = ~(paste("HHID: ",as.character(HHID))),
                highlightOptions = highlightOptions(color = "white",
                                                    weight = 3,
                                                    bringToFront = TRUE))

## Activity location
m <- addCircles(map = m, data = sched,
                group = "Activity location",
                lng = ~DestLng,
                lat = ~DestLat,
                color = "blue",
                radius = 5
                # layerId = ~SchedID,
                # radius = 3,
                # icon = icons,
                # label = ~(paste("SchedID: ",as.character(SchedID))),
                # highlightOptions = highlightOptions(color = "black",
                #                                     weight = 3,
                #                                     bringToFront = TRUE)
                )
labelsRoutes <- sprintf(
  "SchedID: %s <br/>
  O.PC6: %s <br/>
  D.PC6: %s <br/>",
  routes$SchedID,
  routes$OrigPC6,
  routes$DestPC6
) %>% lapply(htmltools::HTML)

## Routes
m <- addPolylines(map = m, data=routes,
                  group = "Routes-Individual",
                  weight = 1,
                  color = "red",
                  layerId = ~(paste0("r-",as.character(SchedID))),
                  opacity = 1,
                  label = labelsRoutes,
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 3,
                                                      bringToFront = TRUE))
  
m <- addLayersControl(map = m,
                      baseGroups = c("OSM (default)","CartoDB Dark"),
                      overlayGroups = c("4-digit postcode area","Activity location","Routes-Individual"),
                      options = layersControlOptions(collapsed = TRUE))

m
