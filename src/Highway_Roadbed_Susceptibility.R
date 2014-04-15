######################################################################
######################################################################
####                                                              ####
#### 8 8888        8    d888888o.   8 8888        8 8 888888888o. ####  
#### 8 8888        8  .`8888:' `88. 8 8888        8 8 8888    `88.####  
#### 8 8888        8  8.`8888.   Y8 8 8888        8 8 8888     `88####  
#### 8 8888        8  `8.`8888.     8 8888        8 8 8888     ,88####  
#### 8 8888        8   `8.`8888.    8 8888        8 8 8888.   ,88'####  
#### 8 8888        8    `8.`8888.   8 8888        8 8 888888888P' ####  
#### 8 8888888888888     `8.`8888.  8 8888888888888 8 8888`8b     ####  
#### 8 8888        8 8b   `8.`8888. 8 8888        8 8 8888 `8b.   ####  
#### 8 8888        8 `8b.  ;8.`8888 8 8888        8 8 8888   `8b. ####  
#### 8 8888        8  `Y8888P ,88P' 8 8888        8 8 8888     `88####
####                                                              #### 
####                                                              ####
####             Evaluation on Hazard Susceptibility              ####
####              of Qinghai-Tibet Highway Roadbed                ####
####                    in Permafrost Regions                     ####
####                     V0.1 - 20/03/2014                        ####
####                        CAREERI, CAS                          ####
####                          Lihui Luo                           ####
####                                                              ####
####     Cold and Arid Regions Environmental and Engineering      ####
####       Research Institute, Chinese Academy of Sciences        ####
####            320 Donggang West Road, Lanzhou, China            ####
####                                                              ####
####                       +86 0931 4967592                       ####
####                       luolh@lzb.ac.cn                        ####
####                                                              ####
####             This script was prepared using R 3.1.0           ####
####                                                              ####
####     INPUTS: 1) srtm_studyarea_dem.tif: strm dem map          ####
####             2) QZ_highway.kml: KML for Qinghai-Tibet Highway ####
####             3) QZ_highway_raster.tif for raster format       ####
####             4) study_area_LUCC.tif: Globcover2009_V2.3       ####
####                                                              ####
####                                                              ####
######################################################################
######################################################################

rm(list = ls())     # clear objects  
graphics.off()      # close graphics windows

# Function: Compute average annual ground surface temperature 
# ZHANG Zhong-qiong，WU Qing-bai. Predicting Changes of Active Layer 
# Thickness on the Qinghai-Tibet Plateau as Climate Warming. Journal of 
# Glaciology and Geocryology, 2012, 34(3):505-511.
comp_ground_temp_LLE <- function(lon, lat, elev){
  ground_temp_LLE <- 62.05 - 0.11*lon - 0.74*lat - 0.006*elev
  return (ground_temp_LLE)
}

# Function: Compute average annual ground surface temperature with slope aspect
# LU Jiahao, NIU Fujun, Cheng Hua,et al. The Permafrost Distribution Model and
# Its Change Trend of Qinghai － Tibet Engineering Corridor. Journal of Mountain 
# Science, 2013, 31(2):226-233.
comp_ground_temp_LEA <- function(lat, elev, asp){
  ground_temp_LEA <- 65.461 - 1.222*lat - 0.005*elev - 0.299*cos(asp)
  return (ground_temp_LEA)
}

# The script requires the following R packages
require(sp)
require(rgdal)
require(raster)
require(rgl)
require(rasterVis)
require(maptools)
require(plotrix)

# output all figure in postscript format
postscript("HRS.ps")

#====================================================================#
# Compute the gorund surface temperature                             #
#====================================================================#

# "SpatialGridDataFrame"
study_area_dem <- readGDAL("srtm_studyarea_dem.tif")
lat_lon <- coordinates(study_area_dem)
elevation <- study_area_dem@data[[1]]

# compute the gorund surface temperature with lon, lat and elevlation
ground_temp_LLE <- comp_ground_temp_LLE(lat_lon[,1], lat_lon[,2], elevation)
ground_temp_data = data.frame(x=lat_lon[,1], y=lat_lon[,2], col=ground_temp_LLE)
ground_temp_LLE_grid = SpatialPixelsDataFrame(points=ground_temp_data[,1:2], 
                                            data=data.frame(ground_temp_LLE))
proj4string(ground_temp_LLE_grid) <- CRS("+proj=longlat +datum=WGS84 +no_defs 
                                          +ellps=WGS84 +towgs84=0,0,0")

# Output the tif map with the gorund surface temperature
writeGDAL(ground_temp_LLE_grid, fname = "annualGroundTemp_LLE.tif", 
          drivername = "GTiff", type = "Float32")

# summary the gorund surface temperature
study_area_groundtemp_LLE <- readGDAL("annualGroundTemp_LLE.tif")
summary(study_area_groundtemp_LLE)

# compute the slope aspect
dem <- raster("srtm_studyarea_dem.tif")
aspectdem = terrain(dem, opt='aspect', unit='degrees')
slopedem = terrain(dem, opt='slope', unit='degrees')

# Output the tif map with the aspect
aspectdem_grid <- as(aspectdem, "SpatialPixelsDataFrame")
proj4string(aspectdem_grid) <- CRS("+proj=longlat +datum=WGS84 +no_defs 
                                          +ellps=WGS84 +towgs84=0,0,0")
writeGDAL(aspectdem_grid, fname = "study_area_aspect.tif", 
          drivername = "GTiff", type = "Float32")

aspectdem_data <- readGDAL("study_area_aspect.tif")

# compute the gorund surface temperature with lat, elevlation and aspect
ground_temp_LEA <- comp_ground_temp_LEA(lat_lon[,2], elevation, aspectdem)
ground_temp_LEA_grid <- as(ground_temp_LEA, "SpatialPixelsDataFrame")
proj4string(ground_temp_LEA_grid) <- CRS("+proj=longlat +datum=WGS84 +no_defs 
                                          +ellps=WGS84 +towgs84=0,0,0")

# Output the tif map with the gorund surface temperature
writeGDAL(ground_temp_LEA_grid, fname = "annualGroundTemp_LEA.tif", 
          drivername = "GTiff", type = "Float32")

# summary the gorund surface temperature
study_area_groundtemp_LEA <- readGDAL("annualGroundTemp_LEA.tif")
summary(study_area_groundtemp_LEA)

# figure attribution
trellis.par.set(sp.theme())
north <- list("SpatialPolygonsRescale", layout.north.arrow(),
              offset = c(92.85000, 35.25000), scale = 0.04)
scale <- list("SpatialPolygonsRescale", layout.scale.bar(),
              offset = c(93.15000, 34.68000), scale = 0.1, 
              fill = c("transparent", "black"))
txt1 <- list("sp.text", c(93.15000, 34.69500), "0")
txt2 <- list("sp.text", c(93.25000, 34.69500), "1 km")
dem.layout <- list(north, scale, txt1, txt2)

# plot dem
print(spplot(study_area_dem, scales=list(draw=T), sp.layout = dem.layout, 
             main="Elevation of Qinghai-Tibet Highway (m)"))

# plot 3D dem
plot3D(dem)

# plot aspect
print(spplot(aspectdem_data, scales=list(draw=T), sp.layout = dem.layout, 
             main=expression("Aspect of Qinghai-Tibet Highway ( " * degree * " )")))

# plot ground surface temperature
print(spplot(study_area_groundtemp_LLE, scales=list(draw=T), sp.layout = dem.layout, 
             ylab=expression('Ground temperature (' * degree * 'C)'), main="LLE"))
print(spplot(study_area_groundtemp_LEA, scales=list(draw=T), sp.layout = dem.layout, 
             ylab=expression('Ground temperature (' * degree * 'C)'), main="LEA"))

# Boxplots
opar<-par(cex=1,mfrow = c(1, 2))
boxplot(ground_temp_LLE, ylim=c(-6,1), 
        ylab=expression('Ground temperature (' * degree * 'C)'), main="LLE")
boxplot(ground_temp_LEA, ylim=c(-6,1), 
        ylab=expression('Ground temperature (' * degree * 'C)'), main="LEA")
par(opar)

# compute highway Roadbed Trend (azimuth angle)
highway <- raster("QZ_highway_raster.tif")
highway_pix <- as(highway, "SpatialPixelsDataFrame")
highway_azimuth <- trackAzimuth(highway_pix@coords, type="snyder_sphere")

# plot kml of highway


# plot highway trend
V <- length(highway_azimuth)
for (l in 1:V){
  if (highway_azimuth[l] < 0){
    highway_azimuth[l] <- -highway_azimuth[l] + 180
  }
}

highway_azimuth[highway_azimuth[1:length(highway_azimuth)] < 0] <- -highway_azimuth + 180 
opar<-par(cex=1,mfrow = c(1, 2))
polar.plot(highway_azimuth/36, names(highway_azimuth), start=90,clockwise=TRUE,
           main=expression('The Azimuth of Qinghai-Tibet Highway (' * degree * ')'),
           lwd=3,line.col=4) 
boxplot(highway_azimuth)
par(opar)

# write KML format for WebGIS
writeOGR(ground_temp_LLE_grid, "annualGroundTemp_LLE.kml", 
         layer = "LLE", driver = "KML", overwrite_layer = TRUE)
writeOGR(ground_temp_LEA_grid, "annualGroundTemp_LEA.kml", 
         layer = "LEA", driver = "KML", overwrite_layer = TRUE)
writeOGR(aspectdem_grid, "study_area_aspect.kml", 
         layer = "aspect", driver = "KML", overwrite_layer = TRUE)

#====================================================================#
# Define the land cover type (Plant Function Type)                   #
# Please refer to Globcover2009_Legend.csv                           #
#====================================================================#

study_area_LUCC <- readGDAL("study_area_LUCC.tif")
lat_lon_lucc <- coordinates(study_area_LUCC)
landcover <- study_area_LUCC@data[[1]]

#====================================================================#
# Comopute the critical embamkment height design value               #
#====================================================================#


# ice content
# highway Roadbed Height
# Highway Road runtime
# highway Roadbed Construction (engineering measure)



dev.off()
