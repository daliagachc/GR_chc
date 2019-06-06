#this script will upload into a raster stack EVI data obtained by Giovanni (nc files)
#it can be adapted easily to do the same for other variables
#It will "produce" two stacks: One with full resolution and one with reduced resolution.
#Reduced resolution has advantage of more "robust" statistics and less NA
library(raster)

#setting the working directory to where the nc files are found
workdir=getwd()
setwd("C:/fastFiles/springCourse/SO2")

#creating a list pointing to all files with extension .nc
temp = list.files(pattern="*.nc")
#extracting the dates corresponding to the raster layers
nameVec=sub(".*PBL.", "", temp)
nameVec=gsub("\\..*", "", nameVec)


#now creating the raster stack
SO2_stack=lapply(temp,raster)
SO2_stack=stack(SO2_stack)
names(SO2_stack)=nameVec#Now the names indicate which month this corresponds to.

#changing back to previous WD
setwd(workdir)

test=mean(SO2_stack,na.rm=T)
test2=log10(test)

raster_export=test2

#setting the right coordenate reference system
crs(raster_export)=CRS("+init=epsg:4326")
projection(raster_export) = CRS("+init=epsg:4326")

#creating file title
title=paste0("So2_2017_06_2018_06")

#finally exporting a asc which can get directly imported in Qgis
#I save this in the "processed data" folder
help=getwd()
setwd("C:/fastFiles/springCourse/SO2/exp")
raster_export=ratify(raster_export)
writeRaster(raster_export,paste0(title,"_log.tif"),overwrite=T)

setwd(help)#back to main wd


