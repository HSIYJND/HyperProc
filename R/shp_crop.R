#' function to crop data to shapefiles from experiment with distinct plots
#'


shp_crop<-function(proc_img=specdfOUT,shpfile){
  #if proc_img includes an extension, read in the file, if proc_img is a df already in r, make spatial.
  if(grepl(".shp",proc_img)){
   specdfOUT_sf <- st_read(proc_img)
  }else if(is.data.frame(proc_img)){specdfOUT_xy <- proc_img[,c("Lon2","Lat2")]
    specdfOUT_sp <- SpatialPointsDataFrame(coords=specdfOUT_xy,data=specdfOUT,proj4string = CRS("+init=epsg:32615")) 
    specdfOUT_sf <- st_as_sf(specdfOUT_sp)
  }else specdfOUT_sf <- proc_img

plotshp <- spTransform(shpfile,proj4string(specdfOUT_sp))
colnames(plotshp)<-toupper(colnames(plotshp))
if("PLOT"%in%colnames(plotshp)==FALSE){plotshp$PLOT<-1}

# #comment this out for faster test runs
cl2<-makeCluster(no_cores)
clusterExport(cl2,c("st_crs","st_crs<-","subset","st_as_sf","st_intersection","st_write","gBuffer"),envir=environment())
parLapply(cl2,sort(unique(plotshp$Plot)),shpfile_plotloop,specdfOUT_sf,PlotShapeFile,filenumber,computer,ProcLoc)
stopCluster(cl2)

#commenting out for faster test runs
means_out <- over(plotshp,specdfOUT_sp,fn=mean)
means_out$Plot <- 1:nrow(means_out)
means_out$File <- filenumber

cv_out <- over(plotshp,specdfOUT_sp,fn=cv,na.rm=T)
colnames(cv_out)<- paste(colnames(cv_out),"cv",sep="_")
cv_out$Plot <- 1:nrow(cv_out)
cv_out$File <- filenumber

tot_out <-cbind(means_out,cv_out)

# tot_out <- data.frame(thisisatest=c(1,2,3,4)) #this is for when I comment out the actual calculations to check the loop
return(tot_out)
}

