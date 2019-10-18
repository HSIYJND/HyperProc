#' loops through plots of shapefile to extract spatial hyperspectral data files for each plot
#'
#' for use in shp_crop

shpfile_plotloop <-function(plotnum,specdfOUT_sf,shpfile,filenumber,outputlocation){
  plottmp <- subset(shpfile,Plot==plotnum)
  plot_buff <- gBuffer(plottmp,width = -2)
  plot_sf <- st_as_sf(plot_buff)
  st_crs(plot_sf)<-st_crs(specdfOUT_sf)
  suppressWarnings(plot_clip <- st_intersection(specdfOUT_sf,plot_sf))

  if(dim(plot_clip)[1]>0){
    st_write(plot_clip,dsn=paste0(outputlocation,"processed",filenumber,"Plot",plotnum,".shp"),layer=paste0("processed",filenumber,"Plot",plotnum),driver="ESRI Shapefile",update = TRUE)}
}
