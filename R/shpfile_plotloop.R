#' loops through plots of shapefile to extract spatial hyperspectral data files for each plot, for use within shp_crop
#' @param plotnum number of the plot of interest
#' @param specdfOUT_sf Hyperspectral spatial file
#' @param shpfile shapefile for experiment with individual plots
#' @param filenumber filenumber of original hyperspectral (raw) block
#' @param outputlocation where to put the new file
#' @import rgeos
#' @export
#' @examples


shpfile_plotloop <-function(plotnum,specdfOUT_sf,shpfile,filenumber,outputlocation){
  plottmp <- subset(shpfile,PLOT==plotnum)
  plot_buff <- gBuffer(plottmp,width = -2)
  plot_sf <- st_as_sf(plot_buff)
  st_crs(plot_sf)<-st_crs(specdfOUT_sf)
  suppressWarnings(plot_clip <- st_intersection(specdfOUT_sf,plot_sf))

  if(dim(plot_clip)[1]>0){
    st_write(plot_clip,dsn=paste0(outputlocation,"processed",filenumber,"Plot",plotnum,".shp"),layer=paste0("processed",filenumber,"Plot",plotnum),driver="ESRI Shapefile",update = TRUE)}
}
