#' orthorectification of imagery
#'
#' This function is the big guy. wraps byframe_corr and spacing_fun, orthorectifies an extensionless bil file that is output by a headwall nano sensor and outputs a dataframe, a sf file, or writes an sf file to folder
#' @param framex frame of interest
#' @param sdf spectral data frame output from ortho fun
#' @param ProcessedIMU output from ortho fun
#' @keywords 
#' @export
#' @examples

ortho_fun <- function(filenumber,ProcessedIMU,bandtowave,output = c("dataframe","spatial","none"),printtofile=T){
  
  system.time(orig_sp <-caTools::read.ENVI(paste0(RemoteSenDataLoc,FolderLoc,"raw_",filenumber)))
  init.dim <- dim(orig_sp)
  dim(orig_sp) <- c(dim(orig_sp)[1]*dim(orig_sp)[2],dim(orig_sp)[3])
  sdf <- as.data.frame(orig_sp)
  #rep x 00000, 11111, 2222 -- columns (aka the width of the pushbroom)
  sdf$x <- rep((1:init.dim[2])-1, each = init.dim[1])
  # rep y 0 1 2 3 4 #individual scans
  sdf$y <- rep((1:init.dim[1])-1,init.dim[2])
  orig_sp <- NULL

  ### change colnames using the bandname to wavelength csv I made
  colnames(sdf)[1:272]<-paste0("nm",bandtowave$Wavelength)
  
  #### frame is filenumber+y
  
    sdf$frame <- filenumber+sdf$y
    
    #to fill in with the next function.
    sdf$Lat2 <- NA; sdf$Lon2 <- NA; sdf$Heading <- NA

    cl<-makeCluster(no_cores)
    clusterExport(cl,c("rbindlist","spacing_fun"))
    specdfOUT<- rbindlist(parLapply(cl,sort(unique(sdf$frame)),byframe_corr,sdf,ProcessedIMU))
    stopCluster(cl)
    
    specdfOUT$StartFrame <- filenumber
    specdfOUT_xy <- specdfOUT[,c("Lon2","Lat2")]
    specdfOUT_sp <- SpatialPointsDataFrame(coords=specdfOUT_xy,data=specdfOUT,proj4string = CRS("+init=epsg:32615")) 
    specdfOUT_sf <- st_as_sf(specdfOUT_sp)

     if(printtofile==T){st_write(specdfOUT_sf,dsn=paste0(ProcLoc,"Final",filenumber,"full.shp"),layer=paste0("Final",filenumber,"full"),driver="ESRI Shapefile",update = TRUE)}
     if(output=="dataframe"){return(specdfOUT)}
     if(output=="spatial"){return(specdfout_sf)}
     if(output=="none"){return(paste("You Selected to have nothing returned"))}
    print(Sys.time())
    print(filenumber)
}
