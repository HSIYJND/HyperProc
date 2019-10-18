#' orthorectification of imagery
#'
#' This function is the big guy. wraps byframe_corr and spacing_fun, orthorectifies an extensionless bil file that is output by a headwall nano sensor and outputs a dataframe, a sf file, or writes an sf file to folder
#' @param filenumber number of the file to process
#' @param ProcessedIMU IMU file created through imu_framecombine, imu_demcombine, and imu_proc.
#' @param bandtowave "standard" uses normal headwall nano values, else call in dataframe that translates band number (1-272) to wavelength in nm (398-1000)
#' @param output should function return a non-spatial dataframe, a spatial (sf) file, or nothing?
#' @param printtofile should the function write a spatial file to the processed files folder
#' @keywords orthorectification, UAV, hyperspectral, push broom sensor, ecological research
#' @export
#' @examples

ortho_fun <- function(filenumber,ProcessedIMU,bandtowave="standard",output = c("dataframe","spatial","none"),printtofile=T){
  
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
  if(bandtowave=="standard"){colnames(sdf)[1:272]<-paste0("nm",
    c("398.604", "400.825", "403.046", "405.267", "407.488", "409.709", 
"411.93", "414.151", "416.372", "418.593", "420.815", "423.036", 
"425.257", "427.478", "429.699", "431.92", "434.141", "436.362", 
"438.583", "440.804", "443.025", "445.246", "447.467", "449.688", 
"451.909", "454.13", "456.351", "458.572", "460.793", "463.014", 
"465.235", "467.457", "469.678", "471.899", "474.12", "476.341", 
"478.562", "480.783", "483.004", "485.225", "487.446", "489.667", 
"491.888", "494.109", "496.33", "498.551", "500.772", "502.993", 
"505.214", "507.435", "509.656", "511.877", "514.099", "516.32", 
"518.541", "520.762", "522.983", "525.204", "527.425", "529.646", 
"531.867", "534.088", "536.309", "538.53", "540.751", "542.972", 
"545.193", "547.414", "549.635", "551.856", "554.077", "556.298", 
"558.519", "560.741", "562.962", "565.183", "567.404", "569.625", 
"571.846", "574.067", "576.288", "578.509", "580.73", "582.951", 
"585.172", "587.393", "589.614", "591.835", "594.056", "596.277", 
"598.498", "600.719", "602.94", "605.162", "607.383", "609.604", 
"611.825", "614.046", "616.267", "618.488", "620.709", "622.93", 
"625.151", "627.372", "629.593", "631.814", "634.035", "636.256", 
"638.477", "640.698", "642.919", "645.14", "647.361", "649.582", 
"651.804", "654.025", "656.246", "658.467", "660.688", "662.909", 
"665.13", "667.351", "669.572", "671.793", "674.014", "676.235", 
"678.456", "680.677", "682.898", "685.119", "687.34", "689.561", 
"691.782", "694.003", "696.224", "698.446", "700.667", "702.888", 
"705.109", "707.33", "709.551", "711.772", "713.993", "716.214", 
"718.435", "720.656", "722.877", "725.098", "727.319", "729.54", 
"731.761", "733.982", "736.203", "738.424", "740.645", "742.866", 
"745.088", "747.309", "749.53", "751.751", "753.972", "756.193", 
"758.414", "760.635", "762.856", "765.077", "767.298", "769.519", 
"771.74", "773.961", "776.182", "778.403", "780.624", "782.845", 
"785.066", "787.287", "789.508", "791.73", "793.951", "796.172", 
"798.393", "800.614", "802.835", "805.056", "807.277", "809.498", 
"811.719", "813.94", "816.161", "818.382", "820.603", "822.824", 
"825.045", "827.266", "829.487", "831.708", "833.929", "836.15", 
"838.372", "840.593", "842.814", "845.035", "847.256", "849.477", 
"851.698", "853.919", "856.14", "858.361", "860.582", "862.803", 
"865.024", "867.245", "869.466", "871.687", "873.908", "876.129", 
"878.35", "880.571", "882.792", "885.014", "887.235", "889.456", 
"891.677", "893.898", "896.119", "898.34", "900.561", "902.782", 
"905.003", "907.224", "909.445", "911.666", "913.887", "916.108", 
"918.329", "920.55", "922.771", "924.992", "927.213", "929.434", 
"931.656", "933.877", "936.098", "938.319", "940.54", "942.761", 
"944.982", "947.203", "949.424", "951.645", "953.866", "956.087", 
"958.308", "960.529", "962.75", "964.971", "967.192", "969.413", 
"971.634", "973.855", "976.076", "978.298", "980.519", "982.74", 
"984.961", "987.182", "989.403", "991.624", "993.845", "996.066", 
"998.287", "1000.51"))}else colnames(sdf)[1:272]<-paste0("nm",bandtowave$Wavelength)


  #### frame is filenumber+y
  
    sdf$frame <- filenumber+sdf$y
    
    #to fill in with the next function.
    sdf$Lat2 <- NA; sdf$Lon2 <- NA; sdf$Heading <- NA
    no_cores <- detectCores()-1
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
