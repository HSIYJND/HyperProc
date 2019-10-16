#' spacing function
#'
#' This function orthorectifies one band -- taking the measurements of the drone location, it places a strip of pixels in the correct location on the ground
#' @param pix_i pixel of interest
#' @param matchimu output from ortho fun
#' @param specdfframe output from ortho fun
#' @keywords 
#' @export
#' @examples

spacing_fun <- function(pix_i,matchimu,specdfframe){
  specdfpix <- specdfframe[specdfframe$x==pix_i,]

  
# if things are numbered the reverse of what they are (everything in these files is completely turned around because frame = 2000 is Y=0 so x goes 640->0), then needs to be switching +/- of the yaw stuff.
  specdfpix$Lat2<-matchimu$Lat+matchimu$TotMidPointCorrectionY-matchimu$YawYOffset+(pix_i*2*matchimu$YawYOffset/640)
  specdfpix$Lon2<-matchimu$Lon+matchimu$TotMidPointCorrectionX-matchimu$YawXOffset+(pix_i*2*matchimu$YawXOffset/640)
  specdfpix$Heading <- matchimu$Heading

  return(specdfpix)
}
