#' corrections for each frame
#'
#' This function places the center of each frame in the correct location
#' @param framex frame of interest
#' @param sdf spectral data frame output from ortho fun
#' @param ProcessedIMU output from ortho fun
#' @keywords 
#' @export
#' @examples

byframe_corr <- function(framex,sdf,ProcessedIMU){
   specdfframe <- sdf[sdf$frame==framex,]
    matchimu <- ProcessedIMU[ProcessedIMU$Frame.==framex,]
    # specdfframebypix <- rbindlist(lapply(sort(unique(specdfframe$x)),spacing_fun,matchimu,specdfframe))
    #changed line 16 to +offset then - offset
    specdfframe$Lat2 <- base::seq(from=matchimu$Lat+matchimu$TotMidPointCorrectionY+matchimu$YawYOffset,to=matchimu$Lat+matchimu$TotMidPointCorrectionY-matchimu$YawYOffset,length.out=nrow(specdfframe))
    specdfframe$Lon2 <- base::seq(from=matchimu$Lon+matchimu$TotMidPointCorrectionX-matchimu$YawXOffset,to=matchimu$Lon+matchimu$TotMidPointCorrectionX+matchimu$YawXOffset,length.out=nrow(specdfframe))    
    specdfframe$Heading <- matchimu$Heading
    return(specdfframe)
}
