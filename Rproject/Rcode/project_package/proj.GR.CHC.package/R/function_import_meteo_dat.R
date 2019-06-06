#' Import meteo data
#'
#' Imports meteorological data for the saltena campaign
#'
#' @param where path to the folder conaining the file
#' @param filename filename with extension
#'
#' @return returns data in data table
#' @export
#'
#' @examples import_meteo_dat()
import_meteo_dat=function(where, filename){
  meteo_file=fread(paste(where,filename,sep="/")) %>%
    mutate(WD=as.numeric(WD),
           WS=as.numeric(WS),
           SWd=as.numeric(SWd),
           SWu=as.numeric(SWu),
           Tair = as.numeric(Tair),
           UTCTime=as.POSIXct(date_time, tz="UTC"),
           localTime=UTCTime)
  attributes(meteo_file$localTime)$tzone="etc/GMT+4"
  return(meteo_file)
}
