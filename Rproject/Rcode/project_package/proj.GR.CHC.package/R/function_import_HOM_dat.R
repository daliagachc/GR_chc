#' Import HOM data
#'
#' imports HOM data created with apitof
#'
#'
#'
#' @param where path to the file
#' @param filename filename with extension
#'
#' @return data in data table
#' @export
#'
#' @examples import_HOM_dat(where,"homdata.csv")
import_HOM_dat=function(where, filename){
  import=fread(paste(where,filename,sep="/"))

  HOM_data=import %>%
    mutate(dateTimeUTC=as.POSIXct(import$TIME*24*3600-24*3600,
                                  origin='0000-01-01 00:00.00 UTC', tz="UTC"),
      localTime= as.POSIXct(import$TIME*24*3600-24*3600,
                                 origin='0000-01-01 00:00.00 UTC', tz="etc/GMT+4"))
  return(HOM_data)
}

