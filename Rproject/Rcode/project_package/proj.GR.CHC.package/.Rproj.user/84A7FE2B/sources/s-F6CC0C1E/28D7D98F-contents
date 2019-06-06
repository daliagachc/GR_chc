#' Import SA data
#'
#' Imports sulfuric acid data derived by apitof for saltena campaign.
#' Data apparently in local
#'
#' @param where path to file containing folder
#' @param filename filename with extension
#'
#' @return data in a data table
#' @export
#'
#' @examples import_SA_dat(filepath,"GEORGE_SA.csv" )
#'
import_SA_dat=function(where,filename){
  SA_data=fread(paste(where,filename,sep="/")) %>%
    mutate(Time=as.POSIXct(TimeSA,format="%d/%m/%Y %H:%M:%S",tz="etc/GMT+4")) %>%
    mutate(dateTimeLocal=Time,
           dateTimeUTC=Time)
  attributes(SA_data$dateTimeUTC)$tzone="UTC"
  return(SA_data)
}
