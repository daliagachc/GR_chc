#' Imports CS kappa data
#'
#' imports kappa corrected condensation sink data, created by Joonas Enroth
#'
#' Issues: Timezone not sure. Have to verify
#'
#' @return a data table with the data
#' @export
#'
#' @examples import_CS_kappa_data
import_CS_kappa_dat=function(where, filename){
  CS_kappa_data=fread(paste(where,filename,sep="/"))%>%
    select(V1,V2) %>%
    rename("timestamp"=V1) %>%
    mutate(dateTime= as.POSIXct(timestamp*24*3600-24*3600,
                                 origin='0000-01-01 00:00.00 UTC', tz="UTC")) %>%
    mutate(localTime=dateTime)%>%
    I
   #CS_kappa_data$localTime=force_tz(CS_kappa_data$localTime,tzone="etc/GMT+4")

  return(CS_kappa_data)
}


