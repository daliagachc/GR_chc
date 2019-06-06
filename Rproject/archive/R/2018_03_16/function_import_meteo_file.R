import_meteo_file=function(){
  meteo_file=fread(paste0(getwd(),"/data/","meteo_cumbre.csv")) %>%
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
