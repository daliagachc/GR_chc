#batch plotting by calling another script for plotting.
#DOes not work properly yet.


help=meteo_file  %>% 
  dplyr::select(UTCTime) %>%
  mutate(YMD=format(UTCTime, "%Y-%m-%d")) %>%
  mutate(date=as.POSIXct(YMD,tz="etc/GMT+4"))

help=subset(help, date > as.POSIXct("2017-12-30",tz="etc/GMT+4"),
            date < as.POSIXct("2018-06-15",tz="etc/GMT+4")) %>%
  dplyr::select(date)

help=unique(help)

for (i in 1:(length(help$date)-1)){
  time1=help$date[i]
  time2=help$date[i+1]
  
  print(c(time1,time2))
  source('C:/fastFiles/projectSpringCourse/Rproject/Rcode/exploration/NAISPLOT_WD_RAD_ACSM_CS_HOM_presentation_batch.R', echo=TRUE)
}
