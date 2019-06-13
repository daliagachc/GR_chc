#batch plotting by calling another script for plotting.
#DOes not work properly yet.
help=seq(as.POSIXct("2017-12-18",tz="etc/GMT+4"), 
         as.POSIXct("2018-06-30",tz="etc/GMT+4"), by="days")


#help=help[help >= as.POSIXct("2018-03-18",tz="etc/GMT+4")]

for (i in 1:(length(help)-1)){
  time1=help[i]
  time2=help[i+1]
  timeselect=c(time1,time2)
  
  print(c(time1,time2))
  try( dev.off())
  try(source('C:/fastFiles/projectSpringCourse/Rproject/Rcode/exploration/NAISPLOT_WD_RAD_ACSM_CS_HOM_presentation_batch.R', echo=TRUE))
  
}
