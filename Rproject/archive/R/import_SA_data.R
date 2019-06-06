#preloads and format SA data
import_SA_data=function(){
  SA_data=fread(paste0(workDir,"/data","/GEORGE_SA.csv")) %>%
    mutate(localTime=as.POSIXct(TimeSA,format="%d/%m/%Y %H:%M:%S",tz="etc/GMT+4"))
  return(SA_data)
}

