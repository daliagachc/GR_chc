#import CS kappa
import_CS_kappa_data=function(){
  CS_kappa_data=fread(paste0(workDir,"/data","/CS_dist_chacaltaya_kappa.sum"))%>%
    select(V1,V2) %>%
    rename("localTime"=V1)%>%
    mutate(localTime= as.POSIXct(localTime*24*3600-24*3600,
                                 origin='0000-01-01 00:00.00 UTC', tz="UTC"))
  CS_kappa_data$localTime=force_tz(CS_kappa_data$localTime,tzone="etc/GMT+4")
  
  return(CS_kappa_data)
}


