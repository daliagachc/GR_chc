importAcsmData=function(){
  
  #imports the acsm data
  
  import=fread(paste0(workDir,"/data/CHC_QACSM.csv"))
  
  
  acsm_data_wide=import %>%
    rename("DateUTC"=1) %>%
    mutate(DateUTC=as.POSIXct(DateUTC,format="%m/%d/%Y %H:%M:%S", tz="UTC")) %>%
    mutate(localTime=DateUTC)
  attributes(acsm_data_wide$localTime)$tzone="etc/GMT+4"
  
  acsm_data_long=gather(data=acsm_data_wide,key="magnitude",value="value",2:6)
  
  return(list(acsm_data_wide,acsm_data_long))
}

# ggplot(data=acsm_data,aes(x=DateUTC2, y=Sulfate))+
#   geom_line()+
#   scale_y_continuous(trans="log10")