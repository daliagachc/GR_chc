NAIS_get_binned_dif=function(data1,data2,ions){
  wd1=data1
  wd2=data2
  
  # wd1=bin_NAIS_hourly(data1)
  # wd2=bin_NAIS_hourly(data2)
  #you can uncomment this if the files are not binned
  
  wd1$ID=1
  wd2$ID=2
  
  wd3=rbind(wd1,wd2) %>%
    dplyr::group_by(startTime,endTime,startSize,endSize,ion)%>%
    dplyr::summarise(diff=median(value[which(ID==1)],na.rm=T)-median(value[which(ID==2)],na.rm=T))
  
  wd3=subset(wd3,ion==ions) %>%
    mutate(value=NULL) %>%
    rename("value"=diff)
  
  return(wd3)
}

#plotNAIS(wd3)