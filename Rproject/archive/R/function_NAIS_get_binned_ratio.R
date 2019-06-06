NAIS_get_binned_ratio=function(data1,data2,ions,binned){
  if (!binned){
    wd1=bin_NAIS_hourly(data1)
    wd2=bin_NAIS_hourly(data2)
  } else {
    wd1=data1
    wd2=data2
  }
  
  
  wd1$ID=1
  wd2$ID=2
  
  # if (!(ions %in% c("positive","negative"))){
  #   wd1$ion="both"
  #   wd2$ion="both"
  # }

  wd3=rbind(wd1,wd2) %>%
    dplyr::group_by(startTime,endTime,startSize,endSize,ion)%>%
    dplyr::summarise(diff=median(value[which(ID==1)],na.rm=T)/median(value[which(ID==2)],na.rm=T))
  
  if (ions %in% c("positive","negative")){
    wd3=subset(wd3,ion==ions)
  }
  wd3=wd3%>%
    mutate(value=NULL) %>%
    rename("value"=diff)
  
  return(wd3)
}

#plotNAIS(wd3)