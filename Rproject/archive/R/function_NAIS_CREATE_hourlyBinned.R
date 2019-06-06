bin_NAIS_hourly=function(data){#creates an houry binned summarised frame. median.
  wd=data
  
  wd=wd %>%
    mutate(year=year(startTime),
           month=month(startTime),
           day=day(startTime)) %>%
    mutate(starting_point=fastPOSIXct(paste0(year,"-",month,"-",day), tz="UTC")) %>%
    mutate(starting_point=force_tz(starting_point,"etc/GMT+4"))
  wd$startTimeLocal=wd$startTime
  wd$startTimeLocal=force_tz(wd$startTimeLocal,tzone="etc/GMT+4")
  
  wd=wd %>%
    mutate(timeSinceMidnight=(as.numeric(startTimeLocal)-as.numeric(starting_point))/3600)%>%
    mutate(timeSinceMidnightLocal=timeSinceMidnight)
  
  wd = wd %>%
    mutate(timeCuts=cut(timeSinceMidnightLocal,breaks=seq(0,24,0.2),include.lowest=T))
  
  
  wd.s=wd %>%
    dplyr::group_by(timeCuts,startSize,endSize,ion) %>%
    dplyr::summarise(value=median(value,na.rm=T)) %>%
    mutate(startTime=convertToLimit(timeCuts,0),
           endTime=convertToLimit(timeCuts,1)) %>%
    as.data.frame()
  
  
  return(wd.s)
  
}#function