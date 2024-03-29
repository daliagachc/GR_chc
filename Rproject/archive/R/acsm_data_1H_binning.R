binHourly=function(df){#bins hourly by median
  wd=df %>%
    mutate(minute=minute(localTime)) %>%
    mutate(min_group=((minute %/%60)*60)) %>%
    dplyr::group_by(YMDH= paste(year(localTime),month(localTime),
                                day(localTime),hour(localTime),min_group,
                                sep="-"),magnitude) %>%
    dplyr::summarise(value=median(value,na.rm=T),
                     numb=n())%>%
    as.data.frame() %>%
    mutate(localTime=as.POSIXct(YMDH,format="%Y-%m-%d-%H-%M",tz="etc/GMT+4"))%>%
    mutate(YMDH=NULL)
  
  return(wd)
}


