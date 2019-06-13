#' Bin hourly
#'
#' bins hourly by median. APparently. I have no idea where this is used currently,
#' might be an artifact
#'
#' @param df dataframe to be binned hourly
#' @param long if long == T, data in long format. If long==F, data in wide format
#'
#' @return binned dataframe, I guess
#' @export
#'
#' @examples none
binHourly=function(df,long){#bins hourly by median
  wd=df %>%
    mutate(minute=minute(localTime)) %>%
    mutate(min_group=((minute %/%60)*60))

  if (long){
    wd=wd%>%
    dplyr::group_by(YMDH= paste(year(localTime),month(localTime),
                                day(localTime),hour(localTime),min_group,
                                sep="-"),magnitude) %>%
    dplyr::summarise(value=median(value,na.rm=T),
                     numb=n())%>%
    as.data.frame() %>%
    mutate(localTime=as.POSIXct(YMDH,format="%Y-%m-%d-%H-%M",tz="etc/GMT+4"))%>%
    mutate(YMDH=NULL)
  } else {
    wd=wd%>%
      dplyr::group_by(YMDH= paste(year(localTime),month(localTime),
                                  day(localTime),hour(localTime),min_group,
                                  sep="-")) %>%
      dplyr::summarise(value=median(value,na.rm=T),
                       numb=n())%>%
      as.data.frame() %>%
      mutate(localTime=as.POSIXct(YMDH,format="%Y-%m-%d-%H-%M",tz="etc/GMT+4"))%>%
      mutate(YMDH=NULL)
  }

  return(wd)
}

