calculate_GR_classic=function(dataframe){
  #calculates the growth rates the "classical" way, 
  #by fitting a line
  #Returns a list with the first element containing
  #growthrates in "wide" format, the second element growthrates
  #in "long" format
  
  get_GR_select=function(dp,time,min,max,n){
    indices=which(dp > min & dp <= max)
    if (length(indices)>=n){
      GR=summary(lm(dp[indices]~time[indices]))$coefficients[2]
      R.sq=summary(lm(dp[indices]~time[indices]))$r.squared
      first=as.numeric(first(time[indices]))
      last=as.numeric(last(time[indices]))
    }
    else{
      GR = NA 
      R.sq=NA
      first=NA
      last=NA}
    return(c(GR,R.sq,first,last))
  }
  
  
  workData= dataframe  %>%
    mutate(timeHour=as.numeric(localTime)/3600)
  

  event_GR_wide=workData %>%
    dplyr::group_by(eventID) %>%
    dplyr::summarise(GR_total=summary(lm(dp~timeHour))$coefficients[2],
                     GR_00_03=get_GR_select(dp,timeHour,0,3,5)[1],
                     GR_03_07 = get_GR_select(dp,timeHour,3,7,5)[1],
                     GR_07_20=get_GR_select(dp,timeHour,7,20,5)[1],
                     GR_20_80=get_GR_select(dp,timeHour,20,80,5)[1],
                     GR_10_25=get_GR_select(dp,timeHour,10,25,5)[1],
                     R_total=summary(lm(dp~timeHour))$r.squared,
                     R_00_03=get_GR_select(dp,timeHour,0,3,5)[2],
                     R_03_07 = get_GR_select(dp,timeHour,3,7,5)[2],
                     R_07_20=get_GR_select(dp,timeHour,7,20,5)[2],
                     R_20_80=get_GR_select(dp,timeHour,20,80,5)[2],
                     R_10_25=get_GR_select(dp,timeHour,10,25,5)[2],
                     start_00_03=get_GR_select(dp,timeHour,0,3,5)[3],
                     start_03_07 = get_GR_select(dp,timeHour,3,7,5)[3],
                     start_07_20=get_GR_select(dp,timeHour,7,20,5)[3],
                     start_20_80=get_GR_select(dp,timeHour,20,80,5)[3],
                     
                     maxsize=max(dp),
                     first=first(localTime),
                     last=last(localTime),
                     duration=(as.numeric(last(localTime))-as.numeric(first(localTime)))/3600)
  
  workData3=gather(event_GR_wide[,c("eventID","GR_total","GR_00_03","GR_03_07","GR_07_20","GR_20_80","GR_10_25")], key="size", value="GR",
                   c("GR_total","GR_00_03","GR_03_07","GR_07_20","GR_20_80","GR_10_25"))
  workData3$size=gsub("GR_","",workData3$size)
  workData4=gather(event_GR_wide[c("eventID","R_total","R_00_03","R_03_07","R_07_20","R_20_80","R_10_25")], key="size", value="R",
                   c("R_total","R_00_03","R_03_07","R_07_20","R_20_80","R_10_25"))
  workData4$size=gsub("R_","",workData4$size)
  
  event_GR_long=merge(workData3,workData4,by=c("eventID","size"))
  
  event_GR_wide=merge(event_GR_wide,eventIDclas,by="eventID")
  event_GR_long=merge(event_GR_long,eventIDclas,by="eventID")
  
  
  outlist=list(event_GR_wide,event_GR_long)
  return(outlist)
}#calculate_GR_classic
