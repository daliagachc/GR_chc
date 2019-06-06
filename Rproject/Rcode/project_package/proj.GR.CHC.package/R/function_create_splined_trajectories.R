#' create splined trajectories
#'
#' @param modesizeframe dataframe containing timeseries information of the growing mode
#' @param eventIDframe dataframe containing information about the eventIDs (used for selecting "good" events)
#' @param sparfac #the "stiffness" of the trajectories /smoothing factor. Between 0 and 1. 
#' 0 creates "wiggly trajectories", 1 very stiff one. Values around 0.95 seem to be good
#'
#'
#' @return a list: 1 element: modesizeframe with some additional columns, for example the 
#' smoothing spline evaluated in the initial timesteps. \Cr
#' 2 element: Another list containing as first element the eventID's of the growing event and as
#' second element the smoothing spline functions for the eventID's in the first element.
#' @export
#'
#' @examples NONE
create_splined_trajectories=function(modesizeframe,eventIDframe,sparfac){
  #this function creates the "trajectory functions" for all the 
  #selected growing events.
  #modesizeframe has to be a dataframe containing timeseries information
  #of the growing mode (right format!)
  #eventIDframe is a dataframe containing information about the eventID
  #(right format!)
  #the "sparfac" describes how "stiff" the spline trajectories are gonna be.
  #--------------
  
  
  help=modesizeframe%>%
    dplyr::group_by(eventID)%>%
    dplyr::summarise(eventStart=min(localTime),
                     begin.hour=hour(min(localTime)))
  
  wd1=subset(npfevent_size_frame)
  wd1=subset(wd1,continued==1)
  workData=merge(wd1,help,by="eventID") %>%
    mutate(timeSinceStart=(as.numeric(localTime)-as.numeric(eventStart))/3600,
           begin.hour.cut=cut(begin.hour,breaks=c(7,9,11,15),include.lowest=T))
  
  mask2=eventIDframe %>%
    dplyr::filter(score10 >=5 & airChange ==0) %>%
    select(eventID)
  workData=merge(workData,mask2,by="eventID") %>%
    select(eventID,localTime,dp,ion)
  
  mask3=npfevent_size_frame %>%
    dplyr::group_by(eventID) %>%
    dplyr::summarise(numb=sum(continued==1)) %>%
    dplyr::filter(numb >= 4) %>%
    select(eventID)
  
  workData=merge(workData,mask3,by="eventID")
  
  #creating the interpolation
  ID=c()
  timelist=list()
  veclist=list()
  derlist=list()
  splined_fun=list()
  filt.tray=list()
  splined.smooth_fun=list()
  for (i in 1:length(unique(workData$eventID))){
    ID[i]=as.character(unique(workData$eventID)[i])
    help=subset(workData,eventID==ID[i]) %>%
      arrange(localTime)
    unfiltered=help$dp
    timelist[[i]]=help$localTime
    splined_fun=splinefun(timelist[[i]],unfiltered)
    #the next one is the actual magic. Smoothed splines
    splined.smooth_fun[[i]]=smooth.spline(x=timelist[[i]],y=unfiltered,spar=sparfac,all.knots=T)
    veclist[[i]]=sgolayfilt(unfiltered,p=1,n=3,m=0,ts=0)
    derlist[[i]]=sgolayfilt(unfiltered,p=1,n=3,m=1,ts=0)
    
    filt.tray[[i]]=data.frame(eventID=rep(ID[i],length(veclist[[i]])),
                              localTime=timelist[[i]],
                              unfiltered=unfiltered,
                              ion=help$ion,
                              gfiltered=veclist[[i]],
                              gfiltered_der=derlist[[i]],
                              #splined=splined_fun(timelist[[i]]),
                              #splined.der=splined_fun(timelist[[i]],deriv=1)*3600,
                              splined.smoothed=predict(splined.smooth_fun[[i]],
                                                       as.numeric(timelist[[i]])),
                              splined.smoothed.der=predict(splined.smooth_fun[[i]],
                                                           as.numeric(timelist[[i]]),deriv=1)$y*3600,
                              spar=sparfac)
    
  }
  splined.smooth_functions=list(ID,splined.smooth_fun)
  
  
  #binding together
  filt.tray.df=do.call("rbind",filt.tray)
  #now, cutting into sizegroups and removing some rows
  filt.tray.df=filt.tray.df %>%
    mutate(sizeGroups_unf=cut(unfiltered,breaks=c(0,3,7,20,70),include.lowest=T),
           sizeGroups_filt=cut(splined.smoothed.y,breaks=c(0,3,7,20,70),include.lowest=T))
  filt.tray.df=subset(filt.tray.df, !is.na(sizeGroups_filt) & !is.na(sizeGroups_unf))
  
  #calculating the mads and zscore
  mads=filt.tray.df%>%
    dplyr::group_by(sizeGroups_filt) %>%
    dplyr::summarise(groupmad=median(abs(splined.smoothed.der-median(splined.smoothed.der,na.rm=T))),
                     groupmedian=median(splined.smoothed.der,na.rm=T))
  filt.tray.df=merge(filt.tray.df,mads,by="sizeGroups_filt") %>%
    mutate(zscore=0.6745*abs(splined.smoothed.der-groupmedian)/groupmad)
  
  
  outlist=list(filt.tray.df,splined.smooth_functions)
  
  return(outlist)
}
