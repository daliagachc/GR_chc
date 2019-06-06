sparfac=0.8 #the factor for the interpolation. 0 -> no smoothing 1 -> much smoothing
#--------------
#mask=event_GR_long %>%select(eventID,R)
#mask=subset(mask,R>=0.75)
#mask=unique(mask)

worknpf=subset(npfevent_size_frame,continued==1)

help=worknpf%>%
  dplyr::group_by(eventID)%>%
  dplyr::summarise(eventStart=min(posixTime),
                   begin.hour=hour(min(posixTime)))

wd1=subset(npfevent_size_frame)
wd1=subset(wd1,continued==1)
workData=merge(wd1,help,by="eventID") %>%
  mutate(timeSinceStart=(as.numeric(posixTime)-as.numeric(eventStart))/3600,
         begin.hour.cut=cut(begin.hour,breaks=c(7,9,11,15),include.lowest=T))

mask2=eventIDclas %>%
  dplyr::filter(score10 >=5 & airChange ==0) %>%
  select(eventID)
workData=merge(workData,mask2,by="eventID") %>%
  select(eventID,posixTime,dp,ion)

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
    arrange(posixTime)
  unfiltered=help$dp
  timelist[[i]]=help$posixTime
  #splined_fun=splinefun(timelist[[i]],unfiltered)
  if (length(unique(unfiltered))>=4){
    splined.smooth_fun[[i]]=smooth.spline(x=timelist[[i]],y=unfiltered,spar=sparfac,all.knots=T)
    splined.smoothed[[i]]=predict(splined.smooth_fun[[i]],
                                  as.numeric(timelist[[i]]))
    splined.smoothed.der[[i]]=predict(splined.smooth_fun[[i]],
                                      as.numeric(timelist[[i]]),deriv=1)$y*3600
  }
  else{
    splined.smooth_fun[[i]]=NA,
    splined.smoothed[[i]]=NA,
    splined.smoothed.der[[i]])

  veclist[[i]]=sgolayfilt(unfiltered,p=1,n=3,m=0,ts=0)
  derlist[[i]]=sgolayfilt(unfiltered,p=1,n=3,m=1,ts=0)
  
  filt.tray[[i]]=data.frame(eventID=rep(ID[i],length(veclist[[i]])),
                            posixTime=timelist[[i]],
                            unfiltered=unfiltered,
                            ion=help$ion,
                            gfiltered=veclist[[i]],
                            gfiltered_der=derlist[[i]],
                            #splined=splined_fun(timelist[[i]]),
                            #splined.der=splined_fun(timelist[[i]],deriv=1)*3600,
                            splined.smoothed=,
                            splined.smoothed.der=,
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




