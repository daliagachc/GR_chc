
#------------------------------DATA HANDLING------------------------------
#getting the evaluated splines
splined_rep_df=evaluate_splined_trajectories(splined.smooth_functions,
                                             filt.tray.df,30) %>%
  mutate(YMD=format(Time,"%Y-%m-%d"))

mask=npfevent_size_frame %>%
  select(eventID,ion)
mask=unique(mask)

wd=merge(splined_rep_df,mask) %>%
  arrange(eventID)

cuts= c(1,3,7,20)
wd$cuts=cut(wd$dp_splined,breaks=cuts,include.lowest=T)
wd=na.omit(wd)

wd.s=wd %>%
  dplyr::group_by(cuts,eventID)%>%
  dplyr::summarise(
    meanDer=mean(dp_splined_der,na.rm=T),
    medianDer=median(dp_splined_der,na.rm=T))
  
mask=npfevent_size_frame %>%
  dplyr::select(UTCTime,eventID) %>%
  mutate(date=format(UTCTime, "%Y/%m/%d")) %>%
  dplyr::select(date,eventID)
mask=unique(mask)

wd.s2=dplyr::inner_join(wd.s,mask,by="eventID") %>%
  mutate(date=as.POSIXct(date,tz="UTC"),
         charge=getCharge_eventID(eventID))

#------------------------------PLOTTING------------------------------

plotdata=wd.s2 %>%
  dplyr::filter(charge=="negative") %>%
  I

ggplot(data=plotdata,aes(x=date,y=medianDer,fill=cuts))+
  geom_col(position=position_dodge(),col="black")+
  #scale_fill_manual(values=c(""))+
  labs(fill="sizerange\n[nm]",x="",y="median GR [nm/h]",
       caption="negatively charged ions")+
  theme(axis.text.x=element_text(angle=90))+
  scale_x_datetime(breaks=date_breaks("5 days"))