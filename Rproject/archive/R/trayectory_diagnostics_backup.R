#evaluate the trayectories

wd=filt.tray.df
# wd=subset(wd,zscore <= 3.5)
# wd=wd %>%
#   mutate(month=factor(month(posixTime)))

# ggplot(data=wd,aes(x=splined.smoothed.der))+
#   facet_wrap(~sizeGroups_filt)+
#   geom_histogram(bins=50,aes(fill=spar))

wd=wd %>%
  rename("x"=splined.smoothed.y,"y"=splined.smoothed.der) %>%
  mutate(sizegroup=cut(x,breaks=c(0,7,20,40),include.lowest=T))
wd=na.omit(wd)

ggplot(data=wd,aes(x=x,y=y))+
  #facet_wrap(~spar)+
  geom_point()+
  xlim(2,45)+
  geom_smooth(method="lm")+
  #geom_smooth(formula=y~1/x^3)+
  labs(x="size [nm]",y="GR [nm/h]",
       title="GR retrieved from splined \"trajectories\"")+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=18))

 

# 
# ggplot(data=wd,aes(x=cut(splined.smoothed.y,breaks=c(0,3,7,20,70),include.lowest=T)))+
#   geom_boxplot(aes(y=splined.smoothed.der,fill=ion),notch=F,varwidth=T,outlier.shape=NA)+
#   scale_y_continuous(breaks=seq(0,40,2),limits=c(0,24))+
#   labs(y="Growth rate [nm/h]")+
#   theme(axis.text = element_text(size=18),
#         axis.title=element_text(size=18),
#         legend.text=element_text(size=18),
#         legend.title=element_text(size=18),
#         panel.grid.major.y=element_line(colour="grey"))

#-------------------------------the spline
# 
wd=filt.tray.df %>%
  select(eventID,posixTime,unfiltered,splined.smoothed.x)%>%
  dplyr::group_by(eventID)%>%
  mutate(timeSinceStart=splined.smoothed.x-min(splined.smoothed.x))%>%
  as.data.frame()%>%
  arrange(eventID,posixTime)
# 
# splined_rep_list=list()
# for (i in 1:length(unique(wd$eventID))){
#   sel=unique(wd$eventID)[i]
#   
#   #creating the values for the splined function
#   ind=which(splined.smooth_functions[[1]]==sel)
#   sfunction=splined.smooth_functions[[2]][[ind]]
#   
#   test=subset(wd,eventID == sel)
#   min=min(test$splined.smoothed.x,na.rm=T)
#   max=max(test$splined.smoothed.x,na.rm=T)
#   timesteps=seq(min,max,60)
#   splined_rep_list[[i]]=data.frame(eventID=sel,secTime=timesteps,dp=predict(sfunction,timesteps)$y,
#                                    timeSinceStart=timesteps-min) %>%
#     mutate(Time=as.POSIXct(secTime,origin="1970/01/01 UTC"))
# }
# splined_rep_df=do.call("rbind",splined_rep_list)
splined_rep_df=evaluate_splined_trajectories(splined.smooth_functions,filt.tray.df,30)


splined_rep_df=splined_rep_df%>%
  mutate(YMD=format(Time,"%Y-%m-%d"))

wd=wd %>%
  mutate(YMD=format(posixTime,"%Y-%m-%d"))

splined_rep_df$ion=getCharge_eventID(splined_rep_df$eventID)
splined_rep_df$index=getInd_eventID(splined_rep_df$eventID)
wd$ion=getCharge_eventID(wd$eventID)

ggplot()+
  geom_line(data=splined_rep_df,aes(x=timeSinceStart/3600,y=dp,group=eventID))+
  geom_point(data=wd,aes(x=timeSinceStart/3600,y=unfiltered,group=eventID,col=ion),size=1)+
  geom_line(data=wd,aes(x=timeSinceStart/3600,y=unfiltered,group=eventID,col=ion))+
  #facet_wrap(~YMD,nrow=5)+
  labs(x="hours since start",y="Dp [nm]")+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15))

ggsave(plot=last_plot(),filename="Trajectories.png",device="png",dpi=300)


