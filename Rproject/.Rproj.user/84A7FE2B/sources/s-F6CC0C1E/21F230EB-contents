#evaluate the trayectories
wd=filt.tray.df %>%
  select(eventID,localTime,unfiltered,splined.smoothed.x)%>%
  dplyr::group_by(eventID)%>%
  mutate(timeSinceStart=splined.smoothed.x-min(splined.smoothed.x))%>%
  as.data.frame()%>%
  arrange(eventID,localTime) %>%
  mutate(YMD=format(localTime,"%Y-%m-%d"))

#getting the evaluated splines
splined_rep_df=evaluate_splined_trajectories(splined.smooth_functions,
                                             filt.tray.df,30)
splined_rep_df=splined_rep_df%>%
  mutate(YMD=format(Time,"%Y-%m-%d"))

splined_rep_df$ion=getCharge_eventID(splined_rep_df$eventID)
splined_rep_df$index=getInd_eventID(splined_rep_df$eventID)
wd$ion=getCharge_eventID(wd$eventID)

ggplot()+
  geom_line(data=splined_rep_df,aes(x=timeSinceStart/3600,y=dp_splined,group=eventID,
                                    size="spline\nfunctions"))+
  scale_size_manual(values=c(0.3))+
  geom_point(data=wd,aes(x=timeSinceStart/3600,y=unfiltered,group=eventID,col=ion),size=1)+
  geom_line(data=wd,aes(x=timeSinceStart/3600,y=unfiltered,group=eventID,col=ion))+
  #facet_wrap(~YMD,nrow=5)+
  labs(x="hours since start of event",y="Dp [nm]",size="",
       col="time evolution\nof growing mode,\nion:")+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15))

# ggsave(plot=last_plot(),filename="Trajectories.png",device="png",dpi=300)
# 

