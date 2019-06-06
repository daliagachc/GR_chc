
wd=filt.tray.df
wd=subset(wd,zscore <= 3.5)

wd=wd %>%
  arrange(eventID,posixTime)%>%
  dplyr::group_by(eventID) %>%
  dplyr::summarise(start=first(splined.smoothed.x,na.rm=T),
                   end=last(splined.smoothed.x,na.rm=T)) %>%
  as.data.frame()

filt.tray.df_reval = list()
for (i in 1:nrow(wd)){
 sel=wd[i,"eventID"]
  #creating the values for the splined function
  ind=which(splined.smooth_functions[[1]]==sel)
  sfunction=splined.smooth_functions[[2]][[ind]]#the function to apply
  
  min=wd[i,"start"]
  max=wd[i,"end"]
  filt.tray.df_reval[[i]]=data.frame(eventID=sel, x= seq(min,max,30),
                         yspline=predict(sfunction,seq(min,max,30))$y,
                         yspline_der=predict(sfunction,seq(min,max,30),deriv=1)$y*3600) %>%
    mutate(Time=as.POSIXct(x,origin="1970/01/01 UTC"))
}

filt.tray.df_reval=do.call("rbind",filt.tray.df_reval)


wd2=filt.tray.df_reval %>%
  arrange(eventID,x)%>%
  dplyr::group_by(eventID)%>%
  mutate(timeSinceStart=x-first(x)) %>%
  as.data.frame()

mask=npfevent_size_frame %>%
  select(eventID,ion)
mask=unique(mask)


wd2=merge(wd2,mask)
wd2=wd2 %>%
  arrange(eventID)

# wd2$cuts=cut(wd2$yspline,breaks=seq(0,28,2),include.lowest=T)
# wd2=na.omit(wd2)
# #levels(wd2$cuts)=as.character(seq(1,40,2))
# 
# used_traj=wd2 %>%
#   select(eventID,cuts)%>%
#   dplyr::group_by(cuts)%>%
#   mutate(traj=length(unique(eventID)))%>%
#   mutate(traj=as.character(traj))



# ggplot(data=wd2,aes(x=cuts))+
#   geom_boxplot(aes(y=yspline_der),notch=F,varwidth=F,outlier.shape=NA,fill="lightgrey")+
#   scale_y_continuous(breaks=seq(0,40,2),limits=c(0,16))+
#   labs(y="Growth rate [nm/h]",x="size [nm]")+
#   stat_summary(aes(x=cuts,y=yspline_der),position="identity",col="orange")+
#   geom_text(data=used_traj,aes(x=cuts,y=13.5,label=traj),size=5,col="red")+
#   annotate(geom="text",x=3,y=15.5,label="Unique trajectories used",size=5,col="red")+
#   theme(axis.text = element_text(size=18),
#         axis.title=element_text(size=18),
#         legend.text=element_text(size=18),
#         legend.title=element_text(size=18),
#         panel.grid.major.y=element_line(colour="grey"),
#         panel.grid.major.x=element_line(colour="lightgrey"),
#         axis.text.x=element_text(angle=60,vjust=0.60),
#         axis.ticks.x=element_line(size=1.5))

wd2$cuts=cut(wd2$yspline,breaks=c(0,3,7,20),include.lowest=T)
wd2=na.omit(wd2)
#levels(wd2$cuts)=as.character(seq(1,40,2))

used_traj=wd2 %>%
  select(eventID,cuts)%>%
  dplyr::group_by(cuts)%>%
  mutate(traj=length(unique(eventID)))%>%
  mutate(traj=as.character(traj))

meds = wd2 %>%
  select(cuts,yspline_der)%>%
  dplyr::group_by(cuts) %>%
  dplyr::summarise(medians=median(yspline_der,na.rm=T))

ggplot(data=wd2,aes(x=cuts))+
  geom_boxplot(aes(y=yspline_der,fill=ion),notch=F,varwidth=F,outlier.shape=NA)+
  scale_y_continuous(breaks=seq(0,40,2),limits=c(0,16))+
  labs(y="Growth rate [nm/h]",x="size [nm]")+
  #stat_summary(aes(x=cuts,y=yspline_der),position="identity",col="orange")+
  geom_text(data=used_traj,aes(x=cuts,y=13.5,label=traj),size=5,col="red")+
  geom_point(data=meds,aes(x=cuts,y=medians),size=6,stroke=1.5,shape=4)+
  annotate(geom="text",x=2,y=15.5,label="Unique trajectories used",size=5,col="red")+
  theme(axis.text = element_text(size=18),
        axis.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        panel.grid.major.y=element_line(colour="grey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        axis.text.x=element_text(angle=60,vjust=0.60),
        axis.ticks.x=element_line(size=1.5))




#plotting
ggsave(plot=last_plot(),filename="boxplot_splined_bigCuts.png",device="png",dpi=300)



# wd2$test=wd2$timeSinceStart+10
# # wd2$hour=hour(wd2$Time)
# ggplot(data=wd2,aes(y=yspline,x=test,col=ion))+
#   geom_point(size=1)+
#   #geom_smooth(method="lm",formula=(y ~ exp(x)),col="red")+
#   theme(axis.text.x=element_blank(),
#         axis.text.y=element_blank())+
#   scale_y_continuous()
# 
# 
# ggplot(data=wd2,aes(x=yspline,yspline_der))+
#   geom_point(size=1)+
#   #geom_smooth(method="lm", formula= (y ~ sqrt(x)), color="red") +
#   #facet_wrap(~eventID)+
#   scale_y_continuous()+
#   ylim(0,20)