
#getting the evaluated splines
splined_rep_df=evaluate_splined_trajectories(splined.smooth_functions,
                                             filt.tray.df,30)
splined_rep_df=splined_rep_df%>%
  mutate(YMD=format(Time,"%Y-%m-%d"))

mask=npfevent_size_frame %>%
  select(eventID,ion)
mask=unique(mask)

wd=merge(splined_rep_df,mask)
wd=wd %>%
  arrange(eventID)

wd$cuts=cut(wd$dp_splined,breaks=seq(1,28,2),include.lowest=T)
wd=na.omit(wd)
#levels(wd$cuts)=as.character(seq(1,40,2))

used_traj=wd %>%
  select(eventID,cuts)%>%
  dplyr::group_by(cuts)%>%
  mutate(traj=length(unique(eventID)))%>%
  mutate(traj=as.character(traj))

#plotting 
ggplot(data=wd,aes(x=cuts))+
  geom_boxplot(aes(y=dp_splined_der),notch=F,varwidth=F,outlier.shape=NA,fill="lightgrey")+
  scale_y_continuous(breaks=seq(0,40,2),limits=c(0,16))+
  labs(y="Growth rate [nm/h]",x="size [nm]")+
  stat_summary(aes(x=cuts,y=dp_splined_der),position="identity",col="orange")+
  geom_text(data=used_traj,aes(x=cuts,y=13.5,label=traj),size=5,col="red")+
  annotate(geom="text",x=3,y=15.5,label="Unique trajectories used",size=5,col="red")+
  theme(axis.text = element_text(size=18),
        axis.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        panel.grid.major.y=element_line(colour="grey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        axis.text.x=element_text(angle=60,vjust=0.60),
        axis.ticks.x=element_line(size=1.5))

# wd$cuts=cut(wd$yspline,breaks=c(0,3,7,20),include.lowest=T)
# wd=na.omit(wd)
# #levels(wd$cuts)=as.character(seq(1,40,2))
# 
# used_traj=wd %>%
#   select(eventID,cuts)%>%
#   dplyr::group_by(cuts)%>%
#   mutate(traj=length(unique(eventID)))%>%
#   mutate(traj=as.character(traj))
# 
# meds = wd %>%
#   select(cuts,yspline_der)%>%
#   dplyr::group_by(cuts) %>%
#   dplyr::summarise(medians=median(yspline_der,na.rm=T))
# 
# ggplot(data=wd,aes(x=cuts))+
#   geom_boxplot(aes(y=yspline_der,fill=ion),notch=F,varwidth=F,outlier.shape=NA)+
#   scale_y_continuous(breaks=seq(0,40,2),limits=c(0,16))+
#   labs(y="Growth rate [nm/h]",x="size [nm]")+
#   #stat_summary(aes(x=cuts,y=yspline_der),position="identity",col="orange")+
#   geom_text(data=used_traj,aes(x=cuts,y=13.5,label=traj),size=5,col="red")+
#   geom_point(data=meds,aes(x=cuts,y=medians),size=6,stroke=1.5,shape=4)+
#   annotate(geom="text",x=2,y=15.5,label="Unique trajectories used",size=5,col="red")+
#   theme(axis.text = element_text(size=18),
#         axis.title=element_text(size=18),
#         legend.text=element_text(size=18),
#         legend.title=element_text(size=18),
#         panel.grid.major.y=element_line(colour="grey"),
#         panel.grid.major.x=element_line(colour="lightgrey"),
#         axis.text.x=element_text(angle=60,vjust=0.60),
#         axis.ticks.x=element_line(size=1.5))
# 
# 
# 
# 
# #plotting
# ggsave(plot=last_plot(),filename="boxplot_splined_bigCuts.png",device="png",dpi=300)
# 
# 
# 
# # wd$test=wd$timeSinceStart+10
# # # wd$hour=hour(wd$Time)
# # ggplot(data=wd,aes(y=yspline,x=test,col=ion))+
# #   geom_point(size=1)+
# #   #geom_smooth(method="lm",formula=(y ~ exp(x)),col="red")+
# #   theme(axis.text.x=element_blank(),
# #         axis.text.y=element_blank())+
# #   scale_y_continuous()
# # 
# # 
# # ggplot(data=wd,aes(x=yspline,yspline_der))+
# #   geom_point(size=1)+
# #   #geom_smooth(method="lm", formula= (y ~ sqrt(x)), color="red") +
# #   #facet_wrap(~eventID)+
# #   scale_y_continuous()+
# #   ylim(0,20)