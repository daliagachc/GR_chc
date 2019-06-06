
#------------------------------DATA HANDLING
#getting the evaluated splines
splined_rep_df=evaluate_splined_trajectories(splined.smooth_functions,
                                             filt.tray.df,30) %>%
  mutate(YMD=format(Time,"%Y-%m-%d"))

mask=npfevent_size_frame %>%
  select(eventID,ion)
mask=unique(mask)

wd=merge(splined_rep_df,mask) %>%
  arrange(eventID)

init=1
end=25
n=8
sequence=seq(log(init),log(end),(log(end)-log(init))/n)
sequence=exp(sequence)
sequence=round(sequence,1)
#sequence=seq(1,40,2)


wd$cuts=cut(wd$dp_splined,breaks=sequence,include.lowest=T)
wd=na.omit(wd)
#levels(wd$cuts)=as.character(seq(1,40,2))

#calculating how many trajectories went into each box 
used_traj=wd %>%
  select(eventID,cuts)%>%
  dplyr::group_by(cuts)%>%
  mutate(traj=length(unique(eventID)))%>%
  mutate(traj=as.character(traj))

means=wd %>% 
  select(cuts,dp_splined_der)%>%
  dplyr::group_by(cuts)%>%
  mutate(means=mean(dp_splined_der,na.rm=T),
         shape="mean") %>%
  as.data.frame()


#plotting 
ggplot(data=wd,aes(x=cuts))+
  geom_boxplot(aes(y=dp_splined_der),notch=F,varwidth=F,outlier.shape=NA,fill="lightgrey")+
  coord_cartesian(ylim=c(0,16))+
  scale_y_continuous(breaks=seq(0,16,2))+
  labs(y="Growth rate [nm/h]",x="Dp [nm]",shape="")+
  geom_point(data=means,aes(x=cuts,y=means,shape=shape),col="orange",size=4)+
  scale_shape_manual(values=c(19))+
  geom_text(data=used_traj,aes(x=cuts,y=13.5,label=traj),size=5,col="red")+
  annotate(geom="text",x=3,y=15.5,label="Unique trajectories used",size=5,col="red")+
  theme(axis.text = element_text(size=14),
        axis.title=element_text(size=14),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        panel.grid.major.y=element_line(colour="grey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        axis.text.x=element_text(angle=60,vjust=0.60),
        axis.ticks.x=element_line(size=1.5),
        legend.position=c(0.88,0.16),
        legend.background=element_blank())
