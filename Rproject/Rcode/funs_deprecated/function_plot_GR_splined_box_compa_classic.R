plotGR_splined_compa=function(){
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
  
  wd$cuts=cut(wd$dp_splined,breaks=c(1,3,7,20),include.lowest=T)
  wd=na.omit(wd)
  #levels(wd$cuts)=as.character(seq(1,40,2))
  
  used_traj=wd %>%
    select(eventID,cuts)%>%
    dplyr::group_by(cuts)%>%
    mutate(traj=length(unique(eventID)))%>%
    mutate(traj=as.character(traj))
  
  meds = wd %>%
    select(cuts,dp_splined_der)%>%
    dplyr::group_by(cuts) %>%
    dplyr::summarise(medians=median(dp_splined_der,na.rm=T),
                     shape=factor("group\nmedian"))
  
  out=ggplot(data=wd,aes(x=cuts))+
    geom_boxplot(aes(y=dp_splined_der,fill=ion),notch=F,varwidth=F,outlier.shape=NA)+
    coord_cartesian(ylim=c(0,16))+
    scale_y_continuous(breaks=seq(0,40,2))+
    labs(y="Growth rate [nm/h]",x="Dp [nm]")+
    #stat_summary(aes(x=cuts,y=yspline_der),position="identity",col="orange")+
    geom_text(data=used_traj,aes(x=cuts,y=13.5,label=traj),size=5,col="red")+
    geom_point(data=meds,aes(x=cuts,y=medians,shape=shape),size=6,stroke=1.5)+
    scale_shape_manual(values = c(4))+
    annotate(geom="text",x=2,y=15.5,label="Unique trajectories used",size=5,col="red")+
    theme(axis.text = element_text(size=14),
          axis.title=element_text(size=14),
          legend.text=element_text(size=14),
          legend.title=element_text(size=14),
          panel.grid.major.y=element_line(colour="grey"),
          panel.grid.major.x=element_line(colour="lightgrey"),
          #axis.text.x=element_text(angle=60,vjust=0.60),
          axis.ticks.x=element_line(size=1.5))
  
  return(out)
}
plotGR_splined_compa()
