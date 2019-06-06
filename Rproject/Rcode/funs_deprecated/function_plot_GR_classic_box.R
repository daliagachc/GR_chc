plotGRclassic=function(){
  
  mask=npfevent_size_frame %>%
    select(eventID,ion)
  
  wd=event_GR_long
  wd=merge(wd,mask)
  wd=unique(wd)
  
  wd=subset(wd, R >= 0.7)
  wd=subset(wd, size %in% c("00_03","03_07","07_20"))
  
  mask2=eventIDclas %>%
    dplyr::filter(score10 >=5 & airChange ==0) %>%
    select(eventID)
  wd=merge(wd,mask2,by="eventID")
  
  
  # ggplot(data=wd, aes(x=GR,fill=ion))+
  #   facet_wrap(~size+ion,ncol=2)+
  #   geom_histogram(bins=15)+
  #   labs(x="GR [nm/h]")
  
  median(subset(wd,size=="00_03")$GR)
  median(subset(wd,size=="03_07")$GR)
  median(subset(wd,size=="07_20")$GR)
  median(subset(wd,size=="10_25")$GR)
  median(subset(wd,size=="20_80")$GR)
  
  wd$size=factor(wd$size)

  counts=wd %>%
    dplyr::group_by(size)%>%
    dplyr::summarise(numb=n())
  
  meds=wd %>% 
    select(size,GR)%>%
    dplyr::group_by(size)%>%
    mutate(medians=median(GR,na.rm=T),
           shape=factor("group\nmedian")) %>%
    as.data.frame()
  
  means=wd %>% 
    select(size,GR)%>%
    dplyr::group_by(size)%>%
    mutate(means=mean(GR,na.rm=T)) %>%
    as.data.frame()
  
  
  levels(wd$size)=c("[1,3]","(3,7]","(7,20]","(20,60]")
  levels(meds$size)=c("[1,3]","(3,7]","(7,20]","(20,60]")
  levels(counts$size)=c("[1,3]","(3,7]","(7,20]","(20,60]")
  
  
  
  out=ggplot(data=wd,aes(x=size))+
    geom_boxplot(aes(x=size,y=GR,fill=ion),notch=F,varwidth=F,outlier.shape=NA)+
    coord_cartesian(ylim=c(0,16))+
    scale_y_continuous(breaks=seq(0,40,2))+
    labs(y="Growth rate [nm/h]",x="Dp [nm]",shape="")+
    geom_point(data=meds,aes(x=size,y=medians,shape=shape),size=6,stroke=1.5)+
    scale_shape_manual(values = c(4))+
    annotate(geom="text",x=2,y=15.5,label="Number of data points",size=5,col="red")+
    geom_text(data=counts, aes(x=size,y=13.5,label=numb,group=size),col="red",size=5)+
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
plotGRclassic()

# 
# 
# ggsave(plot=last_plot(),filename="GR_classical_approach.png",device="png",dpi=300)