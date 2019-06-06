mask=npfevent_size_frame %>%
  select(eventID,ion)

wd=event_GR_long
wd=merge(wd,mask)
wd=unique(wd)

wd=subset(wd, R >= 0.7)
wd=subset(wd, size %in% c("00_03","03_07","07_20","20_80"))

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
median(subset(wd,size=="20_80")$GR)

wd$size=factor(wd$size)
levels(wd$size) <- c("0 - 3 nm", "3 - 7 nm", "7 - 20 nm", "20 - 70 nm")

counts=wd %>%
  dplyr::group_by(size)%>%
  dplyr::summarise(numb=n())

meds=wd %>% 
  select(size,GR)%>%
  dplyr::group_by(size)%>%
  mutate(medians=median(GR,na.rm=T)) %>%
  as.data.frame()

means=wd %>% 
  select(size,GR)%>%
  dplyr::group_by(size)%>%
  mutate(means=mean(GR,na.rm=T)) %>%
  as.data.frame()

ggplot(data=wd)+
  geom_boxplot(aes(x=size,y=GR,fill=ion),notch=F,varwidth=F,outlier.shape=NA)+
  scale_y_continuous(breaks=seq(0,40,2),limits=c(0,16))+
  labs(y="Growth rate [nm/h]")+
  geom_text(data=counts, aes(x=size,y=0,label=numb,group=size),col="red")+
  geom_point(data=meds,aes(x=size,y=medians),size=6,stroke=1.5,shape=4)+
  #geom_point(data=means,aes(x=size,y=means),size=4,col="orange")+
  theme(axis.text = element_text(size=15),
        axis.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        panel.grid.major.y=element_line(colour="grey"))
# 
# ggarrange(plot1,plot2,ncol=2,align="h",common.legend=T,legend="right")

levels(wd$size)=c("[0,3]","(3,7]","(7,20]","(20,60]")
levels(meds$size)=c("[0,3]","(3,7]","(7,20]","(20,60]")
levels(counts$size)=c("[0,3]","(3,7]","(7,20]","(20,60]")



ggplot(data=wd,aes(x=size))+
  geom_boxplot(aes(x=size,y=GR,fill=ion),notch=F,varwidth=F,outlier.shape=NA)+
  scale_y_continuous(breaks=seq(0,40,2),limits=c(0,16))+
  labs(y="Growth rate [nm/h]",x="size [nm]")+
  #stat_summary(aes(x=cuts,y=yspline_der),position="identity",col="orange")+
  geom_point(data=meds,aes(x=size,y=medians),size=6,stroke=1.5,shape=4)+
  annotate(geom="text",x=2,y=15.5,label="Number of data points",size=5,col="red")+
  geom_text(data=counts, aes(x=size,y=13.5,label=numb,group=size),col="red",size=5)+
  theme(axis.text = element_text(size=18),
        axis.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        panel.grid.major.y=element_line(colour="grey"),
        panel.grid.major.x=element_line(colour="lightgrey"),
        axis.text.x=element_text(angle=60,vjust=0.60),
        axis.ticks.x=element_line(size=1.5))




ggsave(plot=last_plot(),filename="GR_classical_approach.png",device="png",dpi=300)