
wd=filt.tray.df_reval 
wd$Time=force_tz(wd$Time,tzone="etc/GMT+4")

wd=wd%>%
  mutate(sizeGroups=cut(yspline,breaks=c(0,3,7,20,50),include.lowest=T))

wd=wd%>%
  mutate(YMDH=format(Time,"%Y-%m-%d-%H"))%>%
  dplyr::group_by(YMDH,sizeGroups)%>%
  dplyr::summarise(GR=median(yspline_der,na.rm=T))%>%
  as.data.frame()

wd2=ACSM_HOM_SA_combined %>%
  mutate(YMDH=format(localTime,"%Y-%m-%d-%H")) %>%
  dplyr::group_by(magnitude,YMDH)%>%
  dplyr::summarise(value=median(value,na.rm=T),
                   numb=n())


wd3=merge(wd,wd2,by="YMDH")
#wd3=subset(wd3,GR <=10 & GR >= 2)

wd3=wd3%>%
  dplyr::group_by(magnitude,sizeGroups)%>%
  mutate(GRmad=median(abs(GR-median(GR,na.rm=T))))%>%
  mutate(GRzscore=0.7*(abs(GR-median(GR,na.rm=T)))/GRmad) %>%
  as.data.frame()

wd3=wd3%>%
  dplyr::group_by(magnitude,sizeGroups)%>%
  mutate(VALmad=median(abs(value-median(value,na.rm=T)))) %>%
  mutate(VALzscore=0.7*(abs(value-median(value,na.rm=T)))/VALmad) %>%
  as.data.frame()

wd3=wd3%>%
  mutate(pairZscore=ifelse(VALzscore>=GRzscore,VALzscore,GRzscore))

levels(wd3$sizeGroups)=c("0 - 3 nm","3 - 7 nm","7 - 20 nm","20 - 40 nm")

test=wd3
test=subset(wd3,GRzscore <= 3.5 & VALzscore <= 3.5)
test=subset(test,magnitude=="CS_kappa")


ggplot(data=test,aes(x=value,y=GR))+
  geom_point()+
  facet_wrap(~sizeGroups,scale="free")+
  scale_x_continuous(trans="log10")+
  scale_y_continuous(trans="log10")+
  geom_smooth(method="lm")+
  labs(x="log(CS relative value)",y="log(GR[nm/h])",title="GR- CS kappa",
       fill="Z score")+
  theme(axis.text.x=element_text(angle=45,vjust=0.8))

ggsave(plot=last_plot(),device="png",filename="GR_CS_kappa.png",dpi=300)