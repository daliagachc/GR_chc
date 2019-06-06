
time1=as.POSIXct("2018-04-01",tz="etc/GMT+4")
time2=as.POSIXct("2018-05-31",tz="etc/GMT+4")

wd=NAIS_ion_df
attributes(wd$startTime)$tzone <- "etc/GMT+4" 
attributes(wd$endTime)$tzone <- "etc/GMT+4" 
wd=subset(wd, startTime >= time1 & startTime < time2)

#plot the negative ions
negplot=plotNAIS(subset(wd,ion=="negative"))
negplot=negplot+theme(legend.position="none")

wd=meteo_file[,c("Time","WD","WS","Tair","SWu")]
# wd=wd %>%
#   mutate(WD=ifelse(WS >= 2,WD,NA))
wd$localTime=wd$Time
attributes(wd$localTime)$tzone = "etc/GMT+4"
wd$WS_cut=cut(wd$WS,breaks=c(0,2,4,Inf),include.lowest=T)

wd=subset(wd,localTime >= time1 & localTime  <= time2)

wdplot=ggplot(data=wd,aes(x=localTime,y=WD,group=1,col=WS_cut))+
  geom_line()+
  scale_x_datetime(date_breaks = "1 day",
                              labels = date_format("%d", tz="etc/GMT+4"),
                              position = "bottom",
                              expand = c(0.01,0.01))+
  scale_y_continuous(breaks=seq(0,360,90))+
  scale_color_manual(values=c("white","black","black"))+
  geom_hline(yintercept=180,col="darkgrey")+
  labs(y="WD")


radplot=ggplot(data=wd,aes(x=localTime,y=normalizeIt(SWu,"max")))+
  geom_line()+
  scale_y_continuous(breaks=c(0,1))+
  scale_x_datetime(date_breaks = "1 day",
                   labels = date_format("%d", tz="etc/GMT+4"),
                   position = "bottom",
                   expand = c(0.01,0.01))+
  labs(y="rad")

wd=acsm_data_long %>%
  mutate(DateLocal=DateUTC)
attributes(wd$DateLocal)$tzone ="etc/GMT+4"
wd=subset(wd, DateLocal >= time1 & DateLocal <= time2)
wd=subset(wd,magnitude %in% c("Nitrate","Organics","Sulfate","Chloride"))
wd.normfacs=wd %>%
  dplyr::group_by(magnitude)%>%
  dplyr::summarise(normfac=max(value,na.rm=T))

wd=merge(wd,wd.normfacs,by="magnitude")%>%
  mutate(value_norm=value/normfac)
  
acsmplot=ggplot(data=wd)+
  geom_line(aes(x=DateLocal, y=value_norm,col=magnitude))+
  scale_color_manual(values=c("blue","darkgreen","Red","Orange"))+
  scale_y_continuous(limits=c(0,1))+
  scale_x_datetime(date_breaks = "1 day",
                   labels = date_format("%d", tz="etc/GMT+4"),
                   position = "bottom",
                   expand = c(0.01,0.01))



#removing labels and adjusting margins
negplot=negplot+theme(axis.title.x=element_blank(),
                      plot.title=element_blank(),
                      plot.margin = unit(c(0,0.5,0,0), "cm"),
                      axis.title.y=element_text(size=12),
                      panel.grid.major.x=element_line(size=0.5,color="grey"),
                      panel.ontop=T)
wdplot=wdplot+theme(axis.title.x=element_blank(),
                    plot.margin = unit(c(0.3,0.5,0,0), "cm"),
                    axis.text.x=element_blank(),
                    axis.title.y=element_text(size=12),
                    panel.grid.major.x=element_line(size=0.5,color="grey"),
                    legend.position="none",
                    #legend.position=c(0.3,0.7),
                    legend.direction="horizontal",
                    legend.title=element_blank())
radplot=radplot+theme(axis.title.x=element_blank(),
                      plot.margin = unit(c(0.3,0.5,0.1,0), "cm"),
                      axis.text.x=element_blank(),
                      axis.title.y=element_text(size=12),
                      panel.grid.major.x=element_line(size=0.5,color="grey"))
acsmplot=acsmplot+theme(axis.title.x=element_blank(),
                        plot.margin = unit(c(0.3,0.5,0,0), "cm"),
                        legend.title=element_blank(),
                        legend.direction="horizontal",
                        axis.text.x=element_blank(),
                        axis.title.y=element_text(size=12),
                        panel.grid.major.x=element_line(size=0.5,color="grey"),
                        legend.position=c(0.3,0.85),
                        legend.box.background = element_rect(colour="white"))+
                  labs(y="normalized value")

ptotal=ggarrange(negplot,acsmplot,wdplot,radplot,
          ncol = 1, nrow  =4,align="v",
          heights=c(1,1,0.75,0.35))
annotate_figure(ptotal,
                top = text_grob(paste0(time1," until ",time2,"-negative ions")))

workDir=getwd()
setwd(paste0(workDir,"/R/figs"))
#saving as png
ggsave(paste0("NAIS_WD_RAD_ACSM_",time1,"--",time2,".png"), plot = last_plot(),
       scale = 1, dpi = 300, device = "png")
setwd(workDir)


