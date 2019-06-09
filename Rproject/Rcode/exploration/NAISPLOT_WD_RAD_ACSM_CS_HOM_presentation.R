
time1=as.POSIXct("2018-02-04 ",tz="etc/GMT+4")
time2=as.POSIXct("2018-02-06 00:00",tz="etc/GMT+4")

wd=NAIS_ion_df
attributes(wd$startTime)$tzone <- "etc/GMT+4" 
attributes(wd$endTime)$tzone <- "etc/GMT+4" 
wd=subset(wd, startTime >= time1 & startTime < time2)

#plot the negative ions
negplot=plotNAIS(subset(wd,ion=="negative"))
negplot=negplot+theme(legend.position="none")

#getting the evaluated splines
splined_rep_df=evaluate_splined_trajectories(splined.smooth_functions,
                                             filt.tray.df,30)
splined_rep_df=subset(splined_rep_df, Time >= time1 & Time < time2)
splined_rep_df=splined_rep_df %>%
  dplyr::filter(substring(eventID,1,1)=="N")


GRlabels=evaluate_splined_trajectories(splined.smooth_functions,
                                             filt.tray.df,50)
GRlabels=subset(GRlabels,timeSinceStart %in% c(250,1000,2500,5500))
GRlabels=subset(GRlabels, Time >= time1 & Time < time2)%>%
  dplyr::filter(substring(eventID,1,1)=="N")


#modifying the plots

negplot=negplot+geom_line(data=splined_rep_df,aes(y=dp_splined*1e-9,x=Time,group=eventID),size=1)+
  geom_text(data=GRlabels, aes(x=Time-5000,dp_splined*1e-9,label=round(dp_splined_der,0)),size=5)+
  geom_point(data=GRlabels, aes(x=Time,dp_splined*1e-9),size=3)+
  labs(fill="dN/d(logDp)\n[cm3]")

wd=meteo_file[,c("localTime","WD","WS","Tair","SWu")]
# wd=wd %>%
#   mutate(WD=ifelse(WS >= 2,WD,NA))
wd$WS_cut=cut(wd$WS,breaks=c(0,2,4,Inf),include.lowest=T)
wd=subset(wd,localTime >= time1 & localTime  <= time2)

wdplot=ggplot(data=wd,aes(x=localTime,y=WD,group=1))+
  geom_line(aes(col=WS_cut))+
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
  geom_point(aes(col="radiation"))+
  scale_color_manual(values=c(NA))+
  scale_x_datetime(date_breaks = "1 day",
                   labels = date_format("%d", tz="etc/GMT+4"),
                   position = "bottom",
                   expand = c(0.01,0.01))+
  labs(y="rad")

#ACSM plot
wd=ACSM_API_CS_KAPPA_combined
wd=subset(wd,magnitude %in% c("Nitrate\n(ACSM)","Sulfate\n(ACSM)",
                              "Ammonium\n(ACSM)"))
wd=wd %>%
  dplyr::group_by(magnitude)%>%
  dplyr::mutate(value_norm=normalizeIt(value,"median"))


wd=subset(wd, localTime >= time1 & localTime <= time2)  


acsmplot=ggplot(data=wd)+
  geom_line(aes(x=localTime, y=value_norm,col=magnitude))+
  geom_point(aes(x=localTime, y=value_norm,col=magnitude),size=0.5)+
  scale_color_manual(values=c("orange","blue","Red"))+
  scale_y_continuous(breaks=c(0,1,2,4))+
  geom_hline(yintercept=1,col="black")+
  scale_x_datetime(date_breaks = "1 day",
                   labels = date_format("%d", tz="etc/GMT+4"),
                   position = "bottom",
                   expand = c(0.01,0.01))

#APITIF-CS plot
wd=ACSM_API_CS_KAPPA_combined

wd=subset(wd,magnitude %in% c("SA\n(APITOF)","CS\n(SMPS)","HOM\n(APITOF)"))
wd=wd %>%
  dplyr::group_by(magnitude)%>%
  dplyr::mutate(value_norm=normalizeIt(value,"median"))
wd=subset(wd, localTime >= time1 & localTime <= time2)

API_CS_plot=ggplot(data=wd)+
  geom_line(aes(x=localTime, y=value_norm,col=magnitude))+
  geom_point(aes(x=localTime, y=value_norm,col=magnitude),size=0.5)+
  scale_y_continuous(trans="log2",breaks=c(0,1,4,8,32))+
  geom_hline(yintercept=1,col="black")+
  scale_x_datetime(date_breaks = "1 day",
                   labels = date_format("%d", tz="etc/GMT+4"),
                   position = "bottom",
                   expand = c(0.01,0.01))

#


#removing labels and adjusting margins
negplot=negplot+theme(axis.title.x=element_blank(),
                      plot.title=element_blank(),
                      #plot.margin = unit(c(0,0.5,0,0), "cm"),
                      axis.title.y=element_text(size=12),
                      #panel.grid.major.x=element_line(size=0.5,color="grey"),
                      #panel.ontop=F,
                      legend.position="right")

wdplot=wdplot+theme(axis.title.x=element_blank(),
                    plot.margin = unit(c(0.3,0.5,0,0), "cm"),
                    axis.text.x=element_blank(),
                    axis.text.y=element_text(size=11),
                    axis.title.y=element_text(size=12),
                    #panel.grid.major.x=element_line(size=0.5,color="grey"),
                    legend.position="right",
                    legend.direction="horizontal",
                    legend.title=element_blank())+
  labs(y="wind\ndirection")+
  theme(legend.text=element_text(colour="white"),
        legend.background = element_rect(colour="white"),
        legend.key = element_rect(fill = "white", color = NA))

acsmplot=acsmplot+theme(axis.title.x=element_blank(),
                        plot.margin = unit(c(0.3,0.5,0,0), "cm"),
                        legend.title=element_blank(),
                        legend.direction="vertical",
                        axis.text.x=element_blank(),
                        axis.text.y=element_text(size=11),
                        axis.title.y=element_text(size=12),
                        #panel.grid.major.x=element_line(size=0.5,color="grey"),
                        legend.position="right",
                        legend.background = element_blank(),
                        legend.text=element_text(size=11))+
                  labs(y="value\n/median")

API_CS_plot=API_CS_plot+
  theme(axis.title.x=element_blank(),
        plot.margin = unit(c(0.3,0.5,0,0), "cm"),
        legend.title=element_blank(),
        legend.direction="vertical",
        axis.text.x=element_blank(),
        axis.text.y=element_text(size=11),
        axis.title=element_text(size=12),
        #panel.grid.major.x=element_line(size=0.5,color="grey"),
        legend.position="right",
        legend.background = element_blank(),
        legend.text=element_text(size=11))+
  labs(y="value\n/median")

radplot=radplot+
  labs(col="")+
  theme(axis.title.x=element_blank(),
                      plot.margin = unit(c(0.3,0.5,0.1,0), "cm"),
                      axis.text.x=element_blank(),
                      axis.title=element_text(size=12),
        axis.text.y=element_text(size=11),
                      #panel.grid.major.x=element_line(size=0.5,color="grey"),
                      legend.position="right",
                      legend.text=element_text(colour="white"),
        legend.background = element_rect(colour="white"),
        legend.key = element_rect(fill = "white", color = NA))+
  labs(y="rad")

#creating a dummyplot if a plot is missing
dummyplot=negplot+coord_cartesian(ylim=c(1e10,1e11))+
  labs(x="",y="",col)

if (nrow(acsmplot$data)==0){
  acsmplot=dummyplot
}
if (nrow(API_CS_plot$data)==0){
  API_CS_plot=dummyplot
}


ptotal=ggarrange(negplot,API_CS_plot,wdplot,radplot,
          ncol = 1, nrow  =4,align="v",
          heights=c(1,0.7,0.7,0.5,0.25))
# 
# g=VAlignPlots(negplot,API_CS_plot,wdplot,radplot)
# annotate_figure(g,
#                 top = text_grob(paste0(time1," until ",time2,"-negative ions")))
# 
# # workDir=getwd()
# # setwd(paste0(workDir,"/R/figs"))
# # #saving as png
# # ggsave(paste0("NAIS_WD_RAD_ACSM_CS_",time1,"--",time2,".png"), plot = last_plot(),
# #        scale = 1, dpi = 300, device = "png")
# # setwd(workDir)
# # 
# 
