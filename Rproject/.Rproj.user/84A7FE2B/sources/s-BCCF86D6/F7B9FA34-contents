maxMad=3.5
minObs=8

temporal_function_plot_cell=function(df,sizegroup,mag){
  
  help=df %>%
    dplyr::filter(sizeGroups==sizegroup,magnitude==mag)
  
  MAD_removed=subset(help,GR_madDist > maxMad) %>%
    mutate(label=paste0("removed outliers (MAD >= ",maxMad,")"))
  
  
  plot=ggplot(data=subset(help,GR_madDist <= maxMad &!is.na(sizeGroups)),aes(x=value,y=GR))+
    geom_point()+
    geom_smooth(method="lm")+
    stat_cor(method="spearman",size=4,col="blue")+
    geom_point(data=MAD_removed,aes(x=value,y=GR,col=label))+
    #facet_wrap(~magnitude+sizeGroups,scale="free",ncol=4)+
    labs(col="", y="GR [nm/h]")+
    theme(legend.position=c(0.6,0.95),
          legend.background=element_blank(),
          axis.title=element_text(size=12),
          axis.text=element_text(size=10))
  
  return(plot)
}

wd=evaluate_splined_trajectories(splined.smooth_functions,
                                 filt.tray.df,15) %>%
  mutate(YMD=format(Time,"%Y-%m-%d"))

wd$Time=force_tz(wd$Time,tzone="etc/GMT+4")
wd=wd%>%
  mutate(sizeGroups=cut(dp_splined,breaks=c(1.3,3,7,10,20,40),include.lowest=T),
         YMDH=format(Time,"%Y-%m-%d-%H"))%>%
  dplyr::group_by(YMDH,sizeGroups)%>%
  dplyr::summarise(GR=median(dp_splined_der,na.rm=T))%>%
  as.data.frame()

wd2=ACSM_API_CS_KAPPA_combined%>%
  mutate(YMDH=format(localTime,"%Y-%m-%d-%H")) %>%
  dplyr::group_by(magnitude,YMDH)%>%
  dplyr::summarise(value=median(value,na.rm=T),
                   numb=n())

wd3=merge(wd,wd2,by="YMDH")

wd3=wd3%>%
  dplyr::group_by(magnitude,sizeGroups)%>%
  mutate(GR_madDist=get_MAD_distance(GR,"double"),
         val_madDist=get_MAD_distance(value,"double")) %>%
  mutate(pair_madDist=ifelse(GR_madDist>=val_madDist,
                             GR_madDist,
                             val_madDist)) %>%
  as.data.frame()





p1=temporal_function_plot_cell(wd3,"(3,7]","Nitrate\n(ACSM)")+
  labs(x="Nitrate [ug/m3]",y="(3,7] nm GR [nm/h]")
p2=temporal_function_plot_cell(wd3,"(3,7]","Ammonium\n(ACSM)")+
  labs(x="Ammonium [ug/m3]",y="(3,7] nm GR [nm/h]")
p3=temporal_function_plot_cell(wd3,"(3,7]","CS\n(SMPS)")+
  labs(x="CS kappa corrected [relative]",y="(3,7] nm GR [nm/h]")
p4=temporal_function_plot_cell(wd3,"(20,40]","CS\n(SMPS)")+
  labs(x="CS kappa corrected [relative]",y="(20,40] nm GR [nm/h]")
p5=temporal_function_plot_cell(wd3,"(3,7]","SA\n(APITOF)")

ggarrange(p1,p2,p3,p4,ncol=2,nrow=2,common.legend=T,legend="bottom")


# 
# 
# #now to the subsetting and renaming
# test=wd3
# test=subset(test,pair_madDist <=2)
# test=subset(test,magnitude=="Sulfate")
# 
# levels(test$sizeGroups)=c("0 - 3 nm","3 - 7 nm","7 - 20 nm","20 - 40 nm")
# 
# test$pair_madDist_cut=cut(test$pair_madDist,breaks=c(0,1,2,4,8,Inf),include.lowest = T)
# 
# ggplot(data=test,aes(x=value,y=GR))+
#   geom_point(aes(col=pair_madDist_cut))+
#   facet_wrap(~sizeGroups,scale="free")+
#   scale_x_continuous(trans="log10")+
#   scale_y_continuous(trans="log10")+
#   geom_smooth(method="lm")+
#   labs(x="log(CS relative value)",y="log(GR[nm/h])",title="GR- Sulfate",
#        fill="Z score")+
#   theme(axis.text.x=element_text(angle=45,vjust=0.8))
# # 
# # ggsave(plot=last_plot(),device="png",filename="GR_Sulfate.png",dpi=300)