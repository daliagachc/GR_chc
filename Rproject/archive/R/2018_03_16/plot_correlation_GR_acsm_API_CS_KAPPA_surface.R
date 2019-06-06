maxMad=3.5
minObs=8

getCorrelation=function(vec1,vec2,minnumb,usedmethod){
  helpdf=data.frame(x=vec1,y=vec2)
  helpdf=na.omit(helpdf)
  if (nrow(helpdf)>=minnumb){
    help=cor.test(helpdf$x,helpdf$y,method=usedmethod)
    out=c(help$estimate,help$p.value)
  } 
  else {
    out=c(NA,NA)
  }
  return(out)
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

#the removed points, useful if you want to show what you removed
MAD_removed=subset(wd3,GR_madDist >= maxMad)
#cleaning the set for outliers
wd3=subset(wd3,GR_madDist < maxMad)


wd3.s=wd3%>%
  dplyr::group_by(magnitude,sizeGroups)%>%
  dplyr::summarise(corr=getCorrelation(GR,value,minObs,"spearman")[1],
                   pval=getCorrelation(GR,value,minObs,"spearman")[2]) %>%
  mutate(pval_cut=cut(pval,breaks=c(0,0.01,0.05,Inf)))%>%
  as.data.frame() %>%
  mutate( magnitude=factor(magnitude))
wd3.s=na.omit(wd3.s)



wd3.s.sig=wd3.s %>% 
  dplyr::filter(pval <= 0.05)%>%
  mutate(siglevel=ifelse(pval < 0.01,"p < 0.01","p < 0.05"))

#plot
ggplot(data=wd3.s,aes(x=magnitude,y=sizeGroups,fill=corr))+
  geom_raster()+
  scale_fill_gradientn(colors=rev(brewer.pal(5,"RdBu")),limits=c(-1,1),oob=squish)+
  geom_text(aes(label=signif(pval,2)))+
  geom_tile(data=wd3.s.sig,aes(col=siglevel),fill="black",alpha=0.0,size=2)+
  scale_color_manual(values=c("green","blue"))+
  #geom_point(aes(col=log10(pval),size=pval_cut))+
  labs(fill="spearman\ncorrelation",colour="significance\nlevel",
       y="GR for Dp groups [nm]",
       caption=paste0("GR outliers with MAD > ",maxMad,
                      " removed. Numbers within cells show p-value (spearman).\nStatistics based on >= ",
                      minObs," data pairs."))+
  theme(axis.text.x=element_text(angle=60,vjust=0.65),
        axis.title.x=element_blank())