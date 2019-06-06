#
wd1=NAIS_ion_df_binned_1H
wd1$size=as.factor(wd1$size)
levels(wd1$size)=c("0 - 3 nm", "3 - 7 nm", "7 - 20 nm", "20 - 70 nm")

wd2=acsm_data_long_1H

wd3=merge(wd1,wd2,by="localTime")

#wd4=subset(wd3,hour(localTime) %in% seq(7,16,1))

wd4=subset(wd3,value.x >1 & value.y >0) %>%
  mutate(N_log=log10(value.x),
         val_log=log10(value.y))

wd4=subset(wd4,ion=="negative")

magList=c("Sulfate","Nitrate","Organics","Ammonium")
plotList=list()

ggplot(data=wd4)+
  geom_point(aes(x=N_log,y=val_log,col=size))+
  scale_x_continuous()+
  facet_wrap(~size+magnitude,dir="v")+
  geom_smooth(aes(x=N_log,y=val_log,group=size),col="black",method="lm")+
  labs(x="log(N)",y="log(value)",title=magList[i],col="particle size")

# 
# for (i in 1:length(magList)){
#   help=subset(wd4, magnitude == magList[i])
#   
#   plotList[[i]]=ggplot(data=help)+
#     geom_point(aes(x=N_log,y=val_log,col=size))+
#     scale_x_continuous()+
#     facet_wrap(~size)+
#     geom_smooth(aes(x=N_log,y=val_log,group=size),col="black",method="lm")+
#     labs(x="log(N)",y="log(value)",title=magList[i],col="particle size")
# }
# for (i in 1:length(plotList)){
#   plotList[[i]]=plotList[[i]]+theme(plot.margin=margin(0,0,0,0),
#                                     legend.position="none")
#   if (i %in% c(1,2)){
#     plotList[[i]]=plotList[[i]]+theme(axis.title.x=element_blank())
#   }
#   if (i %in% c(2,4)){
#     plotList[[i]]=plotList[[i]]+theme(axis.title.y=element_blank())
#   }
# 
# }
# 
# ptotal=ggarrange(plotList[[1]],plotList[[2]],plotList[[3]],plotList[[4]],
#                  ncol=2,nrow=2,common.legend=F, align="hv")
# ptotal
# 
# workDir=getwd()
# setwd(paste0(workDir,"/R/figs"))
# ggsave(file="test.png",device="png",dpi=400,width=6,height=5)
