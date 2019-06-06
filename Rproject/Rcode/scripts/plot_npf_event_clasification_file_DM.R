#plots npf event clasification file done by Diego Aliaga and Alkuin Koenig

#------------------------------DATA IMPORT------------------------------
npf_clas_DM=import_npf_event_clas_DM(paste(here::here(),"data/raw",sep="/"),
                                     "event_clasification_Diego_Max.csv")

#------------------------------DATA HANDLING------------------------------
wd=gather(npf_clas_DM,key="flag",value="value",2:8)
wd$value=as.numeric(wd$value)

wd=wd %>%
  dplyr::filter(flag != "special")

#------------------------------PLOTTING------------------------------

ggplot(data=wd,aes(x=dateUTC,y=flag,fill=factor(value)))+
  geom_raster()+
  scale_x_datetime(breaks = date_breaks('7 days'))+
  theme(axis.text.x=element_text(angle=90))+
  geom_hline(yintercept=1:length(unique(wd$flag))+0.5,col="darkgrey")+
  #geom_vline(xintercept=unique(wd$dateUTC)[1:length(unique(wd$dateUTC))],col="darkgrey")+
  scale_fill_manual(values=c("lightgrey","black"))+
  labs(x="date",y="",fill="flag",title="NPF event clasification")