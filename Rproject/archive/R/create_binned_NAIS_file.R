#SCATTERPLOT

wd=NAIS_ion_df %>%
  select(startTime,endTime,value,startSize,endSize,ion)%>%
  mutate(nm0nm3=ifelse(endSize <= 3e-9,value,-99999),
         nm3nm7=ifelse(startSize >=3e-9 & endSize <7e-9,value,-99999),
         nm7nm20=ifelse(startSize >= 7e-9 & endSize < 20e-9,value,-99999),
         nm20nm70=ifelse(startSize >= 20e-9 & endSize <=70e-9,value,-99999))
wd[wd==-99999]=NA

wd=gather(wd,key="size",value="value",7:10) %>%
  select(endTime,ion,size,value)

#wd=subset(wd,endTime <= as.POSIXct("2017/12/12"))

NAIS_ion_df_binned_5m=wd %>%
  mutate(minute=minute(endTime)) %>%
  mutate(min_group=((minute %/%5)*5)) %>%
  dplyr::group_by(YMDH= paste(year(endTime),month(endTime),
                                   day(endTime),hour(endTime),min_group,
                                   sep="-"),size,ion) %>%
  dplyr::summarise(value=median(value,na.rm=T))%>%
  as.data.frame() %>%
  mutate(localTime=as.POSIXct(YMDH,format="%Y-%m-%d-%H-%M",tz="etc/GMT+4"))%>%
  mutate(YMDH=NULL)

workDir=getwd()
setwd(paste0(workDir,"/data/processed"))
write.csv(wd2,"NAIS_CHC_ions_binned_5m.csv")
setwd(workDir)