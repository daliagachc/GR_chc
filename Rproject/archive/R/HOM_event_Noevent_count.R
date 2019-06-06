wd=NAIS_clasification_file

wd[is.na(wd)]=0
wd=wd %>%
  mutate(eventDay=ifelse(classI==1 | classII==1,
                         "event",ifelse(undefined==1,"undefined",
                                        ifelse(nodata==1,"noData","noEvent"))))%>%
  mutate(date=localTime) %>%
  select(date,eventDay)

wd2=acsm_data_long %>%
  mutate(date=format(localTime, "%Y-%m-%d")) %>%
  mutate(date=as.POSIXct(date,tz="etc/GMT+4")) %>%
  select(localTime,magnitude,value,date)

wd3=HOM_data %>%
  mutate(date=format(localTime, "%Y-%m-%d")) %>%
  mutate(date=as.POSIXct(date,tz="etc/GMT+4"))%>%
  mutate(magnitude="HOM_API",
         value=HOM_FORM_APITOF) %>%
  select(localTime,magnitude,value,date)

wd4=rbind(wd2,wd3)
wd5=merge(wd4,wd,by="date")

wd5=subset(wd5,hour(localTime) %in% seq(7,12,1))

#wd5=subset(wd5,eventDay=="event" & magnitude=="HOM_API")
wd5=subset(wd5,magnitude %in% c("HOM_API"))
wd5=subset(wd5,eventDay %in% c("event","noEvent"))
wd5$eventDay=factor(wd5$eventDay)
levels(wd5$eventDay)=c("event day","no event day")

ggplot(data=wd5,aes(x=(value),y=..count../sum(..count..),fill=eventDay))+
  geom_histogram(color="black")+
  facet_wrap(~eventDay)+
  scale_fill_manual(values=c(colors()[315],"white"))+
  theme(legend.position="none")+
  labs(title="HOM",
       y="normalized count",
       x="HOM relative signal")
# +
#   annotate(x=40,y=40,label="measurements between 07:00 and 12:59",geom="text",
#            size=5)

ggarrange(plot1,plot2,ncol=2)