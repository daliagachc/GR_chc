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
# wd3=wd3%>%
#   dplyr::group_by(magnitude,eventDay) %>%
#   dplyr::summarise(value=median(value,na.rm=T))
normfacs=wd5 %>%
  dplyr::group_by(magnitude) %>%
  dplyr::summarise(normfac=median(value,na.rm=T))

wd6=merge(wd5,normfacs,by="magnitude") %>%
  mutate(value_norm=value/normfac)

test=subset(wd6,eventDay != "noData" )
test=subset(test,magnitude != "Chloride")

ggplot(data=test)+
  geom_boxplot(aes(x=magnitude,y=value_norm,col=eventDay),varwidth=F,notch=F)+
  scale_y_continuous(limits=c(0,1))+
  labs(y="value/median")
