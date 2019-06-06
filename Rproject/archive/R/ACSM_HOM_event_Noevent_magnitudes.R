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

wd6$magnitude=as.factor(wd6$magnitude)
wd6$magnitude=relevel(wd6$magnitude, "Nitrate")
wd6$magnitude=relevel(wd6$magnitude, "Organics")
wd6$magnitude=relevel(wd6$magnitude, "Sulfate")

test=subset(wd6,eventDay != "noData" )
test=subset(test,magnitude != "Chloride")


p=ggplot(data=test)+
  geom_boxplot(aes(x=1,y=value_norm,fill=eventDay),varwidth=T,notch=F,
               outlier.shape=NA,width=1.0)+
  facet_wrap(~magnitude,ncol=5)+
  scale_fill_manual(values=c(colors()[315],"white","lightgrey"))+
  scale_y_continuous(limits=c(0,1))+
  labs(y="value/median",fill="")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p
# 
# dat <- ggplot_build(p)$data[[1]]
# 
# p + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
#                                y=middle, yend=middle), colour="red", size=1)
