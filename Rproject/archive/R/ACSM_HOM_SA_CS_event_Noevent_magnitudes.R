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


wd4=SA_data %>%
  rename("localTime"=Time)%>%
  mutate(date=format(localTime,"%Y-%m-%d")) %>%
  mutate(date=as.POSIXct(date,tz="etc/GMT+4"))%>%
  mutate(magnitude="SA_API",
         value=SA)%>%
  select(localTime,magnitude,value,date)

wd5=CS_kappa_data %>%
  mutate(date=format(localTime,"%Y-%m-%d")) %>%
  mutate(date=as.POSIXct(date,tz="etc/GMT+4"))%>%
  mutate(magnitude="CS_kappa",
         value=V2)%>%
  select(localTime,magnitude,value,date)



wd6=rbind(wd2,wd3)
wd6=rbind(wd6,wd4)
wd6=rbind(wd6,wd5)
wd6=merge(wd6,wd,by="date")


# wd3=wd3%>%
#   dplyr::group_by(magnitude,eventDay) %>%
#   dplyr::summarise(value=median(value,na.rm=T))
normfacs=wd6 %>%
  dplyr::group_by(magnitude) %>%
  dplyr::summarise(normfac=sqrt(sum(value^2,na.rm=T)),
                   median_normfac=median(value/normfac,na.rm=T))

wd7=merge(wd6,normfacs,by="magnitude") %>%
  mutate(value_norm=(value/normfac)/median_normfac)

wd7$magnitude=as.factor(wd7$magnitude)
# wd6$magnitude=relevel(wd6$magnitude, "Nitrate")
# wd6$magnitude=relevel(wd6$magnitude, "Organics")
# wd6$magnitude=relevel(wd6$magnitude, "Sulfate")
#

#--------------------------here a product

ACSM_HOM_SA_combined=wd7

#----------------------------------



wd7=subset(wd7,hour(localTime) %in% seq(7,9,1))




test=subset(wd7,eventDay != "noData" )
test=subset(test,not(magnitude %in% c("Ammonium","Nitrate","Organics","Chloride")))


p=ggplot(data=test)+
  geom_boxplot(aes(x=1,y=value_norm,fill=eventDay),varwidth=F,notch=F,
               outlier.shape=NA,width=1.0)+
  facet_wrap(~magnitude,ncol=5)+
  scale_fill_manual(values=c(colors()[315],"white","lightgrey"))+
  scale_y_continuous(limits=c(0,5))+
  labs(y="normalized value",fill="")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p

ggsave(plot=last_plot(),device="png",filename="CS-HOM-SA_box.png",dpi=300)

