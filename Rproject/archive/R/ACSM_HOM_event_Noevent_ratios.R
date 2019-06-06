wd=NAIS_clasification_file

wd[is.na(wd)]=0
wd=wd %>%
  mutate(eventDay=ifelse(classI==1 | classII==1,
                         "event",ifelse(undefined==1,"undefined",
                                        ifelse(nodata==1,"noData","noEvent"))))%>%
  mutate(date=format(localTime,"%Y-%m-%d")) %>%
  select(date,eventDay)

wd2=combine_acsm_hom(acsm_data_wide,HOM_data,1) %>%
  dplyr::filter(hour(localTime) %in% seq(7,12,1))

wd2=wd2 %>%
  mutate(date=format(localTime, "%Y-%m-%d")) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(ratioOrgHom=mean(Organics/HOM,na.rm=T),
                   ratioNitHom=mean(Nitrate/HOM,na.rm=T))
  

wd3=merge(wd2,wd,by="date")

ggplot(data=wd3) +
  geom_boxplot(aes(x=eventDay,y=ratioNitHom),notch=F)+
  ylim(0,0.1)
# 
# dat <- ggplot_build(p)$data[[1]]
# 
# p + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
#                                y=middle, yend=middle), colour="red", size=1)
