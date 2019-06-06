minR=0.75

#monthly boxplot
wd1=event_GR_long

wd2=npfevent_size_frame %>%
  select(eventID,posixTime) %>%
  dplyr::group_by(eventID) %>%
  dplyr::summarise(month=factor(month(min(posixTime))))

wd3=merge(wd1,wd2)
wd3=subset(wd3, size %in% c("03_07","07_20","20_80"))

wd3=subset(wd3, R >= minR)
ptitle=paste0("Growth rates by month, R^2 >=",minR)

ggplot(data=wd3, aes(x=month,y=GR))+
  facet_wrap(~size)+
  geom_boxplot(varwidth=T,outlier.shape=NA)+
  ylim(c(0,20))+
  labs(title=ptitle,
       y="GR [ng/h]")