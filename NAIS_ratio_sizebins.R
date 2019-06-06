
wd1=NAIS_ion_df_binned_5m
wd1=spread(wd1,key="size",value="value") %>%
  mutate(nm3nm7_all_ratio=nm3nm7/(nm0nm3+nm3nm7+nm7nm20+nm20nm70),
         nm0nm3_all_ratio=nm0nm3/(nm0nm3+nm3nm7+nm7nm20+nm20nm70),
         nm7nm20_all_ratio=nm7nm20/(nm0nm3+nm3nm7+nm7nm20+nm20nm70))

wd1=subset(wd1,localTime >= as.POSIXct("2018/04/06") & localTime <=as.POSIXct("2018/04/18"))

ggplot(data=wd1)+
  #geom_line(aes(x=localTime,y=nm0nm3_all_ratio))+
  #geom_line(aes(x=localTime,y=nm3nm7_all_ratio),col="red")+
  geom_line(aes(x=localTime,y=nm7nm20_all_ratio),col="green")+
  scale_y_continuous(limits=c(0,1))+
  scale_x_datetime(date_breaks = "1 day",
                   labels = date_format("%d", tz="etc/GMT+4"),
                   position = "bottom",
                   expand = c(0.01,0.01))



wd2=acsm_data_long_5m

