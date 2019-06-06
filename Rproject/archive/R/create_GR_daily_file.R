wd=event_GR_long
wd=subset(wd, score10 >= 5)
wd=subset(wd, size == "03_07")
wd=subset(wd, R >= 0.7)

wd2=npfevent_size_frame[,c("UTCTime","eventID")] %>%
  mutate(Date=format(UTCTime, "%Y-%m-%d")) %>%
  mutate(UTCTime=NULL)
wd2=unique(wd2)

wd3=merge(wd,wd2,by="eventID")

wd4=wd3 %>%
  dplyr::group_by(Date) %>%
  dplyr::summarise(GR_03_07 = median(GR,na.rm=T),
                   IDs = paste0(eventID,collapse=","))

workDir=getwd()
setwd(paste0(workDir,"/processedData"))

write.csv(wd4, file="CHC_GR_days.csv")