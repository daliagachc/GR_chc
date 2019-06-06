#imports CS file

workDir=getwd()
setwd(paste0(workDir,"/data/CS"))


import=fread("chacaltaya_CS.sum")

CS_data=import %>%
  mutate(localTime= as.POSIXct(V1*24*3600-24*3600,
                               origin='0000-01-01 00:00.00 UTC', tz="UTC"))
CS_data$localTime=force_tz(CS_data$localTime,"etc/GMT+4")


setwd(workDir)
# 
# ggplot(data=CS_data)+
#   geom_line(aes(x=UTCTime,y=V2))