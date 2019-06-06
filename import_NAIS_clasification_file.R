#imports clasification file

workDir=getwd()
setwd(paste0(workDir,"/data"))

import=fread("event_clasification_Diego_Max.csv")

NAIS_clasification_file=import %>%
  mutate(localTime= as.POSIXct(Day,format="%d.%m.%Y",tz="etc/GMT+4"))

setwd(workDir)