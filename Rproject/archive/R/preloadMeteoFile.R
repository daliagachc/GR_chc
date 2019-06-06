workDir=getwd()

setwd(paste0(workDir,"/Data"))

meteo_file=fread("meteo_cumbre.csv") %>%
  mutate(WD=as.numeric(WD),
         WS=as.numeric(WS),
         SWd=as.numeric(SWd),
         SWu=as.numeric(SWu),
         Tair = as.numeric(Tair),
         Time=as.POSIXct(date_time, tz="UTC"))

setwd(workDir)
