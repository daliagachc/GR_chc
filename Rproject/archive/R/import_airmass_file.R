#imports the cluster file

library(data.table)

workDir=getwd()

setwd(paste0(workDir,"/data"))

airmass_file=fread("Cluster_inf.csv") %>%
  dplyr::rename("C6" = 7,
                "C1"=Amazon_north,
                "C2"=Amazon_south,
                "C3"=10,
                "C5"=Altiplano) %>%
  mutate(Start_time=as.POSIXct(Start_time,format="%d/%m/%Y %H:%M"),
         End_time=as.POSIXct(End_time,format="%d/%m/%Y %H:%M"),
         YMD=format(Start_time, "%Y-%m-%d"))

setwd(workDir)

