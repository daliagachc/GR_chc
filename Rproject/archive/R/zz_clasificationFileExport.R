wd=NAIS_clasification_file
wd[is.na(wd)]=0 
wd=wd%>%
  mutate(Day=NULL) %>%
  rename("Date"=localTime)

workDir=getwd()
setwd(paste0(workDir,"/data/processed"))

write.csv(wd,"CHC_eventDays_Diego_Max.csv")

setwd(workDir)