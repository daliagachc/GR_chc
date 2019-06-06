workingdir=getwd()

setwd("C:/fastFiles/springCourse/MatlabData")

SO2_data=fread("smeardata_20170601120000_SO2_16.csv")
part_data=fread("smeardata_20170601120000_allPart.csv")

setwd(workingdir)

SO2_data_cycle=SO2_data %>%
  dplyr::group_by(Hour) %>%
  dplyr::summarize(mean=mean(HYY_META.SO2168,na.rm=T))

part_data_cycle=part_data %>%
  dplyr::group_by(Hour) %>%
  dplyr::summarize(mean=mean(HYY_DMPS.tconc,na.rm=T))

ggplot()+
  geom_line(data=part_data_cycle, aes(x=Hour,y=mean))