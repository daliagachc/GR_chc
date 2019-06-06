#creates all NAIS3 plots in a certain time interval as a batch

library(cowplot)
library(lubridate)
library(ggplot2)

init_date=as.POSIXct("2018-05-16",format="%Y-%m-%d",tz="UTC")
end_date=as.POSIXct("2018-06-30",format="%Y-%m-%d",tz="UTC")

#the directory of where the two scripts "plot_NAIS3_ion_data.R" and
#plot_NAIS3_particle_data.R are found.
#backslash (\) has to be replaced with slash (/)
scriptDir = "D:/Files/universidad/atmosfera/campaignAerosoles/NAIS3/Rcode"

#the directory where the images get saved to.
figureDir="D:/Files/universidad/atmosfera/campaignAerosoles/NAIS3_plots"

#the directory where the data is found
nais3DataDir = "D:/Files/universidad/atmosfera/campaignAerosoles/NAIS3/data/level0"

date_act=init_date
while (date_act < end_date){
  date_char=as.character(date_act)
  if ((file_test("-f",paste0(nais3DataDir,"/",date_char,".ions.nds")))
    & (file_test("-f",paste0(nais3DataDir,"/",date_char,".particles.nds")))){
    date=date_act
    source(paste(scriptDir,"plot_NAIS3_all.R",sep="/"))
  }
  date_act=date_act+24*3600
}
