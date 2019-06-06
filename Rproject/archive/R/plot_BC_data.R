library(openair)

workDir=getwd()
setwd(paste0(workDir,"/data"))

BCdata=fread("BC_monthly.csv")

setwd(workDir)

workData=na.omit(BCdata)

i=1
#workData=subset(workData,Month==i)
#pollutionRose(workData,pollutant="BC(log)",paddle=F)

polarPlot(workData,x="WS",wd="WD",pollutant="BC_log",statistic="median",units="m/s",
          main = paste0("BC at CHC: ",i,"-2018"), key.footer = "670nm absorption coeff.[1/Mm]", key.header = "log(median)",
          key.position = "bottom",resolution="normal", limits=c(0,2),
          cols=rev(brewer.pal(5,"RdYlBu")))
# 
# pollutionRose(mydata=workData,x="WS",wd="WD",pollutant="abs670")
# 
# pollutionRose(mydata=workData,x="WS",wd="WD",pollutant="abs670")
