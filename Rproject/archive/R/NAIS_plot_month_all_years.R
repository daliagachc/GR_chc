#plot month of interest for all years

selmonth=5


worklist=NAIS_ion_dfs_13_17_norm
normalized=T

#directory to save to (relative to main)
saveto="/R/figs/NAIS_months_norm"


#-------------
yearvec=c("2013","2014","2015","2016","2017","2018")


p=list()
for (i in 1:length(worklist)){
  help=subset(worklist[[i]],month(startTime)==selmonth)
  if (nrow(help)>1){
    wd=help
    if(!normalized){
      p[[i]]=plotNAIS_hourlyBinned(wd)+
        labs(title=paste0(yearvec[i],"-",selmonth," in CHC"))
    } else{
      p[[i]]=plotNAIS_hourlyBinned_norm(wd)+
        labs(title=paste0(yearvec[i],"-",selmonth," in CHC normalized"))
    }
  }
  print(paste0(i,"-done"))
  
}#for i

workDir=getwd()
setwd(paste0(workDir,saveto))
for (i in 1:length(p)){
  ggsave(plot=p[[i]],dpi=300,filename=paste0(yearvec[i],"-",i,".png"),device="png")
}
setwd(workDir)