#plot "climatology" by month for 2013-2018


worklist=NAIS_ion_dfs_13_17
normalized=F

#directory to save to (relative to main)
saveto="/R/figs/NAIS_climatology"

p=list()
for(j in 1:12){
  selmonth=j
  wd=list()
  for (i in 1:length(worklist)){
    help=subset(worklist[[i]],month(startTime)==selmonth)
    if (nrow(help)>1){
      wd[[i]]=help
    }
  }
  wd=rbindlist(wd, use.names=T, fill=T, idcol=NULL)
  
  if(!normalized){
    p[[j]]=plotNAIS_hourlyBinned(wd)+
      labs(title=paste0(selmonth," in CHC (2014-2018)"))
  } else{
    p[[j]]=plotNAIS_hourlyBinned_norm(wd)+
      labs(title=paste0(selmonth," in CHC (2014-2018)"))
  }
  print(paste0(j,"-done"))
}

workDir=getwd()
setwd(paste0(workDir,saveto))
for (i in 1:length(p)){
  ggsave(plot=p[[i]],dpi=300,filename=paste0(i,".png"),device="png")
}
setwd(workDir)