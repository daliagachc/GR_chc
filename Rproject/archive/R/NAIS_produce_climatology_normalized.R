#plot "climatology" by month for 2013-2018

worklist=NAIS_ion_dfs_13_17_norm

# #directory to save to (relative to main)
# saveto="/R/figs/NAIS_climatology"

NAIS_climatology=list()
for(j in 1:12){
  selmonth=j
  wd=list()
  for (i in 1:length(worklist)){
    help=subset(worklist[[i]],month(startTime)==selmonth)
    if (nrow(help)>1){
      wd[[i]]=help
    }
  }
  NAIS_climatology[[j]]=bin_NAIS_hourly(rbindlist(wd, use.names=T, fill=T, idcol=NULL))
  
  print(paste0(j,"-done"))
}
