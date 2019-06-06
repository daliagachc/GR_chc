

NAIS_ion_dfs_13_17_norm=list()
for(i in 1:length(NAIS_ion_dfs_13_17)){
  wd=list()
  for (j in 1:12){
    wd[[j]]=subset(NAIS_ion_dfs_13_17[[i]],month(startTime)==j)
    wd.neg=subset(wd[[j]],ion=="negative")
    wd.pos=subset(wd[[j]],ion=="positive")
    
    normfac_neg=sqrt(sum(wd.neg$value^2))
    normfac_pos=sqrt(sum(wd.pos$value^2))
    wd.neg$value=wd.neg$value/normfac_neg
    wd.pos$value=wd.pos$value/normfac_pos
    
    wd[[j]]=rbind(wd.neg,wd.pos)
  }
  NAIS_ion_dfs_13_17_norm[[i]]=rbindlist(wd, use.names=T, fill=T, idcol=NULL)
  print(paste0(i,"-done"))
}