p=list()
for (i in 1:length(NAIS_ion_dfs_13_17)){
  p[[i]]=plotNAIS_monthselect(NAIS_ion_dfs_13_17[[i]],c(3))
  p[[i]]=p[[i]]+theme(plot.title=element_blank())
  print(i)
}


ggarrange(p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],nrow=5,common.legend=T,legend="right",
          labels=c("2014","2015","2016","2017","2018"),label.x=0.85)