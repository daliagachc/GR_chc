selmonth=5
i=6

wd1=subset(NAIS_ion_dfs_13_17_norm[[i]],month(startTime)==selmonth)
wd2=subset(NAIS_ion_dfs_13_17_norm[[i-1]],month(startTime)==selmonth)

wd3=NAIS_get_binned_dif(wd1,wd2,"negative")

years=c("2013","2014","2015","2016","2017","2018")

plotNAIS=function(data){
  out=ggplot(data =data, aes(x=startTime))+
    geom_rect(aes(xmin=startTime,xmax=endTime,ymin=startSize,ymax=endSize,fill=value))+
    scale_fill_gradientn(colours=rev(brewer.pal(5,"RdYlBu")),oob=squish,
                         limits=c(-0.001,0.001))+
    annotation_logticks(base=10,sides = "lr")+
    scale_y_continuous(trans="log10",breaks = c(1e-9,1e-8,4e-8),
                       limits=c(1e-9,4e-8))+
    theme(plot.title=element_text(size=10),
          plot.margin=margin(0,0,0.1,0,unit="cm"))+
    scale_x_continuous(limits=c(0,23),breaks=seq(0,23,2))+
    labs(x="Hour of the day",y="Dp[m]",fill="Diff/n(normalized)",x="Hour of the day")
  return(out)
}

p=plotNAIS(wd3)+labs(title=paste0(selmonth,".",years[i]," - ",selmonth,".",years[i-1]))
p



