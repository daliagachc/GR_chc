plotNAIS_hourlyBinned_norm=function(data,ions){
  
  convertToLimit=function(x,i){#gets the limit value (i=0, lower..i=1, upper) for an
    #interval created with the cut function.
    if (i==0){#get lower limit
      help=gsub('\\(','',x)
      help=gsub(',.*',"",help)
    } else if (i==1){#get upper limit
      help=gsub('.*,','',x)
      help=gsub('\\]','',help)
    } else{return(-999.9)}#return far away value as error code
    return(as.numeric(help))
  }
  
  wd=data
  
  if (ions %in% c("positive","negative")){
    wd=subset(wd,ion==ions)
    
  }

  #------------------------------
  plotNAIS=function(data){
    out=ggplot(data =data, aes(x=startTime))+
      geom_rect(aes(xmin=startTime,xmax=endTime,ymin=startSize,ymax=endSize,fill=value))+
      scale_fill_gradientn(colours=rev(brewer.pal(5,"RdYlBu")),limits=c(0,2),oob=squish)+
      annotation_logticks(base=10,sides = "lr")+
      scale_y_continuous(trans="log10",breaks = c(1e-9,1e-8,1e-7),
                         limits=c(1e-9,0.7e-7))+
      theme(axis.title.x=element_blank(),
            plot.title=element_text(size=10),
            plot.margin=margin(0,0,0.1,0,unit="cm"))+
      scale_x_continuous(limits=c(0,23),breaks=seq(0,23,2))+
      labs(x="Hour of the day",y="Dp[m]",fill="Normalized\nscale")
    return(out)
  }
  
  
  
  return(plotNAIS(wd))
}
# #------------------------------
# #ptotal=list()
# ptotal[[monthsel[1]]]=plotNAIS(wd.s)
# 
# ggarrange(ptotal[[1]],ptotal[[4]],ptotal[[5]],nrow=3,align="v",
#           common.legend=T,legend="right")