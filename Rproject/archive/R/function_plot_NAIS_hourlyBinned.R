plotNAIS_hourlyBinned=function(data){
  
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

  wd=wd %>%
    mutate(year=year(startTime),
           month=month(startTime),
           day=day(startTime)) %>%
    mutate(starting_point=fastPOSIXct(paste0(year,"-",month,"-",day), tz="UTC")) %>%
    mutate(starting_point=force_tz(starting_point,"etc/GMT+4"))
  wd$startTimeLocal=wd$startTime
  wd$startTimeLocal=force_tz(wd$startTimeLocal,tzone="etc/GMT+4")
  
  wd=wd %>%
    mutate(timeSinceMidnight=(as.numeric(startTimeLocal)-as.numeric(starting_point))/3600)%>%
    mutate(timeSinceMidnightLocal=timeSinceMidnight)
  
  wd = wd %>%
    mutate(timeCuts=cut(timeSinceMidnightLocal,breaks=seq(0,24,0.2),include.lowest=T))
  

  wd.s=wd %>%
    dplyr::group_by(timeCuts,startSize,endSize,ion) %>%
    dplyr::summarise(value=median(value,na.rm=T)) %>%
    mutate(startTime=convertToLimit(timeCuts,0),
           endTime=convertToLimit(timeCuts,1)) %>%
    as.data.frame()
  
  wd.s=subset(wd.s,ion=="negative")
  #
  #------------------------------
  plotNAIS=function(data){
    out=ggplot(data =data, aes(x=startTime))+
      geom_rect(aes(xmin=startTime,xmax=endTime,ymin=startSize,ymax=endSize,fill=value))+
      scale_fill_gradientn(trans = "log10",
                           limits = c(1,max(wd2$value)),
                           colours=rev(brewer.pal(5,"RdYlBu")),oob=squish)+
      annotation_logticks(base=10,sides = "lr")+
      scale_y_continuous(trans="log10",breaks = c(1e-9,1e-8,1e-7),
                         limits=c(1e-9,0.7e-7))+
      theme(axis.title.x=element_blank(),
            plot.title=element_text(size=10),
            plot.margin=margin(0,0,0.1,0,unit="cm"))+
      scale_x_continuous(limits=c(0,23),breaks=seq(0,23,2))+
      labs(x="Hour of the day",y="Dp[m]",fill="dN/d(logDp)\n[cm^-3]")
    return(out)
  }
  return(plotNAIS(wd.s))
}
# #------------------------------
# #ptotal=list()
# ptotal[[monthsel[1]]]=plotNAIS(wd.s)
# 
# ggarrange(ptotal[[1]],ptotal[[4]],ptotal[[5]],nrow=3,align="v",
#           common.legend=T,legend="right")