monthsel=c(2)

#npf event data
npf=NAIS_clasification_file%>%
  mutate(Date=as.POSIXct(Day,format="%d.%m.%Y"),
         YMD=format(Date, "%Y-%m-%d"),
         Day=NULL)
npf[is.na(npf)]=0

#subsetting 
npf=subset(npf,month(Date) %in% monthsel)
npf=subset(npf,classI == 1 )
npf=npf %>%
  select(YMD)

test=npf
exactDate="2018-02-20"

npf=subset(npf,YMD==exactDate)


#nais data
naisWD=NAIS_ion_df

#subsetting
naisWD=subset(naisWD,month(startTime) %in% monthsel)

naisWD=naisWD %>%
    mutate(year=year(startTime),
         month=month(startTime),
         day=day(startTime)) %>%
  mutate(starting_point=fastPOSIXct(paste0(year,"-",month,"-",day), tz="UTC"),
         startTimeUTC=startTime)
attr(naisWD$startTimeUTC, "tzone") <- "UTC"

naisWD=naisWD %>%
  mutate(timeSinceMidnight=(as.numeric(startTimeUTC)-as.numeric(starting_point))/3600)%>%
  mutate(timeSinceMidnightLocal=timeSinceMidnight-4) %>%
  mutate(timeCuts=cut(timeSinceMidnightLocal,breaks=seq(0,24,0.4),include.lowest=T),
         YMD=format(startTime,"%Y-%m-%d"))

#now merging the mask with the naisdata
naisWD=merge(naisWD,npf,by="YMD")

#grouping and summarising
naisWD.s=naisWD %>%
  dplyr::group_by(timeCuts,startSize,endSize,ion) %>%
  dplyr::summarise(value=median(value,na.rm=T)) %>%
  mutate(startTime=convertToLimit(timeCuts,0),
         endTime=convertToLimit(timeCuts,1)) %>%
  as.data.frame()

naisWD.s.spread=spread(naisWD.s,key="ion",value=value) %>%
  mutate(value=negative/positive)

# naisWD.s.spread=naisWD.s

#------------------------------
plotNAIS=function(data){
  out=ggplot(data =data, aes(x=startTime))+
    geom_rect(aes(xmin=startTime,xmax=endTime,ymin=startSize,ymax=endSize,fill=value))+
    scale_fill_gradientn(
                         limits = c(0,4),
                         colours=brewer.pal(3,"Greys"),oob=squish)+
    annotation_logticks(base=10,sides = "lr")+
    scale_y_continuous(trans="log10",breaks = c(1e-9,1e-8,1e-7),
                       limits=c(1e-9,0.7e-7))+
    theme(axis.title.x=element_blank(),
          plot.title=element_text(size=10),
          plot.margin=margin(0,0,0.1,0,unit="cm"))+
    scale_x_continuous(limits=c(0,23),breaks=seq(0,23,2))+
    labs(x="Hour of the day",y="Dp[m]",fill="ratio: negative/positive",
         title=paste0("NAIS CHC",exactDate))
        # title=paste0("NAIS CHC - ",paste0(monthsel,collapse=",")," - 2018"))

  return(out)
}

#nais=subset(naisWD.s,ion=="negative")
plotNAIS(naisWD.s.spread)
# #------------------------------
# #ptotal=list()
# ptotal[[monthsel[1]]]=plotNAIS(wd.s)
# 
# ggarrange(ptotal[[1]],ptotal[[4]],ptotal[[5]],nrow=3,align="v",
#           common.legend=T,legend="right")