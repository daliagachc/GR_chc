plotNAIS=function(data){
  out=ggplot(data =data, aes(x=startTime))+
    geom_rect(aes(xmin=startTime,xmax=endTime,ymin=startSize,ymax=endSize,fill=value))+
    scale_fill_gradientn(trans = "log10",
                         limits = c(1,0.2*10e4),
                         colours=rev(brewer.pal(5,"RdYlBu")))+
    # annotation_logticks(base=10,sides = "lr")+
    scale_y_continuous(trans="log10",breaks = c(1e-9,1e-8,1e-7),
                       limits=c(1e-9,1e-7))+
    scale_x_datetime(date_breaks = "1 day",
                     labels = date_format("%d", tz="etc/GMT+4"),
                     position = "top",
                     expand = c(0.01,0.01))+
    theme(axis.title.x=element_blank())+
    labs(fill="dN/d(logDp)[cm^-3]",y="Dp[m]",col="")+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12))
  
  return(out)
}


plotNAIS_nds=function(data){
  out=ggplot(data =data, aes(x=startTime))+
    geom_rect(aes(xmin=startTime,xmax=endTime,ymin=startSize,ymax=endSize,fill=value))+
    scale_fill_gradientn(trans = "log10",
                         colours=rev(brewer.pal(5,"RdYlBu")))+
    # annotation_logticks(base=10,sides = "lr")+
    scale_y_continuous(trans="log10")+
    scale_x_datetime(date_breaks = "1 hour",
                     labels = date_format("%H", tz="etc/GMT+4"),
                     position = "top",
                     expand = c(0.01,0.01))+
    theme(axis.title.x=element_blank())+
    labs(fill="dN/d(logDp)[cm^-3]",y="Dp[m]",col="")+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12))
  
  return(out)
}


normalizeIt=function(vec,how){
  if (how=="norma"){
    normfac=sqrt(sum(vec^2,na.rm=T))
  }
  if (how=="max"){
    normfac=max(vec,na.rm=T)
  }
  if (how=="median"){
    normfac=median(vec,na.rm=T)
  }
  if (how=="mean"){
    normfac=mean(vec,na.rm=T)
  }
  return(vec/normfac)
}


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


combine_acsm_hom=function(acsmdata,homdata, averagetime){
  wd1=acsm_data_wide %>%
    mutate(YMDH=format(localTime,"%Y-%m-%d %H"),
           localTime=NULL)
  
  wd2=HOM_data %>%
    mutate(YMDH=format(localTime,"%Y-%m-%d %H"),
           localTime=NULL)
  
  wd3=merge(wd1,wd2,by="YMDH") %>%
    dplyr::group_by(YMDH) %>%
    dplyr::summarise(Organics=median(Organics,na.rm=T),
                     Sulfate=median(Sulfate,na.rm=T),
                     Ammonium=median(Ammonium,na.rm=T),
                     Chloride=median(Chloride,na.rm=T),
                     Nitrate=median(Nitrate,na.rm=T),
                     HOM=median(HOM_FORM_APITOF,na.rm=T)) %>%
    mutate(localTime=as.POSIXct(YMDH, format="%Y-%m-%d %H",tz="etc/GMT+4"))
  
  return(wd3)
}

correctDP = function(dp){
  return(10^(log10(dp)*(1/1.073)))
}


getCharge_eventID = function(eventID){
  charge=gsub("Neg.*","Neg",eventID)
  charge=gsub("Pos.*","Pos",charge)
  charge[charge=="Pos"]="positive"
  charge[charge=="Neg"]="negative"
  return(charge)
}

getInd_eventID = function(eventID){
  charge=gsub("Neg","",eventID)
  charge=gsub("Pos","",charge)
  return(charge)
}


lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2),
                        b = format(coef(m)[2], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

#mad-distance calculation function. Double sided or single sided
get_MAD_distance=function(vec,how){
  x=na.omit(vec)
  m=median(x)
  abs.dev=abs(x-m)
  
  if (how == "single"){
    mad = median(abs.dev)
    mad_dist=abs.dev/mad
  }else  if (how =="double"){
    mad.left=median(abs.dev[x<=m])
    mad.right=median(abs.dev[x>=m])
    mad_dist=ifelse(x<=m, abs.dev/mad.left, abs.dev/mad.right)
  }
  else {mad_dist=NA}
  return(mad_dist)
}#get_MAD_distance
