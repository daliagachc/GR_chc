combine_acsm_SA_HOM_CS_kappa=function(){#combines into long format what is mentioned
  wd=acsm_data_long%>%
    mutate(YMD=format(localTime, "%Y-%m-%d")) %>%
    select(localTime,magnitude,value,YMD)
  
  
  wd2 =HOM_data %>%
    mutate(YMD=format(localTime, "%Y-%m-%d")) %>%
    mutate(magnitude="HOM_API",
           value=HOM_FORM_APITOF) %>%
    select(localTime,magnitude,value,YMD)
  
  wd3=SA_data %>%
    rename("localTime"=Time)%>%
    mutate(YMD=format(localTime,"%Y-%m-%d")) %>%
    mutate(magnitude="SA_API",
           value=SA)%>%
    select(localTime,magnitude,value,YMD)
  
  wd4=CS_kappa_data %>%
    mutate(YMD=format(localTime,"%Y-%m-%d")) %>%
    mutate(magnitude="CS_kappa",
           value=V2)%>%
    select(localTime,magnitude,value,YMD)
  
  
  wd5=rbind(wd,wd2)
  wd5=rbind(wd5,wd3)
  wd5=rbind(wd5,wd4)
  
  #putting the magnitudes into neat order and make the labels pretty
  wd5$magnitude=factor(wd5$magnitude, 
                         levels = c("Ammonium","Chloride", "Nitrate","Organics","Sulfate",
                                    "SA_API","HOM_API","CS_kappa"))
  levels(wd5$magnitude)=c("Ammonium\n(ACSM)","Chloride\n(ACSM)", "Nitrate\n(ACSM)",
                            "Organics\n(ACSM)","Sulfate\n(ACSM)",
                            "SA\n(APITOF)","HOM\n(APITOF)","CS\n(SMPS)")
  
  
  return(wd5)
}
