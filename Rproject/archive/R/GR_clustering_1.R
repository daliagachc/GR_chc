#GR_clustering
minR=0.8

wd=event_GR_wide %>%
  mutate(R_00_03=NULL,GR_00_03=NULL,GR_total=NULL,R_total=NULL)

wd=wd %>%
  mutate(GR_03_07=ifelse(R_03_07>=minR,GR_03_07,NA),
         GR_07_20=ifelse(R_07_20>=minR,GR_07_20,NA),
         GR_20_80=ifelse(R_20_80>=minR,GR_20_80,NA),
         special=NULL) 

wd=na.omit(wd)

mask=npfevent_size_frame %>%
  select(ion,eventID) %>%
  filter(ion=="negative")
mask=unique(mask)

wd=merge(wd,mask,by="eventID")

wd=subset(wd,ion="negative") %>%
  select(eventID,GR_03_07, GR_07_20,GR_20_80)


#creating the normalization factor for every day
wd_normfac=wd %>%
  dplyr::group_by(eventID) %>%
  dplyr::summarise(normfac=sqrt(GR_03_07^2+GR_07_20^2+GR_20_80^2))


#combining the two and creating a normalized column
wd_merged=merge(wd,wd_normfac, by="eventID")

wd_merged$GR_03_07=wd_merged$GR_03_07/wd_merged$normfac
wd_merged$GR_07_20=wd_merged$GR_07_20/wd_merged$normfac
wd_merged$GR_20_80=wd_merged$GR_20_80/wd_merged$normfac


toCluster=wd_merged[,1:4]

#plotting the "cluster cuantity selection" plot
wssplot(toCluster[-1])

set.seed(1234)
nc <- NbClust(toCluster[-1], min.nc=2, max.nc=7, method="kmeans")
table(nc$Best.n[1,])

#doing the cluster fit
set.seed(1234)
fit.km <- kmeans(toCluster[-1], 2, nstart=25)                           #3
fit.km$size

fit.km$centers


#which days are assigned to which cluster
df_clustered=data.frame(eventID=wd$eventID, clus=fit.km$cluster)




#plotting cluster centers
test=aggregate(na.omit(df_wide[-1]), by=list(cluster=fit.km$cluster), mean)
test_long=gather(data=test, key=hour,value=value, as.character(0:23))
test_long$hour=as.numeric(test_long$hour)
test_long$cluster=as.factor(test_long$cluster)





# 
#creating the normalization factor for every day
wd_normfac=wd %>%
  dplyr::group_by(eventID) %>%
  dplyr::summarise(normfac=sqrt(GR_3_7^2+GR_7_20^2))


#combining the two and creating a normalized column
wd_merged=merge(wd,wd_normfac, by="eventID")

workData_merged$TGM_norm=workData_merged$TGM/workData_merged$normfac

