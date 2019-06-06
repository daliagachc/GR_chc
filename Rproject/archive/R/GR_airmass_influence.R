thresh=50
minR=0.75

getDominant=function(C1,C2,C3,C5,C6,namevec,thresh){
  who=ifelse(C1 >= thresh, "C1", 
             ifelse(C2>=thresh,"C2",
                    ifelse(C3>=thresh,"C3",
                           ifelse(C5>=thresh,"C5",
                                  ifelse(C6>=thresh,"C6","no dominant")))))
  return(who)
}



#growth rate vs airmass influence

wd1=airmass_file %>%
  mutate(dominant=getDominant(C1,C2,C3,C5,C6,c("C1","C2","C3","C5","C6"),thresh))

wd2 = npfevent_size_frame%>%
  select(posixTime,eventID) %>%
  mutate(YMD=format(posixTime, "%Y-%m-%d")) %>%
  select(YMD, eventID)
wd2=unique(wd2)

wd3=event_GR_long 

wd4=merge(wd1,wd2, by="YMD")
wd5=merge(wd4,wd3, by="eventID") %>%
  mutate(d_std=NULL)
wd5=unique(wd5)

wd5=subset(wd5,R >= minR)
wd5=subset(wd5,score10 >= 5)
wd5=subset(wd5,airChange == 0)
#wd5=subset(wd5,size != "total")
wd5=subset(wd5,dominant != "no dominant")

# wd5$C1_cut=cut(wd5$C1,breaks=c(0,20,40,60,80,100),include.lowest=T)
# wd5$C5_cut=cut(wd5$C5,breaks=c(0,20,40,60,80,100),include.lowest=T)
# wd5$C3_cut=cut(wd5$C3,breaks=c(0,20,40,60,80,100),include.lowest=T)
# 

ptitle=paste0("GR - dominant airmass: >=",thresh, "%")

ggplot(data=wd5,aes(x=size,y=GR,col=dominant))+
  geom_boxplot(outlier.shape=NA,notch=F,varwidth=T,width=1.2)+
  ylim(c(0,20))+
  labs(title=ptitle)