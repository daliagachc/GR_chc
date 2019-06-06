import_GR_mode_data=function(directory,writeFile){
  #preloads all the relevant information for GR (basically the size of the mode for event days)
  #and saves on a file.
  #directory is the path to the data-containing folder.
  #writeFile is a boolean indicating if a processed data
  #file should be created(same directory)

  workDir=getwd()
  
  setwd(directory)
  
  #creating a list pointing to all files with extension .dat
  temp = list.files(pattern="*.dat")
  
  #imported.frame=data.frame(Time=import)
  pos.list=list()
  neg.list=list()
  
  #I now import all positive and negative events. EventIDs are given, separating between
  #negative and positive ion events. Multiple growing "events" a day are flagged.
  poseventcount=1
  negeventcount=1
  for (i in 1:length(temp)){
    import=readMat(temp[i])
    
    posevents=length(import[[1]][[1]])
    
    for (j in 1:posevents){
      help=import[[1]][[1]][[j]][[1]]
      help.frame=data.frame(matLabTime=help[,1],
                            localTime=as.POSIXct(help[,1]*24*3600-24*3600,origin='0000-01-01 00:00.00 UTC', tz="etc/GMT+4"),
                            UTCTime=as.POSIXct(help[,1]*24*3600-24*3600,origin='0000-01-01 00:00.00 UTC', tz="UTC"),
                            dp=help[,2],
                            uncertainties = help[,3],
                            flag=help[,4],
                            dailyevent=j,
                            eventID=paste0("Pos",poseventcount))
      if(j==1){pos.helpframe=help.frame}
      else{pos.helpframe=rbind(pos.helpframe,help.frame)}
      poseventcount = poseventcount+1
    }
    
    negevents=length(import[[1]][[2]])
    for (j in 1:negevents){
      help=import[[1]][[2]][[j]][[1]]
      help.frame=data.frame(matLabTime=help[,1],
                            localTime=as.POSIXct(help[,1]*24*3600-24*3600,origin='0000-01-01 00:00.00 UTC', tz="etc/GMT+4"),
                            UTCTime=as.POSIXct(help[,1]*24*3600-24*3600,origin='0000-01-01 00:00.00 UTC', tz="UTC"),
                            dp=help[,2],
                            uncertainties = help[,3],
                            flag=help[,4],
                            dailyevent=j,
                            eventID=paste0("Neg",negeventcount))
      if(j==1){neg.helpframe=help.frame}
      else{neg.helpframe=rbind(neg.helpframe,help.frame)}
      negeventcount=negeventcount+1
    }
    
    pos.list[[i]]=pos.helpframe
    neg.list[[i]]=neg.helpframe
  }
  
  pos.frame=do.call("rbind", pos.list)
  neg.frame=do.call("rbind",neg.list)
  
  pos.frame$ion = "positive"
  neg.frame$ion= "negative"
  npfevent_size_frame=rbind(pos.frame,neg.frame)
  
  #selecting only good events
  npfevent_size_frame=merge(npfevent_size_frame, eventIDclas,by="eventID")
  
  flag_interrupted=function(flagvec){
    firstind=first(which(flagvec==0))
      if (length(firstind)>=1){
        for (i in firstind:length(flagvec)){
          flagvec[i]=0
        }
      }
    return(flagvec)
  }
  
  #creating a new "continued" flag. If 
  #no new mode information for an hour or more, all the following
  #modes will be flagged with 1
  npfevent_size_frame=npfevent_size_frame %>%
    arrange(eventID,localTime) %>%
    dplyr::group_by(eventID)%>%
    mutate(timedif=localTime-lag(localTime,1),
           continued=ifelse(is.na(timedif) | timedif <= 3600,1,0 ))%>%
    mutate(continued=flag_interrupted(continued)) %>%
    as.data.frame()
  
  #apply the correction
  npfevent_size_frame=npfevent_size_frame%>%
    mutate(dp_uncorrected=dp,
           dp=correctDP(dp))
  
  
  
  if(writeFile){
    write.csv(npfevent_size_frame, "npf_event_size_frame.csv")
  }
  
  
  setwd(workDir)
  
  return(npfevent_size_frame)
}#import_GR_mode_data

