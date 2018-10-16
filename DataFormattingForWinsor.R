library(tidyverse)


AllStreamsCorr <- read.csv("AllStreamsCorrClean.csv")
CaptureHistories <- as.data.frame(AllStreamsCorr) %>% 
 ## filter - 3 streams - zigzag, bear,paradise
  filter(Stream=="Bear"|Stream=="Paradise"|Stream=="Zig Zag")%>%
 ## filter - downstream reaches
  filter(Reach=="lower")%>%
 ## filter species = GP
  filter(Species=="GP")%>%
 ## colapse so only one obs per primary period - if individual was recaptured 
 ## within the same primary period use 1st measurements
  distinct(ID,Primary,.keep_all=TRUE)%>%
  select(ID,Meso,Primary)%>%
  mutate(Meso=ifelse(Meso=="RF",1,
                        ifelse(Meso=="PL",2,0)))%>%
   ## spread by primary
  spread(Primary,Meso)
##replace NA with zero
CaptureHistories[is.na(CaptureHistories)] <- 0

##pull out covariates from initial capture

IndCovsInitial <- as.data.frame(AllStreamsCorr) %>% 
  ## filter - 3 streams - zigzag, bear,paradise
  filter(Stream=="Bear"|Stream=="Paradise"|Stream=="Zig Zag")%>%
  ## filter - downstream reaches
  filter(Reach=="lower")%>%
  ## filter species = GP
  filter(Species=="GP")%>%
  ## colapse so only one obs per primary period - if individual was recaptured 
  ## within the same primary period use 1st measurements
  distinct(ID,Primary,.keep_all=TRUE)%>%
  filter(OldNew=="N")%>%
  ## select relavent columns 
  select("ID","Date", "Stage","Stream","LatLoc","LongLoc","SubSize","SubType","PhotoSVL","MassMg","HeadLength",
         "HeadWidth","TrunkLength","TrunkWidth","TailRemoved","TailRegrown","HumerousLength","FemurLength")%>%
  setNames(paste0('Initial.', names(.)))%>%
  rename("ID"="Initial.ID")


#####pull out covariates from final capture

IndCovsFinal <- as.data.frame(AllStreamsCorr) %>% 
  ## filter - 3 streams - zigzag, bear,paradise
    filter(Stream=="Bear"|Stream=="Paradise"|Stream=="Zig Zag")%>%
  ## filter - downstream reaches
    filter(Reach=="lower")%>%
  ## filter species = GP
    filter(Species=="GP")%>%
  ## colapse so only one obs per primary period - if individual was recaptured 
  ## within the same primary period use 1st measurements
    distinct(ID,Primary,.keep_all=TRUE)%>%
    filter(OldNew =="O")%>%
    group_by(ID)%>%
    filter(Primary == max(Primary))%>%
  ## select relavent columns 
    select("ID","Date", "Stage","Stream","LatLoc","LongLoc","SubSize","SubType","PhotoSVL","MassMg","HeadLength",
           "HeadWidth","TrunkLength","TrunkWidth","TailRemoved","TailRegrown","HumerousLength","FemurLength")%>%
    setNames(paste0('Final.', names(.)))%>%
    rename("ID"="Final.ID")

###bind it all together
ForWinsor <- left_join(CaptureHistories,IndCovsInitial,by="ID")%>%
  left_join(IndCovsFinal,by="ID")#%>%
  ##calculate time between intial and final capture
  #mutate("time.interval" = "Initial.Date" %--% "Final.Date")
  #mutate(MonthsBetweenInitAndFinalCapture = interval(ymd(Initial.Date),ymd(Final.Date)))
  
   
write.csv(ForWinsor,file="DataFormattedForWinsor.csv") 
  
  
  
  