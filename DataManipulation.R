#-----------------------------------------------------------------------------------------
##Load Packages
#-----------------------------------------------------------------------------------------
library(tidyverse)
#-----------------------------------------------------------------------------------------
##Bring in data
#-----------------------------------------------------------------------------------------
AllStreams <- read.csv("All_Streams_Corr.csv")
str(AllStreams)
#-----------------------------------------------------------------------------------------
##Clean up data
#-----------------------------------------------------------------------------------------
#Clean data = AllStreamsCorr
AllStreamsCorr <- AllStreams %>%
#get rid of log transformed variables
    select(-starts_with("ln"))%>%
#get rid of redundant morphology columns
    select(-ends_with("Corr"))%>%
    select(-starts_with("Raw"))%>%
#get rid of reduntant numeric columns
    select(-streamcode,reachcode)%>%
#reorder columns in a way that makes sense
    select(id,O.N,Species,Sex,Photo,Year,Month,Day,surnum,Species,
           stage,stream,reach,meso,latloc,longloc,subsize,Type,meta,
           svl,mass,hl,hw,tl,tw,tailrm,tailrg,hul,fel,Notes)%>%
#rename everything so column names are interpretable 
#All variables capitalized
    rename(ID=id,OldNew=O.N, SurNum=surnum,Stage=stage, Stream=stream, Reach=reach, 
           Meso=meso,LatLoc =latloc,LongLoc=longloc, SubSize=subsize, Metamorph=meta,
           PhotoSVL=svl,MassMg=mass,HeadLength=hl,HeadWidth=hw,TailLength=tl,TailWidth=tw,
           TailRemoved=tailrm,TailRegrown=tailrg,HumerousLength=hul,FemurLength=fel,
           SubType=Type)%>%
#fix sex column
    mutate(Sex=replace(Sex, Sex=="f?",NA))%>%
    mutate(Sex=replace(Sex, Sex=="F?",NA))%>%
    mutate(Sex=replace(Sex, Sex=="M?",NA))%>%
    mutate(Sex=replace(Sex, Sex=="",NA))%>%
    mutate(Sex=replace(Sex, Sex=="-",NA))%>%
#drop unused factor levels
    droplevels()%>%
#replace all XXX and blanks with NA
  mutate(Meso=replace(Meso, Meso=="XXX",NA))%>%
  mutate(Meso=replace(Meso, Meso=="",NA))%>%
  mutate(LatLoc=replace(LatLoc, LatLoc=="XXX",NA))%>%
  mutate(LatLoc=replace(LatLoc, LatLoc=="",NA))%>%
  mutate(LongLoc=replace(LongLoc, LongLoc=="XXX",NA))%>%
  mutate(LongLoc=replace(LongLoc, LongLoc=="",NA))%>%
  mutate(Photo=replace(Photo, Photo=="XXX",NA))%>%
  mutate(Photo=replace(Photo, Photo=="",NA))%>%
  mutate(PhotoSVL=replace(PhotoSVL, PhotoSVL=="XXX",NA))%>%
  mutate(PhotoSVL=replace(PhotoSVL, PhotoSVL=="",NA))%>%
  mutate(MassMg=replace(MassMg, MassMg=="XXX",NA))%>%
  mutate(MassMg=replace(MassMg, MassMg=="",NA))%>%
  mutate(HeadLength=replace(HeadLength, HeadLength=="XXX",NA))%>%
  mutate(HeadLength=replace(HeadLength, HeadLength=="",NA))%>%
  mutate(HeadWidth=replace(HeadWidth, HeadWidth=="XXX",NA))%>%
  mutate(HeadWidth=replace(HeadWidth, HeadWidth=="",NA))%>%
  mutate(TailLength=replace(TailLength, TailLength=="XXX",NA))%>%
  mutate(TailLength=replace(TailLength, TailLength=="",NA))%>%
  mutate(TailWidth=replace(TailWidth, TailWidth=="XXX",NA))%>%
  mutate(TailWidth=replace(TailWidth, TailWidth=="",NA))%>%
  mutate(TailRemoved=replace(TailRemoved, TailRemoved=="XXX",NA))%>%
  mutate(TailRemoved=replace(TailRemoved, TailRemoved=="",NA))%>%
  mutate(TailRegrown=replace(TailRegrown, TailRegrown=="XXX",NA))%>%
  mutate(TailRegrown=replace(TailRegrown, TailRegrown=="",NA))%>%
  mutate(HumerousLength=replace(HumerousLength, HumerousLength=="XXX",NA))%>%
  mutate(HumerousLength=replace(HumerousLength, HumerousLength=="",NA))%>%
  mutate(FemurLength=replace(FemurLength, FemurLength=="XXX",NA))%>%
  mutate(FemurLength=replace(FemurLength, FemurLength=="",NA))
  

  
  
  
  
  
  
  
   recode(AllStreamsCorr$FemurLength,XXX=NA)
  
    lapply(FUN = function(x) recode(x, "XXX"=NA))
#replace all blanks with NA

#fix data types
    mutate_at(vars(PhotoSVL:FemurLength),funs(character)


:FemurLength)
    mutate_at(PhotoSVL:FemurLength=as.numeric(PhotoSVL:FemurLength))
  



    mutate(replace(AllStreamsCorr,Sex==f?,NA))  



  lapply(AllStreamsCorr$Sex, recode(AllStreamsCorr$Sex,f?=NA))
#Replace XXX with NA\
    lapply(FUN = function(x) recode(x, XXX=NA))
  
  
str(AllStreamsCorr)
summary(AllStreamsCorr$Sex)
AllStreamsCorr$Photo

