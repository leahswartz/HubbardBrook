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
           PhotoSVL=svl,MassMg=mass,HeadLength=hl,HeadWidth=hw,TrunkLength=tl,TrunkWidth=tw,
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
  mutate(SurNum=replace(SurNum, SurNum=="XXX",NA))%>%
  mutate(SurNum=replace(SurNum, SurNum=="",NA))%>%
  mutate(Meso=replace(Meso, Meso=="XXX",NA))%>%
  mutate(Meso=replace(Meso, Meso=="",NA))%>%
  mutate(SubSize=replace(SubSize, SubSize=="XXX",NA))%>%
  mutate(SubSize=replace(SubSize, SubSize=="",NA))%>%
  mutate(SubType=replace(SubType, SubType=="XXX",NA))%>%
  mutate(SubType=replace(SubType, SubType=="",NA))%>%
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
  mutate(TrunkLength=replace(TrunkLength, TrunkLength=="XXX",NA))%>%
  mutate(TrunkLength=replace(TrunkLength, TrunkLength=="",NA))%>%
  mutate(TrunkWidth=replace(TrunkWidth, TrunkWidth=="XXX",NA))%>%
  mutate(TrunkWidth=replace(TrunkWidth, TrunkWidth=="",NA))%>%
  mutate(TailRemoved=replace(TailRemoved, TailRemoved=="XXX",NA))%>%
  mutate(TailRemoved=replace(TailRemoved, TailRemoved=="",NA))%>%
  mutate(TailRegrown=replace(TailRegrown, TailRegrown=="XXX",NA))%>%
  mutate(TailRegrown=replace(TailRegrown, TailRegrown=="",NA))%>%
  mutate(HumerousLength=replace(HumerousLength, HumerousLength=="XXX",NA))%>%
  mutate(HumerousLength=replace(HumerousLength, HumerousLength=="",NA))%>%
  mutate(FemurLength=replace(FemurLength, FemurLength=="XXX",NA))%>%
  mutate(FemurLength=replace(FemurLength, FemurLength=="",NA))%>%
  ##add column for primary occasion
  mutate(Primary=ifelse(SurNum==1|SurNum==2|SurNum==3,1,
                   ifelse(SurNum==4|SurNum==5|SurNum==6,2,
                     ifelse(SurNum==7|SurNum==8|SurNum==9,3,
                       ifelse(SurNum==10|SurNum==11|SurNum==12,4,
                         ifelse(SurNum==13|SurNum==14|SurNum==15,5,
                           ifelse(SurNum==16|SurNum==17|SurNum==18,6,
                             ifelse(SurNum==19|SurNum==20|SurNum==21,7,
                               ifelse(SurNum==22|SurNum==23|SurNum==24,8,
                                 ifelse(SurNum==25|SurNum==26|SurNum==27,9,
                                   ifelse(SurNum==28|SurNum==29,10,
                                     ifelse(SurNum==30|SurNum==31|SurNum==32,11,
                                       ifelse(SurNum==33|SurNum==34|SurNum==35|SurNum==36,12,NA)))))))))))))%>%
  ##add date column - first change months to numeric
  mutate(month=ifelse(Month=="Jun",6,
                        ifelse(Month=="Jul",7,
                               ifelse(Month=="Aug",8,
                                      ifelse(Month=="Sep",9,NA)))))%>%
  rename(year=Year,day=Day)%>%
  mutate(Date=make_date(year,month,day))


write.csv(AllStreamsCorr,file="AllStreamsCorrClean.csv") 
  
  
  
  
  
  

