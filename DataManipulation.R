#---------------------------------------------------------------------------------------------
##Salamander Data Wrangling - Leah Swartz 2018
#---------------------------------------------------------------------------------------------

##Goal - create dataframe of Hubbard Brook salamander mark recapture data 
##with all streams, all species, all years. Link to survey covariates for 
##all streams all years

##R Studio project - working directory = Hubbard Brook Leah

#---------------------------------------------------------------------------------------------
##To Do: 
#---------------------------------------------------------------------------------------------

#   1. Add survey covariates for all of Jon's data, link to 
#   2.
#   3.
#   4. 
#   5. 
#   6. 
#   7. 

#---------------------------------------------------------------------------------------------
##Load Packages
#---------------------------------------------------------------------------------------------

library(tidyverse) ## includes ggplot2, tibble, tidyr, readr, purr, dplyr, stringr, forcats

#---------------------------------------------------------------------------------------------
##Bring in data
#---------------------------------------------------------------------------------------------
AllStreams <- read_csv("All_Streams_Corr.csv")
AllStreams2018 <- read_csv("2018SalamanderCaptureDataFinal.csv")
##Deal with wierd formatting in ID columns
AllStreams2018$Last3Digits<-formatC(AllStreams2018$Last3Digits, width = 3, format = "d", flag = "0")
AllStreams2018$ID<- formatC(AllStreams2018$ID, width = 12, format = "s")
str(AllStreams)
str(AllStreams2018)
#---------------------------------------------------------------------------------------------
##Clean up 2012-2015 data
#---------------------------------------------------------------------------------------------

#Clean data = AllStreamsCorr
AllStreamsCorr <- AllStreams %>%
#get rid of log transformed variables
    select(-starts_with("ln"))%>%
#get rid of redundant morphology columns but keep raw long loc and corr long loc and raw lat loc
  ##rename raw long loc and corr long loc
    rename(LongLocRaw = Raw.LongLoc, CorrLongLoc = longloc, LatLocRaw=Raw.LatLoc)%>%
    select(-ends_with("Corr"))%>%
    select(-starts_with("Raw"))%>%
#get rid of reduntant numeric columns
    select(-streamcode,reachcode)%>%
#reorder columns in a way that makes sense
    select(id,O.N,Species,Sex,Photo,Year,Month,Day,surnum,Species,
           stage,stream,reach,meso,LatLocRaw,LongLocRaw,CorrLongLoc, subsize,Type,meta,
           svl,mass,hl,hw,tl,tw,tailrm,tailrg,hul,fel,Notes)%>%
#rename everything so column names are interpretable 
#All variables capitalized
    rename(ElastomerID=id,OldNew=O.N, SurNum=surnum,Stage=stage, Stream=stream, Reach=reach, 
           Meso=meso,LatLoc =LatLocRaw,RawLongLoc=LongLocRaw, SubSize=subsize, Metamorph=meta,
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
  mutate(RawLongLoc=replace(RawLongLoc, RawLongLoc=="XXX",NA))%>%
  mutate(RawLongLoc=replace(RawLongLoc, RawLongLoc=="",NA))%>%
  mutate(CorrLongLoc=replace(CorrLongLoc, CorrLongLoc=="XXX",NA))%>%
  mutate(CorrLongLoc=replace(CorrLongLoc, CorrLongLoc=="",NA))%>%
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
  mutate(Month=ifelse(Month=="Jun",6,
                        ifelse(Month=="Jul",7,
                               ifelse(Month=="Aug",8,
                                      ifelse(Month=="Sep",9,NA)))))%>%
  #rename(year=Year,day=Day)%>%
  mutate(Date=lubridate::make_date(Year,Month,Day))

#---------------------------------------------------------------------------------------------
##Clean up 2018 data
#---------------------------------------------------------------------------------------------
#AllStreams2018$Last3Digits<-formatC(AllStreams2018$Last3Digits, width = 3, format = "d", flag = "0")
#AllStreams2018$ID<- formatC(AllStreams2018$ID, width = 12, format = "s")


AllStreams2018Corr <- AllStreams2018 %>%
##Merge ID and Last3Digits
##first change NA to "" in one column
  replace_na(list(Last3Digits = ""))%>%
##Now unite columns while dropping old (remove=T)
  unite(PitTagID, ID, Last3Digits,sep = "", remove=T)%>%
  mutate(PitTagID=replace(PitTagID, PitTagID=="NA",NA))%>%
##add interreach distance to CorrLongLoc
  mutate(CorrLongLoc=ifelse(Stream=="Paradise" & "Reach"=="upper",LongLoc+750,
                           ifelse(Stream=="Bear"& "Reach"=="upper",LongLoc+900,
                                  ifelse(Stream=="ZigZag" & Reach=="upper",LongLoc+1000, LongLoc))))%>% 
##rename columns to match others
  rename(OldNew=ON,Day=day,LatLocRaw=LatLoc,RawLongLoc=LongLoc)%>%
##change survey number to match up with previous years - add 36 to everything
  mutate(SurNum=SurNum+36)%>%
##add primary column
  mutate(Primary=ifelse(SurNum==37|SurNum==38|SurNum==39,13,
                      ifelse(SurNum==40|SurNum==41|SurNum==42,14,
                             ifelse(SurNum==43|SurNum==44|SurNum==45,15,NA))))%>%
##FOR NOW: Filter out everything but tagged gyrinopholus
  filter(Species=="GP")%>%
  mutate(PitTagID=replace(PitTagID, PitTagID=="999002018052",NA))%>%
  drop_na(PitTagID)%>%
  #filter(%>%
  select(-c(X39,X40))


    
                                    

#write.csv(AllStreamsCorr,file="AllStreamsCorrClean.csv") 



  
#---------------------------------------------------------------------------------------------
##Merge 2018 data with previous years
#---------------------------------------------------------------------------------------------
AllStreamsAllYears<- bind_rows(mutate_all(AllStreamsCorr, as.character), mutate_all(AllStreams2018Corr, as.character))
#write.csv(AllStreamsAllYears,file="AllStreamsAllYears.csv") 

#---------------------------------------------------------------------------------------------
##Create data for Hugo
#---------------------------------------------------------------------------------------------
CaptureHistories <- AllStreamsAllYears %>% 
  #mutate(PitTagID=as.factor(PitTagID))%>% 
##Make new column for elastomerID_PittagID
  group_by(ElastomerID,PitTagID)%>%
  

  ## colapse so only one obs per primary period - if individual was recaptured 
  ## within the same primary period use 1st measurements
  distinct(ID,Primary,.keep_all=TRUE)%>%
  select(ID,Sex,Primary)%>%
  #mutate(Meso=ifelse(Meso=="RF",1,
  #   ifelse(Meso=="PL",2,0)))%>%
  ## spread by primary
  spread(Primary,Meso)
##replace NA with zero
CaptureHistories[is.na(CaptureHistories)] <- 0  





CaptureHistories <- as.data.frame(AllStreamsCorr) %>% 
  ## filter - 3 streams - zigzag, bear,paradise
  #filter(Stream=="Bear"|Stream=="Paradise"|Stream=="Zig Zag")%>%
  ## filter - downstream reaches
  #filter(Reach=="lower")%>%
  ## filter species = GP
  #filter(Species=="GP")%>%
  ## colapse so only one obs per primary period - if individual was recaptured 
  ## within the same primary period use 1st measurements
  distinct(ID,Primary,.keep_all=TRUE)%>%
  select(ID,Sex,Primary)%>%
  #mutate(Meso=ifelse(Meso=="RF",1,
                  #   ifelse(Meso=="PL",2,0)))%>%
  ## spread by primary
  spread(Primary,Meso)
##replace NA with zero
CaptureHistories[is.na(CaptureHistories)] <- 0  
  
##reproducible example
Data <- data.frame(
  PitTagID = as.character(sample(1:20,10)),
  ElastomerID = as.character(sample(30:50,10)),
  SurNum = as.character(sample(1:20,10)),
  Stage= as.factor(sample(c("Larvae", "Adult"), 10, replace = TRUE)), 
  Sex =as.factor(sample(c("Male", "Female"), 10, replace = TRUE)))  
  

