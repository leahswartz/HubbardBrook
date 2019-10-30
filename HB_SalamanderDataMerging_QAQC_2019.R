#---------------------------------------------------------------------------------------------
##Salamander Data Wrangling - Leah Swartz 2019
#---------------------------------------------------------------------------------------------

##Goal - create dataframe of Hubbard Brook salamander mark recapture data 
##with all streams, all species, all years. Link to survey covariates for 
##all streams all years

##R Studio project - working directory = Hubbard Brook Leah

#---------------------------------------------------------------------------------------------
##To Do: 
#---------------------------------------------------------------------------------------------

#   1. Enter survey covariates for all of Jon's data 
#   2. Create CSV with all survey covariates all years
#   3. Merge 2019 data
#   4. Fix 2018 CorrLongLoc
#   5. Genomics Project - List of salamanders to sequence. 
#
#
#   6. 
#   7. 

#---------------------------------------------------------------------------------------------
##Set Working Directory
#---------------------------------------------------------------------------------------------
setwd("C:/Users/Leah Swartz/Box Sync/Box Sync/HubbardBrookLeah/CMRWorkingDirectory")
#---------------------------------------------------------------------------------------------
##Load Packages
#---------------------------------------------------------------------------------------------
library(tidyverse) ## includes ggplot2, tibble, tidyr, readr, purr, dplyr, stringr, forcats
#---------------------------------------------------------------------------------------------
##Bring in data
#---------------------------------------------------------------------------------------------
##2012 - 2015 data
AllStreams2012_2015 <- read_csv("All_Streams_Corr.csv")
##2018 data
AllStreams2018 <- read_csv("2018SalamanderCaptureDataFinal_IDFixed.csv")
##2019 data
AllStreams2019 <- read_csv("2019_salamander_CMR_data_withmeasurements_IDFixed.csv")
##Already genotyped individuals
AlreadyGenotyped <- read.csv("AlreadyGenotypedGP.csv")
##Look at structure
str(AllStreams2012_2015)
str(AllStreams2018)
str(AllStreams2019)
#---------------------------------------------------------------------------------------------
##Clean up 2012-2015 data
#---------------------------------------------------------------------------------------------
#Clean data = AllStreamsCorr
AllStreamsCorr2012_2015 <- AllStreams2012_2015 %>%
  #get rid of log transformed variables
  select(-starts_with("ln"))%>%
  #get rid of redundant morphology columns but keep raw long loc and corr long loc and raw lat loc
  ##rename raw long loc and corr long loc
  rename(LongLocRaw = 'Raw LongLoc', CorrLongLoc = longloc, LatLocRaw='Raw LatLoc')%>%
  select(-ends_with("Corr"))%>%
  select(-starts_with("Raw"))%>%
  #get rid of reduntant numeric columns
  select(-streamcode,reachcode)%>%
  #reorder columns in a way that makes sense
  select(id,'O/N',Species,Sex,Photo,Year,Month,Day,surnum,Species,
         stage,stream,reach,meso,LatLocRaw,LongLocRaw,CorrLongLoc, subsize,Type,meta,
         svl,mass,hl,hw,tl,tw,tailrm,tailrg,hul,fel,Notes)%>%
  #rename everything so column names are interpretable 
  #All variables capitalized
  rename(ElastomerID=id,OldNew='O/N', SurNum=surnum,Stage=stage, Stream=stream, Reach=reach, 
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
AllStreams2018Corr <- AllStreams2018 %>%
  ##add interreach distance to CorrLongLoc
  mutate(CorrLongLoc=ifelse(Stream=="Paradise" & Reach=="upper",LongLoc+750,
                            ifelse(Stream=="Bear"& Reach=="upper",LongLoc+900,
                                   ifelse(Stream=="ZigZag" & Reach=="upper",LongLoc+1000, LongLoc))))%>% 
  ##rename columns to match others
  rename(OldNew=ON,LatLocRaw=LatLoc,RawLongLoc=LongLoc)%>%
  ##change survey number to match up with previous years - add 36 to everything
  mutate(SurNum=SurNum+36)%>%
  ##change mass from g to mg
  mutate(MassMg=MassMg*1000)%>%
  ##add primary column
  mutate(Primary=ifelse(SurNum==37|SurNum==38|SurNum==39,13,
                        ifelse(SurNum==40|SurNum==41|SurNum==42,14,
                               ifelse(SurNum==43|SurNum==44|SurNum==45,15,NA))))%>%
  ##FOR NOW: Filter out everything but tagged gyrinopholus
  filter(Species=="GP")%>%
  mutate(PitTagID=replace(PitTagID, PitTagID=="999002018052",NA))%>%
  drop_na(PitTagID)#%>%

#write.csv(AllStreamsCorr,file="AllStreamsCorrClean.csv") 

#---------------------------------------------------------------------------------------------
##Clean up 2019 data
#---------------------------------------------------------------------------------------------
AllStreams2019Corr <- AllStreams2019  
##add interreach distance to CorrLongLoc
mutate(CorrLongLoc=ifelse(Stream=="Paradise" & Reach=="upper",LongLoc+750,
                          ifelse(Stream=="Bear"& Reach=="upper",LongLoc+900,
                                 ifelse(Stream=="ZigZag" & Reach=="upper",LongLoc+1000, LongLoc))))%>%
  ##rename columns to match others
  rename(OldNew=ON,LatLocRaw=LatLoc,RawLongLoc=LongLoc)%>%
  ##change survey number to match up with previous years - add 36 to everything
  mutate(SurNum=SurNum+45)%>%
  ##change mass from g to mg
  mutate(MassMg=MassMg*1000)%>%
  ##add primary column
  mutate(Primary=ifelse(SurNum==46|SurNum==47|SurNum==48,16,
                        ifelse(SurNum==49|SurNum==50|SurNum==51,17,
                               ifelse(SurNum==52|SurNum==53|SurNum==54,18,NA))))%>%
  ##FOR NOW: Filter out everything but tagged gyrinopholus
  filter(Species=="GP")%>%
  mutate(PitTagID=replace(PitTagID, PitTagID=="999002018052",NA))%>%
  drop_na(PitTagID)#%>%

#write.csv(AllStreamsCorr,file="AllStreamsCorrClean.csv") 
#---------------------------------------------------------------------------------------------
##Merge 2018 data with previous years
#---------------------------------------------------------------------------------------------
AllStreamsAllYears<- bind_rows(mutate_all(AllStreamsCorr, as.character), mutate_all(AllStreams2018Corr, as.character))%>%
  #  mutate(FINALID=ifelse(!is.na(ElastomerID),ElastomerID,PitTagID))
  #select(ElastomerID,PitTagID,FINALID)
  
  
  #write.csv(AllStreamsAllYears,file="AllStreamsAllYears.csv") 
  
  
  
  
  
  















  #---------------------------------------------------------------------------------------------
##Create data for Mary
#---------------------------------------------------------------------------------------------
DataFormattedForMary <- read.csv("AllStreamsAllYears.csv") %>%
  mutate(Date=lubridate::make_date(Year,Month,Day))%>%
  select(ElastomerID,PITTagID=PitTagID,Date,SurNumber=SurNum,PrimaryNumber=Primary,OldNew,Species,Sex,Stage,Stream,Reach,Meso,LatLoc,LongLoc=CorrLongLoc,
         SubSize,SubType,MassMg,TailRemoved,TailRegrown,SVL=PhotoSVL,HeadLength,HeadWidth,TrunkLength,TrunkWidth,HumerousLength,
         FemurLength,Notes)%>%
  mutate(PITTagID=as.factor(PITTagID))%>%
  mutate(Stream=recode(Stream,"Zig Zag" = "ZigZag"))%>%
  mutate(Meso=recode(Meso,"Pl"="PL"))%>%
  mutate(Meso=recode(Meso, "Rf"="RF"))%>%
  mutate(Meso=recode(Meso,"Rn"="RN"))%>%
  mutate(LatLoc=recode(Meso,"WI"="WE"))

write.table(DataFormattedForMary,file="AllStreamsAllYearsDataFormattedForMary.txt",sep="," ,row.names = F)

SurveyCovsFormattedForMary <- read.csv("2018SurveyCovs.csv")%>%
  mutate(Date=lubridate::make_date(Year,Month,Day))
#write.csv(SurveyCovsFormattedForMary,file="SurveyCovsFormattedForMary.csv")

#---------------------------------------------------------------------------------------------
##Create data for Hugo
#---------------------------------------------------------------------------------------------
##first make new column for elastomerID_PittagID and add back into full dataset
#DoubleTagged <- AllStreamsAllYears %>% 
#   filter(!is.na(PitTagID))%>%
#  filter(!is.na(ElastomerID))%>%
# unite(ElastomerID_PitTagID,ElastomerID,PitTagID,sep="_",remove=F)%>%
#select(ElastomerID_PitTagID,ElastomerID,PitTagID,Day,Month,Year,SurNum)


##merge into full dataset
CaptureHistories <- AllStreamsAllYears %>% 
  filter(Stream=="Bear"|Stream=="Paradise"|Stream=="Zig Zag")%>%
  #left_join(DoubleTagged,by=NULL)%>%
  ##make final ID Colum
  #mutate(FinalID = ifelse(!is.na(ElastomerID_PitTagID),ElastomerID_PitTagID,
  # ifelse(!is.na(PitTagID) & is.na(ElastomerID_PitTagID),PitTagID,
  # ifelse(!is.na(ElastomerID)& is.na(ElastomerID_PitTagID), ElastomerID,NA))))%>%
  ## colapse so only one obs per year - if individual was recaptured 
  distinct(FINALID,Year,.keep_all=TRUE)%>%
  select(FINALID,Stage,Year)%>%
  ## spread by year
  spread(Year,Stage)%>%
  ##replace NA with zero
  replace(is.na(.), 0)

##add sex, svl from initial capture
IndCovsInitial <- AllStreamsAllYears %>% 
  filter(Stream=="Bear"|Stream=="Paradise"|Stream=="Zig Zag")%>%
  #left_join(DoubleTagged,by=NULL)%>%
  ##make final ID Colum
  #mutate(FinalID = ifelse(!is.na(ElastomerID_PitTagID),ElastomerID_PitTagID,
  # ifelse(!is.na(PitTagID) & is.na(ElastomerID_PitTagID),PitTagID,
  #ifelse(!is.na(ElastomerID)& is.na(ElastomerID_PitTagID), ElastomerID,NA))))%>%
  ## colapse so only one obs per year - if individual was recaptured 
  distinct(FINALID,Year,.keep_all=TRUE)%>%
  filter(OldNew=="N")%>%
  ## select relavent columns 
  select("FINALID","SurNum", "Sex","Stream","Reach","PhotoSVL","MassMg")%>%
  setNames(paste0('Initial.', names(.)))%>%
  rename(FINALID=Initial.FINALID)


#####pull out covariates from final capture

IndCovsFinal <- AllStreamsAllYears %>% 
  filter(Stream=="Bear"|Stream=="Paradise"|Stream=="Zig Zag")%>%
  ## filter - downstream reaches
  #filter(Reach=="lower")%>%
  ## filter species = GP
  #filter(Species=="GP")%>%
  ## colapse so only one obs per primary period - if individual was recaptured 
  ## within the same primary period use 1st measurements
  distinct(FINALID,Primary,.keep_all=TRUE)%>%
  filter(OldNew =="O")%>%
  group_by(FINALID)%>%
  filter(Primary == max(Primary))%>%
  ## select relavent columns 
  select("FINALID","SurNum", "Sex","Stream","Reach","PhotoSVL","MassMg")%>%
  setNames(paste0('Final.', names(.)))%>%
  rename("FINALID"="Final.FINALID")



DataFormattedForHugo <- left_join(CaptureHistories,IndCovsInitial,by="FINALID")%>%
  left_join(IndCovsFinal,by="FINALID")%>%
  filter(FINALID!="          NA NA")

write.csv(DataFormattedForHugo,file="DataFormattedForHugo.csv") 


#---------------------------------------------------------------------------------------------
##WORKING Create data for Hugo
#---------------------------------------------------------------------------------------------
##first make new column for elastomerID_PittagID and add back into full dataset
DoubleTagged <- AllStreamsAllYears %>% 
  filter(!is.na(PitTagID))%>%
  filter(!is.na(ElastomerID))%>%
  unite(ElastomerID_PitTagID,ElastomerID,PitTagID,sep="_",remove=F)%>%
  select(ElastomerID_PitTagID,ElastomerID,PitTagID,Day,Month,Year,SurNum)


##merge into full dataset
CaptureHistoriesElastomer <- AllStreamsAllYears %>% 
  filter(Stream=="Bear"|Stream=="Paradise"|Stream=="Zig Zag")%>%
  #left_join(DoubleTagged,by=NULL)%>%
  ##make final ID Colum
  #mutate(FinalID = ifelse(!is.na(ElastomerID_PitTagID),ElastomerID_PitTagID,
  #    ifelse(!is.na(PitTagID) & is.na(ElastomerID_PitTagID),PitTagID,
  #ifelse(!is.na(ElastomerID)& is.na(ElastomerID_PitTagID), ElastomerID,NA))))%>%
  ## colapse so only one obs per year - if individual was recaptured 
  distinct(FinalID,Year,.keep_all=TRUE)%>%
  select(FinalID,Stage,Year)%>%
  ## spread by year
  spread(Year,Stage)%>%
  ##replace NA with zero
  replace(is.na(.), 0)

##add sex, svl from initial capture
IndCovsInitial <- AllStreamsAllYears %>% 
  filter(Stream=="Bear"|Stream=="Paradise"|Stream=="Zig Zag")%>%
  left_join(DoubleTagged,by=NULL)%>%
  ##make final ID Colum
  mutate(FinalID = ifelse(!is.na(ElastomerID_PitTagID),ElastomerID_PitTagID,
                          ifelse(!is.na(PitTagID) & is.na(ElastomerID_PitTagID),PitTagID,
                                 ifelse(!is.na(ElastomerID)& is.na(ElastomerID_PitTagID), ElastomerID,NA))))%>%
  ## colapse so only one obs per year - if individual was recaptured 
  distinct(FinalID,Year,.keep_all=TRUE)%>%
  filter(OldNew=="N")%>%
  ## select relavent columns 
  select("FinalID","SurNum", "Sex","Stream","Reach","PhotoSVL","MassMg")%>%
  setNames(paste0('Initial.', names(.)))%>%
  rename(FinalID=Initial.FinalID)


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



ForHugo <- left_join(CaptureHistories,IndCovsInitial,by="ID")







