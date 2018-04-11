#-----------------------------------------------------------------------------------------
##Load Packages
#-----------------------------------------------------------------------------------------
library(tidyverse)
#-----------------------------------------------------------------------------------------
##Bring in data
#-----------------------------------------------------------------------------------------
AllStreams <- read.csv("HB_AllStreamsCorr.csv")
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
           svl,mass,hl,hw,tl,tw,tailrm,tailrg,hul,fel,morphpc1,morphpc2,Notes)%>%
#rename everything so column names are interpretable 
#All variables capitalized
    rename(ID=id,OldNew=O.N, SurNum=surnum,Stage=stage, Stream=stream, Reach=reach, 
           Meso=meso,LatLoc =latloc,LongLoc=longloc, SubSize=subsize, Metamorph=meta,
           SVL=svl,MassMg=mass,HeadLength=hl,HeadWidth=hw,TailLength=tl,TailWidth=tw,
           TailRemoved=tailrm,TailRegrown=tailrg,HumerousLength=hul,FemurLength=fel,
           MorphPC1=morphpc1,MorphPC2=morphpc2,SubType=Type)%>%
#Replace XXX with NA\
    lapply(FUN = function(x) recode(x, XXX=NA))
  
  
str(AllStreamsCorr)
  
