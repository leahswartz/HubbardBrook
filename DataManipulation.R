#--------------------------------------------------------------------------------------
##Load Packages
#--------------------------------------------------------------------------------------
library(tidyverse)
#--------------------------------------------------------------------------------------
##Bring in data
#--------------------------------------------------------------------------------------
AllStreams <- read.csv("HB_AllStreamsCorr.csv")
str(AllStreams)
#--------------------------------------------------------------------------------------
##Get rid of redundant columns
#--------------------------------------------------------------------------------------
#get rid of log transformed variables
AllStreamsCorr <- select(AllStreams, -starts_with("ln"))%>%
  #get rid of redundant morphology columns
  select(-ends_with("Corr"))%>%
  select(-starts_with("Raw"))%>%
  select(-streamcode,reachcode)%>%
  select(id,O.N,Sex,Photo,Year,Month,Day,Species,stage,stream,reach,meso,latloc,longloc,subsize)
