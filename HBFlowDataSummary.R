#---------------------------------------------------------------------------------------------

##Merrill Brook 20 years MS - Flow data
##Leah Swartz Feb. 2019

#---------------------------------------------------------------------------------------------
##Set Working Directory
#---------------------------------------------------------------------------------------------

setwd("C:/Users/Leah Swartz/Box Sync/Box Sync/HubbardBrookLeah")

#---------------------------------------------------------------------------------------------
##Load Packages
#---------------------------------------------------------------------------------------------
library(dataRetrieval)
library(plyr)
library(tidyverse) ## includes ggplot2, tibble, tidyr, readr, purr, dplyr, stringr, forcats
library(lubridate) ## package to make dealing with dates and times easier
library(ggpmisc)
library(hydroTSM)
library(raster)
library(gridExtra)
#---------------------------------------------------------------------------------------------
##Extra functions
#---------------------------------------------------------------------------------------------
#' Richards-Baker (Flashiness) Index [RBI] calculator
#'
#' Input is a vector of mean daily flows.  Output is a value that is the RB Flashiness Index
#'
#' The index is valid over the time period of the data.
#' For example, if the vector contains a month's or a year's data the index will represent that time period.
#' The index will be in the same units as the input data.
#' The function assumes all days are represented (insert NA for missing values).
#' Baker, D.B., R.P. Richards, T.T. Loftus, and J.W. Kramer.  2004.  A New Flashiness Index:
#' Characteristics and Applications to Midwestern Rivers and Streams.  April 2004.
#' Journal of the American Water Resources Association (JAWRA).  Pages 503:522.
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Erik.Leppo@tetratech.com (EWL)
# 20171117
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @param Q Vector of mean daily stream flows.
#' @return Returns a value that represents the RBI.
#' @keywords RBI, Richards Baker Flashiness Index
#' @examples
#' # Get Gage Data via the dataRetrieval package from USGS 01187300 2013
#' data.gage <- dataRetrieval::readNWISdv("03238500", "00060", "1974-10-01", "1975-09-30")
#' head(data.gage)
#' # flow data
#' data.Q <- data.gage[,4]
#' # remove zeros
#' data.Q[data.Q==0] <- NA
#' RBIcalc(data.Q)
#'
#' # QC with document Baker et al., 2004
#' # Table 1, Whiteoak Creek near Georgetown, Ohio (03238500)
#' # Figure 8, upward pointing triangle for 1975 water year value close to "1.0".
#' # Using the data downloaded in journal example calculated RBI values in Excel and R match at 0.9833356.
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
RBIcalc <- function(Q){##FUNCTION.RBIcalc.START
  #
  time.start <- proc.time();
  #
  # Size
  myLen <- length(Q)
  # Add previous record in second column
  Qprev <- c(NA,Q[-myLen])
  # Create dataframe.
  myData <- as.data.frame(cbind(Q,Qprev))
  # delta (absolute)
  myData[,"AbsDelta"] <- abs(myData[,"Q"] - myData[,"Qprev"])
  # SumQ
  SumQ <- sum(myData[,"Q"],na.rm=TRUE)
  # Sum Delta
  SumDelta <- sum(myData[,"AbsDelta"], na.rm=TRUE)
  #
  RBIsum <- SumDelta / SumQ
  #
  time.elaps <- proc.time()-time.start
  # cat(c("Rarify of samples complete. \n Number of samples = ",nsamp,"\n"))
  # cat(c(" Execution time (sec) = ", elaps[1]))
  # flush.console()
  #
  # Return RBI value for data submitted.
  return(RBIsum)
  #
} ##FUNCTION.RBIcalc.END

##Publication Quality Plots Function

theme_Publication <- function(base_size=14, base_family="arial") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}
#---------------------------------------------------------------------------------------------
##Bring in data - W3 = Paradise; W6 = Bear; W7 = Zig Zag; W8=Canyon  
##The Dead Diamond River is the closest USGS gauge to Merrill Brook 
#---------------------------------------------------------------------------------------------

##Hubbard Brook

W3_1957_2012 <- read.csv("w3_stmflow_1957-2012.txt")
W3_2013_2015 <- read.csv("w3_stmflow_2013-2015_5min.txt")
W6_1963_2012 <- read.csv("w6_stmflow_1963-2012.txt")
W6_2013_2015 <- read.csv("w6_stmflow_2013-2015_5min.txt")
W7_1965_2012 <- read.csv("w7_stmflow_1965-2012.txt")
W7_2013_2015 <- read.csv("w7_stmflow_2013-2015_5min.txt")
W8_1965_2012 <- read.csv("w8_stmflow_1968-2012.txt")
W8_2013_2015 <- read.csv("w8_stmflow_2013-2015_5min.txt")
W9_1995_2012 <- read.csv("w9_stmflow_1995-2012.txt")
W9_2013_2015 <- read.csv("w9_stmflow_2013-2015_5min.txt")

##Dead Diamond River

DiamondRiver <- readNWISdv("01052500", "00060",startDate = "1999-01-01", endDate = "2018-12-31")

#---------------------------------------------------------------------------------------------
##Data Wrangling
#---------------------------------------------------------------------------------------------

##Hubbard Brook

Paradise <- W3_1957_2012 %>% bind_rows(W3_2013_2015) %>%
  mutate(DATETIME = ymd_hm(DATETIME))#%>%
#filter(DATETIME>as.POSIXct("2012-01-01 00:00:00", tz="UTC"))

Bear <- W6_1963_2012 %>% bind_rows(W6_2013_2015)%>%
  mutate(DATETIME = ymd_hm(DATETIME))#%>%
#filter(DATETIME>as.POSIXct("2012-01-01 00:00:00", tz="UTC"))

ZigZag <- W7_1965_2012 %>% bind_rows(W7_2013_2015)%>%
  mutate(DATETIME = ymd_hm(DATETIME))#%>%
# filter(DATETIME>as.POSIXct("2012-01-01 00:00:00", tz="UTC"))

Canyon <- W8_1965_2012 %>% bind_rows(W8_2013_2015)%>%
  mutate(DATETIME = ymd_hm(DATETIME))#%>%
# filter(DATETIME>as.POSIXct("2012-01-01 00:00:00", tz="UTC"))

Cascade <- W9_1995_2012 %>% bind_rows(W9_2013_2015)%>%
  mutate(DATETIME = ymd_hm(DATETIME))#%>%
#filter(DATETIME>as.POSIXct("2012-01-01 00:00:00", tz="UTC"))

##Bind all streams together and calcuate daily mean discharge (CFS)

DischargeAllStreams <- Paradise %>% 
  bind_rows(Bear)%>%
  bind_rows(ZigZag)%>%
  bind_rows(Canyon)%>%
  bind_rows(Cascade)%>%
  filter(DATETIME>as.POSIXct("2012-01-01 00:00:00", tz="UTC"))%>%
  #filter(DATETIME>as.POSIXct("1999-01-01 00:00:00", tz="UTC"))%>%
  mutate(WS=as.factor(WS))%>%
  ##add column for season
  mutate(Season = time2season(DATETIME,out.fmt="seasons"))%>%
  mutate(Summer = if_else(Season=="summer", "summer","fallwinterspring"))%>%
  ##add column for year
  mutate(Year = year(DATETIME))#%>%
  ##calcuate daily discharge
  group_by(WS)
#group_by(WS, Summer,Year)%>%
#dplyr::summarise(CV= cv(Discharge_cfs), Mean=mean(Discharge_cfs), SD=sd(Discharge_cfs))
#write.csv(DischargeAllStreams,"CV_Mean_SD_HB_AllStreamsBySeasonAndYear.csv")



##Dead Diamond River
%>%
  dplyr::select(Date, Discharge_cfs=X_00060_00003) %>%
  ##add column for season
  mutate(Season = time2season(Date,out.fmt="seasons"))%>%
  mutate(Summer = if_else(Season=="summer", "summer","fallwinterspring"))%>%
  ##add column for year
  mutate(Year = year(Date))%>%
  group_by(Summer,Year)#%>%
  #dplyr::summarise(CV= cv(Discharge_cfs), Mean=mean(Discharge_cfs), SD=sd(Discharge_cfs))

#write.csv(DiamondRiver,"CV_Mean_SD_Diamond_BySeasonAndYear.csv")
##plot cv from 1941 to present - separated by season
p1<-ggplot(filter(DiamondRiver,Summer=="summer"), aes(x = Year, y = CV)) + 
  geom_line( size = 1) +
  theme_minimal() +
  geom_smooth(method = "lm")+
  ggtitle("DiamondRiver: Summer")
  
p2<-ggplot(filter(DiamondRiver,Summer=="fallwinterspring"), aes(x = Year, y = CV)) + 
    geom_line( size = 1) +
    theme_minimal() +
    geom_smooth(method = "lm")+
  ggtitle("DiamondRiver: Fall,Winter,Spring")

grid.arrange(p1,p2,nrow=2)
#DiamondRiverUV <- readNWISuv("01052500", "00060",  startDate = "1999-01-01", endDate = "2018-10-31")

##plot highest cv and lowest cv

DiamondRiverPlot1 <- readNWISdv("01052500", "00060",startDate = "1999-01-01", endDate = "2018-12-31")%>%
  dplyr::select(Date, Discharge_cfs=X_00060_00003) %>%
  ##add column for season
  mutate(Season = time2season(Date,out.fmt="seasons"))%>%
  mutate(Summer = if_else(Season=="summer", "summer","fallwinterspring"))%>%
  filter(Summer=="summer")%>%
  mutate(Year = year(Date))%>%
  filter(Year=="2000")%>%
  ggplot(aes(x = Date, y = Discharge_cfs)) + 
  geom_line( size = 1) +
  theme_minimal() +
  ylim(0,3000)+
  ggtitle("Dead Diamond River 2000 - lowest CV")

DiamondRiverPlot2 <- readNWISdv("01052500", "00060",startDate = "1999-01-01", endDate = "2018-12-31")%>%
  dplyr::select(Date, Discharge_cfs=X_00060_00003) %>%
  ##add column for season
  mutate(Season = time2season(Date,out.fmt="seasons"))%>%
  mutate(Summer = if_else(Season=="summer", "summer","fallwinterspring"))%>%
  filter(Summer=="summer")%>%
  mutate(Year = year(Date))%>%
  filter(Year=="2002")%>%
  ggplot(aes(x = Date, y = Discharge_cfs)) + 
  geom_line( size = 1) +
  theme_minimal() +
  ylim(0,3000)+
  ggtitle("Dead Diamond River 2002 - highest CV")
grid.arrange(DiamondRiverPlot1,DiamondRiverPlot2,nrow=2)

#---------------------------------------------------------------------------------------------
##Combine years and format date column
##Filter to just 2012 - 2014
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
##Visualize!
#---------------------------------------------------------------------------------------------
ggplot(Paradise, aes(x = DATETIME, y = Discharge_cfs)) + 
  geom_line( size = 1) +
  theme_minimal()+
  ggtitle("Paradise")

ggplot(Bear, aes(x = DATETIME, y = Discharge_cfs)) + 
  geom_line( size = 1) +
  theme_minimal() +
  ggtitle("Bear")

BearPlot <- filter(Bear,DATETIME>as.POSIXct("2012-01-01 00:00:00", tz="UTC"))%>%

BearPlot2012 <- filter(DischargeAllStreams,WS=="6")%>%
  filter(Summer=="summer")%>%
  filter(Year=="2012")%>%
  ggplot(aes(x = DATETIME, y = Discharge_cfs)) + 
  geom_line( size = 1) +
  theme_minimal() +
  ggtitle("Bear2012")+
  ylim(0,100)
BearPlot2013 <- filter(DischargeAllStreams,WS=="6")%>%
  filter(Summer=="summer")%>%
  filter(Year=="2013")%>%
  ggplot(aes(x = DATETIME, y = Discharge_cfs)) + 
  geom_line( size = 1) +
  theme_minimal() +
  ggtitle("Bear2013") +
  ylim(0,100) 
BearPlot2014 <- filter(DischargeAllStreams,WS=="6")%>%
  filter(Summer=="summer")%>%
  filter(Year=="2014")%>%
  ggplot(aes(x = DATETIME, y = Discharge_cfs)) + 
  geom_line( size = 1) +
  theme_minimal() +
  ggtitle("Bear2014")   +
  ylim(0,100)
grid.arrange(BearPlot2012,BearPlot2013,BearPlot2014,nrow=3)


ggplot(ZigZag, aes(x = DATETIME, y = Discharge_cfs)) + 
  geom_line( size = 1) +
  theme_minimal() +
  ggtitle("ZigZag")

ggplot(Canyon, aes(x = DATETIME, y = Discharge_cfs)) + 
  geom_line( size = 1) +
  theme_minimal()+
  ggtitle("Canyon")

CanyonPlot2012 <- filter(DischargeAllStreams,WS=="8")%>%
  filter(Summer=="summer")%>%
  filter(Year=="2012")%>%
  ggplot(aes(x = DATETIME, y = Discharge_cfs)) + 
  geom_line( size = 1) +
  theme_minimal() +
  ggtitle("Canyon2012")+
  ylim(0,100)
CanyonPlot2013 <- filter(DischargeAllStreams,WS=="8")%>%
  filter(Summer=="summer")%>%
  filter(Year=="2013")%>%
  ggplot(aes(x = DATETIME, y = Discharge_cfs)) + 
  geom_line( size = 1) +
  theme_minimal() +
  ggtitle("Canyon2013") +
  ylim(0,100) 
CanyonPlot2014 <- filter(DischargeAllStreams,WS=="8")%>%
  filter(Summer=="summer")%>%
  filter(Year=="2014")%>%
  ggplot(aes(x = DATETIME, y = Discharge_cfs)) + 
  geom_line( size = 1) +
  theme_minimal() +
  ggtitle("Canyon2014")+
  ylim(0,100) 
grid.arrange(BearPlot2012,CanyonPlot2012,BearPlot2013,CanyonPlot2013,BearPlot2014,CanyonPlot2014,nrow=3)
grid.arrange(BearPlot2012,BearPlot2013,BearPlot2014,nrow=3)

ggplot(Cascade, aes(x = DATETIME, y = Discharge_cfs)) + 
  geom_line( size = 1) +
  theme_minimal() +
  ggtitle("Cascade")


#---------------------------------------------------------------------------------------------
##Look at distribution for each stream...
#---------------------------------------------------------------------------------------------
mu <- ddply(DischargeAllStreams, "WS", summarise, grp.mean=mean(Discharge_cfs))
ggplot(DischargeAllStreams, aes(x=Discharge_cfs, color=WS)) +
  geom_histogram(fill="white", position="dodge",binwidth = 1)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=WS),
             linetype="dashed")+
  theme(legend.position="top")+
  xlim(0,10)
  
ggplot(DischargeAllStreams, aes(Discharge_cfs, stat(density), colour = WS)) +
    geom_freqpoly(binwidth = 1)+
    xlim(0,10)

#---------------------------------------------------------------------------------------------
##Calculate summary statistics using package hydroTSM
#---------------------------------------------------------------------------------------------


