#Script for Analysis of Root Growth 2019- 2020
# Read in raw data  from each observation and examine
# Create data structures with data grouped by plot, tubes and windows()
# 
# Start:December 28, 2021
# Veta Bonnewell

library(tidyverse)
library(readr)
library(mgcv)
library(ggplot2)

counts_roots_data  <- read_csv("Data/QUBI-E_counts.csv", na=c("NA", "-", NA))# Converts "NA", "-" to NAs
#summary(counts_roots_data)
head(counts_roots_data)
#spec(counts_roots_data)

raw_roots_data  <- read_csv("Data/QUBI-E_raw.csv", na=c("NA", "-", NA),
                            col_types= cols(
                              Plot= col_character(),
                              Tube = col_character(),  
                              Window = col_factor( ordered=TRUE, include_na = TRUE), # "1" "2" "3" "4"
                              RootID = col_double(),      # 235 roots do not have RootID or data, i.e. NA, 
                              Appeared = col_double(),    # Observation number
                              Appeared.Date = col_date(), # "yyyy-mm-dd"
                              Died = col_double(),        # Observation number
                              Died.Date  = col_date(),    # "yyyy-mm-dd"
                              Initial.Status = col_factor(include_na = TRUE), #"LIVING"     "EMTIPS"     "NOROOTS"    "RHIZOMORPH"
                              Final.Status = col_factor(include_na = TRUE),   # "LIVING"     "EMTIPS"     "NOROOTS"    "RHIZOMORPH"
                                                                              #"DEAD"  "EMTIPSDEAD"      "DEAD CENSORED"   "RHIZOMORPH DEAD"
                              Lifespan = col_double(),    # Days?
                              Maximum.Length =  col_double(),  # mm? QU.counts has length / 10?
                              Maximum.Diameter = col_double(), # mm?
                              Year =col_factor(ordered=TRUE),                      # Year only of observation date
                              Month = col_factor(levels = c("2","3", "4", "5", "6",  "7",  "8",  "9", "10", "11", "12"),
                                                 ordered=TRUE, include_na = TRUE ),# Month only of observation date
                              Day = col_double())                                  # Day of year of observation date
)
############ Data Types and Definition
# Plot chr  
# Tube chr  -> integer/factor  - Multiple tubes per tree
# Window chr?  factor?          Up to 4 windows from 1-4 with increasing depth
# RootID chr  integer           Unique to tube/window
# Appeared (observation number) integer
# Appeared Date -  date first recorded
# Died  chr (observation number?)  need to add na=c("-", NA), cast to integer
# Died.Date date first missing - need to add na=c("-", NA), cast to date
# Initial.Status chr values = LIVING, NOROOTS
# Lifespan  days alive?
# Maximum.Length, mm?  initial length or mximimum length from all observations
# Maximum.Diameter, mm?   "
# Year - year appeared
# Month - month appeared 
# Day - day of year appeared

head(raw_roots_data)
spec(raw_roots_data)

summarise(raw_roots_data,count=n(), rootCountNoNA=sum(!is.na(RootID)))

#Total count is 2851
summary(raw_roots_data)              #Check for NAs - Find 235 are rows with no RootID or data
summary(raw_roots_data$Month)        # Shows factor is ordered

#For additional viewing of structure of raw data use following script
# source("Scripts/Check-Raw-Data.R", echo = TRUE)


#Find out the values for categorical variables, also available from Summary()
levels(raw_roots_data$Window)
levels(raw_roots_data$Initial.Status) 
levels(raw_roots_data$Final.Status)
levels(raw_roots_data$Year)
levels(raw_roots_data$Month)

#See that there are a large number of NAs, how many root are there really?
summarise(raw_roots_data,count=n(), rootCountNoNA=sum(!is.na(RootID)))

#Total count is 2851

#raw_roots_data %>%     #Test  summarise
#group_by(Appeared, Appeared.Date) %>%
#summary()
#summarise(count=n(), minimum_RootID= min(RootID,na.rm=TRUE), maximum_RootID= max(RootID,na.rm=TRUE))

# source("Scripts/RootID_Check.R", echo = TRUE) #Code which checked for duplicate RootIDs, result RootID unique to Plot/Tube/Window

################################################################################################################
# Create data for each observation date
# rootCountNA  - count of roots for observation with NO ROOTS rows deleted
# rootCount - excluding NAs  -USE THIS
# totalLengthmm - sum of (maximum) lengths of each root ? units mm or 10* mm?
# rootCountNorm - root count normalized by min/max of year

New_RootsByDate<- raw_roots_data %>%
  filter(Appeared > 1) %>%
  group_by(Appeared, Appeared.Date,Year, Month, Day) %>%  #These variables should have same value for each Observtion date
  summarise(rootCountNA=n(), rootCount=sum(!is.na(RootID)), 
            totalLengthmm = sum(Maximum.Length, na.rm=TRUE))

# Function fun_normalize(x)
# x is vector with min and max values
# Use min and max to calculate, returns normalized value for a value in x
fun_normalize <- function(x) {       # Create user-defined function
  (x - min(x)) / (max(x) - min(x))
}

New_RootsByDate <- New_RootsByDate %>%
  group_by(Year)  %>%
  mutate(rootCountDate_Norm = fun_normalize(rootCount))

summary(New_RootsByDate)  #Use to check data is as expected

#Plot of counts per tube vs time (all windows combined), 6 tubes * 18 dates = 108 obs.
New_RootsByTube_NoObs1<- raw_roots_data %>%
  filter(Appeared > 1) %>%
  group_by(Appeared, Appeared.Date,Year, Month, Day,Tube) %>%  
  summarise(rootCountTubeNA=n(), rootCountTube=sum(!is.na(RootID)), 
            totalLengthmm = sum(Maximum.Length, na.rm=TRUE))

New_RootsByTube_NoObs1 <- New_RootsByTube_NoObs1 %>%
  group_by(Year)  %>%
  mutate(rootCountTube_Norm = fun_normalize(rootCountTube))

summary(New_RootsByTube_NoObs1)  #Use to check data is as expected

# To increase number of samples, use counts per tube, and counts per window
# Plot of counts per window vs time (ALL tubes COMBINED) 4 windows * 18 dates = 72 obs.
#     Initially just looking to see if curves are similar to that of all and by tube
#     TooDo?:Look to see if use window # as factor, see if pattern on new roots varies with depth
#            Did not do this analysis

New_RootsByWindow_NoObs1<- raw_roots_data %>%
 filter(Appeared > 1) %>%                                        # Remove first obs.
 group_by(Appeared, Appeared.Date,Year, Month, Day, Window) %>%  #These variables should have same value for each Observtion date
 summarise(rootCountWinNA=n(), rootCountWin=sum(!is.na(RootID)),
           totalLengthmm = sum(Maximum.Length, na.rm=TRUE))

New_RootsByWindow_NoObs1 <- New_RootsByWindow_NoObs1 %>%
  group_by(Year)  %>%
  mutate(rootCountWin_Norm = fun_normalize(rootCountWin))

summary(New_RootsByWindow_NoObs1)  #Use to check data is as expected

## Code for curves with loess and variations in span parameter
#   source("Scripts/CurvesUsingLoess.R", echo = TRUE) 

####### Ignore the following for now
# #####Recreate the "QUBI-E.counts" data
# New_Roots<- raw_roots_data %>%
#   group_by(Appeared, Appeared.Date,Year, Month, Day) %>%  #These variables should have same value for each Observtion date
#   summarise(rootCountNA=n(), rootCount=sum(!is.na(RootID)), 
#             totalLengthmm = sum(Maximum.Length, na.rm=TRUE))
# 
# ##DaysTotal is Days since Jan. 1, 2019, will need to be adjusted as each year is added
# #   Need DaysTotal to calculate days between last date of one year and first of next
# New_Roots<-rename(New_Roots, DayOfYear = Day)
# New_Roots <- New_Roots %>% #DaysTotal is Days since Jan. 1, 2019
#   mutate(DaysTotal=ifelse(Year == 2020, DayOfYear + 365, DayOfYear) #Future adapt for Leap Year
#          )
# 
# New_Roots <-New_Roots %>% 
#   ungroup() %>% #Have to ungroup for lag to work
#   mutate(DaysBetween = DaysTotal - lag(DaysTotal, default = first(DaysTotal)),
#          RootsPerDay = rootCount/DaysBetween)
# 
# New_Roots_Nrow <- nrow(New_Roots) 
# New_Roots <-New_Roots[2:New_Roots_Nrow,] %>%
#   group_by(Year)%>%
#   mutate(MaxRoots = max(rootCountNA))
# 
# print(New_Roots,n=20,width = Inf)
# # Note 222-01-013 that rootCount matches mroot.... for QUBI-E except row 6, 582 vs 584
# # Examined my QUBI-E_raw file with excel and see no obvious rows to omit
# # Perhaps different versions of raw data?
#
# End Ignore the following in the Rmarkdown output





