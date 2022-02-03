#Script for Analysis of Root Growth 2019- 2020
# Goal explore GAM for modeling data
# Start:December 28, 2021
# Veta Bonnewell
# 2021-12-28:10-12am mostly R review - Morton time 1 hr -Set up project, start script review raw data types
# 2021-12-29  5-6:30 R review using Haley Wickam's online book- r4ds.had.co.nz  Morton 10pm -1 am read data, check values
# 2021-12-30  ?  review raw data with excel
# 2022-01-04  4:30-6:30
# 2022-01-05  7:00- 8:00   #code for finding RootID for same date, all instances of that Tube, RootID
# 2022-01-06  11:00-12:00  #code for finding RootID for same date in different windows for that date only
# 2022-01-13  11-3:00      #code to geom_smooth of New_RootsByDate, Window, Plots
#                            code to filter out data from observation 1
# 2022-01-20  10-1:30 Install git(remote repo), setup GitHub (origin repo), connect 
#               include setup commend in comments below for parts that may need to do when setup Windows PC
#               2-4  edit script
#                ? cannot commit, find email from github security violation PAT in comment        
#               10-1:30 email, script edit - remove PAT, compare span for Tube Residual Standard Error
#2022-01-26   3 pm - 5pm Review input parameters and output values from optim() for loess() and add comments
#                 optim() did not return an optimal value, for minimizing SSE
#                 plot with  span values 0.1-1.0 for loess() and confirm Residual Standard Error increases with span.
#2022-01-26   12-2pm try to create function for plots, long time n find how to print/write
#                 Add titles to plots, correct plot filename, title, order in docx, 
#                 review loess pages, conclude not useful for stats
#              (2 hrs my time) all bookmarks on desktop in separate docx
#              11pm-12 pm update google docs and email Luke
#2022-02-02    2 hrs listen to Gavin course and take notes ( not recorded as vol hours)
#               6-7pm  source for RootID check, not needed for analysis
#                8:30 -    remove all curves with loess plots and analysis to separate file.
#ToDo: Noam/Gavin courses for next step? or Fridley?
#  Add gam plot default function and use for gam plots
#  Commit
#  Add arg for varying gam plots to plotting function?
#

### Open issues for VB
###
###2022-01-13
###  1. Ask Luke about why no Jan again?
###
###  2. Ask Newton about RootID scheme, branches receive new id?
###      Use excel filter to examine number of entries for a root and values for all vars:
### 3. RootID+WindowId+Tube are unique,
###      i.e same Multiple RootID on same date in multiple windows does not mean same root
###     If root extends from one window to another will be counted as 2 roots
###        
###  3. Units: mm or mm*10??  
###
###  4. When plot TubeIDs by Window see 2 bands in some windows, branches? Not important just curious

###2022-01-20
###  1. Use git so can frequently save annotated intermediate code, IDE to see
###  2. Google docs can save version, less granular?
###  3. Should not compare Residual Standard Errors from different datasets
###      i.e only for different models on same data set
### End issues for VB
##
######Start Git/github setup
#### Future: Windows only
#library(installR)
#updateR()  # checks if current is latest, if not asks if want to update
#
#install.packages(usethis)
#library(usethis)
#use_git_config (user.name = "Veta Bonnewell", user.email = "vbonnewell@yahoo.com")
# in terminal, check results
#git config --global --list
#
# help on setting up default editor for git
#VB skip for now
# https://swcarpentry.github.io/git-novice/02-setup/
#usethis::git_default_branch_configure()
#
#  Get Personal Access Token for Rstudio to access GitHub repository
#  usethis::create_github_token()
#    Copy token  (send email to gmail with token to save)
#   gitcreds::gitcreds_set()
#   Paste token
#  On windows install/setup git, connect Rstudio, clone repository to PC
######End Git/github setup


library(tidyverse)
library(readr)
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
############ Data Definitio
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

summarise(raw_roots_data,count=n(), rootCountNoNA=sum(!is.na(RootID)))

#Total count is 2851
summary(raw_roots_data)              #Check for NAs - Find 235 are rows with no RootID or data
summary(raw_roots_data$Month)        # Shows factor is ordered

#For additional viewing of structure of raw data use following script
# Note that sourcing the script does not send output to console.
# Need to open script and run from that tab to get output
# source("Scripts/Check-Raw-Data.R")
#IGNORE sourcing for now

# Depends on raw_roots_data being the data read from the raw data file
head(raw_roots_data)
spec(raw_roots_data)

#Find out the values for categorical variables, also available from Summary()
levels(raw_roots_data$Window)
levels(raw_roots_data$Initial.Status) 
levels(raw_roots_data$Final.Status)
levels(raw_roots_data$Year)
levels(raw_roots_data$Month)

#See that there are a large number of NAs, how many root are there really?
summarise(raw_roots_data,count=n(), rootCountNoNA=sum(!is.na(RootID)))

#Total count is 2851
summary(raw_roots_data)              #Check for NAs - Find 235 are rows with no RootID or data
summary(raw_roots_data$Month)        # Shows factor is ordered
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
# 

New_RootsByDate<- raw_roots_data %>%
  filter(Appeared > 1) %>%
  group_by(Appeared, Appeared.Date,Year, Month, Day) %>%  #These variables should have same value for each Observtion date
  summarise(rootCountNA=n(), rootCount=sum(!is.na(RootID)), 
            totalLengthmm = sum(Maximum.Length, na.rm=TRUE))

#Plot of counts per tube vs time (all windows combined)
New_RootsByTube_NoObs1<- raw_roots_data %>%
  filter(Appeared > 1) %>%
  group_by(Appeared, Appeared.Date,Year, Month, Day,Tube) %>%  
  summarise(rootCountTubeNA=n(), rootCountTube=sum(!is.na(RootID)), 
            totalLengthmm = sum(Maximum.Length, na.rm=TRUE))

summary(New_RootsByTube_NoObs1)  #Use to check data is as expected

# To increase number of samples, use counts per tube, and counts per window
# Plot of counts per window vs time (ALL tubes COMBINED)
#     Initially just ooking to see if curves are similar to that of all and by tube
#     TooDo?:Look to see if use window # as factor, see if pattern on new roots varies with depth
#     Did not do this analysis

New_RootsByWindow_NoObs1<- raw_roots_data %>%
  filter(Appeared > 1) %>%                                        # Remove first obs.
  group_by(Appeared, Appeared.Date,Year, Month, Day, Window) %>%  #These variables should have same value for each Observtion date
  summarise(rootCountWinNA=n(), rootCountWin=sum(!is.na(RootID)), 
            totalLengthmm = sum(Maximum.Length, na.rm=TRUE))

summary(New_RootsByWindow_NoObs1)  #Use to check data is as expected

## Code for curves with loess and variations in span parameter
#   source("Scripts/CurvesUsingLoess.R", echo = TRUE) 
#

#### Start Use GAM method for smoothing
#
# https://ggplot2.tidyverse.org/reference/geom_smooth.html
#   method = "gam", formula = y ~ s(x, bs = "cs")
#   basis functions (bs) are cubic plines (cs)

# By Tube
summary(New_RootsByTube_NoObs1)  #Use to check data is as expected

jpeg("Analysis/Images/GAM-New_roots_Entire_Interval-Tube")
ggplot(data=New_RootsByTube_NoObs1, mapping =aes(x =Appeared.Date,  y=rootCountTube)) +
  geom_point(mapping = aes(color=Tube )) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))+
  ggtitle(" GAM with default parameters - Tubes By Date")
dev.off()

jpeg("Analysis/Images/GAM-New_roots_Annual_Tube")
ggplot(data=New_RootsByTube_NoObs1, mapping =aes(x =Day,  y=rootCountTube)) +
  geom_point(mapping = aes(color=Tube )) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  ggtitle(" GAM with default parameters - Tubes By Day of Year")
dev.off()

# By Date/Observation 

summary(New_RootsByDate)  #Use to check data is as expected

jpeg("Analysis/Images/GAM-New_roots_Entire_Interval")
New_RootsByDate_Nrows =nrow(New_RootsByDate)
ggplot(data=New_RootsByDate, mapping =aes(x =Appeared.Date,  y=rootCount)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  ggtitle(" GAM with default parameters - All By Date")
dev.off()

jpeg("Analysis/Images/GAM-New_roots_Annual")
ggplot(data=New_RootsByDate, mapping =aes(x =Day,  y=rootCount)) +
  #geom_point(mapping = aes(color=Window )) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  ggtitle(" GAM with default parameters - All By Day of Year")
dev.off()
#### End initial GAM using bs = 'cs'



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






