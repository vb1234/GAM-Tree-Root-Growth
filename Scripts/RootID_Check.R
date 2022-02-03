### 2022-01  Code used to initially understand the raw data file for count of roots
###          This file is not needed for root count analysis 
###          Kept as source if later is of interest
###Code which checked for duplicate RootIDs, result RootID unique to Plot/Tube/Window

### Start check the pattern of RootID, why are there duplicates?
###  Code below shows RootID is generated separately for each tube window
###  That is the same RootID is seen in multiple tubes, and may occur once in each window of a single tube
# #Check for roots with same RootID, see Same RootID in different Tubes

print("in RootID_Check.R file")

RootID_GT1_index<- raw_roots_data %>%
  group_by(RootID) %>%
  summarise(count=n()) %>%
  filter(count > 1)
print(RootID_GT1_index,n=20,width = Inf)

#This gets all rows with RootID in index,  not just those with 2+ instances
#RootID_GT1_rows<- subset(raw_roots_data,RootID %in% RootID_GT1_index$RootID) %>%
# arrange(RootID)%>%
# print(n=20,width = Inf)

# Note that Tube/RootID/Window is unique, may be in same "Appeared" (observation) or not
# Tube/RootID/Window is unique
TubeRootID_GT1_index<- raw_roots_data %>%
  filter(!is.na(RootID))  %>%
  group_by(RootID, Tube) %>%   #If add window, all counts are 1
  summarise(count=n()) %>%
  print(n=20,width = Inf)

#Roots with same id in different windows at same observation time
TubeRootID_GT1_rows<-raw_roots_data %>%
  filter(!is.na(RootID))  %>% #2616 rows
  group_by(RootID, Tube, Appeared) %>%
  filter(n() > 1)  %>%
  arrange(Plot, Tube, RootID,Appeared, Window)%>%
  print(n=20,width = Inf) #Total of 271 rows

#Roots_Dec <-raw_roots_data[,"Appeared"==10] #See that all "roots" are not roots #Error why??
#raw_roots_data["Appeared"==10,.] #why doesn't work
subset(raw_roots_data,Appeared==10)

print(raw_roots_data[30,]) #Check what some of the NAs are in data in this row
# Explicitly cast each column with NA value, not most efficient but most clear

#  Plot RootID vs Appeared to get overall view of data
#  Appeared and Appeared.Date should be idenical
ggplot(data=raw_roots_data) +
  geom_point(mapping = aes(x =Appeared.Date,  y=RootID ))

ggplot(data=raw_roots_data) +
  geom_point(mapping = aes(x =Day,  y=RootID ))+
  facet_grid (Tube ~ Window)
### End check the pattern of RootID
###



