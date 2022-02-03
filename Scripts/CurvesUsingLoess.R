#### Filename: Scripts/CurvesUsingLoess.R
#     Date: Jan. 2022
#     Author: V. Bonnewell
# 
# Code for plots of roots for plots, tubes, windows using geom_smooth with default loess smoothing function
# Usage:
#   source("Scripts/CurvesUsingLoess.R", echo = TRUE) # Code for curves with loess and variations in span paameter
#

#Root count for  plot
#Remove data for first observation since it is actually comulative from trees past growth
#plot new roots over entire time interval 2019 & 2020
jpeg("Analysis/Images/New_roots_Entire_Interval")
New_RootsByDate_Nrows =nrow(New_RootsByDate)
ggplot(data=New_RootsByDate[2:New_RootsByDate_Nrows,], mapping =aes(x =Appeared.Date,  y=rootCount)) +
  geom_point() +
  geom_smooth()+
  ggtitle("Loess with default parameters - All by Date")
dev.off()

#plot new roots timespan 1 year, i.e. overlay years
jpeg("Analysis/Images/New_roots_Annual")
ggplot(data=New_RootsByDate[2:New_RootsByDate_Nrows,], mapping =aes(x =Day,  y=rootCount)) +
  #geom_point(mapping = aes(color=Window )) +
  geom_point() +
  geom_smooth()+
  ggtitle("Loess with default parameters - All by Day of Year")
dev.off()

### Start Plots for Tubes

jpeg("Analysis/Images/New_roots_Entire_Interval-Tube")
ggplot(data=New_RootsByTube_NoObs1, mapping =aes(x =Appeared.Date,  y=rootCountTube)) +
  geom_point(mapping = aes(color=Tube )) +
  geom_smooth()+
  ggtitle("Loess with default parameters - Tube by Date")
dev.off()

jpeg("Analysis/Images/New_roots_Annual_Tube")
ggplot(data=New_RootsByTube_NoObs1, mapping =aes(x =Day,  y=rootCountTube)) +
  geom_point(mapping = aes(color=Tube )) +
  geom_smooth() +
  ggtitle("Loess with default parameters - Tube by Day of Year")
dev.off()
### End Plots for Tubes

####  Start Plots for Windows
# To increase number of samples, use counts per tube, and counts per window
# Plot of counts per window vs time (all tubes combined)

jpeg("Analysis/Images/New_roots_Entire_Interval-Window")
ggplot(data=New_RootsByWindow_NoObs1, mapping =aes(x =Appeared.Date,  y=rootCountWin)) +
  geom_point(mapping = aes(color=Window )) +
  geom_smooth()+
  ggtitle("Loess with default parameters - Window by Date")
dev.off()

jpeg("Analysis/Images/New_roots_Annual-Window")
ggplot(data=New_RootsByWindow_NoObs1, mapping =aes(x =Day,  y=rootCountWin)) +
  geom_point(mapping = aes(color=Window )) +
  geom_smooth()+
  ggtitle("Loess with default parameters - Window by Day of Year")
dev.off()
####  End Plots for Windows

### Start analysis of curves using default Loess with varying span values
# Look at effect of different span values, code from
# http://r-statistics.co/Loess-Regression-With-R.html

#Test on Tube data since more data points
New_RootsByTube_NoObs1_OrderDay<- New_RootsByTube_NoObs1 %>%
  ungroup() %>%
  arrange(Day)

loess75Default <- loess(rootCountTube ~ Day, data = New_RootsByTube_NoObs1_OrderDay)
#Number of Observations: 108 
#Equivalent Number of Parameters: 4.55 
######Residual Standard Error: 42.56 
#Default span = 0.75, degree = 2

#Same for Dates
# data=New_RootsByDate[2:New_RootsByDate_Nrows,], mapping =aes(x =Day,  y=rootCount
New_RootsByDate_NoObs1_OrderDay<-New_RootsByDate[2:New_RootsByDate_Nrows,] %>%
  ungroup() %>%
  arrange(Day)

loessMod75_Date <- loess(rootCount ~ Day, data = New_RootsByDate_NoObs1_OrderDay)
#Number of Observations: 18 
#Equivalent Number of Parameters: 4.85 
####  Residual Standard Error: 142.6   # Much larger than by Tube obove, 
####  Is this faulty conclusion since range is different!!!
####                                   # Only valid for comparisons of models for same data???
#Default span = 0.75, degree = 2
#sum(loessMod75_Date$residuals, na.rm = T)  #[1] -126.4533
#sum(loessMod75_Date$residuals, na.rm = T)^2  # 15990.43

#https://www.statology.org/how-to-interpret-residual-standard-error/
#  Residual Standard Error = square root of (sum of y-_observed - y_predicted)^2/df)
#  df =The degrees of freedom, calculated as the total number of observations – total number of model parameters.
#
# Define loess models
loessMod10 <- loess(rootCountTube ~ Day, data = New_RootsByTube_NoObs1_OrderDay,span=0.10) # 10% smoothing span)
loessMod25 <- loess(rootCountTube ~ Day, data = New_RootsByTube_NoObs1_OrderDay,span=0.25) # 25% smoothing span)
loessMod50 <- loess(rootCountTube ~ Day, data = New_RootsByTube_NoObs1_OrderDay,span=0.50) # 50% smoothing span)
loessMod75 <- loess(rootCountTube ~ Day, data = New_RootsByTube_NoObs1_OrderDay,span=0.75) # 75% smoothing span)

#Results below copied from console window
#loess(rootCountTube ~ Day, data = New_RootsByTube_NoObs1_OrderDay, span = .75)
#Number of Observations: 108 
#Equivalent Number of Parameters: 4.55 
######Residual Standard Error: 42.56 
#Default span = 0.75, degree = 2

#loess(formula = rootCountTube ~ Day, data = New_RootsByTube_NoObs1_OrderDay, span = 0.5)
#Number of Observations: 108 
#Equivalent Number of Parameters: 7.05 
#Residual Standard Error: 42.98

#loess(rootCountTube ~ Day, data = New_RootsByTube_NoObs1_OrderDay,span=0.30)
#Number of Observations: 108 
#Equivalent Number of Parameters: 10.59 
#Residual Standard Error: 42.78 

options(digits=2)
for(i in seq(from=0.2, to=1.0, by=0.1)){
  lmod <- loess(rootCountTube ~ Day, data = New_RootsByTube_NoObs1_OrderDay, span = i)
  print( paste("i  =", i, ",  Obs = ", lmod$n, ",  Residual Standard Error = ", lmod$s))
}

# Following SSE do not include degrees of freedom
loessMod10_SSE <- sum(loessMod10$residuals^2, na.rm = T)  #  3.828922e-25
loessMod25_SSE <- sum(loessMod25$residuals^2, na.rm = T)  #  8758.039
loessMod75_SSE <- sum(loessMod75$residuals^2, na.rm = T)  #  29895.84


# get smoothed output
smoothed10 <- predict(loessMod10) 
smoothed25 <- predict(loessMod25)
smoothed50 <- predict(loessMod50)
smoothed75 <- predict(loessMod75)

# Plot it (original has colors defined, not on Mac??)
jpeg("Analysis/Images/New_roots_TubeDay-LoessSpan10-25-50-75")
plot(New_RootsByTube_NoObs1_OrderDay$rootCountTube, 
     x=New_RootsByTube_NoObs1_OrderDay$Day, 
     type="l", 
     main="Loess Smoothing: Span = 10, 25, 50, 75", 
     xlab="Day", ylab="New Roots per Tube")
lines(smoothed10, x=New_RootsByTube_NoObs1_OrderDay$Day, col="purple")
lines(smoothed25, x=New_RootsByTube_NoObs1_OrderDay$Day, col="green")
lines(smoothed50, x=New_RootsByTube_NoObs1_OrderDay$Day, col="blue")
lines(smoothed75, x=New_RootsByTube_NoObs1_OrderDay$Day, col="red")  #Note this same as geom_smooth
dev.off()

### Start optim() with loess
#The following optimization does not work 
#Using function from http://r-statistics.co/Loess-Regression-With-R.html
#Find optimal span
loessMod <- try(loess(rootCountTube ~ Day, data = New_RootsByTube_NoObs1_OrderDay, span=0.5))
res <- try(loessMod$residuals)
# define function that returns the SSE
calcSSE <- function(x){
  loessMod <- try(loess(rootCountTube ~ Day, data = New_RootsByTube_NoObs1_OrderDay, span=x), silent=T)
  res <- try(loessMod$residuals, silent=T)
  if(class(res)!="try-error"){
    if((sum(res, na.rm=T) > 0)){
      sse <- sum(res^2)  
    }
  }else{
    sse <- 99999
  }
  return(sse)
}

# Run optim to find span that gives minimum sum of squared errors, SSE, starting at 0.5
#           VB - method="SANN", function to generate new candidate point is "default Gaussian Markov Kerne"
#              
#optim(par=c(0.5), calcSSE, method="SANN")
#E, starting at 0.5
# optim(par=c(0.5), calcSSE, method="SANN")
#  Output:  Definition of return values is from help for optim()
#$par   The best set of parameters found.  VB: neg value not valid?
#[1] -0.1035813
#$value  The value of fn corresponding to par, from function code above, 99999 is error, returned when Snn reqhed default of 10000 calls 
#[1] 99999
#$counts
## Number of calls to function  and Number of calls to gradient 
#           10000                             NA 
#$convergence  	An integer code. 0 indicates successful completion (which is always the case for "SANN" and "Brent"). 
#[1] 0
#$message A character string giving any additional information returned by the optimizer, or NULL.
#NULL
#VB comment 1/20/22: I don't think that a negative value of par is valid. Check values .10,.25, .75 
# see above that SSE becomes very large
### End optim() with loess
### End analysis of curves using default Loess with varying span values
