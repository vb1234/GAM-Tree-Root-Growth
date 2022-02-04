####Filename: GamAnalysis_Part1_BasicTrials
#   Date:2022-02-03
#
#
print("Raw Data with NAs and initial observation included")
summary(raw_roots_data)

#### Start Use GAM method for smoothing
##
## https://ggplot2.tidyverse.org/reference/geom_smooth.html
##   method = "gam", formula = y ~ s(x, bs = "cs")
##   basis functions (bs) are cubic plines (cs)

# By Tube
summary(New_RootsByTube_NoObs1)  #Use to check data is as expected

jpeg("Analysis/Images/GAM-New_roots_Entire_Interval-Tube")
my_plottube1 <- ggplot(data=New_RootsByTube_NoObs1, mapping =aes(x =Appeared.Date,  y=rootCountTube)) +
  geom_point(mapping = aes(color=Tube )) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))+
  ggtitle(" GAM with default parameters - Tubes By Date")
dev.off()

jpeg("Analysis/Images/GAM-New_roots_Annual_Tube")
my_plottube2 <-ggplot(data=New_RootsByTube_NoObs1, mapping =aes(x =Day,  y=rootCountTube)) +
  geom_point(mapping = aes(color=Tube )) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  ggtitle(" GAM with default parameters - Tubes By Day of Year")
dev.off()
print(my_plottube1 )
print(my_plottube2 )

#Testing function for printing geom_smooth plots
#Ignore for now, since need plotting function for printing from gam model
#source("Scripts/Root-Functions.R", echo = TRUE)
#my_plot <- my_plot_smooth_gam_fun(New_RootsByTube_NoObs1,
#                                  Day, rootCountTube, "cs",Tube, "Test","my_testfile2.jpeg")

# By Date/Observation 

summary(New_RootsByDate)  #Use to check data is as expected

jpeg("Analysis/Images/GAM-New_roots_Entire_Interval")
New_RootsByDate_Nrows =nrow(New_RootsByDate)
my_plotdate1 <- ggplot(data=New_RootsByDate, mapping =aes(x =Appeared.Date,  y=rootCount)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  ggtitle(" GAM with default parameters - All By Date")
dev.off()

jpeg("Analysis/Images/GAM-New_roots_Annual")
my_plotdate2 <- ggplot(data=New_RootsByDate, mapping =aes(x =Day,  y=rootCount)) +
  #geom_point(mapping = aes(color=Window )) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  ggtitle(" GAM with default parameters - All By Day of Year")
dev.off()

print(my_plotdate1 )
print(my_plotdate2 )
#### End initial GAM using bs = 'cs'

### 2/2/2022 Use code based on Intro to GAM with T and mgcv Gavin...
## Start Analysis with combined years
# Slide 44/132

#guess a value for k
tube_model_k10 <- gam ( rootCountTube ~ s(Day, k=10),
                        data = New_RootsByTube_NoObs1,
                        # family = 'gaussian' #default, type of conditional distribution
                        # bs ='tp'            #default, basis (type of smoother) is low-rank thin plate spline
                        method = "REML")

summary(tube_model_k10)

# Quickly see plot
#ToDo function to use ggplot)
#   or gavin uses library('gratia'); draw(model, scales= fixed) #for complex model
plot(tube_model_k10, se=TRUE)



# > summary(tube_model_k10)
# Family: gaussian 
# Link function: identity 
# 
# Formula:
#   rootCountTube ~ s(Day, k = 10)
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   18.704      4.098   4.564 1.39e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df     F p-value   
# s(Day) 3.796   4.66 4.514 0.00181 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.155   Deviance explained = 18.5%
# -REML = 555.86  Scale est. = 1813.6    n = 108


#Get following error when use values of 20, 50 for k
#Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
#  A term has fewer unique covariate combinations than specified maximum degrees of freedom
tube_model_k5 <- gam ( rootCountTube ~ s(Day, k=5),
                       data = New_RootsByTube_NoObs1,
                       # family = 'gaussian' #default, type of conditional distribution
                       # bs ='tp'            #default, basis (type of smoother) is low-rank thin plate spline
                       method = "REML")
summary(tube_model_k5)

# > summary(tube_model_k5)
# 
# Family: gaussian 
# Link function: identity 
# 
# Formula:
#   rootCountTube ~ s(Day, k = 5)
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   18.704      4.119   4.541 1.52e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df    F p-value   
# s(Day) 3.235  3.685 4.65 0.00161 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.146   Deviance explained = 17.2%
# -REML = 556.02  Scale est. = 1832.5    n = 108

# k = 10 better than , higher Ref.df and deviance explained?

#Check GAM produced
# Slide 64/132
gam.check(tube_model_k10)

# #Check GAM produced
# > # Slide 64/132
#   > gam.check(tube_model_k10)
# 
# Method: REML   Optimizer: outer newton
# full convergence after 6 iterations.
# Gradient range [-2.630855e-08,7.888583e-10]
# (score 555.8641 & scale 1813.557).
# Hessian positive definite, eigenvalue range [0.7776613,53.03742].
# Model rank =  10 / 10 
# 
# Basis dimension (k) checking results. Low p-value (k-index<1) may
# indicate that k is too low, especially if edf is close to k'.
# 
#         k' edf k-index p-value
# s(Day) 9.0 3.8       1    0.34

## End Analysis with combined years

## Start Analysis with entire interval
#guess a value for k, use same value as above, i.e k = 10

#New_RootsByTube_NoObs1<-rename(New_RootsByTube_NoObs1, Appeared_Date = Appeared.Date)
#Appeared.Date and Appeared_Date failed, so use Appear (i.e observation number for now)

tube_model_k10_entire <- gam ( rootCountTube ~ s(Appeared, k=10),
                        data = New_RootsByTube_NoObs1,
                        # family = 'gaussian' #default, type of conditional distribution
                        # bs ='tp'            #default, basis (type of smoother) is low-rank thin plate spline
                        method = "REML")
summary(tube_model_k10_entire)
# summary(tube_model_k10_entire)
# 
# Family: gaussian 
# Link function: identity 
# 
# Formula:
#   rootCountTube ~ s(Appeared, k = 10)
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   18.704      3.744   4.996 2.47e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df     F  p-value    
# s(Appeared) 6.42  7.563 6.116 7.44e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.294   Deviance explained = 33.7%
# -REML = 551.26  Scale est. = 1513.6    n = 108

# Quickly see plot
#ToDo function to use ggplot)
#   or gavin uses library('gratia'); draw(model, scales= fixed) #for complex model
plot(tube_model_k10_entire , se=TRUE)  #?Not plotting?

gam.check(tube_model_k10_entire)
# Method: REML   Optimizer: outer newton
# full convergence after 6 iterations.
# Gradient range [-7.77763e-08,-3.950025e-09]
# (score 551.2625 & scale 1513.603).
# Hessian positive definite, eigenvalue range [1.103848,53.1415].
# Model rank =  10 / 10 
# 
# Basis dimension (k) checking results. Low p-value (k-index<1) may
# indicate that k is too low, especially if edf is close to k'.
# 
#               k'  edf k-index p-value
# s(Appeared) 9.00 6.42    1.22    0.99

## End Analysis with entire interval

#Note Difference in "Deviance explanied", probably due to difference in years,
# how to adjust model for year?, normalize?
# Tried s(Day, k=10) + s(Year, k=1)  does not work

