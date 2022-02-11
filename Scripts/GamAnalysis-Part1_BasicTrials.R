####Filename: GamAnalysis_Part1_BasicTrials
#   Date:2022-02-03
#
#

# print to print comments when sourced in chunk in .Rmd file
print("Raw Data with NAs and initial observation included")
summary(raw_roots_data)


# Start Use GAM method for smoothing
# First just use gam as parameter to geom_smooth to see type of curve, from:
#      https://ggplot2.tidyverse.org/reference/geom_smooth.html
#      method = gam, formula = y ~ s(x, bs = cs
#      basis functions (bs) are cubic plines (cs)

cat("Start Use GAM method for smoothing\n
First just use gam as parameter to geom_smooth to see type of curve, from:\n
     https://ggplot2.tidyverse.org/reference/geom_smooth.html\n
     method = gam, formula = y ~ s(x, bs = cs\n
     basis functions (bs) are cubic plines (cs)\n\n")

# By Tube
summary(New_RootsByTube_NoObs1)  #Use to check data is as expected

#par(mfrow = c(1,2))  # set plots to 1 row, 2 cols #Causes failure in RM for
par(mfrow = c(1,1))  # set plots to 1 row, 2 cols

jpeg("Analysis/Images/GAM-New_roots_Entire_Interval-Tube")
my_plottube1 <- ggplot(data=New_RootsByTube_NoObs1, mapping =aes(x =Appeared.Date,  y=rootCountTube)) +
  geom_point(mapping = aes(color=Tube )) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))+
  ggtitle(" Geom_smooth GAM with default parameters - Tubes By Date")
dev.off()

jpeg("Analysis/Images/GAM-New_roots_Annual_Tube")
my_plottube2 <-ggplot(data=New_RootsByTube_NoObs1, mapping =aes(x =Day,  y=rootCountTube)) +
  geom_point(mapping = aes(color=Tube )) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  ggtitle(" Geom_smooth with default parameters - Tubes By Day of Year")
dev.off()

print(my_plottube1)
print(my_plottube2)

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
  ggtitle("  Geom_smoothGAM with default parameters - All By Date")
dev.off()

jpeg("Analysis/Images/GAM-New_roots_Annual")
my_plotdate2 <- ggplot(data=New_RootsByDate, mapping =aes(x =Day,  y=rootCount)) +
  #geom_point(mapping = aes(color=Window )) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  ggtitle(" Geom_smooth GAM with default parameters - All By Day of Year")
dev.off()

print(my_plotdate1 )
print(my_plotdate2 )
cat("End initial GAM using geom_smooth with bs = cs\n--------------------------\n-------------------\n\n\n")
#End initial GAM using geom_smooth with bs = 'cs'


### 2/2/2022 
  #For now 2/10/22 get both cat code and output, do not spend more time to resolve, edit in word
#Start analysis using Gavin video,# Slide 44/132
cat("Use code based on Intro to GAM with T and mgcv Gavin Simpson video, Slide 44/132\n
       Default parameters for gam():\n
          family = gaussian,  type of conditional distribution\n
          bs =tp   basis (type of smoother) is low-rank thin plate spline:
          Instead of giving a smoothing parameter value, sp=
             Set method to REML, ie.  Smoothing via restricted maximum likelihood\n\n")

## Start Analysis with combined years

tube_model_default_k <- gam ( rootCountTube ~ s(Day),  #ksets number of basis functions, i.e nodes
                        data = New_RootsByTube_NoObs1,
                        # family = 'gaussian' #default, type of conditional distribution
                        # bs ='tp'            #default, basis (type of smoother) is low-rank thin plate spline
                        method = "REML")  #Smoothing via restricted maximum likelihood, 
                                          # instead of giving a smoothing parameter value, "sp="
print(coef(tube_model_default_k))  # shows value of k was 10
cat("coef output above shows that REML resulted in a k = 10\n\n")

#Originally guess a value for k,now just verify output when k = 10 matches default
tube_model_k10 <- gam ( rootCountTube ~ s(Day, k=10),  #ksets number of basis functions, i.e nodes
                        data = New_RootsByTube_NoObs1,
                        # family = 'gaussian' #default, type of conditional distribution
                        # bs ='tp'            #default, basis (type of smoother) is low-rank thin plate spline
                        method = "REML")  #Smoothing via restricted maximum likelihood, 
                                          # instead of giving a smoothing parameter value, "sp="

summary(tube_model_k10) #See Rmarkdown ouput word file
#End analysis using Gavin video,# Slide 44/132
# Continue with models created above

# Start follow code in Noam Ross tutorial, chapters 1-8 
#   Use model created from Gavin video above
#   Use coef() to see k used by REML, plot to see curve from REML
#   Do example of changing smoothing parameter value to see effect on curve fitting
#      Look up sp output value from REML (0.45), and compare with sp 0.1 and sp 0.0001
#      

cat("Start follow code in Noam Ross tutorial, chapters 1-8 \n
  Use model created from Gavin video above\n
  Use coef() to see k used by REML, plot to see curve from REML\n
  Do example of changing smoothing parameter value to see effect on curve fitting\n
     Look up sp output value from REML (0.45), and compare with sp 0.1 and sp 0.0001\n\n")

coef(tube_model_k10)  #Extract model coefficiets, intercept and 9 coefficients
# Quickly see plot
#ToDo function to use ggplot)
#   or gavin uses library('gratia'); draw(model, scales= fixed) #for complex model
plot(tube_model_k10,main = "GAM, method = REML, k=10, sp = 0.448", residuals = TRUE, pch=1,  se=TRUE)

# Look at effect of smoothing parameter (s)  also g=called lambda?
tube_model_k10$sp      #
# s(Day) 
# 0.4480527

#Vary sp 0.1  and 0.0001
tube_model_sp0_1k <- gam ( rootCountTube ~ s(Day),data = New_RootsByTube_NoObs1,
                               sp=0.1,
                               method = "REML")
tube_model_sp0_0001k <- gam ( rootCountTube ~ s(Day),data = New_RootsByTube_NoObs1,
                           sp=0.0001,
                           method = "REML")

plot(tube_model_sp0_1k, main = "Sp = 0.1",  residuals = TRUE, pch=2,  se=TRUE)
plot(tube_model_sp0_0001k, main = "Sp = 0.0001",residuals = TRUE, pch=2,  se=TRUE)

par(mfrow = c(1, 1)) # for laptop
# The number of basis functions and the smoothing parameters interact 
# to control the wiggliness of a smooth function. i.e can use both sp= & k =

cat ("End follow code in Noam Ross tutorial, chapters 1-8\n----------------\n----------------\n\nn")
######  End follow code in Noam Ross tutorial, chapters 1-8

#Start continuation of analysis using Gavin video,# Slide xx/132 
# Look at varying k values, (only k = 5 since limited data cannot use large k values\n
# Use gam.check() to visually see quality of model based on Day(Annual) vs Appeared (entire interval)

cat("Start continuation of analysis using Gavin video,# Slide xx/132\n
Look at varying k values, only k = 5 since limited data cannot use large k values\n
Use gam.check() to visually see quality of model based on Day(Annual) vs Appeared (entire interval)\n
\n----------------\n----------------\n\n")

#Get following error when use values of 20, 50 for k
#Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
#  A term has fewer unique covariate combinations than specified maximum degrees of freedom

tube_model_k5 <- gam ( rootCountTube ~ s(Day, k=5),
                       data = New_RootsByTube_NoObs1,
                       # family = 'gaussian' #default, type of conditional distribution
                       # bs ='tp'            #default, basis (type of smoother) is low-rank thin plate spline
                       method = "REML")
summary(tube_model_k5)
coef(tube_model_k5) 


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

