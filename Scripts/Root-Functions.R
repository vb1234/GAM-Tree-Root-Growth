####
#  Function: my_plot_smooth_gam_fun
#   Create plot using geom_smooth args,ie gam
#  Args:
#   my_data - data.frame with x,y,column for color
#   my_x - column name in my_data for x axis
#   my_y - column name in my_eata for y axix
#   my_basis - type of splines
#   my_aes_color - column name of variable to color points
#   my_title - title for plot, enclosed in quotes
#   my_filename - filepath/name for saving plot in quotes
#  Example:
#    my_plot <- my_plot_smooth_gam_fun(New_RootsByTube_NoObs1,
#                    Day, rootCountTube, "cs",Tube, "Test","my_testfile2.jpeg")
#V. Bonnewell, 02-02-2022 

# 
my_plot_smooth_gam_fun <- function(my_data, my_x, my_y, my_bs, my_aes_color,
                                   my_title, my_filename) { 
  #print(my_data)
  # Need to use quote/unquote to force args to be evaluated as data column names
  my_data <- my_data %>% mutate(my_x = UQ(enquo(my_x)), 
                                my_y = UQ(enquo(my_y)),
                                my_aes_color= UQ(enquo(my_aes_color))
  )
  #print(my_data)
  # p1 <- ggplot(data=my_data, mapping =aes(x =my_x,  y=my_y)) +
  p1 <- ggplot(data=my_data, aes(x = my_x,  y=my_y)) +  
    geom_point(mapping = aes(color=my_aes_color ))+
    geom_smooth(method = "gam", formula = y ~ s(x, bs = my_bs))+
    ggtitle(my_title)
  ggsave(my_filename, plot=p1)
  return(p1)
}

####
#  Function 
#  Name:my_plot_smooth_loess_fun2
#   Create plot using default geom_smooth,ie loess, span 0.75, CI 0.95
#  Args:
#   my_data - data.frame with x,y,column for color
#   my_x - column name in my_data for x axis
#   my_y - column name in my_eata for y axix
#   my_aes_color - column name of variable to color points
#   my_title - title for plot, enclosed in quotes
#   my_filename - filepath/name for saving plot
#  Example:
#    my_plot <- my_smooth_loess_fun(New_RootsByTube_NoObs1,
#                    Day, rootCountTube, Tube, "Test","my_testfile2.jpeg")

my_plot_smooth_loess_fun2 <- function(my_data, my_x, my_y, my_aes_color, 
                                     my_title, my_filename) { 
  #print(my_data)
  # Need to use quote/unquote to force args to be evaluated as data column names
  my_data <- my_data %>% mutate(my_x = UQ(enquo(my_x)), 
                                my_y = UQ(enquo(my_y)),
                                my_aes_color= UQ(enquo(my_aes_color))
  )
  #print(my_data)
  # p1 <- ggplot(data=my_data, mapping =aes(x =my_x,  y=my_y)) +
  p1 <- ggplot(data=my_data, aes(x = my_x,  y=my_y)) +  
    # geom_point()
    geom_point(mapping = aes(color=my_aes_color ))+
    geom_smooth()+
    ggtitle(my_title)
  ggsave(my_filename, plot=p1)
  return(p1)
}


##Example call to function
# my_plot <- my_plot_smooth_loess_fun2(New_RootsByTube_NoObs1,
#                                     Day, 
#                                     rootCountTube,
#                                     Tube,
#                                     "Test",
#                                     "my_testfile.jpeg"
# )
# jpeg("my_testfile2")  #Works to write to file outside of function
# plot(my_plot)  #Does not plot to Rstudio Plot window on mac, so may as well writeto file in function
# dev.off()

########End Test function

