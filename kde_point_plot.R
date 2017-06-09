library(MASS)
library(ggplot2)
library(KernSmooth)
library(reshape2)
# 
# This scripts contains functions to create scatter plots with the points colored 
# by the kernel density estimation (KDE). The umbrella function is kde_point_plot(). Call the 
# function with the X and Y vectors of the points as the minimal arguments. The optional arguments
# are: 
#   x_bins (positive integer) (default = 250)  The number of bins to divide the x values into. A higher number gives a smoother coloring 
#   y_bins (positive integer) (default = 250)  The number of bins to divide the y values into. A higher number gives a smoother coloring 
#   log    (TRUE or FALSE) (default = FALSE) If TRUE, the log is taken of each input vector
#   shape (Standard ggplot shapes) (default = 20) The shape of the data points
#   size (positive number) (default = 1) The size of the data point in mm  
#   low_color (color name or value) (default = "black") The color to represent the least dense points
#   high_color (color name or value) (default = "red") The color to represent the most dense points
  
  
# The highest level function for creating the plot. Returns a ggplot object of the plot
kde_point_plot <- function(x, y, x_bins=250, y_bins=250, log=FALSE, 
                           shape=20, size=1, low_color="white", mid_color="black", high_color="red"){
  
  # Create a dataframe with the data vectors
  new_df <- point_kde(x=x,y=y,x_bins=x_bins,y_bins=y_bins,log=log)
  
  # Create the ggplot object
  p <- ggplot(data = new_df, mapping=aes(x = x, y = y, color=z)) + 
    geom_point(shape=shape, size=size) + 
    scale_color_gradient2(low = low_color, mid=mid_color, high = high_color)
  return(p)
} 

# Creates a KDE heatmap
kde_heatmap <- function(x,y, x_bins=250, y_bins=250, log=FALSE, low_color="black", mid_color="black", high_color="red"){
  # Create the dataframe
  df <- make_xy_df(x=x,y=y,log=log)
  # Create the bins
  bins <- make_bins(x=df$x, y=df$y, x_bins=x_bins, y_bins=y_bins)  
  # Flatten the density grid
  melted_density <- melt(bins$fhat)
  # Make the heatmap
  p <- ggplot(data=melted_density, mapping = aes(x = Var1, y=Var2, fill=value)) + 
    scale_fill_gradient2(low = low_color, mid=mid_color, high = high_color)+ 
    geom_tile()
  return(p)
}


# identifies the density estimate for each bin  
grid_lookup <- function(x, y, grid){
  # Create the empty results list
  results <- c()
  
  # For each point, get the x and y index of the density data 
  for (i in 1:length(x)){
    x_coord <- x[i]
    y_coord <- y[i]
    # Append the density to the results list
    results <- c(results, grid[x_coord, y_coord])
  }
  return(results)
}

point_kde <- function(x,y,x_bins=100, y_bins=100, log=FALSE){
  df <- make_xy_df(x=x,y=y,log=log)
  # Make the bins
  bins <- make_bins(x=df$x, y=df$y, x_bins=x_bins, y_bins=y_bins)  
  # For each datapoint, identify the x and y bin number
  df$x_bin <- findInterval(x = df$x, vec = bins$x1, left.open = TRUE)
  df$y_bin <- findInterval(x = df$y, vec = bins$x2, left.open = TRUE)
  # Lookup the value for each data point and add the result as the z column
  df$z <- grid_lookup(df$x_bin,df$y_bin, bins$fhat)
  return(df)
}

make_bins <- function(x, y, x_bins, y_bins){
  # Create a list of the bandwith values
  xy_bandwiths <- c(x_bandwith=bw.nrd(x), y_bandwith=bw.nrd(y))
  print(xy_bandwiths)
  # Create the bins with the density values
  bins <- bkde2D(x = data.frame(x=x,y=y), bandwidth = xy_bandwiths, gridsize = c(x_bins,y_bins))
  return(bins)
}

make_xy_df <- function(x,y,log=FALSE){
  # If log is false, create dataframe with raw data
  if(log==FALSE){df <- data.frame(x=x,y=y)}
  # If log is true, take the log of the data before adding to dataframe
  if(log==TRUE){df <- data.frame(x=log(x),y=log(y))}
  return(df)
}

