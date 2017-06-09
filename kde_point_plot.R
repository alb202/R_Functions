library(MASS)
library(ggplot2)

grid_lookup <- function(x, y, grid){
  results <- c()
  for (i in 1:length(x)){
    x_coord <- x[i]
    y_coord <- y[i]
    results <- c(results, grid[x_coord, y_coord])
  }
  return(results)
}

point_kde <- function(x,y,x_bins=100, y_bins=100, log=FALSE){
  if(log==FALSE){df <- data.frame(x=x,y=y)}
  if(log==TRUE){df <- data.frame(x=log(x),y=log(y))}
  xy_bandwiths <- c(x_bandwith=bw.nrd(df$x), y_bandwith=bw.nrd(df$y))
  print(xy_bandwiths)
  bins <- bkde2D(x = df, bandwidth = xy_bandwiths, gridsize = c(300,300))
  df$x_bin <- findInterval(x = df$x, vec = bins$x1, left.open = TRUE)
  df$y_bin <- findInterval(x = df$y, vec = bins$x2, left.open = TRUE)
  df$z <- grid_lookup(df$x_bin,df$y_bin, bins$fhat)
  return(df)
}

kde_point_plot <- function(x, y, x_bins=250, y_bins=250, log=FALSE, shape=20, size=1, low_color="black", high_color="red"){
  new_df <- point_kde(x=x,y=y,x_bins=x_bins,y_bins=y_bins,log=log)
  p <- ggplot(data = new_df, mapping=aes(x = x, y = y, color=z)) + 
    geom_point(shape=shape, size=size) + 
    scale_color_continuous(low = low_color, high = high_color)
  return(p)
} 