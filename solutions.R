library(animation);
library(plotrix);


line.length <- function(points){
  return(norm(points[1,]-points[2,], type="2"));
}


draw.chord <- function(points, threshold){
  color <- "blue";
  chord.length <- line.length(points);
  longer <- chord.length > threshold;
  if(longer){
    color <- "red";
  }
  lines(points[,1], points[,2], col=color);
  
  return(longer);
}


draw.chord.by.midpoint <- function(center.x, center.y, radius,
                                   midpoint.x, midpoint.y){
  
  side.length <- sin(pi/3)*radius*2;
  
  len <- sqrt((midpoint.x-center.x)^2+(midpoint.y-center.y)^2);
  theta <- atan2(midpoint.y-center.y, midpoint.x-center.x);
  
  chord.halflength = sqrt(radius^2-len^2);
  theta1 <- theta + pi/2;
  theta2 <- theta1 + pi;
  
  chord.endpoint1.x <- midpoint.x + cos(theta1)*chord.halflength;
  chord.endpoint1.y <- midpoint.y + sin(theta1)*chord.halflength;
  chord.endpoint2.x <- midpoint.x + cos(theta2)*chord.halflength;
  chord.endpoint2.y <- midpoint.y + sin(theta2)*chord.halflength;
  
  points <- matrix(c(chord.endpoint1.x, chord.endpoint2.x,
                     chord.endpoint1.y, chord.endpoint2.y), nrow=2, ncol=2)
  
  return(draw.chord(points, side.length));
}

method.one <- function(center.x, center.y, radius){
  endpoint1.theta <- runif(1,0,2*pi);
  endpoint2.theta <- runif(1,0,2*pi);
  endpoint1.x = center.x + radius * cos(endpoint1.theta);
  endpoint1.y = center.y + radius * sin(endpoint1.theta);
  endpoint2.x = center.x + radius * cos(endpoint2.theta);
  endpoint2.y = center.y + radius * sin(endpoint2.theta);
  midpoint.x = ( endpoint1.x + endpoint2.x )/2;
  midpoint.y = (endpoint1.y + endpoint2.y)/2;
  return(c(midpoint.x,midpoint.y));
}


method.two <- function(center.x, center.y, radius){
  radius.theta = runif(1,0,2*pi);
  d.from.center = runif(1,0,radius);
  midpoint.x = center.x + d.from.center * cos(radius.theta);
  midpoint.y = center.y + d.from.center  * sin(radius.theta);
  
  return(c(midpoint.x,midpoint.y));
}