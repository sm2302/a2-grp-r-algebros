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
