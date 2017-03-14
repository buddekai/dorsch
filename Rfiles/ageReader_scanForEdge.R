# Diese FUnktion beginnt vom Mittelpunkt des Bildes 


scanForEdge <- function(image.grey, image.information, distance){
  
  # Parameter für mittleren Abstandsabweichung
  
  parameter.distance.deviation <- 2
  
  
  
  
  start <- dim(image.grey)/2
  
  edge <- c(0,0)
  #counter <- 1
  
  # obere Kante nach rechts gehen
  i <- 1
  while(i <= dim(image.grey)[2]){
    
    edge <- rbind(edge, findEdge(image.information = image.information,
                                 image.grey = image.grey,
                                 start.pixel = start,
                                 end.pixel = c(1,i)))
    
    i <- i + distance
    #counter <- counter + 1
    #print(counter)
  }
  
  # rechte Kante nach unten gehen
  i <- 1
  last.pixel.right <- dim(image.grey)[2]
  
  while(i <= dim(image.grey)[1]){
    
    edge <- rbind(edge, findEdge(image.information = image.information,
                                 image.grey = image.grey,
                                 start.pixel = start,
                                 end.pixel = c(i,last.pixel.right)))
    
    i <- i + distance
    #counter <- counter + 1
    #print(counter)
  }
  
  # untere Kante nach links gehen
  i <- last.pixel.right
  
  last.pixel.bottom <- dim(image.grey)[1]
  
  while(i >= 1){
    
    edge <- rbind(edge, findEdge(image.information = image.information,
                                 image.grey = image.grey,
                                 start.pixel = start,
                                 end.pixel = c(last.pixel.bottom,i)))
    
    i <- i - distance
    #counter <- counter + 1
  }
  
  # linke Kante nach oben gehen
  i <- last.pixel.bottom
  
  while(i > 1){
    
    edge <- rbind(edge, findEdge(image.information = image.information,
                                 image.grey = image.grey,
                                 start.pixel = start,
                                 end.pixel = c(i,1)))
    
    i <- i - distance
    #counter <- counter + 1
    #print(counter)
  }
  
  
  edge <- edge[-1,]
  edge <- unname(edge)
  
  # Loesche alle Ausreisser
  # bestimmE dafür den Abstand eines Punkte zum nächsten
  
  number.of.points <- dim(edge)[1]
  
  df.edge <- data.frame(edge)
  names(df.edge) <- c("x1", "y1")
  
  df.edge$x2 <- c(df.edge$x1[2:number.of.points], df.edge$x1[1])
  df.edge$y2 <- c(df.edge$y1[2:number.of.points], df.edge$y1[1])
  
  df.edge$dist <- (df.edge$x2-df.edge$x1)^2+(df.edge$y2-df.edge$y1)^2
  
  mean.distance <- mean(df.edge$dist)
  
  df.edge$outlier <- ifelse(
    test = df.edge$dist > parameter.distance.deviation*mean.distance,
    1, 0)
  
  outliers <- which(df.edge$dist > parameter.distance.deviation*mean.distance)

  outliers <- edge[outliers,]
  
  # Fuer die Farbmarkierungen
  image.information[cbind(edge,1)] <- 1
  image.information[cbind(edge,2)] <- -1
  image.information[cbind(edge,3)] <- -1
  
  image.information[cbind(outliers, 3)] <- 1
  image.information[cbind(outliers, 2)] <- -1
  image.information[cbind(outliers, 1)] <- -1
  
  return(image.information)
}