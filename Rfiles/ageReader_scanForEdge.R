# Diese FUnktion beginnt vom Mittelpunkt des Bildes und sucht den Rand.
# Danach zeichnet es das umrahmende Rechteck ins Bild.


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
  names(df.edge) <- c("y1", "x1")
  
  df.edge$y2 <- c(df.edge$y1[2:number.of.points], df.edge$y1[1])
  df.edge$x2 <- c(df.edge$x1[2:number.of.points], df.edge$x1[1])
  
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
  
  
  # Fuege den Rahmen und neuen Mittelpunkt hinzu
  top.y <- min(df.edge$y1[df.edge$outlier==0])
  right.x <- max(df.edge$x1[df.edge$outlier==0])
  bottom.y <- max(df.edge$y1[df.edge$outlier==0])
  left.x <- min(df.edge$x1[df.edge$outlier==0])
  
  top.border <- getLineIndices(start.x = left.x, start.y = top.y,
                              end.x = right.x, end.y = top.y)
  right.border <- getLineIndices(start.x = right.x, start.y = top.y,
                               end.x = right.x, end.y = bottom.y)
  bottom.border <- getLineIndices(start.x = right.x, start.y = bottom.y,
                               end.x = left.x, end.y = bottom.y)
  left.border <- getLineIndices(start.x = left.x, start.y = bottom.y,
                               end.x = left.x, end.y = top.y)
  
  border <- rbind(top.border, right.border, bottom.border, left.border)
  
  image.information[cbind(border[,2], border[,1], 3)] <- -1
  image.information[cbind(border[,2], border[,1], 2)] <- 1
  image.information[cbind(border[,2], border[,1], 1)] <- -1
  
  # Fuege neue Schnittlinien hinzu
  left.point <- c(left.x, df.edge$y1[df.edge$x1 == left.x])
  right.point <- c(right.x, df.edge$y1[df.edge$x1 == right.x])
  
  first.line <- getLineIndices(start.x = left.point[1],
                               start.y = left.point[2],
                               end.x = right.x, end.y = top.y)
  
  second.line <- getLineIndices(start.x = right.point[1],
                               start.y = right.point[2],
                               end.x = left.x, end.y = top.y)
  
  lines <- rbind(first.line, second.line)
  image.information[cbind(lines[,2], lines[,1], 3)] <- -1
  image.information[cbind(lines[,2], lines[,1], 2)] <- 1
  image.information[cbind(lines[,2], lines[,1], 1)] <- -1
  
  return(image.information)
}