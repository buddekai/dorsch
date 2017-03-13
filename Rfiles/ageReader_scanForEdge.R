# Diese FUnktion beginnt vom Mittelpunkt des Bildes 


scanForEdge <- function(image.grey, image.information, distance){
  
  start <- dim(image.grey)/2
  
  edge <- c(0,0)
  #counter <- 1
  
  # linke Kante nach unten gehen
  i <- 1
  
  while(i <= dim(image.grey)[1]){
    
    edge <- rbind(edge, findEdge(image.information = image.information,
              image.grey = image.grey,
              start.pixel = start,
              end.pixel = c(i,1)))
    
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
  
  # untere Kante nach rechts gehen
  i <- 1
  last.pixel.bottom <- dim(image.grey)[1]
  
  while(i <= dim(image.grey)[2]){
    
    edge <- rbind(edge, findEdge(image.information = image.information,
                                 image.grey = image.grey,
                                 start.pixel = start,
                                 end.pixel = c(last.pixel.bottom,i)))
    
    i <- i + distance
    #counter <- counter + 1
    #print(counter)
  }
  
  
  
  edge <- edge[-1,]
  edge <- unname(edge)
  
  edge <- cbind(edge,1)
  
  image.information[edge] <- 1
  
  return(image.information)
}