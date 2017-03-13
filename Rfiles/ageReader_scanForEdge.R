
scanForEdge <- function(image.grey, image.information, distance){
  
  start <- dim(image.grey)/2
  
  
  i <- 1
  counter <- 1
  
  # linke Kante nach unten gehen
  while(i <= dim(image.grey)[1]){
    
    edge[counter] <- findEdge2(image.information = image.information,
              image.grey = image.grey,
              start.pixel = start,
              end.pixel = c(i,1))
    
    i <- i + distance
    counter <- counter + 1
    print(counter)
  }
  
  
}