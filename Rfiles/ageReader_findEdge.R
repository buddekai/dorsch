# Funktion um den Rand des Otolithen zu finden.

findEdge <- function(image.information, image.grey,
                      start.pixel, end.pixel){
  
  # Endparameter
  
  parameter.for.end <- 1.5
  
  start.pixel = start
  end.pixel = c(i,1)
  
  results <- getLineIndices(start.x = start.pixel[2],
                            start.y = start.pixel[1],
                            end.x = end.pixel[2],
                            end.y = end.pixel[1])
  
  moving.average <- image.grey[start.pixel[2], start.pixel[1]]
  
  end <- dim(results)[1]
  
  i <- 1
  current.point <- image.grey[results[i, 2], results[i, 1]]
  test <- current.point > moving.average / parameter.for.end
  
  while(test){
    i <- i+1
    current.point <- image.grey[results[i, 2], results[i, 1]]
    
    moving.average <- (3*moving.average + current.point)/4
    print(i)
    print(current.point)
    print(moving.average)
    print(results[i, 2])
    print(results[i, 1])
    test <- current.point > moving.average / parameter.for.end
  }
  
  result <- c(results[i, 1], results[i, 2])
  return(result)
  
  
}