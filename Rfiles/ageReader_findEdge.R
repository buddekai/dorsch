# Funktion um den Rand des Otolithen zu finden.

findEdge <- function(image.information, image.grey,
                      start.pixel, end.pixel){
  
  # Endparameter
  
  parameter.for.end <- 1.5
  
  results <- getLineIndices(start.x = start.pixel[2],
                            start.y = start.pixel[1],
                            end.x = end.pixel[2],
                            end.y = end.pixel[1])
  
  moving.average <- image.grey[start.pixel[1], start.pixel[2]]
  
  end <- dim(results)[1]
  
  i <- 1
  
  current.point <- image.grey[results[i, 2], results[i, 1]]
  test <- current.point > moving.average / parameter.for.end
  
  while(test){
    i <- i+1
    current.point <- image.grey[results[i, 2], results[i, 1]]
    
    moving.average <- (3*moving.average + current.point)/4

    test <- current.point > moving.average / parameter.for.end
    if(i == end){
      test <- FALSE
    }
  }
  
  result <- c(results[i, 2], results[i, 1])
  
  return(result)
}