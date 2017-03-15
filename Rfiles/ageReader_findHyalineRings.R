# Funktion um hyaline Ringe zu finden

findHyalineRings <- function(image.grey, image.information,
                             first.point, second.point){
  
  
  # Parameter Sektion ####
  parameter.for.hyaline <- 1.1
  
  # bis hier
  
  
  connecting.line <- getLineIndices(start.x = first.point[1],
                                    start.y = first.point[2],
                                    end.x = second.point[1],
                                    end.y = second.point[2])
  
  # Mittelwert des Bildes entlang der Linie
  connecting.line.mean <- mean(image.grey[connecting.line[,2],
                                          connecting.line[,1]])
  
  number.of.points.along.line <- dim(connecting.line)[1]
  df.connecting.line <- data.frame(connecting.line)
  names(df.connecting.line) <- c("x", "y")
  df.connecting.line$hyaline <- 0
  
  for(i in 1:number.of.points.along.line){
    
    current.point <- image.grey[connecting.line[i,2],
                                connecting.line[i,1]]
    
    if(current.point > parameter.for.hyaline * connecting.line.mean){
      df.connecting.line$hyaline[i] <- 1
    }
  }
  
  hyline.points <- connecting.line[which(df.connecting.line$hyaline==1),]
  
  image.information[cbind(hyline.points[,2], hyline.points[,1], 3)] <- -1
  image.information[cbind(hyline.points[,2], hyline.points[,1], 2)] <- -1
  image.information[cbind(hyline.points[,2], hyline.points[,1], 1)] <- 1
  
  return(image.information)
  
}