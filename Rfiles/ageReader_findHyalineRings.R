# Funktion um hyaline Ringe zu finden

findHyalineRings <- function(image.grey, image.information,
                             first.point, second.point, line.to.follow,
                             parameter.for.hyaline,
                             points.to.jump,
                             repeate.fill.up,
                             min.hyaline.length){
  

  
  # Parameter Sektion ####
  #parameter.for.hyaline <- 1.1
  #points.to.jump <- 5
  #wie oft soll versucht werden die Lücken zuschliessen:
  #repeate.fill.up <- 2
  # Wie lang soll eine zusammenhaengende Markierung mindestens sein?
  #min.hyaline.length <- 48
  
  # Parameter bis hier
  
  if(missing(line.to.follow)){
    connecting.line <- getLineIndices(start.x = first.point[1],
                                      start.y = first.point[2],
                                      end.x = second.point[1],
                                      end.y = second.point[2])
  }else{
    index.of.line.start <- which(first.point[1] == line.to.follow[,1])
    index.of.line.end <- which(second.point[1] == line.to.follow[,1])
    
    if(index.of.line.start < index.of.line.end){
      connecting.line <-
        line.to.follow[index.of.line.start:index.of.line.end,]
    }else{
      connecting.line <-
        line.to.follow[index.of.line.end:index.of.line.start,]
    }
    
    # Reihenfolge der Werte umkehren, damit immer von der Mitte aus
    # gerechnet wird.
    connecting.line[,1] <- rev(connecting.line[,1])
    connecting.line[,2] <- rev(connecting.line[,2])
  }
  
  # Farbwerte entlang der Linie mitteln
  
  image.grey <- smoothGreyLine(image.grey = image.grey,
                               connecting.line = connecting.line)
  
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
  
  
  # Versuche Luecken aufzufuellen
  
  while(repeate.fill.up > 0){
    points.to.jump.copy <- points.to.jump
    
    while(points.to.jump > 0){
      pattern <- c(1, rep(0, points.to.jump), 1)
      pattern <- paste(pattern, collapse = "")
      
      replacement <- c(1, rep(1, points.to.jump), 1)
      replacement <- paste(replacement, collapse = "")
      
      hyaline.vector <- paste(df.connecting.line$hyaline, collapse = "")
      
      hyaline.vector <- gsub(pattern, replacement, hyaline.vector)
      
      df.connecting.line$hyaline <- 
        as.numeric(unlist(strsplit(hyaline.vector, split = "")))
      
      points.to.jump <- points.to.jump - 1
    }
    
    points.to.jump <- points.to.jump.copy
    repeate.fill.up <- repeate.fill.up - 1
  }
  
  marker <- NULL
  # Loesche alle Markierungen die kuerzer sind als der Parameter
  for(i in 1:length(df.connecting.line$hyaline)){
    if(df.connecting.line$hyaline[i] == 1){
      marker <- c(marker, i)
    }else{
      marker <- NULL
    }
    
    if(length(marker) >= min.hyaline.length ){
      df.connecting.line$hyaline[marker] <- 3
    }
  }
  
  df.connecting.line$hyaline[df.connecting.line$hyaline != 3] <- 0
  df.connecting.line$hyaline[df.connecting.line$hyaline == 3] <- 1
  
  # Loesche die kuerze zweier Markierungen, die zu dicht beieinanderliegen
  df.connecting.line$ring <- 0
  ring <- 1
  if(df.connecting.line$hyaline == 1){
    ring.change <- TRUE
  }else{
    ring.change <- FALSE
  }
  
  
  for(i in 1:length(df.connecting.line$ring)){
    
    if(df.connecting.line$hyaline[i] == 0){
      df.connecting.line$ring[i] <- 0
      
      if(ring.change == TRUE){
        ring.change <- FALSE
        ring <- ring + 1
      }
      
    }else{
      df.connecting.line$ring[i] <- ring
      
      if(ring.change == FALSE){
        ring.change <- TRUE
      }
    }
  }
  
  if(df.connecting.line$hyaline[length(df.connecting.line$ring)] == 1){
    number.of.ring.gaps <- ring-1
  }else{
    number.of.ring.gaps <- ring-2
  }
  
  if(number.of.ring.gaps > 1){
    for(i in 1:number.of.ring.gaps){
     first.ring.length <- sum(df.connecting.line$ring == i)
     second.ring.length <- sum(df.connecting.line$ring == i+1)
     
     rows.of.first <- which(df.connecting.line$ring == i)
     rows.of.second <- which(df.connecting.line$ring == i+1)
     
     rows.in.between <-
       rows.of.second[1] - rows.of.first[length(rows.of.first)] - 1
     
     # Loesche den duenneren Ring, falls der Abstand zu klein ist.
     if(rows.in.between < min.hyaline.length){
       if(length(rows.of.first) < length(rows.of.second)){
         df.connecting.line$hyaline[df.connecting.line$ring == i] <- 0
       }else{
         df.connecting.line$hyaline[df.connecting.line$ring == i+1] <- 0
       }
       
     }
    }
  }
  
  
  # Loesche alle Ringe, die zu nah am Start- oder Endpunkt sind
  df.connecting.line$ring <- 0
  ring <- 1
  if(df.connecting.line$hyaline == 1){
    ring.change <- TRUE
  }else{
    ring.change <- FALSE
  }
  
  
  for(i in 1:length(df.connecting.line$ring)){
    
    if(df.connecting.line$hyaline[i] == 0){
      df.connecting.line$ring[i] <- 0
      
      if(ring.change == TRUE){
        ring.change <- FALSE
        ring <- ring + 1
      }
      
    }else{
      df.connecting.line$ring[i] <- ring
      
      if(ring.change == FALSE){
        ring.change <- TRUE
      }
    }
  }
  
  max.ring <- max(df.connecting.line$ring)
  
  if(max.ring > 1){
    
    # Loesche ggf. in der Naehe des Mittelpunktes
    rows.of.first <- which(df.connecting.line$ring==1)
    if(rows.of.first[1] < min.hyaline.length){
      df.connecting.line$hyaline[rows.of.first] <- 0
    }
    
    # Loesche ggf. in der Naehe des Endpunktes
    last.row <- length(df.connecting.line$hyaline)
    rows.of.last <- which(df.connecting.line$ring==max.ring)
    if(rows.of.last[length(rows.of.last)] + min.hyaline.length > last.row){
      df.connecting.line$hyaline[rows.of.last] <- 0
    }
    
  }
  
  # Gehe noch einmal alle Ringe durch und nummeriere diese
  
  df.connecting.line$ring <- 0
  ring <- 1
  if(df.connecting.line$hyaline == 1){
    ring.change <- TRUE
  }else{
    ring.change <- FALSE
  }
  
  
  for(i in 1:length(df.connecting.line$ring)){
    
    if(df.connecting.line$hyaline[i] == 0){
      df.connecting.line$ring[i] <- 0
      
      if(ring.change == TRUE){
        ring.change <- FALSE
        ring <- ring + 1
      }
      
    }else{
      df.connecting.line$ring[i] <- ring
      
      if(ring.change == FALSE){
        ring.change <- TRUE
      }
    }
  }
  
  
  # Fuege die Information (hyaline Ringe) zu image.information hinzu
  
  hyline.points <- connecting.line[which(df.connecting.line$hyaline==1),]
  
  image.information[cbind(hyline.points[,2], hyline.points[,1], 3)] <- -1
  image.information[cbind(hyline.points[,2], hyline.points[,1], 2)] <- -1
  image.information[cbind(hyline.points[,2], hyline.points[,1], 1)] <- 1
  
  
  
  
  return(image.information)
  
}