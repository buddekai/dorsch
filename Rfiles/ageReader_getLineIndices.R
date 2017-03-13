# Berechne die Punkte um eine "gerade" Linie zu zeichnen, die zwei
# Punkte miteinander verbindet.


getLineIndices <- function(start.x, start.y,
                           end.x, end.y){
  
  # unterscheide folgende Faelle
  # 1. horizontale bzw. 2. vertikale Linien
  # 3. Anstieg zwischen 0 und 1
  # 4. Anstieg groesser als 1
  # 5. Anstieg zwischen 0 und -1
  # 6. Anstieg kleiner als -1
  
  reverse.it <- FALSE
  
  # zu 1.
  if(end.y == start.y){
    points <- matrix(c(start.x:end.x))
    points <- cbind(points, start.y)
    
  }else if(end.x == start.x){ # zu 2.
    points <- matrix(c(start.y:end.y))
    points <- cbind(points, start.x)
    
  }else{
    
    slope <- (end.y - start.y)/(end.x-start.x)
    
    # zu 3.
    if(0 < slope & slope <= 1){
      
      if(end.y - start.y < 0){
        .dumx <- start.x
        .dumy <- start.y
        
        start.x <- end.x
        start.y <- end.y
        
        end.x <- .dumx
        end.y <- .dumy
        
        reverse.it <- TRUE
      }
      
      A <- 2*(end.y-start.y)
      B <- A - 2*(end.x-start.x)
      P <- A-(end.x-start.x)
      points <- c(start.x, start.y)
      current.point <- c(start.x, start.y)
      
      while(!isTRUE(all.equal(current.point,c(end.x,end.y)))){
        if(P < 0){
          current.point <- c(current.point[1]+1,current.point[2])
          points <- rbind(points, current.point)
          P <- A + P
        }else{
          current.point <- c(current.point[1]+1,current.point[2]+1)
          points <- rbind(points, current.point)
          P <- B + P
        }
      }
      
    }
    
    # zu 4.
    if(slope > 1){
      
      if(end.y - start.y < 0){
        .dumx <- start.x
        .dumy <- start.y
        
        start.x <- end.x
        start.y <- end.y
        
        end.x <- .dumx
        end.y <- .dumy
        
        reverse.it <- TRUE
      }
      
      A <- 2*(end.x-start.x)
      B <- A - 2*(end.y-start.y)
      P <- A-(end.y-start.y)
      points <- c(start.x, start.y)
      current.point <- c(start.x, start.y)
      
      while(!isTRUE(all.equal(current.point,c(end.x,end.y)))){
        if(P < 0){
          current.point <- c(current.point[1],current.point[2]+1)
          points <- rbind(points, current.point)
          P <- A + P
        }else{
          current.point <- c(current.point[1]+1,current.point[2]+1)
          points <- rbind(points, current.point)
          P <- B + P
        }
      }
      
    }
    
    # zu 5.
    
    if(slope < 0 & slope >= -1){
      
      if(end.y - start.y > 0){
        .dumx <- start.x
        .dumy <- start.y
        
        start.x <- end.x
        start.y <- end.y
        
        end.x <- .dumx
        end.y <- .dumy
        
        reverse.it <- TRUE
      }
      
      A <- abs(2*(end.y-start.y))
      B <- A - 2*abs(end.x-start.x)
      P <- A - abs(end.x-start.x)
      points <- c(start.x, start.y)
      current.point <- c(start.x, start.y)
      
      while(!isTRUE(all.equal(current.point,c(end.x,end.y)))){
        if(P < 0){
          current.point <- c(current.point[1]+1,current.point[2])
          points <- rbind(points, current.point)
          P <- A + P
        }else{
          current.point <- c(current.point[1]+1,current.point[2]-1)
          points <- rbind(points, current.point)
          P <- B + P
        }
      }
    }
    
    # zu 6.
    if(slope < -1){
      
      if(end.y - start.y > 0){
        .dumx <- start.x
        .dumy <- start.y
        
        start.x <- end.x
        start.y <- end.y
        
        end.x <- .dumx
        end.y <- .dumy
        
        reverse.it <- TRUE
      }
      
      A <- 2*abs(end.x-start.x)
      B <- A - 2*abs(end.y-start.y)
      P <- A-abs(end.y-start.y)
      points <- c(start.x, start.y)
      current.point <- c(start.x, start.y)
      
      while(!isTRUE(all.equal(current.point,c(end.x,end.y)))){
        if(P < 0){
          current.point <- c(current.point[1],current.point[2]-1)
          points <- rbind(points, current.point)
          P <- A + P
        }else{
          current.point <- c(current.point[1]+1,current.point[2]-1)
          points <- rbind(points, current.point)
          P <- B + P
        }
      }

    }
    
  }
  
  if(reverse.it){
    points <- apply(points, 2, rev)
  }
  
  points <- unname(points)
  
  rm(reverse.it, A, B, current.point, P, slope)
  
  return(points)
}
