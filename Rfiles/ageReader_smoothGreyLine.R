# Funktion um die Farbwerte entlang einer Linie zu glaetten.

smoothGreyLine <- function(image.grey = image.grey,
                           connecting.line = connecting.line){
  
  #image.grey[connecting.line[,2], connecting.line[,1]] <- 
  #  0.5  * image.grey[connecting.line[,2], connecting.line[,1]] +
  #  0.25 * image.grey[connecting.line[,2]-1, connecting.line[,1]] +
  #  0.25 * image.grey[connecting.line[,2]+1, connecting.line[,1]]
  
  image.grey[connecting.line[,2], connecting.line[,1]] <- 
    (image.grey[connecting.line[,2], connecting.line[,1]] +
       image.grey[connecting.line[,2]-1, connecting.line[,1]] +
       image.grey[connecting.line[,2]+1, connecting.line[,1]]) / 3
  
  #image.grey[connecting.line[,2], connecting.line[,1]] <- 
  #  (image.grey[connecting.line[,2], connecting.line[,1]] +
  #     image.grey[connecting.line[,2]-1, connecting.line[,1]] +
  #     image.grey[connecting.line[,2]+1, connecting.line[,1]] +
  #     image.grey[connecting.line[,2]-2, connecting.line[,1]] +
  #     image.grey[connecting.line[,2]+2, connecting.line[,1]] +
  #     image.grey[connecting.line[,2]-3, connecting.line[,1]] +
  #     image.grey[connecting.line[,2]+3, connecting.line[,1]] +
  #     image.grey[connecting.line[,2]-4, connecting.line[,1]] +
  #     image.grey[connecting.line[,2]+4, connecting.line[,1]] +
  #     image.grey[connecting.line[,2]-5, connecting.line[,1]] +
  #     image.grey[connecting.line[,2]+5, connecting.line[,1]]) / 11
  
  #image.grey[connecting.line[,2], connecting.line[,1]] <- 
  #  (0.2 * image.grey[connecting.line[,2], connecting.line[,1]] +
  #     0.1 * image.grey[connecting.line[,2]-1, connecting.line[,1]] +
  #     0.1 * image.grey[connecting.line[,2]+1, connecting.line[,1]] +
  #     0.1 * image.grey[connecting.line[,2], connecting.line[,1]-1] +
  #     0.1 * image.grey[connecting.line[,2], connecting.line[,1]+1] +
  #     0.1 * image.grey[connecting.line[,2]-1, connecting.line[,1]-1] +
  #     0.1 * image.grey[connecting.line[,2]-1, connecting.line[,1]+1] +
  #     0.1 * image.grey[connecting.line[,2]+1, connecting.line[,1]-1] +
  #     0.1 * image.grey[connecting.line[,2]+1, connecting.line[,1]+1])
  
  return(image.grey)
}