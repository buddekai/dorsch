# Funktion um die Zeilen und Spalten im Bild zu glaetten.
# Als direction gilt entweder: "horizontal" oder "vertical"

# Es wird ein Binomialfilter benutzt.

smoothGreyImage <- function(image.grey, direction){
  
  if(direction == "vertical"){
    
    image.grey.1 <- image.grey
    image.grey.1 <- rbind(0, image.grey)
    image.grey.1 <- head(image.grey.1, -1)
    
    image.grey.2 <- image.grey
    image.grey.2 <- rbind(image.grey, 0)
    image.grey.2 <- image.grey.2[-1,]
    
    image.grey <- 0.25*image.grey.1 + 0.5*image.grey + 0.25*image.grey.2
    
  }else if(direction == "horizontal"){
    
    image.grey.1 <- image.grey
    image.grey.1 <- cbind(0, image.grey)
    image.grey.1 <- image.grey.1[,-dim(image.grey.1)[2]]
    
    image.grey.2 <- image.grey
    image.grey.2 <- cbind(image.grey, 0)
    image.grey.2 <- image.grey.2[,-1]
    
    image.grey <- 0.25*image.grey.1 + 0.5*image.grey + 0.25*image.grey.2
    
  }else{
    print("Please enter a correct direction (horizontal/vertical)")
  }
  
  rm(image.grey.1, image.grey.2)
  return(image.grey)
}