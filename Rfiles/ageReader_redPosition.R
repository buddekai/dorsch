# Function to mark position with a red cross


redPosition <- function(image, position){
  
  position2 <- position + c(1,0)
  position3 <- position + c(0,1)
  position4 <- position + c(-1,0)
  position5 <- position + c(0,-1)
  
  position6 <- position + c(2,0)
  position7 <- position + c(0,2)
  position8 <- position + c(-2,0)
  position9 <- position + c(0,-2)
  
  position10 <- position + c(1,1)
  position11 <- position + c(-1,1)
  position12 <- position + c(-1,-1)
  position13 <- position + c(1,-1)
  
  image[position[1], position[2], 1] <- 1
  image[position2[1], position2[2], 1] <- 1
  image[position3[1], position3[2], 1] <- 1
  image[position4[1], position4[2], 1] <- 1
  image[position5[1], position5[2], 1] <- 1
  image[position6[1], position6[2], 1] <- 1
  image[position7[1], position7[2], 1] <- 1
  image[position8[1], position8[2], 1] <- 1
  image[position9[1], position9[2], 1] <- 1
  
  image[position10[1], position10[2], 1] <- 0.7
  image[position11[1], position11[2], 1] <- 0.7
  image[position12[1], position12[2], 1] <- 0.7
  image[position13[1], position13[2], 1] <- 0.7
  
  rm(position2, position3, position4, position5, position6, position7,
     position8, position9, position10, position11, position12, position13)
  
  return(image)
}