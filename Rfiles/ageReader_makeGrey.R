makeGrey <- function(image, grey.mode){
  
  if(missing(grey.mode)){
    grey.mode <- "normal.grey"
  }
  
  if(!grey.mode  %in% c("normal.grey", "luminosity.grey", "red.grey",
                        "green.grey", "blue.grey")){
    grey.mode <- "normal.grey"
  }
  
  mat.R <- image[,,1]
  mat.G <- image[,,2]
  mat.B <- image[,,3]
  
  # Normal grey
  if(grey.mode == "normal.grey"){
    image.grey <- (mat.R + mat.G + mat.B)/3
  }
  
  
  
  # Luminosity method
  if(grey.mode == "luminosity.grey"){
    image.grey <- 0.21 * mat.R + 0.72 * mat.G + 0.07 * mat.B
  }
  
  # Grey from red layer
  if(grey.mode == "red.grey"){
    image.grey <- mat.R
  }
  
  # Grey from green layer
  if(grey.mode == "green.grey"){
    image.grey <- mat.G
  }
  
  # Grey from blue layer
  if(grey.mode == "blue.grey"){
    image.grey <- mat.B
  }
  
  rm(mat.R, mat.G, mat.B)
  
  return(image.grey)
}