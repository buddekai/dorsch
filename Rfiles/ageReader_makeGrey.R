makeGrey <- function(image){
  
  mat.R <- image[,,1]
  mat.G <- image[,,2]
  mat.B <- image[,,3]
  
  # Luminosity method
  #image.grey <- 0.21*mat.R + 0.72*mat.G + 0.07*mat.B
  image.grey <- mat.R
  
  rm(mat.R, mat.G, mat.B)
  
  return(image.grey)
}