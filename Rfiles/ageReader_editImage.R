# Funktion um ein eingelesenes Bild (bunt) farblich zu verändern

# Für die Helligkeit wird zu jedem Pixel eine Zahl zwischen (+-)0..1
# addiert. Sollte das Ergebnis grösser (kleiner) als 1 (0) sein, so gilt der
# Maximalwert 1 (Minimalwert 0).

# Der Kontrast kann von -1 bis 1 verändert werden. Bei 0 verändert sich
# nichts.

# Die Farbintensitätsveränderungenwerte können Werte von 0..1 Werte
# annehmen. Diese werden dann mit dem Maximalwert der jeweiligen Farbe
# multipliziert.


editImage <- function(image, brightness, contrast, red, green, blue){

  if(missing(brightness)){
    brightness <- -2
  }
  if(missing(contrast)){
    contrast <- -2
  }
  if(missing(red)){
    red <- -1
  }
  if(missing(green)){
    green <- -1
  }
  if(missing(blue)){
    blue <- -1
  }
  
  # Helligkeitsveränderung ####
  if(brightness >= -1 & brightness <= 1){
    image[,,1] <- image[,,1]+brightness
    image[,,2] <- image[,,2]+brightness
    image[,,3] <- image[,,3]+brightness
    
    image[,,1][image[,,1] > 1] <- 1
    image[,,1][image[,,1] < 0] <- 0
    
    
    image[,,2][image[,,2] > 1] <- 1
    image[,,2][image[,,2] < 0] <- 0
    
    
    image[,,3][image[,,3] > 1] <- 1
    image[,,3][image[,,3] < 0] <- 0
  }
  
  # Kontrastveränderung ####
  if(contrast >= -1 & contrast <= 1){
    
    # Kontrast-Korrektur-Faktor
    contrast.factor <- 259 * (contrast + 1) / ( 255 *( 259/255 - contrast))
    
    image[,,1] <- contrast.factor*(image[,,1]-0.5)+0.5
    image[,,2] <- contrast.factor*(image[,,2]-0.5)+0.5
    image[,,3] <- contrast.factor*(image[,,3]-0.5)+0.5
    
    image[,,1][image[,,1] > 1] <- 1
    image[,,1][image[,,1] < 0] <- 0
    
    
    image[,,2][image[,,2] > 1] <- 1
    image[,,2][image[,,2] < 0] <- 0
    
    
    image[,,3][image[,,3] > 1] <- 1
    image[,,3][image[,,3] < 0] <- 0
  }
  
  
  # Farbintensitätsveränderungen ####
  if(0 <= red & red <= 1){
    image[,,1] <- red*image[,,1]/max(image[,,1])
  }
  if(0 <= green & green <= 1){
    image[,,2] <- green*image[,,2]/max(image[,,2])
  }
  if(0 <= blue & blue <= 1){
    image[,,3] <- blue*image[,,3]/max(image[,,3])
  }
  
  rm(brightness, contrast, red, green, blue, contrast.factor)
  
  return(image)
}