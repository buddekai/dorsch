# Funktion zum Finden eines Helligkeitssprunges
# entweder wird entlang der x- oder entlang der y-Achse gelaufen
# Falls x-Koordinate gegeben -> laufe in y-Richtung
# direction: up or down or left or right

findEdge <- function(image.grey, xcoord, ycoord, direction){
  
  missing(ycoord)
  if(missing(ycoord)){
    ycoord <- -1
  }
  if(missing(xcoord)){
    xcoord <- -1
  }
  
  # Parameter ####
  
  
  
  # Schwarzwert soll das x-fache des Mittelwertes der ersten zehn Punkte
  # sein
  black.parameter <- 2.0
  
  # Funktion ####
  
  # Finde heraus, in welche Richtung gegangen werden soll.
  if(ycoord != -1){
    
    print("Bkah")
    
  }else if(xcoord != -1){
    
    # Nimm die ersten 10 Werte, die kleiner sind als 0.1.
    # Dann berechne den gleitenden Durchschnitt und sieh nach,
    # wann die ersten drei Werte kommen, die grösser sind als dieser.
    
    # Folgende Zeilen nur zu Testzwecken
    #print(image.grey[,xcoord])
    #df.test.line <- data.frame("blackvalues"=image.grey[,xcoord])
    #df.test.line$belowblacklimit <- df.test.line$blackvalues < black.limit
    # Bis hier

    if(direction == "down"){
      # Teste, ob der Anfang wirklich schwarz ist
      if( sum(image.grey[1:10, xcoord] < 0.1 ) == 10 ){
        moving.average <- mean(image.grey[1:10, xcoord])
        # Nicht wirklich ein moving average -> Löschen?
        
        black.limit <- black.parameter*moving.average
        test.limit <- mean(image.grey[, xcoord])
        
        # Finde den ersten Wert, der grösser ist als black.limit und nachdem
        # keine dunkle Stelle mehr kommt (bis zur Mitte)
        
        #max.black <- which(image.grey[,xcoord] < black.limit)
        max.black <- which(image.grey[,xcoord] < test.limit)
        max.black <- max.black[max.black < dim(image.grey)[1] / 2]                   
        max.black <- max(max.black)
        
        edge <- c(max.black+1, xcoord)
        
      }else{
        print("it is not black enough")
      }
    }
  }else{
    print("Neither x- nor y-axis have been defined.")
  }
}