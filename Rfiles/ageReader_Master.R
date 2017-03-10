# Paketmaster-File

ageReader <- function(original.directory,
                      image.subdirectory,
                      user.file){
  
  # 0. Grundlagen ###
  
  options(stringsAsFactors = FALSE)
  source("C:\\Users\\Kai\\Documents\\Projekte\\ageReader\\R files\\ageReader_makeGrey.R")
  source("C:\\Users\\Kai\\Documents\\Projekte\\ageReader\\R files\\ageReader_findEdge.R")
  source("C:\\Users\\Kai\\Documents\\Projekte\\ageReader\\R files\\ageReader_redPosition.R")
  source("C:\\Users\\Kai\\Documents\\Projekte\\ageReader\\R files\\ageReader_editImage.R")
  source("C:\\Users\\Kai\\Documents\\Projekte\\ageReader\\R files\\ageReader_smoothGreyImage.R")
  # 1. Importiere User-Datei und erweitere Sie um Spalten
  
  # Setzen des Inputordners
  setwd(input.directory)
  
  # Importiere csv-Datei
  df.user.file <- data.frame(
    read.csv(file = user.file, sep=";"))
  
  df.user.file$AGE <- ""
  df.user.file$OUTER_ZONE <- ""
  df.user.file$READABILITY <- ""
  
  df.user.file$IMPORTED <- NA
  
  # 2. Importiere Fotos und untersuche diese ###
  
  # Gehe ins richtige Verzeichnis
  .old.directory <- getwd()
  .new.directory <- paste(.old.directory, "/", image.subdirectory, sep="")
  setwd(.new.directory)
  
  # Speichere die Dateinamen ab und stelle sicher, dass wirklich nur tifs
  # enthalten sind
  
  file.names <- list.files()
  file.names <- file.names[grepl("tif", file.names)]
  
  if(require("tiff")){
    print("The pacakge tiff is loaded correctly.")
  } else {
    print("Trying to install tiff.")
    install.packages("tiff")
    if(require(tiff)){
      print("tiff is installed and loaded.")
    } else {
      stop("We could not install tiff.")
    }
  }
  
  # Lade Foto in R und bearbeite es
  options(warn=-1)
  #for(i in 1:length(file.names)){
  for(i in 1:1){
    image <- readTIFF(source = file.names, info = FALSE)
    image.copy <- image
    # Als graues Bild abspeichern
    image.grey <- makeGrey(image)
    image.grey.copy <- image.grey
    
    
    # Test#
    
    #image <- editImage(image, contrast = -0.3)
    
    #for(k in 1:10){
    #  image.grey <- smoothGreyImage(image.grey = image.grey,
    #                                direction = "vertical")
    #  image.grey <- smoothGreyImage(image.grey = image.grey,
    #                                direction = "horizontal")
    #}
    
    
    #bis hier
    
    
    # Finde obere Kante
    xcoords <- seq(dim(image.grey)[2]/4, 3*dim(image.grey)[2]/4, by=4)
    
    for(j in xcoords){
      edge <- findEdge(image.grey = image.grey, xcoord = j,
                       direction = "down")
      
      # Write red mark there in original picture
      image <- redPosition(image = image, position = edge)
    }
    
    # Schreibe graues Bild und editiertes Farbbild in neuen
    # Ordner
    
    dir.create(output.directory, showWarnings = FALSE)
    setwd(output.directory)
    
    writeTIFF(what = image, where = "testbunt.tiff", bits.per.sample = 8L,
              compression = "none", reduce = TRUE)
    writeTIFF(what = image.grey, where = "testgrau.tiff", bits.per.sample = 8L,
              compression = "none", reduce = TRUE)
    setwd(.new.directory)
  }
  options(warn=0)
  

  setwd(.old.directory)
  # Zurücksetzen des Pfades
  setwd(original.directory)
  
  
}