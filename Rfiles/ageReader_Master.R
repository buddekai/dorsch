# Paketmaster-File

ageReader <- function(original.directory,
                      image.subdirectory,
                      user.file){
  
  # 0. Grundlagen ###
  
  options(stringsAsFactors = FALSE)
  source("Rfiles\\ageReader_makeGrey.R")
  source("Rfiles\\ageReader_findEdge.R")
  source("Rfiles\\ageReader_redPosition.R")
  source("Rfiles\\ageReader_editImage.R")
  source("Rfiles\\ageReader_smoothGreyImage.R")
  source("Rfiles\\ageReader_getLineIndices.R")
  source("Rfiles\\ageReader_scanForEdge.R")
  
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
    print("The package tiff is loaded correctly.")
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
    image <- readTIFF(source = file.names[i], info = FALSE)
    image.copy <- image
    # Als graues Bild abspeichern
    image.grey <- makeGrey(image)
    image.grey.copy <- image.grey
    
    # Speichere ein farbiges Bild mit Nullen ab, das nach und nach mit
    # Werten für Markierungen gefüllt wird.
    
    image.information <- array(data = 0, dim = dim(image))
    
    # Finde den Rand von innen heraus.
    image.information <- scanForEdge(image.grey = image.grey,
                                     image.information = image.information,
                                     distance = 20)
    image.information.copy <- image.information
    
    # Umrahme den Otolithen
    
    # Schreibe graues Bild und editiertes Farbbild in neuen
    # Ordner
    
    setwd(original.directory)
    dir.create(output.directory, showWarnings = FALSE)
    setwd(output.directory)
    
    #writeTIFF(what = image, where = "testbunt.tiff",
    #          bits.per.sample = 8L, compression = "none", reduce = TRUE)
    #writeTIFF(what = image.grey, where = "testgrau.tiff",
    #          bits.per.sample = 8L, compression = "none", reduce = TRUE)
    
    #image.information <- pmax(image, image.information)
    image.information <- image + image.information
    
    image.information <- ifelse(image.information < 0, 0, image.information)
    image.information <- ifelse(image.information > 1, 1, image.information)
    
    image.name <- paste(gsub(".tif", "", file.names[i]),
                        "bearbeitet.tiff", sep="")
    
    writeTIFF(what = image.information, where = image.name,
              bits.per.sample = 8L, compression = "none", reduce = TRUE)
    
    setwd(.new.directory)
    
  }
  options(warn=0)
  
  
  setwd(.old.directory)
  # Zuruecksetzen des Pfades
  setwd(original.directory)
  
  
}