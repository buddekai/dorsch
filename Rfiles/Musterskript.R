# Musterskirpt zum Arbeiten mit dem ageReader-Paket


# 1. Definition der Pfade f�r die Bilder und f�r die User-Datei ####

original.directory <- getwd()

# Der gew�hlte Ordner soll sowohl die Userdatei als auch den Unterordner mit
# den Fotos enthalten.
input.directory <-
  "C:\\Users\\Kai\\Documents\\Projekte\\ageReader\\input"
output.directory <- 
  "C:\\Users\\Kai\\Documents\\Projekte\\ageReader\\output"

# Unterodner mit den Fotos (bitte tiffs).

image.subdirectory <- "otoliths_tiff"

# user-Datei (im csv-Format, durch Punkte getrennte Werte)

user.file <- "otoliths_userfile.csv"


# 2. Ab hier arbeitet das Paket selbstst�ndig. ####

source("C:\\Users\\Kai\\Documents\\Projekte\\ageReader\\R files\\ageReader_Master.R")

ageReader(original.directory = original.directory,
          image.subdirectory = image.subdirectory,
          user.file = user.file)

setwd(original.directory)