# Musterskirpt zum Arbeiten mit dem ageReader-Paket


# 1. Definition der Pfade fuer die Bilder und fuer die User-Datei ####

original.directory <- getwd()

# Der gewaehlte Ordner soll sowohl die Userdatei als auch den Unterordner
# mit den Fotos enthalten.
input.directory <- "input"
output.directory <- "output"

# Unterodner mit den Fotos (bitte tiffs).

image.subdirectory <- "otoliths_tiff"

# user-Datei (im csv-Format, durch Punkte getrennte Werte)

user.file <- "otoliths_userfile.csv"


# 2. Bearbeitung der Bilder ####


# 3. Finden des Alters usw. (Hauptaufgabe) ####

source("Rfiles/ageReader_Master.R")

ageReader(original.directory = original.directory,
          image.subdirectory = image.subdirectory,
          user.file = user.file)

setwd(original.directory)
