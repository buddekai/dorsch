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

start.time <- Sys.time()
ageReader(original.directory = original.directory,
                       image.subdirectory = image.subdirectory,
                       user.file = user.file)

end.time <- Sys.time()
time.taken <- end.time - start.time
print(paste("Time taken in seconds:", time.taken, sep=" "))

setwd(original.directory)

rm(image.subdirectory,
   input.directory,
   original.directory,
   output.directory,
   user.file,
   start.time, end.time, time.taken)

