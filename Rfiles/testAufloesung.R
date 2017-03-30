# Einlesen der czi-Datei fuer den genauen Wert der Aufloesung

input.file <- "input/otoliths_czi/DEM1416_001.czi"
metadata <- paste(readLines(input.file, n = 461, skipNul = TRUE), collapse = " ")
metadata <- gsub(".*<Metadata>", "<Metadata>", metadata)
metadata <- gsub(" +", "", metadata)
scale <- gsub(".*(<Items>.+</Items>).*", "\\1", metadata)
scale.x <- as.numeric(gsub(".*<DistanceId=\"X\"><Value>(.{1,30})</Value>.*", "\\1", scale))
scale.y <- as.numeric(gsub(".*<DistanceId=\"Y\"><Value>(.{1,30})</Value>.*", "\\1", scale))

if(scale.x == scale.y){
  # Jetzt kann die Skalierung abgespeichert werden.
  print("alles gut")
}else{
  print("Scales in x- and y-directions are not the same.")
}