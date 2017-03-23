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

# From here for test-purposes


# All parameters to choose from:
#test.parameter.for.hyaline <- c(1.1)
#test.points.to.jump <- c(5)
#test.repeate.fill.up <- c(2)
#test.min.hyaline.length <- c(20, 40)

#test.parameter.for.hyaline <- c(1.0, 1.2)
test.parameter.for.hyaline <- c(1.02)
test.points.to.jump <- c(5)
test.repeate.fill.up <- c(2)
test.min.hyaline.length <- c(20)

runs <- length(test.parameter.for.hyaline) * length(test.points.to.jump) *
  length(test.repeate.fill.up) * length(test.min.hyaline.length)

run <- 1

number.of.results <- 4 * runs
df.results <- data.frame(Image = rep(0, number.of.results),
                         HyalinePar = rep(0, number.of.results),
                         Jump = rep(0, number.of.results),
                         Repeate = rep(0, number.of.results),
                         MinLength = rep(0, number.of.results),
                         RingsLeft = rep(0, number.of.results),
                         RingsRight = rep(0, number.of.results),
                         EqualRings = rep(0, number.of.results),
                         AllRings = rep(0, number.of.results))

# Change hyaline ring parameter
for(m in 1:length(test.parameter.for.hyaline)){
  parameter.for.hyaline <- test.parameter.for.hyaline[m]
  
  # Change points to jump parameter
  for(n in 1:length(test.points.to.jump)){
    points.to.jump <- test.points.to.jump[n]
    
    # Change repeate fill up parameter
    for(o in 1:length(test.repeate.fill.up)){
      repeate.fill.up <- test.repeate.fill.up[o]
      
      # Change hyaline length parameter
      for(p in 1:length(test.min.hyaline.length)){
        min.hyaline.length <- test.min.hyaline.length[p]
        
        print(paste("This is run ", run, " from ", runs, sep=""))
        print(paste("Hyaline ring: ", parameter.for.hyaline,
                    ", jumps: ", points.to.jump, ", repeate: ",
                    repeate.fill.up, ", min.length ", min.hyaline.length,
                    sep=""))
        
        
        df.results <- ageReader(original.directory = original.directory,
                                image.subdirectory = image.subdirectory,
                                user.file = user.file,
                                parameter.for.hyaline,
                                points.to.jump,
                                repeate.fill.up,
                                min.hyaline.length,
                                df.results)
        
        run <- run + 1
        
      }
    }
  }
}

write.table(df.results, file = paste("ErgebnisPar",
                                     paste(test.parameter.for.hyaline,
                                           collapse = ""),
                                     ".csv", sep=""),
            sep = ";", dec = ",", row.names = FALSE)

# Bis hier.

end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

setwd(original.directory)

rm(image.subdirectory,
   input.directory,
   original.directory,
   output.directory,
   user.file,
   start.time,
   end.time,
   time.taken)

