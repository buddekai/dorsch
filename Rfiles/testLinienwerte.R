# Testfunktion um die optimale Bearbeitung der Helligkeitswerte zu finden,
# so dass am Ende alle hyalinen Ringe gefunden werden.

# Nach dem Finden des ersten Ringes soll der Mittelwert neu berechnet
# werden, ohne dass die Punkte bis zum Minimum nach dem Mittelwert
# dazugezaehlt werden.

# Parameter section ####

# Anzahl der Werte, die am Ende herausgenommen werden können:
remove.points <- 10

# Parameter fuer Ausreisser
t <- 2

# Parameter fuer Hyaline Ringe
par.hyaline <- 1.1 # Weniger als 1.1 ist nicht gut.

# Wie gross darf die Lücke sein
points.to.jump <- 10
repeate.fill.up <- 2

# Punkte nach dem ersten Ring, unter denen ein Minimum gesucht wird
points.to.look.for.minimum <- 20

# Punkte, die am Ende uebersprungen werden koennen, wenn die Werte ueber dem
# Mittelwert liegen
points.for.reconnecting <- 20

# Starte Funktion hier ######

# Lade die Linie mit dem Ergebnis: "Linienwerte"
load("input/Bild1Linie2.RData")

Linienwerte <- Linienwerte[1:(length(Linienwerte)-remove.points)]
Linienwerte.copy <- Linienwerte

# Plotte die Werte
plot(Linienwerte.copy, pch = 4)

# Finde Ausreisser und loesche sie
lower.limit <- median(Linienwerte) - t * sd(Linienwerte)
upper.limit <- median(Linienwerte) + t * sd(Linienwerte)

Linienwerte <- Linienwerte[!Linienwerte > upper.limit]
Linienwerte <- Linienwerte[!Linienwerte < lower.limit]

Linienwerte.copy2 <- Linienwerte
plot(Linienwerte.copy2, pch = 4)

# Glaetten der Kurve
for(j in 1:100){
  for(i in 1:(length(Linienwerte)-2)){
    Linienwerte[i+1] <- 0.5 * Linienwerte[i+1] +
      0.25 * (Linienwerte[i] + Linienwerte[i+2])
  }
}


Linienwerte.copy3 <- Linienwerte
plot(Linienwerte.copy3, pch = 4)


# Ringe finden Teil 1 ####
# Schliesse alle möglichen Lücken
df.Linienwerte <- data.frame(value = Linienwerte)

limit1 <-  par.hyaline * mean(Linienwerte)

df.Linienwerte$ring <- ifelse(Linienwerte > limit1, 1, 0)

repeate.fill.up.copy <- repeate.fill.up
while(repeate.fill.up > 0){
  points.to.jump.copy <- points.to.jump
  
  while(points.to.jump > 0){
    pattern <- c(1, rep(0, points.to.jump), 1)
    pattern <- paste(pattern, collapse = "")
    
    replacement <- c(1, rep(1, points.to.jump), 1)
    replacement <- paste(replacement, collapse = "")
    
    hyaline.vector <- paste(df.Linienwerte$ring, collapse = "")
    hyaline.vector <- gsub(pattern, replacement, hyaline.vector)
    
    df.Linienwerte$ring <-
      as.numeric(unlist(strsplit(hyaline.vector, split = "")))
    
    points.to.jump <- points.to.jump - 1
  }
  
  points.to.jump <- points.to.jump.copy
  repeate.fill.up <- repeate.fill.up - 1
}
repeate.fill.up <- repeate.fill.up.copy

# Zaehle Ringe
ring <- 1
if(df.Linienwerte$ring[1] != 0){
  ring.change <- TRUE
}else{
  ring.change <- FALSE
}


for(i in 1:length(df.Linienwerte$ring)){
  
  if(df.Linienwerte$ring[i] == 0){
    
    if(ring.change == TRUE){
      ring.change <- FALSE
      ring <- ring + 1
    }
    
  }else{
    df.Linienwerte$ring[i] <- ring
    
    if(ring.change == FALSE){
      ring.change <- TRUE
    }
  }
}

number.of.rings <- max(df.Linienwerte$ring)

# Loesche alle hyaline Ringe, die keine richtigen Maxima sind
for(i in 1:number.of.rings){
  max.index <- which(df.Linienwerte$value == max(df.Linienwerte$value[df.Linienwerte$ring == i]))
  last.index <- which(df.Linienwerte$ring == i)[length(which(df.Linienwerte$ring == i))]
  
  if(max.index == last.index){
    df.Linienwerte$ring[df.Linienwerte$ring == i] <- 0
  }
}

# Zaehle Ringe
ring <- 1
if(df.Linienwerte$ring[1] != 0){
  ring.change <- TRUE
}else{
  ring.change <- FALSE
}


for(i in 1:length(df.Linienwerte$ring)){
  
  if(df.Linienwerte$ring[i] == 0){
    
    if(ring.change == TRUE){
      ring.change <- FALSE
      ring <- ring + 1
    }
    
  }else{
    df.Linienwerte$ring[i] <- ring
    
    if(ring.change == FALSE){
      ring.change <- TRUE
    }
  }
}

# Loesche alle Ringe, die kleiner sind als

# TODO oder auch nicht.



# Ringe finden Teil 2 ####
# untersuche noch einmal alle Punkte nach dem Minimum des ersten Ringes.

# Finde alle Maxima und Minima nach dem ersten Ring.

last.index.of.first.ring <-
  which(df.Linienwerte$ring == 1)[length(which(df.Linienwerte$ring == 1))]

index.of.minima <- NULL
index.of.maxima <- NULL

dummy.min <- df.Linienwerte$value[last.index.of.first.ring]
dummy.max <- 0
dummy.counter <- 0

for(i in last.index.of.first.ring : (length(df.Linienwerte$value) -
                                     points.to.look.for.minimum)){
  
  current.value <- df.Linienwerte$value[i]
  
  if(current.value < dummy.min){
    dummy.min <- df.Linienwerte$value[i]
    if(all(dummy.min < df.Linienwerte$value[i:(i+points.to.look.for.minimum)])){
      
    }
  }
  
  
  
}

dummy.range <- i:(i+points.to.look.for.minimum)
min.of.range <- min(df.Linienwerte$value[dummy.range])
dummy.min.index <- which(df.Linienwerte$value == min.of.range)
dummy.min.index <- dummy.min.index[dummy.min.index %in% dummy.range]


### Bis hier ######



index.range <- last.index.of.first.ring : 
                  (last.index.of.first.ring + points.to.look.for.minimum)

first.ring.minimum <-
  which(df.Linienwerte$value ==
          min(df.Linienwerte$value[index.range]))
first.ring.minimum <- first.ring.minimum[
  first.ring.minimum %in% index.range]



index.range <- first.ring.minimum:nrow(df.Linienwerte)

limit2 <-  par.hyaline * mean(df.Linienwerte$value[index.range])

df.Linienwerte$ring[index.range] <-
  ifelse(df.Linienwerte$value[index.range] > limit2, 1, 0)

while(points.to.jump > 0){
  pattern <- c(1, rep(0, points.to.jump), 1)
  pattern <- paste(pattern, collapse = "")
  
  replacement <- c(1, rep(1, points.to.jump), 1)
  replacement <- paste(replacement, collapse = "")
  
  hyaline.vector <- paste(df.Linienwerte$ring, collapse = "")
  hyaline.vector <- gsub(pattern, replacement, hyaline.vector)
  
  df.Linienwerte$ring <-
    as.numeric(unlist(strsplit(hyaline.vector, split = "")))
  
  points.to.jump <- points.to.jump - 1
}

# Zaehle Ringe
ring <- 1
if(df.Linienwerte$ring[1] != 0){
  ring.change <- TRUE
}else{
  ring.change <- FALSE
}


for(i in 1:length(df.Linienwerte$ring)){
  
  if(df.Linienwerte$ring[i] == 0){
    
    if(ring.change == TRUE){
      ring.change <- FALSE
      ring <- ring + 1
    }
    
  }else{
    df.Linienwerte$ring[i] <- ring
    
    if(ring.change == FALSE){
      ring.change <- TRUE
    }
  }
}

# Loesche alle hyaline Ringe, die keine richtigen Maxima sind
number.of.rings <- max(df.Linienwerte$ring)
for(i in 1:number.of.rings){
  max.index <- which(df.Linienwerte$value == max(df.Linienwerte$value[df.Linienwerte$ring == i]))
  last.index <- which(df.Linienwerte$ring == i)[length(which(df.Linienwerte$ring == i))]
  
  if(max.index == last.index){
    df.Linienwerte$ring[df.Linienwerte$ring == i] <- 0
  }
}

# Zaehle Ringe
ring <- 1
if(df.Linienwerte$ring[1] != 0){
  ring.change <- TRUE
}else{
  ring.change <- FALSE
}


for(i in 1:length(df.Linienwerte$ring)){
  
  if(df.Linienwerte$ring[i] == 0){
    
    if(ring.change == TRUE){
      ring.change <- FALSE
      ring <- ring + 1
    }
    
  }else{
    df.Linienwerte$ring[i] <- ring
    
    if(ring.change == FALSE){
      ring.change <- TRUE
    }
  }
}

# Loesche Ringe, die zu dicht beieinander sind. ####

number.of.rings <- max(df.Linienwerte$ring)

for(i in 1:(number.of.rings-1)){
  last.point <- which(df.Linienwerte$ring == (i))[
    length(which(df.Linienwerte$ring == (i)))]
  first.point <- which(df.Linienwerte$ring == (i+1))[1]
  ring.seperating.points <- first.point - last.point - 1
  
  if(ring.seperating.points < points.for.reconnecting){
    if(all(df.Linienwerte$value[last.point:first.point] >
           mean(df.Linienwerte$value))){
      df.Linienwerte$ring[last.point:first.point] <- 1
    }
  }
}


# Zaehle erneut Ringe ####

ring <- 1
if(df.Linienwerte$ring[1] != 0){
  ring.change <- TRUE
}else{
  ring.change <- FALSE
}


for(i in 1:length(df.Linienwerte$ring)){
  
  if(df.Linienwerte$ring[i] == 0){
    
    if(ring.change == TRUE){
      ring.change <- FALSE
      ring <- ring + 1
    }
    
  }else{
    df.Linienwerte$ring[i] <- ring
    
    if(ring.change == FALSE){
      ring.change <- TRUE
    }
  }
}

number.of.rings <- max(df.Linienwerte$ring)

# Plotte abschliessendes Bild ####



df.Linienwerte.copy <- df.Linienwerte
df.Linienwerte.copy$value[df.Linienwerte.copy$ring == 0] <- 0
text <- paste(number.of.rings, "hyaline rings detected", sep=" ")

plot(df.Linienwerte.copy$value, pch = 4, main = text)
