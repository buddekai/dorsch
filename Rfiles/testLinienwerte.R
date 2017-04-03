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
load("input/line.RDa")
Linienwerte <- test
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

# Benutze nur die Punkte die weniger als die Hälfte vom Bildrand entfernt
# liegen
find.first.ring.within <- as.integer(nrow(df.Linienwerte)/2)

limit1 <-  par.hyaline * mean(Linienwerte[1:find.first.ring.within])

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


# Loesche Ringe, die zu dicht beieinander sind. ####

number.of.rings <- max(df.Linienwerte$ring)

if(number.of.rings > 1){
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


# Loesche alle Ringe, die nicht der erste Ring sind

df.Linienwerte$ring[df.Linienwerte$ring != 1] <- 0


# Ringe finden Teil 2 ####
# untersuche noch einmal alle Punkte nach dem Maximum des ersten Ringes.

# Finde alle Maxima und Minima nach dem ersten Ring.

last.index.of.first.ring <-
  which(df.Linienwerte$ring == 1)[length(which(df.Linienwerte$ring == 1))]

index.of.minima <- NULL
index.of.maxima <- NULL

final.index.of.minima <- NULL
final.index.of.maxima <- NULL

dummy.min <- df.Linienwerte$value[last.index.of.first.ring]
dummy.max <- 0
dummy.counter <- 0

find.min <- TRUE
find.max <- FALSE


df.Linienwerte$Anstieg <- NA
for(i in last.index.of.first.ring : (length(df.Linienwerte$value) -
                                     points.to.look.for.minimum)){
  
  if(df.Linienwerte$value[i] > df.Linienwerte$value[i-1]){
    df.Linienwerte$Anstieg[i] <- "up"
    
    if(find.min){
      index.of.minima <- c(index.of.minima, i-1)
      find.min <- FALSE
    }
    find.max <- TRUE
    
  }else{
    df.Linienwerte$Anstieg[i] <- "down"
    
    if(find.max){
      index.of.maxima <- c(index.of.maxima, i-1)
      find.max <- FALSE
    }
    find.min <- TRUE
  }
  
}

df.Linienwerte$max <- 0
df.Linienwerte$min <- 0

df.Linienwerte$max[index.of.maxima] <- 1
df.Linienwerte$min[index.of.minima] <- 1

# Loesche letztes Minimum, falls eins zu viel ist.
if((length(index.of.minima) - 1) == length(index.of.maxima)){
  df.Linienwerte$min[index.of.minima[length(index.of.minima)]] <- 0
  index.of.minima <- index.of.minima[-length(index.of.minima)]
}



# Loesche Extrema, die keine richtigen sind.
if(length(index.of.minima) == length(index.of.maxima)){
  if(index.of.minima[1] < index.of.maxima[1]){
    if(length(index.of.minima) > 1){
      
      i <- 1
      
      # Beginn: aufsteigender Ast, finde richtiges Minimum
      while(i == 1){
        
        # Finde richtiges Minimum
        dum.current.max <- df.Linienwerte$value[index.of.maxima[i]]
        dum.current.min <- df.Linienwerte$value[index.of.minima[i]]
        dum.current.fraction <- dum.current.max / dum.current.min
        
        # Wenn der Unterschied nicht gross genug ist, so loesche das Maximum
        # und waehle das kleinste Minimum
        if(dum.current.fraction < par.hyaline){
          
          df.Linienwerte$max[index.of.maxima[i]] <- 0
          index.of.maxima <- index.of.maxima[-i]
          
          if(df.Linienwerte$value[index.of.minima[i]] <
             df.Linienwerte$value[index.of.minima[i+1]]){
            df.Linienwerte$min[index.of.minima[i+1]] <- 0
            index.of.minima <- index.of.minima[-(i+1)]
          }else{
            df.Linienwerte$min[index.of.minima[i]] <- 0
            index.of.minima <- index.of.minima[-i]
          }
        }else{
          i <- i + 1
        }
      }
      
      # Alle weitere Extrema untersuchen
      while(i < length(index.of.maxima)){
        
        changed <- FALSE
        # absteigender Ast
        # Finde richtiges Maximum
        dum.current.max <- df.Linienwerte$value[index.of.maxima[i-1]]
        dum.current.min <- df.Linienwerte$value[index.of.minima[i]]
        dum.current.fraction <- dum.current.max / dum.current.min
        
        # Wenn der Unterschied nicht gross genug ist, so loesche das
        # Minimum und waehle das groesste Maximum
        if(dum.current.fraction < par.hyaline){
          
          df.Linienwerte$min[index.of.minima[i]] <- 0
          index.of.minima <- index.of.minima[-i]
          
          if(df.Linienwerte$value[index.of.maxima[i-1]] <
             df.Linienwerte$value[index.of.maxima[i]]){
            df.Linienwerte$max[index.of.maxima[i-1]] <- 0
            index.of.maxima <- index.of.maxima[-(i-1)]
          }else{
            df.Linienwerte$max[index.of.maxima[i]] <- 0
            index.of.maxima <- index.of.maxima[-i]
          }
          
          changed <- TRUE
        }
        
        # aufsteigender Ast
        # Finde richtiges Minimum
        dum.current.max <- df.Linienwerte$value[index.of.maxima[i]]
        dum.current.min <- df.Linienwerte$value[index.of.minima[i]]
        dum.current.fraction <- dum.current.max / dum.current.min
        
        # Wenn der Unterschied nicht gross genug ist, so loesche das Maximum
        # und waehle das kleinste Minimum
        if(dum.current.fraction < par.hyaline){
          
          df.Linienwerte$max[index.of.maxima[i]] <- 0
          index.of.maxima <- index.of.maxima[-i]
          
          if(df.Linienwerte$value[index.of.minima[i]] <
             df.Linienwerte$value[index.of.minima[i+1]]){
            df.Linienwerte$min[index.of.minima[i+1]] <- 0
            index.of.minima <- index.of.minima[-(i+1)]
          }else{
            df.Linienwerte$min[index.of.minima[i]] <- 0
            index.of.minima <- index.of.minima[-i]
          }
          
          changed <- TRUE
        }
        
        # Fange beim ursprünglichen i wieder an, falls ein Wert gelöscht
        # wurde.
        if(!changed){
          i <- i + 1
        }
        
          
      }
    }
  }else{
    print("Es wurde nach dem ersten Maximum kein Minimum gefunden!")
  }
}else{
  print("Es wurden nicht gleich viele Extrema gefunden.")
}


# Finde Ringe zwischen zwei Minima
# Berechne den Mittelwert aus allen Punkten zwischen zwei Maxima

for(i in 1:(sum(df.Linienwerte$max)-1)){
  
  if(i==1){
    current.range <-
      last.index.of.first.ring:which(df.Linienwerte$max==1)[1]
  }else{
    current.range <-
      which(df.Linienwerte$max==1)[i-1]:which(df.Linienwerte$max==1)[i]
  }
  
  current.mean.value <- mean(df.Linienwerte$value[current.range])
  
  # Markiere den Ring zwischen zwei Minima
  current.range <-
    which(df.Linienwerte$min==1)[i]:which(df.Linienwerte$min==1)[i+1]
  
  
  
  copy.par.hyaline <- par.hyaline
  current.range.ring <- which(
    df.Linienwerte$value[current.range] > par.hyaline * current.mean.value)
  while(length(current.range.ring)==0){
    par.hyaline <- ((par.hyaline - 1)/2+1)
    current.range.ring <- which(
      df.Linienwerte$value[current.range] > par.hyaline * current.mean.value)
  }
  par.hyaline <- copy.par.hyaline
  
  df.Linienwerte$ring[current.range[current.range.ring]] <- 1
}

# Finde den letzten Ring #####
current.range <-
  which(df.Linienwerte$min==1)[sum(df.Linienwerte$min)]:
  (length(df.Linienwerte$value)-points.to.look.for.minimum)
current.mean.value <- mean(df.Linienwerte$value[current.range])

# Markiere den Ring zwischen zwei Minima
current.range.ring <- which(
  df.Linienwerte$value[current.range] > par.hyaline * current.mean.value)

# Falls das Maximum nicht gross genug ist.
copy.par.hyaline <- par.hyaline
current.range.ring <- which(
  df.Linienwerte$value[current.range] > par.hyaline* current.mean.value)
while(length(current.range.ring)==0){
  par.hyaline <- ((par.hyaline - 1)/2+1)
  current.range.ring <- which(
    df.Linienwerte$value[current.range] > par.hyaline* current.mean.value)
}
par.hyaline <- copy.par.hyaline

df.Linienwerte$ring[current.range[current.range.ring]] <- 1



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

# Plotte abschliessendes Bild ####



df.Linienwerte.copy <- df.Linienwerte
df.Linienwerte.copy$value[df.Linienwerte.copy$ring == 0] <- 0
text <- paste(number.of.rings, "hyaline rings detected", sep=" ")

plot(df.Linienwerte.copy$value, pch = 4, main = text)
