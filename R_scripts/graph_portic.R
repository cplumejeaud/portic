#####################################################
### Installer et charger des bibliothèques
#####################################################

.libPaths()

## Rajouter le chemin vers les librairies dans le path
.libPaths(c( .libPaths(), "C:/Tools/R") )

#Installer au boulot
install.packages("RPostgreSQL", "C:/Tools/R/")
install.packages("FactoMineR", "C:/Tools/R/")
install.packages("lsr", "C:/Tools/R/")
install.packages("tidyverse", "C:/Tools/R/")
install.packages("fmsb", "C:/Tools/R/")

install.packages("questionr", "C:/Tools/R/")
install.packages("igraph", "C:/Tools/R/")



library(questionr)

data(hdv2003)

d <- hdv2003

summary(hdv2003)
nrow(d)
names(d)

data(rp2012)

d <- rp2012
nrow(d)
names(d)

library(igraph)

g1 <- graph ( c("Nantes", "laRochelle", "laRochelle", "Bordeaux"), directed=TRUE)
plot(g1)

#g2 <- g1 + edge ( c("Dunkerque", "Nantes", "Nantes", "laRochelle", "laRochelle", "Brest"))
g2 <- g1 + graph ( c("Dunkerque", "Nantes", "Nantes", "laRochelle", "laRochelle", "Brest"), directed=TRUE)

plot(g2)
is_simple(g2)
is.multiple(g2)

simplify(g2)
# les noeuds / vertices en anglais
V(g2)
# les arêtes / Edges en anglais
E(g2)
#Vue matrices
g2[]

## Ajouter des infos sur les ports
V(g2)$longitude <-
V(g2)$latitude <-
V(g2)$shiparea <-
V(g2)$amiraute <-
V(g2)$province <-
V(g2)$etat <-

## Ajouter des infos sur les arêtes (trajectoires de navires)
E(g2)$tonnage <-
E(g2)$tonnage_class <-
E(g2)$ship_class <-
E(g2)$cargo_class <-
E(g2)$taxe <- 
E(g2)$homeport <- 
E(g2)$flag <- 
E(g2)$date <-
E(g2)$distance_miles <-
E(g2)$max_duration_days <-

