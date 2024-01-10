###############################################################################
## Christine Plumejeaud-Perreau, U.M.R 7301 MIGRINTER
## Script d'analyse des données d'accueil DNA en Nouvelle-Aquitaine, par commune
###############################################################################

meslibrairiesR <- "C:/Tools/R4"
# Ajouter meslibrairiesR dans ces chemins : 
.libPaths(c( .libPaths(), meslibrairiesR) )

getwd()

library(tidyverse)
library(mapsf)

setwd("C:/Travail/Dev/portic_humanum/PORTIC_stats/")
getwd() 


## Sauver / reprendre son travail sous R
# https://mtes-mct.github.io/parcours-r/m1/sauvegarder-son-travail.html 
dir.create("./outputs")

save(list = ls(), file = "outputs/env_entier.RData") 
# sauvegarde de tout l'environnement sur le répertoire choisi 

rm(list = ls()) # suppression de notre environnement dans R 

load("outputs/env_entier.RData") 
# chargement de l'environnement stocké sur l'ordinateur 


########################################################################
## Lire les données
########################################################################

###  Accès distant SSH (après avoir ouvert la connexion SSH avec PUTTY, voir PPTX)
#system('ssh -f plumegeo@134.158.33.179  -L 8002:localhost:5432 -N')
system('sshpass -p PluUMR7266* ssh plumegeo@134.158.33.179 -L 8092:localhost:5432 -N')

######## ATTENTION : AJOUTER LE MOT DE PASSE ########
## Ouverture de la connexion sur la base de données

con <- dbConnect(dbDriver("PostgreSQL"), host='localhost', port='8002', dbname='portic_v7', user='postgres', password='mesange17')


library(RPostgres)


con <- dbConnect(RPostgres::Postgres(), host='localhost', port='5435', dbname='portic_v7', user='postgres', password='postgres')


#postgresqlpqExec(con, "SET client_encoding = 'utf-8'")

query = " select duration+1 as duration_jours, distance_dep_dest_miles , round(distance_dep_dest_miles / (duration+1)) as vitesse, "
query = paste(query , "p.tonnage , p.tonnage_unit , p.tonnage_class ")
query = paste(query ,"from navigoviz.uncertainity_travels ut left join navigoviz.pointcall p on p.pkid = ut.departure_pkid ")
query = paste(query ," where p.tonnage_class in ('[101-200]', '[201-500]')")
#--  distance_dep_dest_miles / (duration+1) > 250
#-- and departure_uhgs_id not in ('A0390929', 'A0394917') 
query = paste(query , "and arrivee_net_route_marker != 'Z' and depart_net_route_marker != 'Z'  ")
query = paste(query ,"and position('=' in departure_out_date) > 0 and position('=' in destination_in_date) > 0 ")
query = paste(query , "and doc_depart = doc_destination ")
query = paste(query , "order by vitesse")

pointcalls<- dbGetQuery(con, query)

# longueurs converties en km
# elapsetime converties en min
query = "select nbpoints, elapsetime/60000 as elapsetime , coalesce(broader_length/1000, direct_length/1000) as broaderlength, st_length(st_makeline(pointpath))/1000 as pathlength"
query = paste(query ," from navigoviz.routepaths ")
query = paste(query ," WHERE pointpath  IS not null")

paths <- dbGetQuery(con, query, encoding = "utf8")
paths$broaderlength
paths$pathlength
summary(paths)
# 
# nbpoints        elapsetime                 broaderlength       pathlength     
# Min.   : 2.00   Min.   :  0.00233   Min.   :    0.0   Min.   :    0.0  
# 1st Qu.: 3.00   1st Qu.:  0.02456   1st Qu.:  113.3   1st Qu.:  271.8  
# Median : 7.00   Median :  0.45923   Median :  465.8   Median :  875.7  
# Mean   :10.27   Mean   :  2.94040   Mean   : 1838.7   Mean   : 1692.8  
# 3rd Qu.:14.00   3rd Qu.:  1.16048   3rd Qu.: 1732.4   3rd Qu.: 1888.8  
# Max.   :68.00   Max.   :125.83388   Max.   :24722.3   Max.   :19868.5  
#                                     NA's   :37                         

hist(paths$broaderlength)
hist(paths$broaderlength)

attach(paths)

## Sauver les données dans votre répertoire local sous la forme d'un fichier CSV (importable facilement sous Excel)
## Encodage : UTF8
write.table(paths, "./paths.csv", sep = "\t")
write.table(colnames(paths), "./variablespaths.csv", sep = "\t")

## Plus tard, vous pourrez rechargez depuis ces fichiers vos données (si pas d'accès à la BD par exemple)
test <- read.delim("paths.csv", header = TRUE, sep = "\t", encoding="UTF8")
colnames(test)

##########################################################################
## Correlation between

##########################################################################

pairs(~elapsetime+broaderlength+nbpoints+pathlength,data=paths,
      main="What drives mostly the calculus duration ? \n It is nbpoints, broaderlength or pathlength ?")
dev.copy(png,'./outputs/scatter_matrix_duration.png')
dev.off()

# broaderlength and duration
plot(broaderlength, elapsetime, main="Relation between path length (broadly) and duration of the computing",
     xlab="Path length (broadly) in km", ylab="Calculus duration (min)", pch=19)

abline(lm(elapsetime~broaderlength), col="red")
#lines(lowess(broaderlength,elapsetime), col="blue") # lowess line (x,y)

dev.copy(png,'./outputs/relation_broaderlength_duration.png')
dev.off()


# pathlength and duration
plot(pathlength, elapsetime, main="Relation between computed path length and duration of the computing",
     xlab="Path length (exact) in km", ylab="Calculus duration (min)", pch=19)

abline(lm(elapsetime~pathlength), col="red")
#lines(lowess(broaderlength,elapsetime), col="blue") # lowess line (x,y)

dev.copy(png,'./outputs/relation_pathlength_duration.png')
dev.off()

# nbpoints and duration
plot(nbpoints, elapsetime, main="Relation between number of computed points (after simpification) \n and duration of the computing",
     xlab="Number of points on path", ylab="Calculus duration (min)", pch=19)

abline(lm(elapsetime~nbpoints), col="red")
#lines(lowess(broaderlength,elapsetime), col="blue") # lowess line (x,y)

dev.copy(png,'./outputs/relation_nbpoints_duration.png')
dev.off()

plot(broaderlength, pathlength, main="Relation between longueur exacte \n and estimation de la longueur",
     xlab="estimation de la longueur", ylab="longueur exacte", pch=19)

abline(lm(pathlength~broaderlength), col="red")
#lines(lowess(broaderlength,elapsetime), col="blue") # lowess line (x,y)

dev.copy(png,'./outputs/relation_nbpoints_duration.png')
dev.off()

##########################################################################
## Supprimer les valeurs exceptionnelles
##########################################################################

M <- mean(broaderlength, na.rm = TRUE) #1838.658
Q1 <- quantile(broaderlength, 0.25,  na.rm = TRUE) #113.3431 Q3
Q3 <- quantile(broaderlength, 0.75,  na.rm = TRUE) #1732.441 
bornesup <- M+1.5*(Q3-Q1) #4267.306 
borneinf <-M-1.5*(Q3-Q1) #-589.9893

hist(paths$broaderlength,freq=TRUE)

hist(broaderlength, main="Distribution of the path lengths to compute",
     xlim = c(0, 5000), breaks=c(0, 100, 250, 500, 1000, 2500, 5000,  max(broaderlength, na.rm = TRUE) ), freq=FALSE, xlab="Length (estimate) in km")

hist(broaderlength, main="Distribution of the path lengths to compute",
     xlim = c(0, 10000), breaks=c(0, 100, 250, 500, 1000, 2500, 5000,  max(broaderlength, na.rm = TRUE) ), freq=FALSE, xlab="Length (estimate) in km")
lines(density(broaderlength, na.rm = T), col="blue",lty=1)


subset <- paths %>% filter (broaderlength<5000) 
plot(subset$broaderlength, subset$elapsetime, main="Relation between path length (broadly) and duration of the computing",
     xlab="Path length (broadly) in km", ylab="Calculus duration (min)", pch=19)

abline(lm(subset$elapsetime~subset$broaderlength), col="red")

################################################################################
################################################################################
## Eurocarto, septembre 2022
################################################################################
################################################################################

library(RPostgres)


con <- dbConnect(RPostgres::Postgres(), host='localhost', port='5435', dbname='portic_v7', user='postgres', password='postgres')


query = " select 1, m_offset, p_distance_ratio, nbpoints, r.elapsetime, st_length(st_intersection(st_makeline(r.pointpath), s.polygone3857))/1000 as km_intersection, st_length(st_makeline(p1.point3857 ,p2.point3857)) as direct_length"
query = paste(query , "from navigoviz.shoreline s, navigoviz.routepaths  r, ports.port_points p1, ports.port_points p2")
query = paste(query , "where st_intersects(st_makeline(r.pointpath), s.polygone3857) and p1.uhgs_id =r.from_uhgs_id and p2.uhgs_id =r.to_uhgs_id ")


#select 1, m_offset, p_distance_ratio, nbpoints, r.elapsetime, st_length(st_intersection(st_makeline(r.pointpath), s.polygone3857))/1000 as km_intersection, st_length(st_makeline(p1.point3857 ,p2.point3857)) as direct_length
#from navigoviz.shoreline s, navigoviz.routepaths  r, ports.port_points p1, ports.port_points p2
#where st_intersects(st_makeline(r.pointpath), s.polygone3857) and p1.uhgs_id =r.from_uhgs_id and p2.uhgs_id =r.to_uhgs_id ;


pathstats<- dbGetQuery(con, query)


setwd("C:/Travail/Dev/portic_humanum/PORTIC_stats/")
getwd() 

################################
# Utiliser la librairie doBy pour calculer des statistiques regroupées par ce facteur
# Aide : https://www.rdocumentation.org/packages/doBy/versions/4.6.7/topics/by-summary
library(doBy)

summaryBy(km_intersection  ~ m_offset, pathstats, fun.names=c("min", "max", "moyenne", "ecart-type", "nombre"),
          FUN = function(x) {
            return(c(
              min(x, na.rm=TRUE),
              max(x, na.rm=TRUE),
              mean(x, na.rm=TRUE), 
              sd(x, na.rm=TRUE), 
              length(na.omit(x))
            ))
          })

################################
## Utiliser mutate pour transformer le type d'une variable (par exemple)
## Rq : vu le nombre de variables de   pointcalls cela peut devenir ennuyeux.
## Donc on vous propose mutate_at qui agit sur un ensemble de variables listées par vars()
## et avec des fonctions listées dans list()
library(lubridate)
library(tidyverse)
library(mapsf)

pathstatsplus <- pathstats %>%
  mutate_at(vars(m_offset), list(as.factor))




resume <- pathstatsplus %>%
  group_by(m_offset) %>%
  summarise(mean = round(mean(km_intersection)), 
            n = n(), 
            m = round(median(km_intersection)), 
            q3 = quantile(km_intersection,probs=0.75),
            q1 = quantile(km_intersection,probs=0.25))%>%
  arrange( m)

# Aide pour le boxplot : https://duclert.org/r-graphiques/boxplot-R.php
boxplot(tonnage ~ ship_class_codage, pathstatsplus, las=2, xlab = "",
        main="Distribution du tonnage par classe de navire ")

#https://www.r-graph-gallery.com/9-ordered-boxplot.html
# Create a vector named "new_order" containing the desired order
new_order <- with(pathstatsplus, reorder(m_offset , km_intersection, median , na.rm=T))


##Reordonner les levels de m_offset pour correspondre au tri par median
resume$m_offset <- factor(unique(resume$m_offset), levels=unique(resume$m_offset))

boxplot(pathstatsplus$km_intersection ~ new_order, ylim=c(-100,500),
        ylab="intersection (km)" , col="#69b3a2", boxwex=0.4 , 
        las=2, xlab = "", 
        main="Distribution of length (km) of paths that intersects the landside  \n group by offset parameter")
        #\n sorted by median intersection length(km) (blue) \n with mean intersection length (km) (red)")
text(cex=0.7, col="red", x=resume$m_offset, -30, resume$mean, xpd=TRUE, srt=0) #insert the text on the graph.
text(cex=0.7, col="blue", x=resume$m_offset, -75, resume$m, xpd=TRUE, srt=35) #insert the text on the graph.

dev.copy(png,'./outputs/boxplot_km_intersection_m_offset.png')
dev.off()

### Again, but in % of intersection

#% d'intersection sur la longueur totale du chemin
pathstatsplus$percent_km <- pathstatsplus$km_intersection /  (pathstatsplus$direct_length/1000 )
pathstatsplus$percent_km <- pathstatsplus$percent_km * 100
summary(pathstatsplus$percent_km)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.00001  0.01072  0.05089  1.14127  0.20558 93.44662        1
 
hist(pathstatsplus$percent_km)
#summary(pathstatsplus$km_intersection / (pathstatsplus$direct_length/1000) )

resume <- pathstatsplus %>%
  group_by(m_offset) %>%
  summarise(mean = mean(percent_km, na.rm = TRUE), 
            n = n(), 
            m = median(percent_km, na.rm = TRUE), 
            q3 = quantile(percent_km,probs=0.75, na.rm = TRUE),
            q1 = quantile(percent_km,probs=0.25, na.rm = TRUE),
            min = min(percent_km,na.rm = TRUE),
            max = max(percent_km, na.rm = TRUE))%>%
  arrange( m)

#quantile(pathstatsplus$percent_km,probs=0.75, na.rm = TRUE)
#https://www.r-graph-gallery.com/9-ordered-boxplot.html
# Create a vector named "new_order" containing the desired order
new_order <- with(pathstatsplus, reorder(m_offset , percent_km, median , na.rm=T))


##Reordonner les levels de m_offset pour correspondre au tri par median
resume$m_offset <- factor(unique(resume$m_offset), levels=unique(resume$m_offset))

boxplot(pathstatsplus$percent_km ~ new_order, ylim=c(-4,10),
        ylab="ratio of intersection (%)" , col="#69b3a2", boxwex=0.4 , 
        las=2, xlab = "", 
        main="Distribution of percentage (%) of paths that intersects the landside  \n group by offset parameter")
#\n sorted by median intersection length(km) (blue) \n with mean intersection length (km) (red)")
text(cex=0.7, col="red", x=resume$m_offset, -1, round(resume$max, 3), xpd=TRUE, srt=0) #insert the text on the graph.
text(cex=0.7, col="blue", x=resume$m_offset, -2, round(resume$m, 3), xpd=TRUE, srt=35) #insert the text on the graph.
text(cex=0.7, col="black", x=resume$m_offset, -3, round(resume$mean, 3), xpd=TRUE, srt=0) #insert the text on the graph.


