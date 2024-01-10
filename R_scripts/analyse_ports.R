######################################################################
## Christine PLUMEJEAUD, 28/01/2020
##
## 0. Ouverture des sources de donn?es : lire la base de donn?es
## 1. Lire les tonnage des bateaux sur les ports
## 2. 
##
######################################################################

setwd("C:\\Travail\\Dev\\portic_humanum\\PORTIC_stats")
getwd()

#####################################################
### Installer et charger des biblioth?ques
#####################################################

.libPaths()

## Rajouter le chemin vers les librairies dans le path
meslibrairiesR <- "C:/Tools/R4"
# Ajouter meslibrairiesR dans ces chemins : 
.libPaths(c( .libPaths(), meslibrairiesR) )

#Installer au boulot
install.packages("RPostgreSQL", "C:/Tools/R/")
install.packages("FactoMineR", "C:/Tools/R/")
install.packages("lsr", "C:/Tools/R/")
install.packages("tidyverse", "C:/Tools/R/")
install.packages("fmsb", "C:/Tools/R/")


## Charger les libraries
library(lsr)
library(RPostgreSQL)
library(tidyverse)
require(dplyr)


######################################################################################################
##
## 0. Ouverture des sources de donn?es : lire la base de donn?es
## 
#####################################################################################################

## Quel est le r?pertoire de travail courant ?
getwd()
setwd("C:/Travail/ULR_owncloud/ANR_PORTIC/Data/ports/gazetteer")

## Ouverture de la connexion sur la base de donn?es
con <- dbConnect(dbDriver("PostgreSQL"), host='localhost', port='5432', dbname='portic_v4', user='postgres', password='postgres')
con <- dbConnect(dbDriver("PostgreSQL"), host='localhost', port='5432', dbname='portic_v5', user='postgres', password='postgres')
con <- dbConnect(dbDriver("PostgreSQL"), host='localhost', port='5432', dbname='portic_v6', user='postgres', password='postgres')


## Tester la connexion

data <- dbGetQuery(con, paste("select distance_cote_meters, distance_cote_meters_precise, case when distance_cote_meters> distance_cote_meters_precise then distance_cote_meters_precise else distance_cote_meters end as distance 
from ports.port_points where distance_cote_meters is not null and distance_cote_meters_precise < 1145074"))

summary(data)

distance_cote_meters distance_cote_meters_precise    distance       
 Min.   :      0      Min.   :     6.6             Min.   :     0.0  
 1st Qu.:   1038      1st Qu.:   493.7             1st Qu.:   490.2  
 Median :   5223      Median :  1215.5             Median :  1213.7  
 Mean   : 735535      Mean   :  8866.1             Mean   :  8857.1  
 3rd Qu.: 183904      3rd Qu.:  4479.9             3rd Qu.:  4471.7  
 Max.   :9053403      Max.   :482358.6             Max.   :482358.6  

hist(data$distance_cote_meters /1000, breaks = 30, 
main="Distribution des distances ? la cote", xlab="Distance ? la c?te en kilom?tres")

hist(data$distance_cote_meters /1000, breaks = c(0, 1, 5, 7, 10, 50, 100, 1000, 4000, 9000), 
main="Distribution des distances ? la cote", xlab="Distance ? la c?te en kilom?tres")

distance_cote_meters
 Min.   :      0     
 1st Qu.:   1038     
 Median :   5264     
 Mean   : 735940     
 3rd Qu.: 184657     
 Max.   :9053403     
 NA's   :8      

distance_cote_meters distance_cote_meters_precise    distance        
 Min.   :      0      Min.   :      6.6            Min.   :      0.0  
 1st Qu.:   1038      1st Qu.:    493.7            1st Qu.:    490.6  
 Median :   5264      Median :   1215.8            Median :   1214.1  
 Mean   : 735940      Mean   :   9988.9            Mean   :   9979.9  
 3rd Qu.: 184657      3rd Qu.:   4495.7            3rd Qu.:   4495.7  
 Max.   :9053403      Max.   :1145144.2            Max.   :1145074.1  


hist(data$distance_cote_meters/1000, xlim = c(0, 10000), breaks = 100, col = "lightblue", freq = FALSE, 
main="Distribution des distances ? la cote", xlab="Distance ? la c?te en kilom?tres")
den <- density(data$distance_cote_meters/1000, )
lines(den, col = "red")

summary(data$distance_cote_meters_precise)
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
     17.2   15874.4   48387.3   85501.2  122451.7 1146068.1 

hist(data$distance_cote_meters_precise/1000, xlim = c(0, 10000), breaks = 100, col = "lightblue", freq = FALSE, 
main="Distribution des distances ? la cote", xlab="Distance ? la c?te en kilom?tres")
den <- density(data$distance_cote_meters/1000, )
lines(den, col = "red")


hist(data$distance/1000, xlim = c(0, 250), breaks = 50, col = "lightblue", freq = FALSE, 
main="Distribution des distances ? la c?te", xlab="Distance ? la c?te en kilom?tres")
den <- density(data$distance/1000, )
lines(den, col = "red")
dev.copy(png,'./histogramme_distance_cote.png')
dev.off()


data <- dbGetQuery(con, paste("select distance_water 
from ports.port_points where distance_water is not null "))

summary(data)
distance_water    
 Min.   :     0.0  
 1st Qu.:   448.3  
 Median :  1094.4  
 Mean   :  5699.2  
 3rd Qu.:  2590.0  
 Max.   :482358.6

sd(data$distance_water/1000) / mean(data$distance_water/1000)

hist(data$distance_water/1000, xlim = c(0, 20), breaks = 400, col = "lightblue", freq = TRUE, 
main="Distribution des distances ? la c?te ou aux fleuves", xlab="Distance ? la c?te ou aux fleuves en kilom?tres", 
ylab="Fr?quence")
#den <- density(data$distance_water/1000, )
#lines(den, col = "red")
dev.copy(png,'./histogramme_distance_cote_zoom.png')
dev.off()

hist(data$distance_water/1000, xlim = c(0, 500), breaks = 200, col = "lightblue", freq = TRUE, 
main="Distribution des distances ? la c?te ou aux fleuves", xlab="Distance ? la c?te ou aux fleuves en kilom?tres")
den <- density(data$distance_water/1000, )
lines(den, col = "red")
dev.copy(png,'./histogramme_distance_cote.png')
dev.off()

-------------------------------------------
print(G5) 
# 31681

Marseille <- dbGetQuery(con, paste("select count(*) from navigocheck.check_pointcall 
where (source like '%ADBdR, 200E, 543%')
and data_block_leader_marker = 'A'"))

print(Marseille) 
# 3296

### R?cup?rer toutes les donn?es corrig?es de la BD : data_block_leader_marker = 'A'

navigo  <- dbGetQuery(con, paste("select * from navigocheck.check_pointcall 
where 
((source like '%ADVar%' or source like '%ADMorbihan%' or source like '%G5%') and pointcall_outdate like '%1787%' and data_block_leader_marker = 'A')
or (source like '%ADBdR, 200E, 543%' and data_block_leader_marker = 'A')
"))

navigo  <- dbGetQuery(con, paste("select p.*, ports.toponyme, ports.amiraute, ports.province, ports.shiparea, ports.country2019_name
from navigocheck.check_pointcall p left join  navigoviz.port_points ports on p.pointcall_uhgs_id = ports.uhgs_id
where 
((source like '%ADVar%' or source like '%ADMorbihan%' or source like '%G5%') and pointcall_outdate like '%1787%' and data_block_leader_marker = 'A')
or (source like '%ADBdR, 200E, 543%' and data_block_leader_marker = 'A')
"))

dim(navigo)
#  34977   100


#liste des variables pr?sentes 
colnames(navigo)

## Sauver les donn?es dans votre r?pertoire local sous la forme d'un fichier CSV (importable facilement sous Excel)
## Encodage : UTF8
write.table(navigo, "./navigo.csv", sep = "\t")

## Plus tard, vous pourrez rechargez depuis ces fichiers vos donn?es (si pas d'acc?s ? la BD par exemple)
navigo<- read.delim("navigo.csv", header = TRUE, sep = "\t", encoding="UTF8")
colnames(navigo)

#######################################################################################
##
## 1. Lire les tonnage des bateaux sur les ports
##
#######################################################################################


## Transformer ship_tonnage en numerique (texte, dont certains pas des nombres)
tonnage <- as.numeric(navigo$ship_tonnage)
hist(tonnage)
#Rajouter la colonne tonnage, avec la transformation en nombre de ceux possibles
navigo<-cbind(navigo, tonnage)

## 1 tonneau = 24 quintaux, donc les valeurs de marseille doivent ?tre converties en tonneaux en divisant par 24

## Distinguer dans une variable les sources G5 et les sources Marseille
unique(navigo$source)
dim(navigo) #34954   103
type_source <- rep("G5", dim(navigo)[1])
navigo[grep("ADBdR, 200E, 543", navigo$source), ]$source 
type_source[grep("ADBdR, 200E, 543", navigo$source)] <- "Marseille"
type_source <- as.factor(type_source)
summary(type_source)
       G5 Marseille 
	31681      3296
navigo <- cbind(navigo, type_source)

navigo[navigo$type_source=="Marseille", ]$tonnage <- navigo[navigo$type_source=="Marseille", ]$tonnage / 24
navigo[navigo$type_source=="Marseille", ]$sum_tonnage <- navigo[navigo$type_source=="Marseille", ]$sum_tonnage/ 24

## Evaluer la taille des ports en nombre de bateaux pass?s au pointcall
ports <- aggregate(pkid ~ pointcall_uhgs_id , data = navigo, FUN = length)
colnames(ports) <- c("UHGS_ID", "nb_ships")

## Evaluer la taille des ports en tonnage
portst <- aggregate(navigo$tonnage  ~ pointcall_uhgs_id, data = navigo, FUN = sum, na.action=na.omit)
colnames(portst) <-  c("UHGS_ID",  "sum_tonnage")

ports <- merge(ports, portst, by = c("UHGS_ID"))

colnames(ports) <- c("UHGS_ID", "nb_ships", "sum_tonnage")

## Sauver la table ports
write.table(ports, "./ports.csv", sep = "\t")

## Distribution du tonnage et du nombre de bateaux par ports
help(hist)

hist(ports$sum_tonnage, breaks = 30, main="Histogram for sum of tonnage per ports", xlab="Sum of tonnage per ports")
dir.create(c("fig"))
dev.copy(png,'./fig/tonnage_per_port.png')
dev.off()

hist(ports$nb_ships, breaks = 30, 
main="Histogram for count of pointcall (Observed only) per ports", xlab="Count of pointcall per ports")
dev.copy(png,'./fig/count_pointcall_per_port.png')
dev.off()

## Pour les ports ayant vu passer moins de 1000 bateaux
dev.off()
hist(ports[ports$nb_ships < 1000,]$nb_ships, breaks = c(0, 10, 25, 50, 100, 500, 750, 1000), main="Histogram for count of pointcall (Observed only) per ports \n if less than 1000 pointcall)", xlab="Count of pointcall per ports")
dev.copy(png,'./fig/count_pointcall_per_port_1000.png')
dev.off()

## Fusionner les nouvelles variables de ports avec le dataframe navigo
test <- merge(navigo, ports, by.x = c("pointcall_uhgs_id"), by.y = c("UHGS_ID"))
dim(test) # 34954   103
## Pourquoi j'ai un peu moins de ligne : parce que certaines lignes (23) de navigo n'avaient pas de uhgs_id
dim(navigo[is.na(navigo$pointcall_uhgs_id), ])
23 101

#34954+23 = 34977 : OK
navigo <- test

# Les ports de plus de 1000 bateaux
dim(navigo[navigo$nb_ships > 1000,]) #20060    103
## Nom de ports ayant vu passer plus de 1000 pointcall
unique(navigo[navigo$nb_ships > 1000,]$toponyme)
 [1] "Rouen"            "Nantes"           "Marennes"         "Lorient"         
 [5] "Boulogne sur Mer" "Calais"           "Bordeaux"         "Le Havre"        
 [9] "La Rochelle"      "Dunkerque"        "Marseille"

## Afficher la distribution du tonnage des ports de plus de 1000 bateaux
g <- ggplot(navigo[navigo$nb_ships > 1000,], aes(x = factor(toponyme), y = tonnage)) 
g<- g+ geom_boxplot()
g<- g+ labs(x = "Ports (Only for those having more than 1000 pointcall)")
g<- g+ labs(title = "Distribution of tonnage for big ports")
g <- g+ theme(axis.text.x = element_text(angle = 90, hjust = 1))


dev.off()
g
dev.copy(png,'./fig/sum_tonnage_per_port_1000.png')
dev.off()

## Afficher la distribution du tonnage des ports de plus de 1000 bateaux
g <- ggplot(navigo[navigo$nb_ships > 1000,], aes(x = factor(toponyme), y = tonnage)) 
g<- g+ geom_boxplot()
g<- g+ labs(x = "Ports (Only for those having more than 1000 pointcall)")
g<- g+ labs(title = "Distribution of tonnage for big ports")
g <- g+ theme(axis.text.x = element_text(angle = 90, hjust = 1))


dev.off()
g
dev.copy(png,'./fig/sum_tonnage_per_port_1000.png')
dev.off()

## Afficher la distribution du tonnage des ports de plus de 1000 bateaux SANS Marseille
g <- ggplot(navigo[navigo$nb_ships > 1000 & navigo$toponyme!="Marseille",], aes(x = factor(toponyme), y = tonnage)) 
g<- g+ geom_boxplot()
g<- g+ labs(x = "Ports (Only for those having more than 1000 pointcall)")
g<- g+ labs(title = "Distribution of tonnage for big ports")
g <- g+ theme(axis.text.x = element_text(angle = 90, hjust = 1))


dev.off()
g
dev.copy(png,'./fig/sum_tonnage_per_port_1000_withoutMarseille.png')
dev.off()


## Afficher la distribution du tonnage des ports d'une m?me amiraut? (Nantes par exemple)

g <- ggplot(navigo[navigo$amiraute=="Nantes",], aes(x = factor(toponyme), y = tonnage)) 
g<- g+ geom_boxplot()
g<- g+ labs(x = "Ports de Nantes")
g<- g+ labs(title = "Distribution of tonnage for Nantes admiralty")
g <- g+ theme(axis.text.x = element_text(angle = 90, hjust = 1))


dev.off()
g
dev.copy(png,'./fig/sum_tonnage_per_port_Nantes.png')
dev.off()


help(geom_boxplot)
help(ggplot)
help(labs)


############
## Radar chart ##Pas fini
#############


# Library
library(fmsb)
 
# Create data: note in High school for Jonathan:
data <- as.data.frame(matrix( sample( 2:20 , 10 , replace=T) , ncol=10))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )
 
# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <- rbind(rep(20,10) , rep(0,10) , data)
 
# Check your data, it has to look like this!
# head(data)

# The default radar chart 
radarchart(data)

colnames(navigo)
tonnage
ship_name
ship_local_id
ship_homeport
ship_homeport_id
ship_flag
ship_flag_id
pointcall_function
pointcall_indate 
pointcall_indate 

############################################################################
## Analyse nombre de cargo
## 15 juillet
############################################################################

setwd("C:\\Travail\\ULR_owncloud\\ANR_PORTIC\\Data\\ports\\archives_sql")

data <- dbGetQuery(con, paste("select  json_array_length(all_cargos::json) 
from navigoviz.pointcall p
where p.source_suite = 'Marseille' and all_cargos is not null"))

summary(data)
json_array_length
 Min.   : 1.000   
 1st Qu.: 1.000   
 Median : 2.000   
 Mean   : 2.292   
 3rd Qu.: 3.000   
 Max.   :48.000

v <- data$json_array_length

## Pour les ports ayant vu passer moins de 1000 bateaux
dev.off()
hist(v[v < 11] , main="Distribution du nombre de cargo par pointcall \n si moins de 11)", xlab="Nombre de cargaisons")
dev.copy(png,'./fig/count_cargo_per_pointcall.png')
dev.off()

