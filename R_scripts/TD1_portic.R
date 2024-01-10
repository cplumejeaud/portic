##############################################################
## Cours M2 geostats, Universit? de la Rochelle
## 
## C. Plumejeaud-Perreau, UMR 7266 LIENSs
## 12/09/2020
## Script support ? rendre compl?t? avec les r?ponses
#########################################################################

rm(list=ls())

meslibrairiesR <- "C:/Tools/R4"
.libPaths(c( .libPaths(), meslibrairiesR) )


#################################
## Lire les ports : exercice sur 6 points
#################################

## Charger les librairies requises

## Se mettre dans votre r?pertoire de travail
getwd()
setwd("C:/Travail/CNRS_mycore/Cours/Cours_M2_stat/Scripts")


## Lire la description des ports geocod?s du fichier ports.csv (l'encodage est UTF-8)
## Aide : http://informatique-mia.inrae.fr/r4ciam/sites/default/files/download/tutoriels/readTable.pdf

ports <- read.table("./data/portic/ports.csv", sep = "\t", encoding="UTF-8")
head(ports, 10)

---
Note /2pts

## indiquer le nombre de variables qui d?crivent les ports, et le nombre de ports qui sont d?crits
dim(ports)
[1] 1250   11

colnames(ports)
[1] "pkid"          "uhgs_id"       "toponyme"      "latitude"      "longitude"     "amiraute"     
 [7] "province"      "state_1787"    "substate_1787" "oblique"       "type" 

## Donner la nature de la variable "oblique" et de la variable "type" pour R

class(ports$type) #character
class(ports$oblique) #logical

is.character(ports$oblique) #FALSE
is.logical(ports$oblique) #TRUE
is.factor(ports$oblique) #FALSE

is.character(ports$type) #TRUE
is.logical(ports$type) #FALSE
is.factor(ports$type) #FALSE

Autre solution 
summary(ports)
      pkid          uhgs_id            toponyme            latitude        longitude           amiraute        
 Min.   :   1.0   Length:1250        Length:1250        Min.   :-34.36   Min.   :-100.6616   Length:1250       
 1st Qu.: 314.2   Class :character   Class :character   1st Qu.: 40.67   1st Qu.:  -2.4519   Class :character  
 Median : 626.5   Mode  :character   Mode  :character   Median : 43.57   Median :   3.0423   Mode  :character  
 Mean   : 626.5                                         Mean   : 44.03   Mean   :  -0.3574                     
 3rd Qu.: 938.8                                         3rd Qu.: 49.33   3rd Qu.:   9.3073                     
 Max.   :1251.0                                         Max.   : 71.17   Max.   :  88.1397                     
                                                        NA's   :11       NA's   :11                            
   province          state_1787        substate_1787      oblique            type          
 Length:1250        Length:1250        Length:1250        Mode:logical   Length:1250       
 Class :character   Class :character   Class :character   TRUE:152       Class :character  
 Mode  :character   Mode  :character   Mode  :character   NA's:1098      Mode  :character 



## Quelles sont les diff?rentes valeurs que peut prendre le type d'un port
unique (ports$type)
[1] NA               "si?ge amiraut?" "oblique" 

## transformer la variable "type" pour en faire une variable cat?gorielle pour R
ports$type <- as.factor(ports$type)

## indiquer les niveaux qui sont d?clar?s 
levels(ports$type)

oblique si?ge amiraut?

## Compter le nombre de ports qui sont si?ge d'amiraut?, ou oblique
table(ports$type)
oblique si?ge amiraut? 
           104             48

# Mauvaise solution, sauf si on indique na.omit()
----------
length(test[ports$type=="si?ge amiraut?", ]$type)
length(test[ports$type=="oblique", ]$type)

length(na.omit(test[ports$type=="si?ge amiraut?", ]$type)) #48

---
Note /2pt

## Lire le fichier obliques.csv (l'encodage est UTF-8) qui a ; comme s?parateur de colonne

obliques <- read.table("./data/portic/obliques.csv", sep = ";", header=TRUE, encoding="UTF-8")

colnames(obliques)

[1] "port"    "uhgs_id" "d1781"   "d1782"   "d1783"   "d1784"   "d1785"  
[8] "d1786"   "d1787"   "d1788"   "d1789"   "d1790"   "d1791" 


## Trouver le nombre de ports dont on n'a pas les cong?s en 1787 (renseign?s avec NA) - Utilisez is.na

obliques$d1787

Solution 1
is.na(obliques$d1787)

v <- dim(obliques[is.na(obliques$d1787),])
v

solution 3
sum(! is.na(obliques$d1787))

dim(obliques[is.na(obliques$d1787),])[1]
16

Solution 2
length(obliques$d1787) #164
length(na.omit(obliques$d1787)) #148
manquants <- length(obliques$d1787) - length(na.omit(obliques$d1787)) 
16

## Identifier ces ports en les listant par leur nom et leur code identifiant "uhgs_id"
obliques[is.na(obliques$d1787),]$port

[1] "AberWrac'h"     "Ciboure"        "Ile Rousse"     "Saint-Florent" 
 [5] "Bayeux"         "Jard/Mer"       "Plouescat"      "Caen"          
 [9] "Ouessant"       "Forest La"      "Port-en-Bessin" "Port-Louis"    
[13] "Port-Vendres"   "Courseulles"    "Lu\xe7on"       "Bonifacio"


c("a", "b", "c")

a <- "toto"
b <- 1
c <- TRUE
c(a, b, c)

c(12, 14, 17)


obliques[is.na(obliques$d1787), c(1,2)]
             port  uhgs_id
47      AberWrac'h A0150810
48         Ciboure A0127400
49      Ile Rousse A0126271
50   Saint-Florent A0122646
62          Bayeux     <NA>
64        Jard/Mer     <NA>
66       Plouescat     <NA>
68            Caen A0132409
71        Ouessant A0129606
100      Forest La A0141847
101 Port-en-Bessin A0136783
102     Port-Louis A0124198
103   Port-Vendres A0210592
113    Courseulles A0180068
133       Lu<e7>on A0211235
163      Bonifacio A0127306

li <- c(47, 48, 163)
col <- c(1,2)
obliques[li, col ]


---
Note /2pt

## D?terminer les ports "obliques" dont on n'a pas les cong?s en 1787 
## dans la liste des ports g?ocod?s 
## en utilisant une jointure sur la colonne uhgs_id 
## Aide sur https://duclert.org/r-array-listes-dataframes/dataframes-avances-R.php
## Aide sur #https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/merge

colnames(ports)
[1] "pkid"          "uhgs_id"       "toponyme"      "latitude"     
 [5] "longitude"     "amiraute"      "province"      "state_1787"   
 [9] "substate_1787" "oblique"       "type"  


verifports <- merge(ports, obliques, all.x = FALSE, all.y = TRUE, by = c("uhgs_id"))

#rm(verifports)
#attach(verifports)


dim(verifports)
[1] 164  23

colnames(verifports)
1] "uhgs_id"       "pkid"          "toponyme"      "latitude"     
 [5] "longitude"     "amiraute"      "province"      "state_1787"   
 [9] "substate_1787" "oblique"       "type"          "port"         
[13] "d1781"         "d1782"         "d1783"         "d1784"        
[17] "d1785"         "d1786"         "d1787"         "d1788"        
[21] "d1789"         "d1790"         "d1791"

# Retrouvez l'indice de la colonne d1787 avec grep
grep("d1787", colnames(verifports))
19

v <- c("ma", "girafe", "a", "un_long_cou")
grep("g", v)

sort(v, decreasing = TRUE)

w <- c("girafe", "a", "zebre")
match("girafe", v)
match(w, v)


# Retrouvez l'indice des variables "toponyme", "oblique", "type" et "d1787" avec match
match(c("toponyme", "oblique", "type", "d1787"), colnames(verifports))
[1] 3 10 11 19


# Affichez les valeurs de "toponyme", "oblique", "type" et "d1787" pour les ports dont on n'a pas les cong?s en 1787 

verifports[is.na(verifports$d1787), c(3,10,11,19)]
          toponyme oblique           type d1787
3    Saint Florent    TRUE        oblique    NA
8       Port Louis    TRUE        oblique    NA
13      Ile Rousse    TRUE        oblique    NA
15       Bonifacio    TRUE        oblique    NA
16   Socoa Ciboure    TRUE        oblique    NA
20        Ouessant    TRUE        oblique    NA
26            Caen    TRUE si?ge amiraut?    NA
35  Port en Bessin    TRUE        oblique    NA
51       La Forest    TRUE        oblique    NA
60    Aber Wrac' h    TRUE        oblique    NA
88     Courseulles    TRUE        oblique    NA
138   Port Vendres    TRUE        oblique    NA
140          Lu?on    TRUE        oblique    NA
158           <NA>      NA           <NA>    NA
162           <NA>      NA           <NA>    NA
163           <NA>      NA           <NA>    NA

'

## Modifier les valeurs de oblique ? FALSE pour ces ports
verifports[is.na(verifports$d1787), c(3, 10, 11, 19)]$oblique <- FALSE

## +1 point Afficher le nom de ces ports dans l'ordre alphab?tique (utilisez sort)
sort(verifports[is.na(verifports$d1787), c(3, 10, 11, 19)]$toponyme)
[1] "Aber Wrac' h"   "Bonifacio"      "Caen"           "Courseulles"    "Ile Rousse"     "La Forest"      "Lu?on"         
 [8] "Ouessant"       "Port en Bessin" "Port Louis"     "Port Vendres"   "Saint Florent"  "Socoa Ciboure" 


## Sauver les ports corrig?s dans un fichier CSV avec le ; comme s?parateur, encodage UTF-8
write.table(verifports, "./data/portic/verifports.csv", sep = ";", fileEncoding="UTF-8")


##############################
## Tonnages : exercice sur 10 points
##############################

---
### 2.1 Note /1pt

## Lire la table des observations (les pointcalls) et l'enregistrer dans un dataframe "pointcalls"
## Fichier pointcalls.csv encod? en UTF-8 - trouvez vous m?me le bon s?parateur.
#con <- dbConnect(dbDriver("PostgreSQL"), host='localhost', port='5432', dbname='portic_v5', user='postgres', password='postgres')
#pointcalls <- dbGetQuery(con, paste("select pkid, record_id, source_doc_id, source_suite , source_main_port_uhgs_id, source_main_port_toponyme, pointcall, pointcall_uhgs_id , pointcall_action, pointcall_function, coalesce ( outdate_fixed, indate_fixed ) as pointcall_date,  homeport_uhgs_id , homeport,  ship_id , ship_flag_id , ship_name , \"class\" as ship_class, tonnage , tonnage_unit , captain_id, captain_name, citizenship, in_crew, commodity_purpose, commodity_standardized, quantity , tax_concept , q01 , q02, q03 from navigoviz.pointcall WHERE substring(pointcall_out_date for 4)::int = 1787 OR substring(pointcall_in_date for 4)::int= 1787"))
#write.table(pointcalls, "./data/portic/pointcalls.csv", sep = "\t")

pointcalls<- read.table("./data/portic/pointcalls.csv", sep = "\t", encoding="UTF-8")

attach(pointcalls)

unique(pointcall_action)

#write.table(colnames(pointcalls), "./data/portic/dico_pointcalls.csv", sep = "\t")
#write.table(colnames(ports), "./data/portic/dico_ports.csv", sep = "\t")

---
Note /1pt

### Transformer les tonnages exprim?s en quintaux en tonneaux

class(tonnage_unit)

unique(pointcalls$tonnage_unit)
 unique(pointcalls$tonnage_unit)
[1] "tx"       NA         "quintaux" "Ton"      "Tx"


Solution 1

pointcalls[which(tonnage_unit=="quintaux"), ]$tonnage

# Moyenne des quintaux
mean(pointcalls[which(tonnage_unit=="quintaux"), ]$tonnage)
3828.645
# Moyenne des tonnages
mean(pointcalls[which(tonnage_unit!="quintaux"), ]$tonnage, na.rm = TRUE)
59.1751

summary(pointcalls[which(tonnage_unit=="quintaux"), ]$tonnage)
 Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
     40    3500    4000    3829    4500    6500 
summary(pointcalls[which(tonnage_unit!="quintaux"), ]$tonnage)
 Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.50   18.00   37.00   59.18   73.00 1667.00     123 

library(doBy)
summaryBy(tonnage ~ tonnage_unit, pointcalls, FUN = c(mean, sd, length, sum, min))
  tonnage_unit tonnage.mean tonnage.sd tonnage.length
1     quintaux    3828.6453  1152.7076            406
2          Ton     137.0000     0.0000              2
3           tx           NA         NA          62379
4           Tx     398.6667   167.1566              3
5         <NA>           NA         NA          13025

# Conversion des quintaux en tonneaux en divisant par 24
pointcalls[which(tonnage_unit=="quintaux"), ]$tonnage <- pointcalls[which(tonnage_unit=="quintaux"), ]$tonnage / 24.0

summary(pointcalls[which(tonnage_unit=="quintaux"), ]$tonnage)
 Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.667 145.833 166.667 159.527 187.500 270.833

Solution 2 : transformer en facteurs les unit?s 
f <- as.factor(pointcalls$tonnage_unit)
levels(f) 
[1] "quintaux" "Ton"      "tx"       "Tx" 
table(f)
quintaux      Ton       tx       Tx 
     406        2    62379        3

# Regrouper les "Ton"      "Tx"  "tx" en "tonneaux"
levels(f) <- c("quintaux", "tonneaux",      "tonneaux",       "tonneaux") 

levels(f) 
[1] "quintaux" "tonneaux"
match("Ton", pointcalls$tonnage_unit) # 26220

f[26220] #tonneaux

table(f)
f
quintaux tonneaux 
     406    62384

summary(f)
quintaux tonneaux     NA's 
     406    62384    13025 

# Remplacer par ce facteur ? 2 modalit?s la variable tonnage_unit 
pointcalls$tonnage_unit <- f

# Utiliser la librairie doBy
# Aide : https://www.rdocumentation.org/packages/doBy/versions/4.6.7/topics/by-summary
library(doBy)
summaryBy(tonnage ~ tonnage_unit, pointcalls, FUN = c(mean, sd), na.rm = TRUE, fun.names=c("moyenne", "ecart-type"))

summaryBy(tonnage  ~ tonnage_unit, pointcalls, fun.names=c("moyenne", "ecart-type", "nombre"),
	 FUN = function(x) {
		return(c(
			mean(x, na.rm=TRUE), 
			sd(x, na.rm=TRUE), 
			length(na.omit(x))
		))
	})

  tonnage_unit tonnage.moyenne tonnage.ecart-type tonnage.nombre
1     quintaux       3828.6453         1152.70761            406
2     tonneaux         59.1751           71.31296          62261
3         <NA>             NaN                 NA              0


# Conversion des quintaux en tonneaux en divisant par 24
pointcalls[which(tonnage_unit=="quintaux"), ]$tonnage <- pointcalls[which(tonnage_unit=="quintaux"), ]$tonnage / 24.0
library(doBy)
summaryBy(tonnage  ~ tonnage_unit, pointcalls, fun.names=c("moyenne", "ecart-type", "nombre"),
	 FUN = function(x) {
		return(c(
			mean(x, na.rm=TRUE), 
			sd(x, na.rm=TRUE), 
			length(na.omit(x))
		))
	})

  tonnage_unit tonnage.moyenne tonnage.ecart-type tonnage.nombre
1     quintaux        159.5269           48.02948            406
2     tonneaux         59.1751           71.31296          62261
3         <NA>             NaN                 NA              0

---
Note /1pt

### Observer la distribution des tonnages avec un histogramme (hist())
# Aide : https://www.datamentor.io/r-programming/histogram/
hist(pointcalls$tonnage)

# Avec un titre : main = ""
hist(pointcalls$tonnage, main="Distribution des tonnages des navires, exprim?s en tonneaux")

# En zoomant sur la majeure partie xlim = c()
hist(pointcalls$tonnage, main="Distribution des tonnages des navires, exprim?s en tonneaux",
xlim = c(0, 500) )

# En d?taillant la partie sous 100
hist(pointcalls$tonnage, main="Distribution des tonnages des navires, exprim?s en tonneaux",
xlim = c(0, 500), 
breaks=c(0, 10, 20, 30, 40, 50, 75, 100, 200, 300, 500, 1667 ))

max(pointcalls$tonnage)
min(pointcalls$tonnage)
max(pointcalls$tonnage, na.rm = TRUE) #1667
max(pointcalls$tonnage, na.rm = T)
max(na.omit(pointcalls$tonnage))

# Rajouter une barre verticale rouge signalant la moyenne
abline(v=mean(pointcalls$tonnage, na.rm = T), lty = 3, col="red")

# Rajouter une barre verticale bleue signalant la m?diane 
abline(v=median(pointcalls$tonnage, na.rm = T), lty=2, col="blue")

# Rajouter la courbe de densit? des observations 
lines(density(pointcalls$tonnage, na.rm = T))

# S?parer les observations en quintaux des observations en tonneaux
lines(density(pointcalls[which(pointcalls$tonnage_unit=="quintaux"), ]$tonnage, na.rm = T), col="green")
lines(density(pointcalls[pointcalls$tonnage_unit=="tonneaux", ]$tonnage, na.rm = T), col="chocolate")

getwd()

dev.copy(png,'./figures/Portic/titi.png')
dev.off()
dev.off()


---

## pour plus tard : prendre le log : son effet
hist(log(pointcalls$tonnage), main="Distribution du log des tonnages des navires, exprim?s en tonneaux")
hist(log(pointcalls[which(tonnage_unit=="quintaux"), ]$tonnage), main="Distribution du log des tonnages des navires, exprim?s en tonneaux")
hist(na.omit(pointcalls[pointcalls$tonnage_unit=="tonneaux", ]$tonnage), main="Distribution du log des tonnages des navires, exprim?s en tonneaux")
hist(log(na.omit(pointcalls[pointcalls$tonnage_unit=="tonneaux", ]$tonnage)), main="Distribution du log des tonnages des navires, exprim?s en tonneaux")




---
Note /1pt

### Discr?tiser dans une nouvelle variable tonnage_class suivant ces bornes
# [1-20]; [21-50]; [51-100]; [101-200]; [201-500]; [501 et plus] . 
# Exemple : un tonnage de 24 sera dans la classe '[21-50]'

max(pointcalls$tonnage, na.rm = T)# 1667

tonnage_class <- cut(pointcalls$tonnage, c(0, 20, 50, 100, 200, 500, max(pointcalls$tonnage, na.rm = T)))

is.factor(tonnage_class) # TRUE
levels(tonnage_class)
[1] "(0,20]"         "(20,50]"        "(50,100]"       "(100,200]"      "(200,500]"      "(500,1.67e+03]"

# Rajouter la colonne tonnage_class au dataframe pointcalls
pointcalls <- cbind(pointcalls, tonnage_class)

---
Note /3pt

### Analyser la moyenne des tonnages par classe ainsi d?finie
### Pour cela  : 
### Faire un boxplot 
### Calculer la moyenne du tonnage et son intervalle de confiance pour chaque classe 
### Repr?senter cet intervalle sur le boxplot

# Aide pour le boxplot : https://duclert.org/r-graphiques/boxplot-R.php
boxplot(tonnage ~ tonnage_class, pointcalls)
dev.copy(png,'./figures/Portic/boxplot_tonnage_class.png')
dev.off()

-- Solution 1
v1 <- na.omit(pointcalls[pointcalls$tonnage > 0 & pointcalls$tonnage <= 20, ]$tonnage)
mean(v1)
10.67937

length(v1)
19126

Valeur de Student
Z0.975 <- qt(.975, df = 19126-1)

## Intervalle de la moyenne : 
mean(v1) + Z0.975 * sd(v1)/sqrt(19126)
10.58962
mean(v1) - Z0.975 * sd(v1)/sqrt(19126)
10.76912

-- Solution 2
v1 <- na.omit(pointcalls[pointcalls$tonnage > 0 & pointcalls$tonnage <= 20, ]$tonnage)
student <- t.test(v1, conf.int = 0.95)
student$conf.int
10.58962 10.76912

v2 <- na.omit(pointcalls[pointcalls$tonnage > 20 & pointcalls$tonnage <= 50, ]$tonnage)
student <- t.test(v2, conf.int = 0.95)
print(student$conf.int) # 35.45936 35.68056
v3 <- na.omit(pointcalls[pointcalls$tonnage > 50 & pointcalls$tonnage <= 100, ]$tonnage)
student <- t.test(v3, conf.int = 0.95)
print(student$conf.int) # 73.12188 73.61154
v4 <- na.omit(pointcalls[pointcalls$tonnage > 100 & pointcalls$tonnage <= 200, ]$tonnage)
student <- t.test(v4, conf.int = 0.95)
print(student$conf.int) #143.7484 145.1224
v5 <- na.omit(pointcalls[pointcalls$tonnage > 200 & pointcalls$tonnage <= 500, ]$tonnage)
student <- t.test(v5, conf.int = 0.95)
print(student$conf.int) # 280.7886 286.6737
v5 <- na.omit(pointcalls[pointcalls$tonnage > 500 , ]$tonnage)
student <- t.test(v5, conf.int = 0.95)
print(student$conf.int) # 631.3490 710.4719

-- Solution 3
levels(tonnage_class)
tapply(pointcalls$tonnage, list(tonnage_class= pointcalls$tonnage_class), mean)
tapply(pointcalls$tonnage, list(tonnage_class= pointcalls$tonnage_class), t.test)

summaryBy(tonnage ~ tonnage_class, pointcalls,  fun.names=c("n","moyenne", "Z0.975", "largeur", "borne_inf", "borne_sup" ), 
	FUN = function(x) {
			return(c(
				length(na.omit(x)),
				mean(x, na.rm=TRUE) ,
				qt(.975, df = length(na.omit(x))-1),
				qt(.975, df = length(na.omit(x))-1) * sd(x, na.rm=TRUE)/sqrt(length(na.omit(x))),
				mean(x, na.rm=TRUE) + qt(.975, df = length(na.omit(x))-1) * sd(x, na.rm=TRUE)/sqrt(length(na.omit(x))),
				mean(x, na.rm=TRUE) - qt(.975, df = length(na.omit(x))-1) * sd(x, na.rm=TRUE)/sqrt(length(na.omit(x)))
		))
	})


   tonnage_class tonnage.n tonnage.moyenne tonnage.Z0.975 tonnage.largeur tonnage.borne_inf tonnage.borne_sup
1         (0,20]     19126        10.67937       1.960088      0.08974729          10.76912          10.58962
2        (20,50]     20275        35.56996       1.960081      0.11059831          35.68056          35.45936
3       (50,100]     13542        73.36671       1.960139      0.24482810          73.61154          73.12188
4      (100,200]      7041       144.43538       1.960301      0.68700623         145.12238         143.74837
5      (200,500]      2549       283.73117       1.960895      2.94254435         286.67371         280.78862
6 (500,1.67e+03]       134       670.91045       1.977961     39.56141973         710.47187         631.34903

tapply(pointcalls$tonnage, list(tonnage_class= pointcalls$tonnage_class), t.test)

meslibrairiesR <- "C:/Tools/R4"
.libPaths(c( .libPaths(), meslibrairiesR) )
install.packages("gplots", meslibrairiesR)
library(gplots )
chooseCRANmirror()
available.packages()
#setRepositories()

plotmeans((pointcalls$tonnage)~pointcalls$tonnage_class,xlab="Classe de tonnage", ylab="Tonnage",
main="Tonnage moyen par classe de tonnage\n ? 95% CI")
dev.copy(png,'./figures/Portic/moyenne_classe_tonnage.png')
dev.off()

---
Note /1pt
### Fabriquer une variable taxe : q01*120+q02*12+q03 et rajouter la ? pointcalls (utiliser cbind)

-- mauvaise solution
taxe <- pointcalls$q01*120+ pointcalls$q02*12 + pointcalls$q03

-- Il faut mettre des 0 ? la place des NA dans q01, q02, q03

x[is.na(x)] <- 0 # Solution tr?s rapide mais qui remplace tous les NA de toutes les colonnes (m?me cat?gorielles) en 0


dim(pointcalls)
[1] 75815    31

match (c("q01", "q02", "q03"), colnames(pointcalls))
28 29 30

# D?finir une fonction qui remplace les NA par des 0
replaceNA <- function(x) {
	x[is.na(x)] <- 0
	return(x)
}

# Un ?chantillon pour tester la fonction
pointcalls[c(26273, 26274),c(28, 29, 30)]
      q01 q02 q03
26273  NA   9   9
26274  NA   9   9

apply(pointcalls[c(26273, 26274),c(28, 29, 30)], 2, replaceNA)
      q01 q02 q03
26273   0   9   9
26274   0   9   9

# Utiliser apply : https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/apply
# Remplace les NA en appliquant la fonction replaceNA dans les colonnes pr?cis?es - c(28, 29, 30) - et (dim = 2) 
lescolonnesq <- apply(pointcalls[,c(28, 29, 30)], 2, replaceNA)
colnames(lescolonnesq) #"q01" "q02" "q03"
dim(lescolonnesq )
[1] 75815     3
class (lescolonnesq) #[1] "matrix" "array"
as.data.frame(lescolonnesq)$q01

pointcalls$q01 <- as.data.frame(lescolonnesq)$q01 
pointcalls$q02 <- as.data.frame(lescolonnesq)$q02 
pointcalls$q03 <- as.data.frame(lescolonnesq)$q03 

taxe <- pointcalls$q01*120+ pointcalls$q02*12 + pointcalls$q03

# Rajouter taxe ? pointcalls
pointcalls <- cbind(pointcalls, taxe)

---
Note /1pt

### S?lectionner les ports fr?quent?s par plus de 1000 navires

## Evaluer la taille des ports en nombre de bateaux pass?s au pointcall
## On regroupe par l'identifiant des ports : pointcall_uhgs_id, et on compte le nombre de lignes 
aggports <- aggregate(pkid ~ pointcall_uhgs_id , data = pointcalls, FUN = length)
colnames(aggports) <- c("uhgs_id", "nb_ships")


ports <- merge(ports, aggports, by = c("uhgs_id"))

pointcalls <- merge(pointcalls, aggports, by.x = c("pointcall_uhgs_id"), by.y = c("uhgs_id"))

colnames(ports)
[1] "uhgs_id"       "pkid"          "toponyme"      "latitude"      "longitude"     "amiraute"      "province"      "state_1787"    "substate_1787" "oblique"      
[11] "type"          "nb_ships" 

ports[ports$nb_ships > 1000, ]$toponyme
[1] "Marseille"        "Le Havre"         "Rouen"            "Bordeaux"         "Dunkerque"        "La Rochelle"      "Nantes"           "Boulogne sur Mer"
 [9] "Lorient"          "Charente"         "Angleterre"       "Marennes"         "Saint Malo"       "Cherbourg"        "Honfleur"         "Calais"          
[17] "Bayonne"          "Angleterre" 

## Evaluer la taille des ports en tonnage pass?s au pointcall
aggports <- aggregate(tonnage  ~ pointcall_uhgs_id , data = pointcalls, FUN = sum, na.action=na.omit)
colnames(aggports) <- c("uhgs_id", "sum_tonnage")

ports <- merge(ports, aggports, by = c("uhgs_id"))
colnames(ports)

pointcalls <- merge(pointcalls, aggports, by.x = c("pointcall_uhgs_id"), by.y = c("uhgs_id"))
colnames(pointcalls )


ports <- ports[order(ports$nb_ships, decreasing = TRUE),]
head(ports , 18)

# Sauver ces nouvelles variables
write.table(ports, "./data/portic/ports_ajouts.csv", sep = "\t")
write.table(pointcalls, "./data/portic/pointcalls_ajouts.csv", sep = "\t")

---
Note /1pt

### Dessiner les taxes en fonction des tonnages pour ces grands ports

## Pour tous les ports 
plot(pointcalls$tonnage, pointcalls$taxe, main="Variation des taxes \n en fonction du tonnage des navires \n pour tous les ports 1787")

dev.copy(png,'./figures/Portic/Xtonnage_Ytaxe_tous.png')
dev.off()

## Pour seulement les grands ports
grandsports <- pointcalls[pointcalls$nb_ships > 1000, ]

plot(grandsports$tonnage, grandsports$taxe, main="Variation des taxes en fonction du tonnage des navires \n pour les grands ports 1787")
dev.copy(png,'./figures/Portic/Xtonnage_Ytaxe_tous.png')
dev.off()


attention, on a les taxes qui valent 0 si leur valeur ?tait NA
grandsports <- pointcalls[pointcalls$nb_ships > 1000 & pointcalls$taxe > 0, ]

plot(grandsports$tonnage, grandsports$taxe, main="Variation des taxes (si connues) \n en fonction du tonnage des navires \n pour les grands ports 1787")
dev.copy(png,'./figures/Portic/Xtonnage_Ytaxe_grands.png')
dev.off()


plot(grandsports$tonnage, grandsports$taxe, xlim=c(1,200), ylim=c(1,1000), main="Variation des taxes (si connues) \n en fonction du tonnage des navires \n pour les grands ports 1787")
dev.copy(png,'./figures/Portic/Xtonnage_Ytaxe_grands_zoom.png')
dev.off()

##############################
## 3 Distance ? la c?te maritime, distance aux rivi?res : exercice sur 4 pts
##############################


## Lire les donn?es de distance des ports aux c?tes maritimes ou aux rivi?res
## lues depuis le fichier distances.txt (le separateur est une tabulation)
distances <- read.table("./data/portic/distances.txt", sep = "\t")
colnames(distances)

"uhgs_id"               "distance_cote_meters"  "distance_river_meters" "distance_water"

---
3.1 Note /1 pt

### Donner le r?sum? statistique de la distribution de "distance_water" : min, max, moyenne, ?cart-type, Q1 et Q3
### Repr?senter la distribution de cette distribution avec hist()

summary(distances$distance_water)
 Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
     0.0    448.3   1094.4   5699.2   2590.0 482358.6
sd(distances$distance_water)
[1] 26781.22

hist(distances$distance_water/1000)
# Aide : https://www.datamentor.io/r-programming/histogram/
# Aide : https://www.statmethods.net/advgraphs/parameters.html

# Avec un titre : main = ""
hist(distances$distance_water/1000, main="Distance ? l'eau (maritime ou fluvial) des ports en kilom?tres", freq=FALSE,
xlab="distance en km")

# Montrer la densit?
lines(density(distances$distance_water/1000, na.rm = T), col="blue",lty=1)
# S?parer les distances ? la mer des distances aux rivi?res
lines(density(distances$distance_cote_meters/1000, na.rm = T), col="cyan",lty=5)
lines(density(distances$distance_river_meters/1000, na.rm = T), col=258, lty=3)
# Ajouter une l?gende
legend("topright", inset=.05, title="Distances",
   c("min(c?te maritime, rivi?re)","c?te maritime","rivi?re"), fill=c("blue", "cyan", 258), horiz=FALSE)
dev.copy(png,'./figures/Portic/hist_distances.png')
dev.off()

# En zoomant sur la majeure partie xlim = c()
hist(distances$distance_water/1000, main="Distance ? l'eau (maritime ou fluvial) des ports en kilom?tres",
xlim = c(0, 10))

# En d?taillant la partie sous 10 km
hist(distances$distance_water/1000, main="Distance ? l'eau (maritime ou fluvial) des ports en kilom?tres",
xlim = c(0, 10), breaks=c(0, 0.25, 0.5, 1, 2, 3, 4, 5, 7.5, 10, max(distances$distance_water/1000, na.rm = TRUE) ))

# Rajouter une barre verticale rouge signalant la moyenne
abline(v=mean(distances$distance_water/1000, na.rm = T), lty=2, col="red")

# Rajouter une barre verticale verte signalant la m?diane 
abline(v=median(distances$distance_water/1000, na.rm = T), lty=2, col="green")

# Rajouter la courbe de densit? des observations (en bleu)
lines(density(distances$distance_water/1000, na.rm = T), col="blue",lty=1)
dev.copy(png,'./figures/Portic/hist_distances_zoom.png')
dev.off()

hist(log(1/(distances$distance_water/1000)), main="Distance ? l'eau (maritime ou fluvial) des ports en kilom?tres")

---
3.2 Note /1 pt
## Evaluer la normalit? de la distribution "distance_water" avec QQplot et avec un test (Shapiro-Wilks)
 
qqnorm(distances$distance_water/1000,datax=TRUE, main="QQplot des distances ? l'eau pour les ports" )
qqline(distances$distance_water/1000,datax=TRUE)
dev.copy(png,'./figures/Portic/qqplot_distances.png')
dev.off()

shapiro.test(distances$distance_water/1000)
H0 : teste la normalit?
W = 0.16846, p-value < 2.2e-16
Conclusion : ce n''est pas une distribution normale


qqnorm(log(1/(distances$distance_water/1000)), main="QQplot du log inverse des distances ? l'eau pour les ports")
qqline(log(1/(distances$distance_water/1000)),datax=TRUE)
dev.copy(png,'./figures/Portic/qqplot_loginverse_distances.png')
dev.off()

shapiro.test(log(1/(distances$distance_water/1000)))
H0 : teste la normalit?
W = 0.98158, p-value = 5.179e-10
Conclusion : ce n'est pas une distribution normale


---
3.3 Note /1 pt
## Calculer l'intervalle de confiance dans la moyenne de (distance_water) 


v <- na.omit(distances$distance_water/1000)

summary(v)
 Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
  0.0000   0.4483   1.0944   5.6992   2.5900 482.3586

student <- t.test(v, conf.int = 0.95)
student$conf.int
[1] 4.047223 7.351211

---
3.4 Note /1 pt
### Identifier les ports exceptionnels (valeurs < ou > ? la moyenne + 1.5 (Q3-Q1)) et donner leur nombre

M <- mean(v)
Q1 <- quantile(v, 0.25) #0.4482674 
Q3 <- quantile(v, 0.75) #2.590009
bornesup <- M+1.5*(Q3-Q1) #8.91183 
borneinf <-M-1.5*(Q3-Q1) #2.486604

5.699217 + 1.5*(2.590009-0.4482674)
5.699217 - 1.5*(2.590009-0.4482674)


length(distances[distances$distance_water/1000<borneinf | distances$distance_water/1000>bornesup,]$uhgs_id ) #856
length(distances[distances$distance_water/1000>borneinf & distances$distance_water/1000<bornesup,]$uhgs_id ) #156
length(distances[distances$distance_water/1000>bornesup,]$uhgs_id ) #105
length(distances[distances$distance_water/1000<borneinf,]$uhgs_id ) #751


### A quelle r?alit? renvoient ces ports exceptionnels ? votre avis ? 
ports qui n'ont pas pu ?tre identifi?s et donc associ?s soit ? des lieux de naufrage en mer, ou des lieux "g?n?riques" [la Corse, l'Angleterre]

---
3.5 Note /1 pt
## Ajouter ? port les donn?es de distances , 
## Faire une jointure pour cela sur la colonne uhgs_id 
## Aide sur https://duclert.org/r-array-listes-dataframes/dataframes-avances-R.php
## Aide sur #https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/merge

colnames(ports)

ports <- merge(ports, distances, by = c("uhgs_id"))
colnames(ports)

pointcalls <- merge(pointcalls, distances, by.x = c("pointcall_uhgs_id"), by.y = c("uhgs_id"))
colnames(pointcalls )

# Sauver ces nouvelles variables
write.table(ports, "./data/portic/ports_ajouts.csv", sep = "\t")
write.table(pointcalls, "./data/portic/pointcalls_ajouts.csv", sep = "\t")




