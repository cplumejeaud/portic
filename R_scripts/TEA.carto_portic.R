########################################################################
## Cours M2 geostats, Université de la Rochelle
## 
## C. Plumejeaud-Perreau, UMR 7266 LIENSs
## 20/09/2020
## Script correction du TEA du 18/09/2020 
#########################################################################

## Où mettez vous vos librairies ?
meslibrairiesR <- "C:/Tools/R4"
## Rajouter le chemin vers les librairies dans le path
.libPaths(c( .libPaths(), meslibrairiesR) )

## Charger les libraries
library(tidyverse)

## Nettoyer votre environnement de toutes variables existantes
rm(list=ls())

##########################################################################
1.	Refaire une jointure entre pointcalls et ports en utilisant tidyverse et enregistrer dans points1787
##########################################################################

## Lire les données

pointcalls <- read.table("./data/portic/pointcalls_ajouts.csv", sep = "\t")
ports <- read.table("./data/portic/ports_ajouts.csv", sep = "\t")


colnames(pointcalls)
[1] "pointcall_uhgs_id"         "pkid"                      "record_id"                 "source_doc_id"            
[5] "source_suite"              "source_main_port_uhgs_id"  "source_main_port_toponyme" "pointcall"                
[9] "pointcall_action"          "pointcall_function"        "pointcall_date"            "homeport_uhgs_id"         
[13] "homeport"                  "ship_id"                   "ship_flag_id"              "ship_name"                
[17] "ship_class"                "tonnage"                   "tonnage_unit"              "captain_id"               
[21] "captain_name"              "citizenship"               "in_crew"                   "commodity_purpose"        
[25] "commodity_standardized"    "quantity"                  "tax_concept"               "q01"                      
[29] "q02"                       "q03"                       "tonnage_class"             "taxe"                     
[33] "nb_ships"                  "sum_tonnage"               "distance_cote_meters"      "distance_river_meters"    
[37] "distance_water" 

## Utiliser mutate pour transformer le type d'une variable (par exemple)

## Exemple avec pointcall_action qui est en fait une variable qualitative
class(pointcalls$pointcall_action)
"character"
unique(pointcalls$pointcall_action)
"In-out"         "Out"            "In"             "Staying"        "Sailing around" "Transit" 

## Syntaxe tidyverse avec le %<% (PIPE) qui permet d'enchainer des transformations sur le même tableau
pointcalls <- as_tibble(pointcalls)
pointcalls <- pointcalls %>%
  mutate(pointcall_action = as.factor(pointcall_action))

## Rq : vu le nombre de variables de   pointcalls cela peut devenir ennuyeux.
## Donc on vous propose mutate_at qui agit sur un ensemble de variables listées par vars()
## et avec des fonctions listées dans list()

pointcalls <- pointcalls %>%
  mutate_at(vars(pointcall_action, pointcall_function, source_suite, ship_class, tonnage_class, source_suite), list(as.factor))  %>%
  mutate_at(vars(tonnage, taxe, quantity, in_crew), list(as.numeric)) 

summary(pointcalls)

colnames(ports)
[1] "uhgs_id"               "pkid"                  "toponyme"              "latitude"             
[5] "longitude"             "amiraute"              "province"              "state_1787"           
[9] "substate_1787"         "oblique"               "type"                  "nb_ships"             
[13] "sum_tonnage"           "distance_cote_meters"  "distance_river_meters" "distance_water" 

ports <- ports %>%
  mutate_at(vars(uhgs_id, toponyme, amiraute, province, state_1787, substate_1787, type), list(as.factor))  %>%
  mutate_at(vars(longitude, latitude, nb_ships, sum_tonnage,distance_cote_meters, distance_river_meters, distance_water), list(as.numeric)) 

summary(ports)

## JOINTURES

A ( x1, x2)
B ( x3, x4, x5)
#Preciser la colone commune à chaque tableau A et B 
# jointure à gauche left_join : tout A plus les colonnes de B, lignes de A
# jointure à droite right_join: tout B plus les colonnes de A, lignes de B
# jointure interne inner_join : que les lignes en commun entre A et B
# jointure externe : que les lignes non partagées 
# jointure complète full_join : toutes les lignes des deux tableaux, avec valeurs manquantes si nécessaire


# Ajouter les dimensions descriptives des ports geocodés (ports) à tous les pointcalls
# - pointcalls : ports d'observation

points1787 <- pointcalls %>% 
  left_join(ports, by = c("pointcall_uhgs_id" = "uhgs_id")) 

colnames(points1787)
1] "pointcall_uhgs_id"         "pkid.x"                    "record_id"                 "source_doc_id"            
[5] "source_suite"              "source_main_port_uhgs_id"  "source_main_port_toponyme" "pointcall"                
[9] "pointcall_action"          "pointcall_function"        "pointcall_date"            "homeport_uhgs_id"         
[13] "homeport"                  "ship_id"                   "ship_flag_id"              "ship_name"                
[17] "ship_class"                "tonnage"                   "tonnage_unit"              "captain_id"               
[21] "captain_name"              "citizenship"               "in_crew"                   "commodity_purpose"        
[25] "commodity_standardized"    "quantity"                  "tax_concept"               "q01"                      
[29] "q02"                       "q03"                       "tonnage_class"             "taxe"                     
[33] "nb_ships.x"                "sum_tonnage.x"             "distance_cote_meters.x"    "distance_river_meters.x"  
[37] "distance_water.x"          "pkid.y"                    "toponyme"                  "latitude"                 
[41] "longitude"                 "amiraute"                  "province"                  "state_1787"               
[45] "substate_1787"             "oblique"                   "type"                      "nb_ships.y"               
[49] "sum_tonnage.y"             "distance_cote_meters.y"    "distance_river_meters.y"   "distance_water.y" 

## Observer que les colonnes qui portaient le même nom à gauche (x) et à droite (y) de la jointure sont renommées
## avec nom_variable.x ou nom_variable.y : "sum_tonnage.x"  et "sum_tonnage.y"  par exemple
## Ce n'est pas forcément pratique
## rename_with(tableau, une fonction, une sélection de colonnes)
## .fn = ~gsub(".x", "_pointcall", .x, fixed = TRUE) : voir la doc de gsub : on remplace ".x" par "_pointcall" dans la variable .x passée en paramètres
## .cols = ends_with(".x") : on sélectionne les variables qui se terminent avec .x

## Voici une fonction qui remplace tous les .x par "_pointcall" afin de signaler que c'était la variable du tableau pointcalls

points1787 <- rename_with(points1787, .fn = ~gsub(".x", "_pointcall", .x, fixed = TRUE), .cols = ends_with(".x"))

## Voici une fonction qui remplace tous les .y par "_port" afin de signaler que c'était la variable du tableau ports

points1787 <- rename_with(points1787, .fn = ~gsub(".y", "_port", .x, fixed = TRUE), .cols = ends_with(".y"))


points1787 <- as_tibble(points1787)

colnames(points1787)
dim(points1787)  
# 72289    52

## Note : si les tonnages n'avaient pas déjà été bien convertis en tonneaux, 
## Voici comment il aurait fallu faire en tidyverse
## Tranformer les tonnage : si tonnage_unit == quintaux, alors tonnage <- tonnage / 24
unique(points1787$tonnage_unit)

## Mutate crée (rajoute) la variable tonnage_tx dans le tableau
## ifelse (condition, si vraie alors instruction 1, si faux alors instruction 2)
## condition : les unités sont données en quintaux :  tonnage_unit == "quintaux"
## instruction 1 : diviser les tonnages par 24 : tonnage / 24.0
## instruction 2 : garder les tonnages déjà exprimés en tonneaux : tonnage
points1787 <- points1787 %>%
  mutate (tonnage_tx =  ifelse(tonnage_unit == "quintaux", tonnage / 24.0, tonnage ))

# exercice
# Ajouter les dimensions descriptives des : 
# - homeport_uhgs_id : ports d'attache
# - ship_flag_id : pavillon du navire
# - source_main_port_uhgs_id : port où est saisi le congé (oblique si possible)


dim(points1787)  
# 72284 57 

colnames(points1787)

## Vu en TEA
## Equivalent à cette commande vu en TD1 avec tidyverse ?
test <- pointcalls[which(tonnage_unit=="quintaux"), ]$tonnage 
## Filtrer pour ne retenir que les tonnages exprimés en quintaux
## Puis sélectionner seulement la variable tonnage 
## Enregistrer dans test
test <- points1787 %>%
  filter(tonnage_unit == "quintaux") %>%
  select(tonnage)



############################################################################
## 2.	Afficher la distribution du tonnage des ports de plus de 1000 bateaux par ports (boxplot et plot). 
## Explorer les facets
############################################################################

## 2.a Mauvaise approche : vous utilisez le dataframe ports, qui contient sum_tonnage et nb_ships par ports
## Remarque : vous ne verrez pas de distribution des tonnages car ils sont sommés par chaque port

## Faire un boxplot de base, avec tous les ports
ggplot(ports, aes(x = factor(toponyme), y = sum_tonnage)) +
  geom_boxplot()
## On ne voit rien (trop de ports en abscisse)

# Les ports de plus de 1000 bateaux
dim(ports[ports$nb_ships > 1000,]) #17 16

## Nom de ports ayant vu passer plus de 1000 pointcall
unique(ports[ports$nb_ships > 1000,]$toponyme)
[1] "Rouen"            "Nantes"           "Marennes"         "Lorient"          "Boulogne sur Mer"
[6] "Calais"           "Saint Malo"       "Bordeaux"         "Le Havre"         "Honfleur"        
[11] "Bayonne"          "La Rochelle"      "Dunkerque"        "Cherbourg"        "Marseille"       
[16] "Angleterre"


## Vider le graphique
dev.off()

## Afficher la distribution du tonnage des ports de plus de 1000 bateaux

## g est une variable qui contient le graphique. On agrège des options sur le graphique avant de l'afficher
## Remarque : la syntaxe sur plusieurs lignes exige un + en fin de ligne 
## chaque fois que vous rajoutez une option sur le graphique
## Pas de + sur la dernière ligne. C'est comme le PIPE (%>%) de tidyverse. 

## graphique de base : on sélectionne les variable
g <- ggplot(ports[ports$nb_ships > 1000,], aes(x = factor(toponyme), y = sum_tonnage)) 
## on choisit le type de graphique (boxplot)
g<- g+ geom_boxplot()
## on rajoute un titre à l'axe des x
g<- g+ labs(x = "Ports (Only for those having more than 1000 pointcall)")
## on rajoute un titre au graphique (à comparer à l'usage de main vu en TD1)
g<- g+ labs(title = "Distribution of tonnage for big ports")
## on dit que les étiquettes sur l'axe des x seront orientées verticalement pour être plus lisibles
g <- g+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Afficher le résultat
print(g)


## Enregistrer notre résultat dans un fichier image(qui montre juste la valeur sommée par port des tonnage et pas leur distribution)
dev.copy(png,'./figures/Portic/sum_tonnage_per_port_1000.png')
dev.off()

## 2.a Bonne approche :utiliser le dataframe pointcalls, 
## avec en x les noms des ports dans la variable poincall 
## et en y la valeur de tonnage de chaque navire visitant ce port
## Afficher la distribution du tonnage des ports de plus de 1000 bateaux
g <- ggplot(pointcalls[pointcalls$nb_ships > 1000,], aes(x = factor(pointcall), y = tonnage)) 
g<- g+ geom_boxplot()
g<- g+ labs(x = "Ports (Only for those having more than 1000 pointcall)")
g<- g+ labs(title = "Distribution of tonnage for big ports")
g <- g+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(g)



## Note : certains m'ont fait remarquer qu'on voit le plus souvent cette syntaxe sur les sites d'aide
## pas de variable g ni de g<- : du coup le graphique s'affiche tout de suite, pas besoin de faire print(g)
## Ne pas oublier le + en fin de ligne sinon...
ggplot(pointcalls[pointcalls$nb_ships > 1000,], aes(x = factor(pointcall), y = tonnage)) +
  geom_boxplot()

dev.off()

dev.copy(png,'./figures/Portic/boxplot_tonnage_per_port_1000.png')
dev.off()

##########################################################################
## 3. Afficher la distribution du tonnage des ports d'une même amirauté (Nantes par exemple)
############################################################################

g <- ggplot(pointcalls[pointcalls$amiraute=="Nantes",], aes(x = factor(pointcall), y = tonnage)) 
## ceci ne peut pas marcher. 

## En effet, est-ce que amirauté est une variable de pointcalls ?
## Non : colnames(pointcalls)
grep ("amiraute", colnames(pointcalls)) ## Renvoie 0


## Par contre, comme on a fait la jointure entre pointcalls et ports enregistrée dans point1787
## On a bien dans point1787 une variable amiraute

## Connaitre les ports de Nantes avec tidyverse
points1787 %>% 
  filter(amiraute == "Nantes") %>% 
  select (toponyme) %>% 
  distinct(toponyme)

1 Piriac       
2 Nantes       
3 Montoir      
4 Couëron      
5 Bourgneuf    
6 Le Pornic    
7 Mesquer      
8 Roche Bernard
9 La Plaine    
10 Paimboeuf    
11 Le Pouliguen 
12 Méan         
13 Saint Nazaire
14 Port Launay  
15 Le Croisic   
16 Beslon      

## On doit donc s'attendre à avoir cette liste de ports en abcisse du graphique 
## si on sélectionne les ports de l'amirauté de Nantes

g <- ggplot(points1787[points1787$amiraute=="Nantes",], aes(x = factor(pointcall), y = tonnage)) 
g<- g+ geom_boxplot()
g<- g+ labs(x = "Ports of Nantes admiralty")
g<- g+ labs(title = "Distribution of tonnage for Nantes admiralty")
g <- g+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(g)


dev.copy(png,'./figures/Portic/boxplot_tonnage_per_port_in_admiralty_Nantes.png')
dev.off()

## Lorsque vous êtes coincés sur une fonction, demandez de l'aide 
## (à gauche en bas dans R_studio, ou en ligne de commande)
help(geom_boxplot)
help(ggplot)
help(labs)


##########################################################################
## 4.	Lire travels : Existe t-il des moments favorables à de longs trajets ?
## L'objectif est de manipuler les dates avec lubridate, COMME dans l'exemple des flights 
## sur le cours de Julier Barnier / Benoit Simon-Bouhet
##########################################################################

## Lire travels : un fichier origine - destination, qui donne les dates de départ et d'arrivée des navires
## departure : nom du port de départ
## destination : nom du port d'arrivée
## outdate_fixed : date de départ
## indate_fixed : date d'arrivée


travels <- read.table("./data/travels.csv", header=TRUE, dec=",", sep="\t")
travels <- as_tibble(travels)

head(travels)

colnames(travels)
[1] "source_doc_id"               "source_component"            "source_main_port_uhgs_id"    "id"                         
[5] "travel_rank"                 "ship_id"                     "travel_uncertainity"         "ship_name"                  
[9] "ship_flag_id"                "ship_class"                  "tonnage"                     "tonnage_class"              
[13] "tonnage_unit"                "captain_id"                  "captain_name"                "birthplace"                 
[17] "status"                      "citizenship"                 "homeport_uhgs_id"            "homeport"                   
[21] "departure"                   "departure_uhgs_id"           "outdate_fixed"               "departure_action"           
[25] "departure_function"          "departure_navstatus"         "destination"                 "destination_uhgs_id"        
[29] "indate_fixed"                "destination_action"          "destination_function"        "destination_navstatus"      
[33] "commodity_purpose"           "commodity_standardized"      "quantity"                    "commodity_purpose2"         
[37] "commodity_standardized2"     "quantity2"                   "commodity_purpose3"          "commodity_standardized3"    
[41] "quantity3"                   "commodity_purpose4"          "commodity_standardized4"     "quantity4"                  
[45] "tax_concept"                 "taxe"                        "distance_dep_dest"           "distance_homeport_dep"      
[49] "distance_dep_dest_miles"     "distance_homeport_dep_miles"

## Charger la librairie lubridate
## Il existe une cheat sheet en ligne très pratique
library(lubridate) #installé avec tidyverse mais pas forcément installé

## Répondre à la question : 
## des longs trajets ? : prendre la variable distance_dep_dest calculée (à vol d'oiseau en km)
## des dates de départ : prendre out_date, mais cela ferait une analyse du nombre départ par jour (365 jours)
## Idee : regrouper sur des semaines les départs 
## et calculer pour cela le numero de semaine correspondant à la date de départ : 
## week(outdate_fixed)
travels_rythms <- travels %>%
  mutate(outdate = week(outdate_fixed)) %>%
  mutate(indate = week(indate_fixed)) %>%
  mutate(km_dep_dest = round(as.numeric(distance_dep_dest), 0)) %>%
  #mutate(duration = ddays(indate_fixed-outdate_fixed)) %>%
  select(departure, destination, outdate, indate, km_dep_dest)

## On a créé un petit dataframe travels_rythms  qui contient juste 5 variables et 84083 trajets
dim(travels_rythms)
84083     5

## Distribution des longueurs de trajets (en y) en fonction des semaines de l'année (en x)
ggplot(data = travels_rythms,
       mapping = aes(x = as.factor(outdate), y = km_dep_dest)) + 
  geom_boxplot()+
  labs(x = "Semaines 1787",
       y = "kilometres parcourus",
       title = "Distance parcourue en fonction des semaines de l'année 1787",
       subtitle = "Existe t-il des moments favorables à de longs trajets ?",
       caption = "Source : PORTIC 2020 - Navigocorpus (G5 et Marseille)")
# Pas de départ pour une saison / semaine de préférence quelque soit le trajet

dev.copy(png,'./figures/Portic/boxplot_distance_per_week.png')
dev.off()

## Au fait, le résumé statistique de cette variable km_dep_dest nous dit quoi ? 
summary(travels_rythms$km_dep_dest)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA''s 
      0      68     217     532     576   10872     753 
## Un voyage médian fait 217 km

## Calculer le quartile Q3 des longueurs de trajets, et considérer que au dela de Q3, ce sont les longs trajet
Q3 <- quantile(travels_rythms$km_dep_dest, 0.75, na.rm = T)
75% 
576
Q1 <- quantile(travels_rythms$km_dep_dest, 0.25, na.rm = T)
25% 
68 
M <- mean(travels_rythms$km_dep_dest, na.rm = T)
531.9546

borne_sup <- M + 1.5*(Q3 - Q1) #1293.955

## Distribution des longueurs de LONGs trajets (en y) en fonction des semaines de l'année (en x)
ggplot(data = filter(travels_rythms, km_dep_dest>borne_sup) ,
       mapping = aes(x = as.factor(outdate), y = km_dep_dest)) + 
  geom_boxplot()+
  labs(x = "Semaines 1787",
       y = "kilometres parcourus",
       title = "Distance parcourue en fonction des semaines de l'année 1787",
       subtitle = "Existe t-il des moments favorables à de longs trajets ?",
       caption = "Source : PORTIC 2020 - Navigocorpus (G5 et Marseille)")

dev.copy(png,'./figures/Portic/boxplot_LONGUES_distance_per_week.png')
dev.off()

## Des variations apparaissent mais il faudrait zoomer, non ?

## Ajout d'un zoom sur les semaines 9 à 20
# scale_x_discrete(limits = as.character(9:20))+
  
  ggplot(data = filter(travels_rythms, km_dep_dest>borne_sup) ,
         mapping = aes(x = as.factor(outdate), y = km_dep_dest)) + 
  geom_boxplot()+
  scale_x_discrete(limits = as.character(40:53))+
  labs(x = "Semaines 1787",
       y = "kilometres parcourus",
       title = "Distance parcourue en fonction des semaines de l'année 1787",
       subtitle = "Existe t-il des moments favorables à de longs trajets ?",
       caption = "Source : PORTIC 2020 - Navigocorpus (G5 et Marseille)")
# Les semaines 11, 12 et 14 et 15 sont propices au départ - la semaine 43 et 53 aussi. Pourquoi ?
# Mois de Mars et fin Octobre ? Voir la relation avec les campagnes de pêche à la morue ?

##########################################################################
## 5.	Lire travels : Existe t-il des moments favorables à des sorties en mer ?
##########################################################################

## On affiche le nombre de départ par semaine, pour tous les voyages dont la date de départ est connue
## complete.cases(outdate) : ne retient que les départs dont outdate <> NA (est connue)
## Graphique en barre : geom_bar


as.factor(na.omit(travels_rythms$outdate))
as.factor(travels_rythms$outdate)

## Avec les NA's (mais ils sont nombreux)
ggplot(data = travels_rythms, mapping = aes(x = as.factor(outdate))) +
  geom_bar()

## Sans les valeurs manquantes de date de départ (outdate)
# Version classique
ggplot(data = travels_rythms[! is.na(travels_rythms$outdate),],
       mapping = aes(x = as.factor(na.omit(outdate)))) + 
  geom_bar() +
  

## Sans les valeurs manquantes de date de départ (outdate)
# Version tidyverse
ggplot(data = travels_rythms %>% filter(!is.na(outdate)),
       mapping = aes(x = as.factor(outdate))) + 
                       geom_bar()+
                       labs(x = "Semaines 1787",
                            y = "Nombre de départs hebdomadaires",
                            title = "Activité de l'année 1787",
                            subtitle = "Existe t-il des moments favorables aux sorties en mer ?",
                            caption = "Source : PORTIC 2020 - Navigocorpus (G5 et Marseille)")
# Peut-être des semaines de préférence pour les départs, mais difficile à lire
dev.copy(png,'./figures/Portic/barplot_nombre_depart_per_week.png')
dev.off()

## Zoomer soit sur le début soit la fin de l'année en sélectionnant un sous-ensemble de semaines
## Chaque semaine est un numéro dans outdate. 
length(levels(as.factor(travels_rythms$outdate))) #53 semaines dans l'année

## Zoomer et décomposer en 2 parties

# Partie 1
ggplot(data = travels_rythms %>% filter(!is.na(outdate)),
       mapping = aes(x = as.factor(outdate))) + 
  geom_bar()+
  ## Ajout d'un zoom sur les semaines 7 à 30
  scale_x_discrete(limits = as.character(1:26))+
  labs(x = "Semaines 1787 - de 1 à 26",
       y = "Nombre de départs hebdomadaires",
       title = "Activité de l'année 1787",
       subtitle = "Existe t-il des moments favorables aux sorties en mer ?",
       caption = "Source : PORTIC 2020 - Navigocorpus (G5 et Marseille)")
dev.copy(png,'./figures/Portic/barplot_nombre_depart_per_week_debut.png')
dev.off()

# Partie 2
ggplot(data = travels_rythms %>% filter(!is.na(outdate)),
       mapping = aes(x = as.factor(outdate))) + 
  geom_bar()+
  ## Ajout d'un zoom sur les semaines 7 à 30
  scale_x_discrete(limits = as.character(27:53))+
  labs(x = "Semaines 1787 - de 1 à 26",
       y = "Nombre de départs hebdomadaires",
       title = "Activité de l'année 1787",
       subtitle = "Existe t-il des moments favorables aux sorties en mer ?",
       caption = "Source : PORTIC 2020 - Navigocorpus (G5 et Marseille)")
dev.copy(png,'./figures/Portic/barplot_nombre_depart_per_week_fin.png')
dev.off()

### Exercice à venir
1. regrouper les départs par mois
2. évaluer par une anova (voir cours TD1/TD2) si le nombre de départs est sensiblement différent suivant les mois
3. Concernant la longueur des trajets parcourus, on vous donne un distancier en miles (nautiques) entre les ports
3.a Faire la jointure pour rajouter la distance en miles entre ports dans travels
3.b Evaluer la normalité de la distribution en miles des trajets 
- par un qqplot
- par un test numérique adapté
3.c Evaluer ce que serait des trajets exceptionnellement long suivant les conclusions de votre test (passer en log ?)


##########################################################################
## 6. Carte de la taille des ports, colorés en fonction de la province
##########################################################################

## 6.a Carte des ports simple

library("sf")
library("cartography")


# Retirer les ports de coordonnées inconnues
tableau <- filter(ports, !is.na(longitude) & !is.na(latitude))

## 4326 : projection EPSG du WGS 84
# transformer les longitudes et latitudes en POINT, et rajouter les attributs du tableau comme variable
portsf <- st_as_sf(tableau, coords=c("longitude", "latitude"), crs = 4326)


# Que les ports de France (qui ont une amirauté de renseignée) : !is.na veut dire "qui n'est pas nulle"
portmetrosf <- filter(portsf, !is.na(amiraute))

# La carte basique avec les points
plot(st_geometry(portmetrosf))
# Remarquez les marges énormes autour

## Projection EPSG 3857
portmetrosf <- st_transform(portmetrosf, 3857)

# La carte basique avec les points projetés en EPSG 3857
plot(st_geometry(portmetrosf))

## Refaire le graphique avec des marges réduites
## Les couches et styles cartographiques s'empilent les uns sur les autres dans l'ordre d'appel
## comme dans un SIG classique

# reduire la largeur des marges avec par(mar=())
par(mar = c(0.1,0.1,1.5,0.1)) 

# coloriser en bleu les cercles, avec comme largeur de contour lwd = 1
# Les couleurs sous R : en hexadécimal (#aec8f2), en texte (darkblue), et aussi RGB et aussi code interne à R
## Voir le site
plot(st_geometry(portmetrosf), col="#aec8f2", border="darkblue", lwd=1)

# Rajoute des points à l'intérieur des cercle pour les ports oblique à TRUE
# pch = forme du point
# col : couleur du point
# cex : taille du point
# add : T : rajouter la couche sur la précédente
plot(st_geometry(filter(portmetrosf, !is.na(oblique) & oblique == TRUE)), pch=20, col="black", cex=0.2, add=T)


library(cartography)

## Rajouter les éléments indispensables
# Rajouter le cartouche, la légende, le titre, l'échelle, l'orientation
layoutLayer(title = "Status des ports du Royaume de France, 1787",
            author = "C.Plumejeaud, Cours M2 stats 2020", sources = "ANR Portic, 2020",
            scale = 200, tabtitle = FALSE,
            frame = FALSE,theme = "blue.pal",
            north = TRUE)


## Rajouter les noms des ports de plus de 1000 navires
filter(portmetrosf, nb_ships> 1000)$toponyme # Test du filtre

# Mettre les noms des grands ports
labelLayer(x = filter(portmetrosf, nb_ships> 1000), 
           txt = "toponyme", 
           halo=TRUE, 
           cex = 0.6, col= "#000000", bg = "#FFFFFF50", 
           overlap = FALSE) 

dev.copy(png,'./figures/Portic/carte_ports.png')
dev.off()

dev.off() # Effacer la carte


## 6.b Carte de la taille des ports, colorés en fonction de la province 

## Définir la résolution de l'export en image à l'avance
ppi <- 300
png("figures/Portic/carte_province_nbships.png", width = 4*ppi, height = 3*ppi, res=ppi) 

# c(bas, gauche, haut, droite)
# OMA : augmenter la largeur de la MARGE EXTERNE (oma) sur les bords gauche et droit de 1
# MAR : reduire la largeur de la MARGE INTERNE (mar), sauf en haut ( + 1.5)
par(mar = c(0.5,0,1.5,0), oma = c(0,0,0,0))
plot(st_geometry(portmetrosf), col="blue", border="darkblue", lwd=1)


propSymbolsTypoLayer(x = portmetrosf, var = "nb_ships", var2 = "province",
                     symbols = "circle",
                     inches = 0.2,
                     border = "white", 
                     lwd = 0.1,
                     legend.var.pos = "right", 
                     legend.var.title.txt = "Nombre de navires déclarés",
                     legend.var2.pos = "left", 
                     legend.var2.title.txt = "Province du port")

plot(st_geometry(filter(portmetrosf, !is.na(oblique) & oblique == TRUE)), pch=20, col="black", cex=0.2, add=T) # Les obliques
# Nommer les ports de plus de 1000 navires
labelLayer(x = filter(portmetrosf, nb_ships> 1000), txt = "toponyme", halo=TRUE, cex = 0.6, col= "#000000", bg = "#FFFFFF50", overlap = FALSE) 
# Rajouter le cartouche, la légende, l'échelle, etc.
layoutLayer(title = "Status des ports du Royaume de France, 1787",
            author = "C.Plumejeaud, Cours M2 stats 2020", sources = "ANR Portic, 2020",
            scale = 200, tabtitle = FALSE,
            frame = FALSE,theme = "blue.pal",
            north = TRUE)

dev.off()


## Exercice à venir
Faire une carte de tous les ports dans le monde (selon cette source), colorisés en fonction de leur state ou substate si il est renseigné
La taille des cercles devra être proportionnelle au tonnage des navires
Rajouter le nom de ceux dont le tonnage est "important" (à vous de définir un seuil suivant une règle que vous expliquerez)

