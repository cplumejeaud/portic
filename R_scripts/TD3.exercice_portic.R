########################################################################
## Cours M2 geostats, Université de la Rochelle
## 
## C. Plumejeaud-Perreau, UMR 7266 LIENSs
## 20/09/2020
## Exercice à rendre pour le TD4 du 5 novembre avant 9h45 
#########################################################################

## Où mettez vous vos librairies ?
meslibrairiesR <- "C:/Tools/R4"
## Rajouter le chemin vers les librairies dans le path
.libPaths(c( .libPaths(), meslibrairiesR) )

## Charger les libraries
library(tidyverse)

## Nettoyer votre environnement de toutes variables existantes

##########################################################################
## Lire travels : un fichier origine - destination, qui donne les dates de départ et d'arrivée des navires
## departure : nom du port de départ
## destination : nom du port d'arrivée
## outdate_fixed : date de départ
## indate_fixed : date d'arrivée
##########################################################################

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

#########################################################################
# 1.a Créer une variable mois qui est renseignée avec le mois correspondant à outdate_fixed si renseignée, 
# par la valeur du mois de indate_fixed sinon, et qui est une catégorielle. 
# Aide : https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_lubridate.pdf
# Fabriquer aussi une variable « duration » représentant la durée des voyages 
# et exprimant la différence en nombre de jours entre indate_fixed et outdate_fixed.
#########################################################################

## Charger la librairie lubridate
library(lubridate) 


## Répondre à la question : 
## des longs trajets ? : prendre la variable miles_dep_dest calculée (par mer en miles)
## des dates de départ : prendre out_date, mais cela ferait une analyse du nombre départ par jour (365 jours)
## Idee : regrouper sur des mois les départs 
## et calculer pour cela le numero de mois correspondant à la date de départ : 
## month(outdate_fixed)
## Calculer également la durée des trajets en jour (duration)
travels_rythms <- travels %>%
  mutate(outdate = month(outdate_fixed)) %>%
  mutate(indate = month(indate_fixed)) %>%
  mutate(miles_dep_dest = round(as.numeric(distance_dep_dest_miles), 0)) %>%
  mutate(duration = ymd(travels$indate_fixed)-ymd(travels$outdate_fixed)) %>%
  select(departure, destination, outdate, indate, duration,  miles_dep_dest)

travels_rythms <- as_tibble(travels_rythms)

## On a créé un petit dataframe travels_rythms  qui contient juste 6 variables et 84083 trajets
dim(travels_rythms)
84083     6

colnames(travels_rythms)
"departure"      "destination"    "outdate"        "indate"         "duration"       "miles_dep_dest"

## Créer une variable mois de départ (ou d'arrivée si la date de départ n'est pas connue)
travels_rythms <- travels_rythms %>% 
  mutate(mois = as.factor(ifelse (!is.na(outdate), outdate, indate)))

levels(travels_rythms$mois)
  Levels: 1 2 3 4 5 6 7 8 9 10 11 12

#########################################################################
# 1.b Visualiser avec un diagramme en barre le nombre de départ par mois.
#  Spécifier les noms des mois sous les barres, placés verticalement 
# http://www.sthda.com/french/wiki/ggplot2-graduation-des-axes-guide-pour-personnaliser-les-etiquettes-des-graduations-logiciel-r-et-visualisation-de-donnees ). Commentez
#########################################################################
  
ggplot(data = travels_rythms %>% filter(!is.na(mois)),
       mapping = aes(x = mois)) + 
  geom_bar()+
  labs(x = "Mois 1787 - de Janvier à Décembre",
       y = "Nombre de départs mensuels",
       title = "Activité de l'année 1787",
       subtitle = "Existe t-il des mois favorables aux sorties en mer ?",
       caption = "Source : PORTIC 2020 - Navigocorpus (G5 et Marseille)")+
  scale_x_discrete(labels=c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Aout", "Septembre", "Octobre", "Novembre", "Décembre"), expand=c(0,0))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.copy(png,'./figures/Portic/barplot_nombre_depart_per_month.png')
dev.off()

## Commenter : quels sont les mois favorables aux sorties en mer ? 
## Commenter : quels sont les mois les plus défavorables aux sorties en mer ?

#########################################################################
# 1.c Ajoutez une colonne travel_class qui vaut "court" si miles_dep_dest < 30, 
# "moyen" si miles_dep_dest est entre 200 et "long" si miles_dep_dest vaut plus que 200 miles.
# Faites attention à l’ordre des classes 
# https://abcdr.thinkr.fr/comment-reordonner-les-niveaux-dun-facteur-dans-r-factor/
#########################################################################

travels_rythms <- travels_rythms %>%
  mutate (travel_class =  as.factor(ifelse(miles_dep_dest < 30, "court", ifelse(miles_dep_dest < 200, "moyen", "long" ) )))
levels(travels_rythms$travel_class)
"court" "long"  "moyen"
travels_rythms$travel_class<-factor(travels_rythms$travel_class,c("court","moyen","long"))
"court" "moyen" "long" 

#########################################################################
# 1.d Rajouter une option dans le diagramme en barre distinguant le type de voyage. 
# Vous ferez attention à filtrer les valeurs inconnues. 
# Commenter : à votre avis, est-ce les sorties en mer dépendent de la distance à parcourir ?
#########################################################################

ggplot(data = travels_rythms %>% filter(!is.na(mois) & !is.na(miles_dep_dest)),
       mapping = aes(x = mois, fill=factor(travel_class))) + 
  geom_bar()+
  labs(x = "Mois 1787 - de Janvier à Décembre",
       y = "Nombre de départs mensuels",
       title = "Activité de l'année 1787",
       subtitle = "Existe t-il des mois favorables aux sorties en mer ?",
       caption = "Source : PORTIC 2020 - Navigocorpus (G5 et Marseille)")+
  scale_x_discrete(labels=c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Aout", "Septembre", "Octobre", "Novembre", "Décembre"), expand=c(0,0))+
  scale_y_continuous("Nombre de départs mensuels", expand=c(0,0)) + 
  scale_fill_manual("travel_class", values=c("pink","green", "blue"), labels=c("court < 30 miles" ,"moyen [30-200 miles]", "long > 200 miles")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.copy(png,'./figures/Portic/barplot_nombre_depart_per_month_per_travel_class.png')
dev.off()

## Difficile à dire

#########################################################################
## 1.e Réaliser un test de chi2 pour vérifier si le type de voyage est lié au mois. Commenter
# https://juba.github.io/tidyverse/04-bivarie.html#croisement-de-deux-variables-qualitatives
#########################################################################

## Analyse d'un khi-2 entre nombre de départs par mois et classe des voyages (court/moyen/long)
tab <- tapply(as.factor(travels_rythms$mois), list(mois = travels_rythms$mois, type_voyage = travels_rythms$travel_class), length)


      type_voyage
      mois court moyen long
      1   1300  1976 1350
      2   1366  2173 1365
      3   1456  2890 2071
      4   1602  2792 2082
      5   1722  3118 1833
      6   1751  3460 1985
      7   1900  3655 2010
      8   2033  3812 1925
      9   1684  3334 1778
      10  1365  2820 1816
      11  1330  2452 1566
      12  1263  2248 1643

## Solution avec tidyverse (voir https://juba.github.io/tidyverse/04-bivarie.html#croisement-de-deux-variables-qualitatives)
tab <- table(as.factor(travels_rythms$mois), travels_rythms$travel_class)

res<-chisq.test(tab)
print(res)
X-squared = 302.33, df = 22, p-value < 2.2e-16

## Interpréter
#H0 independance est rejetée au risque 2.2 -16. 
#Le mois fait varier significativement le type de voyage entrepris (court, moyen ou long)

#########################################################################
## Nous allons pratiquer un test de variance sur les distances parcourues en fonction des mois
#########################################################################

#########################################################################
# 2.a Realisation de l'anova et interprétation du test 
#https://statistique-et-logiciel-r.com/anova-sur-mesures-repetees/
#########################################################################

summary(aov(formula=(travels_rythms$miles_dep_dest) ~ travels_rythms$mois))  
  Df    Sum Sq Mean Sq F value Pr(>F)    
  travels_rythms$mois    11 2.742e+07 2492324   9.145 <2e-16 ***
    Residuals           74914 2.042e+10  272530                   
  ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  9157 observations deleted due to missingness
# Donc H0 (indépendance) rejetée au risque p<2e-16 : il y a un lien entre mois de départ et distance parcourue
# Cependant, un pb de taille : les 2 pré-requis n'ont pas été vérifiés !

#########################################################################
# 2.b Vérifier l’homogénéité des variances par mois avec le test de Bartlett  
#########################################################################
  
bartlett.test(miles_dep_dest~mois, travels_rythms )
  Bartlett''s K-squared = 849.26, df = 11, p-value < 2.2e-16

# On rejete H0 a tort au risque 2.2e-16, donc on accepte H0 (homogénéité des variances)
# Interprétation : donc les variances de distances parcourus sont équivalentes en fonction des mois


# 2.c Vérifier la normalité des distributions
#  Représenter par facets la distribution des distances parcourues par chaque mois 
#  https://besibo.github.io/DA/viz.html#facets 
# Vérifier la normalité de la distribution pour le mois de Janvier et de Juin 
# avec un test numérique (trouvez en un adapté). Conclure et commenter votre résultat sur l’anova.
                                                                                   
#########################################################################                                                                                   
# Représenter par facets la distribution des distances parcourues par chaque mois 
#########################################################################

ggplot(travels_rythms %>% filter(!is.na(mois)), 
       aes(x = miles_dep_dest, fill = factor(mois))) +
  geom_histogram(bins = 20, color = "grey30") +
  facet_wrap(~factor(mois), ncol = 3)
dev.copy(png,'./figures/Portic/hist_miles_dep_dest_per_month_facets.png')
dev.off()

# Vérifier la normalité de la distribution pour le mois de Janvier et de Juin avec un test numérique (trouvez en un adapté). 

shapiro.test(travels_rythms [travels_rythms$mois==1, ]$miles_dep_dest)
W = 0.41038, p-value < 2.2e-16
# distribution pas normale en janvier

shapiro.test(travels_rythms [travels_rythms$mois==6, ]$miles_dep_dest)
# Erreur : la taille de l'échantillon doit être comprise entre 3 et 5000
ks.test(travels_rythms [travels_rythms$mois==6, ]$miles_dep_dest,"pnorm",
        mean=mean(travels_rythms [travels_rythms$mois==6, ]$miles_dep_dest, na.rm=T), 
        sd=sd(travels_rythms [travels_rythms$mois==6, ]$miles_dep_dest, na.rm=T))
  D = 0.31622, p-value < 2.2e-16
  alternative hypothesis: two-sided
  Warning message:
    In ks.test(travels_rythms[travels_rythms$mois == 6, ]$miles_dep_dest,  :
                 aucun ex-aequo ne devrait être présent pour le test de Kolmogorov-Smirnov
# aucun ex-aequo ne devrait être présent pour le test de Kolmogorov-Smirnov

library(nortest)
lillie.test(travels_rythms [travels_rythms$mois==6, ]$miles_dep_dest)
  D = 0.31622, p-value < 2.2e-16
# distribution pas normale en juin

# Conclusion : les histogrammes montrent que les distributions ne sont pas normales, quelque soit le mois. 
# L''étude de Janvier et Juin le confirme, et donc les résultats de l''ANOVA ne peuvent être considérés comme valides. 
# Pour l'instant on ne sait pas si les longueur des trajets sont influencées par la période de départ

#########################################################################
##  2.d  Réaliser un test de Wilcox (supporte non normalité des distributions) comparant 
#  - le mois de Janvier et le mois de Juin
#  - le mois de Mars et le mois de Novembre 
# Aide : https://juba.github.io/tidyverse/04-bivarie.html#croisement-dune-variable-quantitative-et-dune-variable-qualitative  
#########################################################################
  
d <- travels_rythms %>% filter((mois==1 | mois==6) & !is.na(mois))
wilcox.test(d$miles_dep_dest ~ d$mois)
W = 16623350, p-value = 0.9077
# Les distributions de distance dans les deux sous-populations (janvier et Juin) sont équivalentes

d <- travels_rythms %>% filter((mois==3 | mois==11) & !is.na(mois))
wilcox.test(d$miles_dep_dest ~ d$mois)
W = 17807101, p-value = 0.0004098
# Les distributions de distance dans les deux sous-populations (mars et novembre) sont différentes

# En conclusion, certains mois ont oui une influence sur la longueur des trajets, ou diffèrent significativement assez, mais pas tous
# Quels sont les mois qui diffèrent le plus ? Réponse avec le 

#########################################################################
## 2.e Réaliser le test de Krustal-Wallis qui supporte la non-normalité des entrées
## https://www.datanovia.com/en/fr/lessons/test-de-kruskal-wallis-dans-r/
#########################################################################

library(rstatix)

res.kruskal <- travels_rythms %>% kruskal_test(miles_dep_dest ~  mois)
print(res.kruskal)
  .y.                n statistic    df        p method        
  * <chr>          <int>     <dbl> <int>    <dbl> <chr>         
    1 miles_dep_dest 84083      124.    11 3.14e-21 Kruskal-Wallis
# Oui, on note une influence du mois en général sur la durée des trajets

travels_rythms %>% kruskal_effsize(miles_dep_dest ~ mois)
  .y.                n effsize method  magnitude
  * <chr>          <int>   <dbl> <chr>   <ord>    
    1 miles_dep_dest 84083 0.00134 eta2[H] small 
# Mais cette influence est faible (eta-2 vaut 0.00134)
## 0.00134 : un petit effet du mois sur la longueur des trajets entrepris si on considère tous les mois
## Mais si on prend mois par mois, on peut distinguer des choses plus intéressantes


class(travels_rythms$miles_dep_dest)

# Version qui combine tous les groupes (12) 2 à 2 pour détecter les mois qui sont assez différents des autres
# https://www.datanovia.com/en/fr/lessons/test-de-kruskal-wallis-dans-r/


res <- travels_rythms %>% 
  wilcox_test(miles_dep_dest ~ mois, p.adjust.method = "bonferroni")
# Prend du temps

# A tibble: 66 x 9
.y.            group1 group2    n1    n2 statistic          p    p.adj p.adj.signif
* <chr>          <chr>  <chr>  <int> <int>     <dbl>      <dbl>    <dbl> <chr>       
1 miles_dep_dest 1      2       5207  5586 11532096  0.159      1        ns          
2 miles_dep_dest 1      3       5207  7114 14068807  0.00000284 0.000187 ***         
3 miles_dep_dest 1      4       5207  7386 14665152  0.059      1        ns          
4 miles_dep_dest 1      5       5207  7432 15695484. 0.126      1        ns          
5 miles_dep_dest 1      6       5207  7988 16623350  0.908      1        ns          
6 miles_dep_dest 1      7       5207  8351 17593280  0.613      1        ns          
7 miles_dep_dest 1      8       5207  8547 18343278. 0.054      1        ns          
8 miles_dep_dest 1      9       5207  7479 15638394  0.641      1        ns          
9 miles_dep_dest 1      10      5207  6654 13311233  0.000283   0.019    *           
10 miles_dep_dest 1      11      5207  5919 12191795  0.214      1        ns 

# ... 56 more rows
1      3 marche
1      10 marche

dim(res)

res %>% filter (p.adj<0.05) %>% select(group1, group2, p.adj)
dim(res[res$p.adj<0.05,c(2, 3)]) ## 22 lignes

##########################################################################
## Exercice 2
##########################################################################
setwd("C:/Travail/CNRS_mycore/Cours/Cours_M2_stat/Scripts")

#########################################################################
## 1.a Lire les données de 1789 dans data_1789_c.csv avec read.delim et les enregistrer dans pointcalls
#########################################################################

pointcalls <- read.delim("./data/portic/data_1789_c.csv", encoding="UTF-8") 

dim(pointcalls)
[1] 31278    79

#########################################################################
## 1.b Déterminer les source_component dont les source_main_port_uhgs_id 
## et source_main_port_toponyme qui n'ont pas été renseignés 
#########################################################################

unique(pointcalls %>% filter (is.na(source_main_port_toponyme))  %>% select(source_component))
    source_component
  9              ANF, G5-76
  37           ANF, G5-151B
  41             ANF, G5-62
  72            ANF, G5-125
  3616 ANF, G5, 154-1, 1539

#########################################################################  
## 1.c Compter le nombre d'observations à chaque pointcall par component_source (group_by)
## Le port principal de la source est celui qui a le plus d'observations
## Aide https://besibo.github.io/DA/wrangling.html#cr%C3%A9er-des-r%C3%A9sum%C3%A9s-avec-summarise-et-group_by
## Le tableau missings est une variable que vous créerez avec : 
## source_component
## pointcall
## pointcall_uhgs_id 
## nb_obs
#########################################################################
  
missings <- pointcalls %>% 
  filter (is.na(source_main_port_toponyme)) %>% 
  select(source_component, pointcall, pointcall_uhgs_id) %>%
  group_by(source_component, pointcall, pointcall_uhgs_id)  %>% 
  summarise(nb_obs = n()) %>% 
  arrange(desc(nb_obs))

# observer les 10 premières lignes de missings et commenter
head(missings, 10)

  # A tibble: 10 x 4
  # Groups:   source_component, pointcall [10]
  source_component pointcall          pointcall_uhgs_id nb_obs
  <chr>            <chr>              <chr>              <int>
    1 ANF, G5-76       Dunkerque          A0204180            2597
  2 ANF, G5-76       Angleterre         A0394917             962
  3 ANF, G5-62       Charente           A0171758             602
  4 ANF, G5-151B     Saint Martin de Ré A0127055             479
  5 ANF, G5-125      Noirmoutier        A0136403             287
  6 ANF, G5-62       La Rochelle        A0198999             235
  7 ANF, G5-125      Nantes             A0124817             163
  8 ANF, G5-76       Amsterdam          A0620777             111
  9 ANF, G5-76       Londres            A0381691              99
  10 ANF, G5-151B     La Rochelle        A0198999              96

#########################################################################  
## 1.d Lister dans un tableau "keep" les 5 sources dont les champs source_main_port_uhgs_id  
##  et source_main_port_toponyme ne sont pas renseignés dans la dataframe missings
## Ce sont ceux dont le nombre d'observation est maximal  
#########################################################################

  keep <- missings %>%  
  group_by(source_component)  %>% 
  summarise(max = max(nb_obs)) 

  1 ANF, G5-125            287
  2 ANF, G5-151B           479
  3 ANF, G5-62             602
  4 ANF, G5-76            2597
  5 ANF, G5, 154-1, 1539     1
  
##########################################################################
## 1.e Faites une jointure entre keep et missings pour récupérer dans keep les pointcall et pointcall_uhgs_id correspondants            
##########################################################################
  
keep<-  keep %>% 
    left_join(missings , by = c("source_component" = "source_component", "max"="nb_obs")) 

Resultat attendu
# A tibble: 7 x 4
source_component       max pointcall            pointcall_uhgs_id
<chr>                <int> <chr>                <chr>            
  1 ANF, G5-125            287 Noirmoutier          A0136403         
2 ANF, G5-151B           479 Saint Martin de Ré   A0127055         
3 ANF, G5-62             602 Charente             A0171758         
4 ANF, G5-76            2597 Dunkerque            A0204180         
5 ANF, G5, 154-1, 1539     1 Honfleur             A0187836         
6 ANF, G5, 154-1, 1539     1 Le Havre             A0187101         
7 ANF, G5, 154-1, 1539     1 Saint Valery en Caux A0135548 
    
##########################################################################
## 1.f Faire une jointure entre pointcalls et le tableau keep  sur la valeur de source_component
## Exploiter la jointure pour renseigner source_main_port_toponyme et source_main_port_uhgs_id
## lorsqu'ils sont nuls
##########################################################################

pointcalls  <- pointcalls %>% 
  left_join(keep, by = c("source_component" = "source_component")) 


pointcalls <- pointcalls %>% 
  mutate(source_main_port_toponyme = ifelse (is.na(source_main_port_toponyme), pointcall.y, source_main_port_toponyme)) %>%
  mutate(source_main_port_uhgs_id = ifelse (is.na(source_main_port_uhgs_id), pointcall_uhgs_id.y, source_main_port_uhgs_id)) 

#########################################################################
## 1.g suppléments
## Dans pointcalls, renommer les colonnes pointcall.x en pointcall et  pointcall_uhgs_id.x en pointcall_uhgs_id
## Dans pointcalls, supprimer les colonnes pointcall.y et  pointcall_uhgs_id.y
## Sauver le résultat dans data_1789_d.csv
#########################################################################

colnames(pointcalls)[17] <- "pointcall"
colnames(pointcalls)[18] <- "pointcall_uhgs_id"

## Dans pointcalls, supprimer les colonnes pointcall.y et  pointcall_uhgs_id.y
pointcalls <- pointcalls %>% select(-c(pointcall.y, pointcall_uhgs_id.y))

## Sauver le résultat
write.table(pointcalls, "./data/portic/data_1789_d.csv", sep = "\t")



############################################################################
## 2.a Lire les ports dans ports_ajouts.csv et la liste des ports du Poitou spécifiée dans etude_1789.csv 
## et utiliser cette seconde liste pour faire une carte localisant ces ports (uniquement ceux là), 
## en ajoutant les noms de ces ports.
############################################################################
pointcalls <- read.table("./data/portic/data_1789_d.csv", sep = "\t")

## Utiliser mutate pour transformer le type d'une variable (par exemple)
## Rq : vu le nombre de variables de   pointcalls cela peut devenir ennuyeux.
## Donc on vous propose mutate_at qui agit sur un ensemble de variables listées par vars()
## et avec des fonctions listées dans list()
library(lubridate)
library(tidyverse)
pointcalls <- pointcalls %>%
  mutate_at(vars(pointcall_action, pointcall_function, ship_class, tonnage_class, data_block_leader_marker ), list(as.factor))  %>%
  mutate_at(vars(tonnage, taxe, q01, q02, q03, quantity, in_crew, latitude, longitude), list(as.numeric)) %>%
  mutate_at(vars(homeport, homeport_uhgs_id, homeport_admiralty, homeport_states), list(as.factor)) %>%
  mutate_at(vars(source_suite, source_component, source_main_port_uhgs_id, source_main_port_toponyme, tonnage_class, source_suite), list(as.factor)) %>%
  mutate_at(vars(outdate_fixed, indate_fixed), list(ymd))

summary(pointcalls)


library(readr)
poitou <- read_delim("./data/portic/etude_1789.csv", ";", escape_double = FALSE, trim_ws = TRUE)



## Filtrer les pointcalls pour ne retenir que ceux inclus dans la liste des source_component du poitou
study <- pointcalls %>% filter (source_component %in% poitou$source_component)
dim(study)
13519    80

sort(unique(study$source_main_port_toponyme))
[1] "Aligre de Marans"        "Ars en Ré"               "Beauvoir-sur-Mer"        "Champagné-les-Marais"   
[5] "Charente"                "Esnandes"                "Ile de Bouin"            "La Flotte en R�"        
[9] "La Perrotine"            "La Rochelle"             "La Tranche sur Mer"      "Le Chateau d' Oléron"   
[13] "Marennes"                "Moricq"                  "Mortagne"                "Noirmoutier"            
[17] "Ribérou Saujon"          "Rochefort"               "Royan"                   "Sables d' Olonne"       
[21] "Saint Denis d' Oléron"   "Saint Gilles sur Vie"    "Saint Martin de Ré"      "Saint Michel en l' Herm"
[25] "Soubise"          


summary(study)

## Charger les données et calculer des résumés associés aux ports
ports <- read.table("./data/portic/ports_ajouts.csv", sep = "\t")

## Calculer le nb de congés délivrés
## Calculer le nb de navires observés différents
## Calculer la somme du tonnage des navires observés
## Calculer le tonnage moyen des navires observés
## Calculer le tonnage median des navires observés
## Et rajouter ces informations au dataframe portsf avec une jointure INTERNE 
# entre source_main_port_uhgs_id et la colonne uhgs_id


study  <- study %>% 
  group_by(source_main_port_uhgs_id) %>% 
  summarise(nb_conges = length(unique(source_doc_id)),
            nb_ships_locaux = length(unique(na.omit(ship_name))),
            sum_tonnage_locaux = sum(tonnage, na.rm = TRUE),
            mean_tonnage = mean(tonnage, na.rm = TRUE),
            median_tonnage = median(tonnage, na.rm = TRUE)) %>% 
  inner_join(ports, by = c("source_main_port_uhgs_id" = "uhgs_id")) 

#associer les données de localisation pour ces ports

colnames(study)
[1] "source_main_port_uhgs_id" "nb_conges"                "nb_ships_locaux"          "sum_tonnage_locaux"      
[5] "mean_tonnage"             "median_tonnage"           "pkid"                     "toponyme"                
[9] "latitude"                 "longitude"                "amiraute"                 "province"                
[13] "state_1787"               "substate_1787"            "oblique"                  "type"                    
[17] "nb_ships"                 "sum_tonnage"              "distance_cote_meters"     "distance_river_meters"   
[21] "distance_water"

### Cartographier l'activité de ces ports

library("sf")
library("cartography")


## 4326 : projection EPSG du WGS 84
# transformer les longitudes et latitudes en POINT, et rajouter les attributs du tableau comme variable
studysf <- st_as_sf(study, coords=c("longitude", "latitude"), crs = 4326)


## Projection EPSG 3857
studysf <- st_transform(studysf, 3857)



colnames(studysf)
[1] "source_main_port_uhgs_id" "nb_conges"                "nb_ships_locaux"          "sum_tonnage_locaux"      
[5] "mean_tonnage"             "median_tonnage"           "pkid"                     "toponyme"                
[9] "amiraute"                 "province"                 "state_1787"               "substate_1787"           
[13] "oblique"                  "type"                     "nb_ships"                 "sum_tonnage"             
[17] "distance_cote_meters"     "distance_river_meters"    "distance_water"           "geometry"           


## Refaire le graphique avec des marges réduites
## Les couches et styles cartographiques s'empilent les uns sur les autres dans l'ordre d'appel
## comme dans un SIG classique
# reduire la largeur des marges avec par(mar=())
dev.off()

## Définir la résolution de l'export en image à l'avance
#ppi <- 300
#png("figures/Portic/carte_ports_poitou-3a.png", width = 3*ppi, height = 4*ppi, res=ppi) 

# c(bas, gauche, haut, droite)
# OMA : augmenter la largeur de la MARGE EXTERNE (oma) sur les bords gauche et droit de 1
# MAR : reduire la largeur de la MARGE INTERNE (mar), sauf en haut ( + 1.5)
par(mar = c(0.5,0,1.5,0), oma = c(0,0,0,0))

## Créer une bounding box correspondant à l'emprise de studysf
bbox <- st_bbox(st_geometry(studysf))
#xmin: -250468.9 ymin: 5697936 xmax: -87200.23 ymax: 5942074

#####################################################
## 2.b Ajoutez les fonds de carte des côtes 
#####################################################

## Rajouter le trait de cote

## Option 1. Suivre les indications de https://rgeomatic.hypotheses.org/1288 et utiliser le package rnaturalearthdata à installer

library(rnaturalearth)
library(rnaturalearthdata)

install.packages("rnaturalearth", meslibrairiesR)
install.packages("rnaturalearthdata", meslibrairiesR)

coastline <- ne_coastline(scale = 50, returnclass = "sf")
coastline <- st_transform(coastline, 3857)

plot(st_geometry(coastline), 
     xlim = c(bbox[1],  bbox[3]), 
     ylim = c(bbox[2],  bbox[4]), 
     col="lightblue", lwd=1)

##  Option 2 : le fichier  trait_cote_monde_gshhs_i_L1_4326.geojson
coastline <- st_read(dsn = "./data/portic/trait_cote_monde_gshhs_i_L1_4326.geojson")
#coastline <- coastline %>% filter(id %in% c("1037","1642","2188","3389","0-E"))
coastline <- st_transform(coastline, 3857)

plot(st_geometry(coastline), 
     xlim = c(bbox[1],  bbox[3]), 
     ylim = c(bbox[2],  bbox[4]), 
     col="bisque", lwd=1)

## Rajouter les rivières
rivers <- st_read(dsn = "C:\\Travail\\ULR_owncloud\\ANR_PORTIC\\Data\\ports\\rivers\\rivers_poitou_3857_b.geojson")
rivers <- st_transform(rivers, 3857)

plot(st_geometry(rivers), 
     xlim = c(bbox[1]*0.1,  bbox[3]*0.1), 
     ylim = c(bbox[2],  bbox[4]), 
     col="lightblue", add=T, lwd=1)

# Coloriser en noir les cercles, avec comme largeur de contour lwd = 1
plot(st_geometry(studysf), col="black", lwd=1, add=T)

# Nommer les ports de la carte  
labelLayer(x = studysf, txt = "toponyme",cex = 0.6, col= "#000000",overlap = FALSE) 

# Rajouter le cartouche, la légende, l'échelle, etc.
layoutLayer(title = "Ports du Poitou, 1787",
            author = "C.Plumejeaud, Cours M2 stats 2020", sources = "ANR Portic, 2020",
            scale = 15, tabtitle = FALSE,
            frame = FALSE,theme = "blue.pal",
            north = TRUE)


## Comparer les avantages et inconvénients des cartes réalisées avec les deux fonds de carte.

#####################################################
## 2.c Rajouter la visualisation sur l’un des fonds des nombres de congés délivrés (cercles proportionnels) 
## et en colorant les ports en fonction de leur amirauté d’appartenance. 
#####################################################

## rajouter les nombre de congés délivrés représentés par des cercles proportionnels sur les ports
propSymbolsTypoLayer(x = studysf, var = "nb_conges", var2 = "amiraute",
                     symbols = "circle",
                     inches = 0.2,
                     border = "white", 
                     lwd = 0.1,
                     legend.var.pos = "topright", 
                     legend.var.title.txt = "Nombre de congés enregistrés",
                     legend.var2.pos = "left", 
                     legend.var2.title.txt = "Amirauté du port")

# Signaler si on a bien les congés des ports
plot(st_geometry(filter(studysf, !is.na(oblique) & oblique == TRUE)), pch=20, col="black", cex=0.2, add=T) # Les obliques


dev.off()

dev.off()





