######################################################################
## Christine PLUMEJEAUD, 19/02/2021 - 26/02/2021
##
## 0. Ouverture des sources de données : lire la base de données
## 1. les tonnage des bateaux 
##  1.1 vérifier la corrélation entre tonnage et in_crew sur Marseille quand tonnage_uncertainity est 0
##  1.2 analyser la moyenne des tonnages par classe de navire
##  1.3 vérifier que des ports comme Marennes ou Nantes sur-classent les navires (tonnge 1.5 fois plus important)
## 2. Mail de Silvia : les motifs des sorties (pêches ou destination Angleterre)
## justifieraient des sous/sur tonnages
######################################################################

#setwd("C:\\Travail\\Dev\\portic_humanum\\PORTIC_stats")
setwd("D:\\Dev\\PORTIC_stats")
getwd()

#meslibrairiesR <- "C:/Tools/R4"
meslibrairiesR <- "C:/Tools/R/R4"
# Ajouter meslibrairiesR dans ces chemins : 
.libPaths(c( .libPaths(), meslibrairiesR) )

library(RPostgreSQL)
library(tidyverse)
library(Cairo)

install.packages("RPostgres", meslibrairiesR)
install.packages("Cairo", meslibrairiesR)




## Sauver / reprendre son travail sous R
# https://mtes-mct.github.io/parcours-r/m1/sauvegarder-son-travail.html 
dir.create("./outputs")

save(list = ls(), file = "outputs/env_entier.RData") 
# sauvegarde de tout l'environnement sur le répertoire choisi 

rm(list = ls()) # suppression de notre environnement dans R 

load("outputs/env_entier.RData") 
# chargement de l'environnement stocké sur l'ordinateur 

rm(subset)

######################################################################
######################################################################
## 1. Récupérer les données depuis Postgres
######################################################################
######################################################################
con <- dbConnect(dbDriver("PostgreSQL"), host='localhost', port='5432', dbname='portic_v6', user='postgres', password='postgres')

library(RPostgres)
con <- dbConnect(RPostgres::Postgres(), host='localhost', port='5432', dbname='portic_v6', user='postgres', password='postgres')

#postgresqlpqExec(con, "SET client_encoding = 'utf-8'")


data_poitou <- dbGetQuery(con, "select * from csv_extract where source_subset = 'Poitou_1789'")
pointcalls <- dbGetQuery(con, "select * from csv_extract ", encoding = "utf8")
# 108530 obs and 119 variables


## Utiliser mutate pour transformer le type d'une variable (par exemple)
## Rq : vu le nombre de variables de   pointcalls cela peut devenir ennuyeux.
## Donc on vous propose mutate_at qui agit sur un ensemble de variables listées par vars()
## et avec des fonctions listées dans list()
library(lubridate)
library(tidyverse)
pointcalls <- pointcalls %>%
  mutate_at(vars(pointcall_action, pointcall_function, ship_class, flag, ship_flag_id, tonnage_class, data_block_leader_marker ), list(as.factor))  %>%
  mutate_at(vars(state_1789, substate_1789_fr, ferme_bureau, ferme_direction), list(as.factor))  %>%
  mutate_at(vars(partner_balance_1789, partner_balance_supp_1789),  list(as.factor))  %>%
  mutate_at(vars(homeport_state_1789, homeport_substate_1789_fr, homeport_ferme_bureau, homeport_ferme_direction), list(as.factor))  %>%
  mutate_at(vars(homeport_partner_balance_1789, homeport_partner_balance_supp_1789),  list(as.factor))  %>%
  mutate_at(vars(tonnage, q01, q02, q03, quantity, latitude, longitude, nb_products), list(as.numeric)) %>%
  mutate_at(vars(taxe, taxe2, taxe3, taxe4, taxe5, nb_taxes), list(as.numeric)) %>%
  mutate_at(vars(pointcall_admiralty, pointcall_province, pointcall_status, homeport_province, homeport_admiralty), list(as.factor)) %>%
  mutate_at(vars(source_suite, source_main_port_uhgs_id, source_main_port_toponyme, tonnage_class, source_subset), list(as.factor)) %>%
  mutate_at(vars(outdate_fixed, indate_fixed), list(ymd)) %>%
  mutate_at(vars(source_available), list(as.logical))%>%
  mutate_at(vars(ship_class_codage), list(as.factor))


summary(pointcalls)
dim(pointcalls)# 108530        119

######################################################################
######################################################################
##  1.0 Convertir les quintaux en tonneaux
######################################################################
######################################################################

pointcalls$tonnage <- as.numeric(pointcalls$tonnage)
unique(pointcalls$tonnage_unit)
# [1] "Tx"        NA          "quintaux"  "tx"        "Quintaux"  "tonneaux"  "tonnaux"   "Ton"      
# [9] "quintaux]"

#transformer en facteurs les unités 
f <- as.factor(pointcalls$tonnage_unit)
levels(f) 
#"quintaux"  "Quintaux"  "quintaux]" "Ton"       "tonnaux"   "tonneaux"  "tx"        "Tx"
table(f)
# quintaux  Quintaux quintaux]       Ton   tonnaux  tonneaux        tx        Tx 
# 740        10         1         3         4         2     82055       368 

# Regrouper les "Ton"      "Tx"  "tx" en "tonneaux"
levels(f) <- c("quintaux", "quintaux", "quintaux", "tonneaux","tonneaux","tonneaux","tonneaux" ,"tonneaux") 
#ATTENTION, Ton n'est pas tonneaux, mais c'est 3 navires. Il faudrait idéalement filtrer ces 3 entrées

levels(f) 
#[1] "quintaux" "tonneaux"

table(f)
# f
# quintaux tonneaux 
# 751    82432

# Remplacer par ce facteur à 2 modalités la variable tonnage_unit 
pointcalls$tonnage_unit <- f

# Utiliser la librairie doBy pour calculer des statistiques regroupées par ce facteur
# Aide : https://www.rdocumentation.org/packages/doBy/versions/4.6.7/topics/by-summary
library(doBy)

summaryBy(tonnage  ~ tonnage_unit, pointcalls, fun.names=c("moyenne", "ecart-type", "nombre"),
          FUN = function(x) {
            return(c(
              mean(x, na.rm=TRUE), 
              sd(x, na.rm=TRUE), 
              length(na.omit(x))
            ))
          })

# tonnage_unit tonnage.moyenne tonnage.ecart-type tonnage.nombre
# 1     quintaux      3865.25503         1118.20965            745
# 2     tonneaux        56.45138           68.59994          82283
# 3         <NA>             NaN                 NA              0

## Convertir les quintaux en tonneaux
pointcalls[which(pointcalls$tonnage_unit=="quintaux"), ]$tonnage <- pointcalls[which(pointcalls$tonnage_unit=="quintaux"), ]$tonnage / 24.0

summaryBy(tonnage  ~ tonnage_unit, pointcalls, fun.names=c("moyenne", "ecart-type", "nombre"),
          FUN = function(x) {
            return(c(
              mean(x, na.rm=TRUE), 
              sd(x, na.rm=TRUE), 
              length(na.omit(x))
            ))
          })

# tonnage_unit tonnage.moyenne tonnage.ecart-type tonnage.nombre
# 1     quintaux       161.05229           46.59207            745
# 2     tonneaux        56.45138           68.59994          82283
# 3         <NA>             NaN                 NA              0

### Observer la distribution des tonnages avec un histogramme (hist())
# Aide : https://www.datamentor.io/r-programming/histogram/
hist(pointcalls$tonnage)

# Avec un titre : main = ""
hist(pointcalls$tonnage, main="Distribution des tonnages des navires, exprimés en tonneaux")

# En zoomant sur la majeure partie xlim = c()
hist(pointcalls$tonnage, main="Distribution des tonnages des navires, exprimés en tonneaux",
     xlim = c(0, 500) )

# En détaillant la partie sous 100
hist(pointcalls$tonnage, main="Distribution des tonnages des navires, exprimés en tonneaux",
     xlim = c(0, 500), 
     breaks=c(0, 10, 20, 30, 40, 50, 75, 100, 200, 300, 500, 1667 ))


max(pointcalls$tonnage, na.rm = TRUE) #1667

# Rajouter une barre verticale rouge signalant la moyenne
abline(v=mean(pointcalls$tonnage, na.rm = T), lty = 3, col="red")

# Rajouter une barre verticale bleue signalant la médiane 
abline(v=median(pointcalls$tonnage, na.rm = T), lty=2, col="blue")

# Rajouter la courbe de densité des observations 
lines(density(pointcalls$tonnage, na.rm = T))

# Séparer les observations en quintaux des observations en tonneaux
lines(density(pointcalls[which(pointcalls$tonnage_unit=="quintaux"), ]$tonnage, na.rm = T), col="green")
lines(density(pointcalls[pointcalls$tonnage_unit=="tonneaux", ]$tonnage, na.rm = T), col="chocolate")
 legend()
getwd()

dev.copy(png,'./outputs/tonnage_distributions_comparées.png')
dev.off()

#http://www.sthda.com/french/wiki/ggplot2-courbe-de-distribution-guide-de-demarrage-rapide-logiciel-r-et-visualisation-de-donnees#changer-la-couleur-de-la-courbe-de-distribution-par-groupe
ggplot(pointcalls %>% filter(tonnage> 0 & tonnage < 500), aes(x=tonnage, color=tonnage_unit, fill=tonnage_unit)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+
  geom_density(alpha=.2)
dev.copy(png,'./outputs/tonnage_distributions_comparées_ggplot.png')
dev.off()

######################################################################
################################################################################
##  1.1 vérifier la corrélation entre tonnage et in_crew sur Marseille 
## quand tonnage_uncertainity est 0
################################################################################
######################################################################

pointcalls$in_crew <- as.numeric(pointcalls$in_crew)
subset <- pointcalls %>% filter (!is.na(in_crew) & !is.na(tonnage)) 
#532 obs
subset2 <- pointcalls %>% filter (!is.na(in_crew) & !is.na(tonnage)) %>% filter (tonnage_uncertainity==0)
#194 obs


dev.off()
plot(subset$in_crew, subset$tonnage)
plot(subset2$in_crew, subset2$tonnage)

# library (Hmisc)
# rescorr<-rcorr(subset$in_crew, subset$tonnage)
# MatCor<-rescorr$r

grep(pattern = "in_crew", colnames(subset)) #51
grep(pattern = "tonnage", colnames(subset)) #43 44 45 91
"tonnage" %in% colnames(subset)
match(c("tonnage"), colnames(subset))

Mat <- subset[, c(grep(pattern = "in_crew", colnames(subset)), match(c("tonnage"), colnames(subset)))]
Mat2 <- subset2[, c(grep(pattern = "in_crew", colnames(subset2)), match(c("tonnage"), colnames(subset2)))]

## Correlation

cor(subset$in_crew, subset$tonnage)
#0.7328775


cor(subset2$in_crew, subset2$tonnage)
#0.5714567

library(corrplot)


cor.mtest(Mat)
cor.mtest(Ma2)

corrplot(cor(Mat),method="number")
corrplot(cor(Mat2),method="number")

corrplot(cor(Mat), type="upper", order="hclust", 
         p.mat = cor.mtest(Mat)$p, sig.level = 0.01)

model<-lm(tonnage~in_crew,data=subset)
summary(model)


model2<-lm(tonnage~in_crew,data=subset2)
summary(model2)

library(texreg)
screenreg (model, ci.force = TRUE)
screenreg (model2, ci.force = TRUE)

# ===========================
#   Model 1     : model<-lm(tonnage~in_crew,data=subset)  
# ---------------------------
#   (Intercept)   54.75 *      
#   [46.30; 63.21]
# in_crew        8.01 *      
#   [ 7.38;  8.64]
# ---------------------------
#   R^2            0.54        
# Adj. R^2       0.54        
# Num. obs.    532           
# ===========================
#   * 0 outside the confidence interval.


# ==========================
#   Model 2      : model2<-lm(tonnage~in_crew,data=subset2)
# --------------------------
#   (Intercept)   34.49 *     
#   [8.26; 60.71]
# in_crew       10.79 *     
#   [8.60; 12.98]
# --------------------------
#   R^2            0.33       
# Adj. R^2       0.32       
# Num. obs.    194          
# ==========================
#   * 0 outside the confidence interval.

## Figures


ggplot(data = subset,
       mapping = aes(x = in_crew, y=tonnage)) + 
  geom_point()+
  labs(x = "Taille de l'équipage",
       y = "Tonnage du bateau lorsque connu (converti en tonneaux)",
       title = "Relation entre taille de l'équipage à l'arrivée (in_crew) et tonnage des navires",
       subtitle = "La forme de la relation est-elle linéaire ?",
       caption = "Source : ANR PORTIC, C. Plumejeaud, février 2021")
dev.copy(png,'./outputs/plot_model.png')
dev.off()

ggplot(data = subset,
       mapping = aes(x = in_crew, y=scale(model$residuals))) + 
  geom_point()+
  geom_line(y=0)+
  labs(x = "Taille de l'équipage",
       y = "Résidus normalisés du tonnage du bateau (converti en tonneaux) estimé",
       title = "Résidus de la régression entre le log 10 de la densité de population et l'éloignement à une unité urbaine",
       subtitle = "les résidus sont-ils homogènes ?",
       caption = "Source : ANR PORTIC, C. Plumejeaud, février 2021")
dev.copy(png,'./outputs/plot_residus.png')
dev.off()


# ajouter l’intervalle de prédiction (qui présuppose la normalité des résidus, attention)
pointcallsSupp <- cbind(subset, predict(model, interval="prediction"))
pointcallsSupp2 <- cbind(subset2, predict(model2, interval="prediction"))


# lwr (comme lower) et upr (comme upper) représentent les bornes maximales inférieures et supérieures des prédictions possibles avec le modèle 3.


## Coloriser les tonnages en fonction de leur incertitude
pointcallsSupp$tonnage_uncertainity <- as.factor(pointcallsSupp$tonnage_uncertainity)
levels(pointcallsSupp$tonnage_uncertainity)

## Choisir sa palette de couleur
# tmaptools::palette_explorer()
#maPalette <- tmaptools::get_brewer_pal("OrRd", n = 3)
#maPalette <-tmaptools::get_brewer_pal("Blues", n = 3, contrast = c(0.43, 0.8))
maPalette <- tmaptools::get_brewer_pal("Oranges", n = 3, contrast = c(0.43, 0.8))

#viridisLite::viridis(3, begin = 0.55, end = 1)

ggplot(pointcallsSupp, aes(y=tonnage, x=in_crew, color=tonnage_uncertainity))+
  geom_point( )+
  #scale_color_manual(values=maPalette)+
  geom_smooth(colour="red", method="lm", fill="red") +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+    
  labs(y = "Tonnage du bateau lorsque connu (converti en tonneaux)",
       x= "Taille de l'équipage",
       title = "Régression linéaire simple entre la taille de l'équipage \n et le tonnage du navire",
       caption = "Source : ANR PORTIC, C. Plumejeaud, février 2021")+
  theme_classic()+
  annotate("text", size=3, x = 35, y = 1, label = "tonnage = 54.75  + 8.01 * in_crew (pval<0.001)")
dev.copy(png,'./outputs/plot_model_equation_intervalle_conf.png')
dev.off()

ggplot(pointcallsSupp2, aes(y=tonnage, x=in_crew, color=tonnage_uncertainity))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+    
  labs(y = "Tonnage du bateau lorsque connu (converti en tonneaux)",
       x= "Taille de l'équipage",
       title = "Régression linéaire simple entre la taille de l'équipage \n et le tonnage du navire",
       caption = "Source : ANR PORTIC, C. Plumejeaud, février 2021")+
  theme_classic()+
  annotate("text", x = 13, y = 1, label = "tonnage = 34.49  + 10.79 * in_crew (pval<0.001)")
dev.copy(png,'./outputs/plot_model2_equation_intervalle_conf.png')
dev.off()

################################################################################
################################################################################
## 1.2 analyser la moyenne des tonnages par classe de navire
################################################################################
################################################################################


subset <- pointcalls %>% filter (ship_class_codage != 'unknown' & !is.na(tonnage)) 
#12461 lignes
subset2 <-  subset %>% filter (tonnage_uncertainity==0)
subset2 <-  subset %>% filter (tonnage < 500)
#10866 lignes
hist(subset$tonnage)
                


unique(pointcalls$ship_class_codage)
# [1] unknown      brigantin    tartane      allège       pinque       bateau      
# [7] polacre      félouque     navire       vaisseau     corvette     bombarde    
# [13] bisque       chasse-marée chaloupe     chebec       frégate      senau       
# [19] goelette     barque       sloop        galiote      cotre        lougre      
# [25] brick        queche       schooner     gabare       gondole      cutter      
# [31] ship    

unique(subset$ship_class_codage)

# Aide pour le boxplot : https://duclert.org/r-graphiques/boxplot-R.php
boxplot(tonnage ~ ship_class_codage, subset, las=2, xlab = "",
        main="Distribution du tonnage par classe de navire ")

#https://www.r-graph-gallery.com/9-ordered-boxplot.html
# Create a vector named "new_order" containing the desired order
new_order <- with(subset2, reorder(ship_class_codage , tonnage, median , na.rm=T))


resume <- subset2 %>%
  group_by(ship_class_codage) %>%
  summarise(mean = round(mean(tonnage)), 
            n = n(), 
            m = round(median(tonnage)), 
            q3 = quantile(tonnage,probs=0.75),
            q1 = quantile(tonnage,probs=0.25))%>%
  arrange( m)

##Reordonner les levels de ship_class_codage pour correspondre au tri par median
resume$ship_class_codage <- factor(unique(resume$ship_class_codage), levels=unique(resume$ship_class_codage))

boxplot(subset2$tonnage ~ new_order, ylim=c(-100,500),
        ylab="tonnage en tonneaux" , col="#69b3a2", boxwex=0.4 , 
        las=2, xlab = "", 
        main="Distribution du tonnage par classe de navire \n ordonnées par tonnage median (bleu) \n avec le tonnage moyen (rouge)")
text(cex=0.7, col="red", x=resume$ship_class_codage, -30, resume$mean, xpd=TRUE, srt=0) #insert the text on the graph.
text(cex=0.7, col="blue", x=resume$ship_class_codage, -75, resume$m, xpd=TRUE, srt=35) #insert the text on the graph.

dev.copy(png,'./outputs/boxplot_tonnage_ship_class_zoom500.png')
dev.off()

summaryBy(tonnage ~ ship_class_codage, subset,  
          fun.names=c("n","moyenne", "Z0.975", "largeur", "borne_inf", "borne_sup" ), 
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
# allège          et schooner         n'ont pas tonnage
tapply(subset$tonnage, list(ship_class_codage= subset$ship_class_codage), t.test)
tapply(subset$tonnage, list(ship_class_codage= subset$ship_class_codage), mean)

new_order <- with(subset2, reorder(ship_class_codage , tonnage, median , na.rm=T))

# resume <- subset %>%
#   group_by(ship_class_codage) %>%
#   summarise(mean = round(mean(tonnage)), n = n(),) %>%
#   
# resume$mean
# levels(resume$ship_class_codage)

resume$ship_class_codage <- factor(unique(resume$ship_class_codage))

install.packages("gplots", meslibrairiesR)
library(gplots )

plotmeans((subset$tonnage)~subset$ship_class_codage, xlab="", ylab="Tonnage",
          main="Tonnage moyen par classe de navire avec son intervalle de confiance à 95%",
          n.label = FALSE, text.n.label = "", las=2,connect=FALSE)#mean.labels=resume$mean, cex.sub = 2

plotmeans((subset2$tonnage)~new_order, xlab="", ylab="Tonnage",
          main="Tonnage moyen (rouge) par classe de navire \n avec son intervalle de confiance à 95%",
          n.label = FALSE, text.n.label = "", las=2,connect=FALSE)#mean.labels=resume$mean, cex.sub = 2

#Ajouter les tonnages moyens  
text(col="red",cex=0.7, x=resume$ship_class_codage, y=resume$mean+30, resume$mean, xpd=TRUE, srt=0) #insert the text on the graph.

dev.copy(png,'./outputs/moyenneCI_tonnage_ship_class.png')
dev.off()      



#########################################################################
# Réaliser une anova
# 1.2.a Vérifier l’homogénéité des variances par classe de navire avec le test de Bartlett  
#########################################################################

##refactor (allege et schooner ne sont plus de la partie)
subset$ship_class_codage <- factor(subset$ship_class_codage)
levels(subset$ship_class_codage )

bartlett.test(tonnage~ship_class_codage, subset )
#Bartlett's K-squared = Inf, df = 27, p-value < 2.2e-16

# On rejete H0 a tort au risque 2.2e-16, donc on accepte H0 (homogénéité des variances)
# Interprétation : donc les variances de tonnage sont équivalentes en fonction des classes de navire

#########################################################################
# Réaliser une anova
# 1.2.b Vérifier la normalité des distributions
#########################################################################

# Représenter par facets la distribution des distances parcourues par chaque mois 
#  https://besibo.github.io/DA/viz.html#facets 

ggplot(subset %>% filter(tonnage<100), 
       aes(x = tonnage, fill = factor(ship_class_codage))) +
  geom_histogram(bins = 20, color = "grey30") +
  facet_wrap(~factor(ship_class_codage), ncol = 3)

# Vérifier la normalité de la distribution pour les classes barque et bateau
library(nortest)
lillie.test(subset [subset$ship_class_codage=="barque", ]$tonnage)
#D = 0.31622, p-value < 2.2e-16
# distribution pas normale pour les barques

lillie.test(subset [subset$ship_class_codage=="bateau", ]$tonnage)
# distribution pas normale pour les bateaux

#########################################################################
# Réaliser une anova
## 1.2.c Réaliser le test de Krustal-Wallis qui supporte la non-normalité des entrées
## https://www.datanovia.com/en/fr/lessons/test-de-kruskal-wallis-dans-r/
#########################################################################


library(rstatix)

res.kruskal <- subset %>% kruskal_test(tonnage ~  ship_class_codage)
print(res.kruskal)
# .y.                n statistic    df        p method        
# * <chr>          <int>     <dbl> <int>    <dbl> <chr>         
#   1 tonnage 12461     7617.    27     0 Kruskal-Wallis
# Oui, on note une influence de la classe des navires en général sur le tonnage

subset %>% kruskal_effsize(tonnage ~ ship_class_codage)
# .y.                n effsize method  magnitude
# * <chr>          <int>   <dbl> <chr>   <ord>    
#   1 tonnage 12461   0.610 eta2[H] large 
# Et cette influence est forte (eta-2 vaut 0.610)
## Mais si on prend classe par classe, on peut distinguer des choses plus intéressantes



# Version qui combine tous les groupes (27) 2 à 2 pour détecter les classes qui sont assez spécialisées en tonnage
# https://www.datanovia.com/en/fr/lessons/test-de-kruskal-wallis-dans-r/


res <- subset %>% 
  wilcox_test(tonnage ~ ship_class_codage, p.adjust.method = "bonferroni")

dim(res)
#378 9

res %>% filter (p.adj<0.05) %>% select(group1, group2, p.adj)
dim(res[res$p.adj<0.05,c(2, 3)]) ## 235 lignes avec un impact fort


aregrouper <- res %>% filter (p.adj>0.1) %>% select(group1, group2, p.adj)

######################################################################
######################################################################
# 1.3 Identifier les ports comme Marennes ou Nantes 
# qui sur-classent les navires (tonnge 1.65 fois plus important)
######################################################################
######################################################################

subset <- pointcalls %>% 
  filter (!is.na(ship_id) & !is.na(tonnage)) %>%
  mutate_at(vars(ship_id), list(as.factor))

#65573 obs


unique(subset$ship_id)
#8990 entrées


# Calculer par navire les résumés de tonnage, et le nombre de obliques visités
# Filtrer pour ne garder que les navires visitant plus qu'un port oblique, 
# et avec des tonnages différents
resume <- subset %>%
  group_by(ship_id) %>%
  summarise(mean = round(mean(tonnage)), 
            n = n(), 
            mediane = round(median(tonnage)), 
            q3 = quantile(tonnage,probs=0.75),
            q1 = quantile(tonnage,probs=0.25),
            nbobliques = length(unique(source_main_port_toponyme)))%>%
  arrange(mediane) %>%
  filter (n>1 & nbobliques > 1 ) #& mediane!=mean
#3122 ships / 1058 ships si mediane!=mean / 8911 ships

#Filtrer subset pour conserver les navires intéressants identifiés dans resume
keep<-  subset %>% 
  inner_join(resume , by = c("ship_id" )) 
#16425

#Pour les navires visitant plus qu'un port oblique, et avec des tonnages différents
#Calculer par navire, par port visité, le tonnage moyen et le nombre de visites
resume2 <- keep %>%
  group_by(ship_id,source_main_port_toponyme) %>%
  summarise(moyenne = round(mean(tonnage)), 
            n = n(), 
            min = min(tonnage),
            max = max(tonnage),
            mediane = round(median(tonnage)), 
            q3 = quantile(tonnage,probs=0.75),
            q1 = quantile(tonnage,probs=0.25))%>%
  arrange(mediane) 
# 4313 / 10212

# 
# resume2  %>% filter (source_main_port_toponyme!='Marennes') %>%
#   group_by(ship_id,source_main_port_toponyme) %>%
#   summarise(moyenneSans = round(mean(moyenne))) 
# library(reshape2)
# mat <- dcast(resume2, ship_id ~ source_main_port_toponyme, fun.aggregate = function(x) mean(x))

#Determiner le port avec la moyenne maximale
resume3 <- resume2  %>% 
  group_by(ship_id) %>%
  summarise(maxPort = max(moyenne), q3Port = median(q3), maxMediane = max(mediane) )  
#1058  / 3122

#Rajouter à resume2 le port avec le tonnage max
resume4<-  resume2 %>% 
  inner_join(resume3 , by = c("ship_id" )) %>%
  arrange(mediane) #Determiner le port avec la moyenne ou la médiane maximale
# 4313 / 10212

#Ne retenir que ceux de :

# - tonnage moyen maximal
resume5<-  resume4 %>% 
  filter(moyenne==maxPort) %>%
  group_by(source_main_port_toponyme)%>%
  summarise(n = n())%>%
  arrange(n)

write.table(resume5, "./outputs/resume5.csv", sep = "\t")

# - tonnage Q3 > Q3 médian
resume5b<-  resume4 %>% 
  filter(q3>=q3Port) %>%
  group_by(source_main_port_toponyme)%>%
  summarise(n = n())%>%
  arrange(n)

write.table(resume5b, "./outputs/resume5b.csv", sep = "\t")

# - tonnage médian maximal
resume5c<-  resume4 %>% 
  filter(mediane>=maxMediane) %>%
  group_by(source_main_port_toponyme)%>%
  summarise(n = n())%>%
  arrange(n)

write.table(resume5c, "./outputs/resume5c.csv", sep = "\t")

#####################
# Figures
hist(resume5b$n)

ggplot(resume5, aes(x=n)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+
  geom_density(alpha=.2)+
  labs(x = "Volume de navires sur-évalués (Moyenne maximale pour le navire)",
       y = "Densité",
       title = "Nombres de navires sur-évalués par port oblique")

dev.copy(png,'./outputs/Ports_surtonnage_max.png')
dev.off() 

ggplot(resume5b, aes(x=n)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+
  geom_density(alpha=.2)+
  labs(x = "Volume de navires sur-évalués (Q3 > Q3 médian pour le navire)",
       y = "Densité",
       title = "Nombres de navires sur-évalués par port oblique")
dev.copy(png,'./outputs/Ports_surtonnage_q3.png')
dev.off() 

ggplot(resume5c, aes(x=n)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+
  geom_density(alpha=.2)+
  labs(x = "Volume de navires sur-évalués (Médiane maximale pour le navire)",
       y = "Densité",
       title = "Nombres de navires sur-évalués par port oblique")
dev.copy(png,'./outputs/Ports_surtonnage_mediane.png')
dev.off() 

#####################
## Essais
# Le Havre, Bordeaux, Nantes, Marennes
# portsAnormaux <-  resume5c %>% 
#   filter(n>=quantile(n,probs=0.95))%>% 
#   select(source_main_port_toponyme)
# 
# #resume4$ecart_moyen <- (resume4$maxPort - resume4$moyenne)/resume4$moyenne*100
# portsAnormaux$source_main_port_toponyme  
# #Le Havre Bordeaux Nantes   Marennes

#####################
# Bonne méthode
#####################

compareOne2Others <- function(port = "Dunkerque") {
  #Utiliser keep ou subset
  resumeOthers <- keep %>% 
    filter( (source_main_port_toponyme !=  port)) %>%
    group_by(ship_id) %>%
    summarise(moyenne = round(mean(tonnage)), 
              n = n(), 
              min = min(tonnage),
              max = max(tonnage),
              mediane = round(median(tonnage)), 
              q3 = quantile(tonnage,probs=0.75),
              q1 = quantile(tonnage,probs=0.25))
  
  resumeLeHavre <- keep %>% filter(source_main_port_toponyme == port) %>%
    group_by(ship_id,source_main_port_toponyme) %>%
    summarise(moyenne = round(mean(tonnage)), 
              n = n(), 
              min = min(tonnage),
              max = max(tonnage),
              mediane = round(median(tonnage)), 
              q3 = quantile(tonnage,probs=0.75),
              q1 = quantile(tonnage,probs=0.25))

  finLeHavre <- resumeLeHavre %>% 
    inner_join(resumeOthers , by = c("ship_id" )) %>% 
    mutate (ratioMoyenne = moyenne.x/moyenne.y )%>%
    mutate (ratioMediane = mediane.x/mediane.y )%>%
    mutate (ratioq3 = q3.x/q3.y )%>%
    mutate (ratiomax = max.x/max.y )
    
    #group_by(source_main_port_toponyme) %>%
    #summarise(surtonnageMoyen = mean(ratioMoyenne))
  
  #print(finLeHavre$source_main_port_toponyme)
  #print(finLeHavre$surtonnageMoyen)
  return (finLeHavre)
}

compareOne2Others("Le Havre") #1.028287
compareOne2Others("Rouen")
compareOne2Others("Marennes") #1.269249
compareOne2Others("Bordeaux") #0.9838033
compareOne2Others("Nantes") #1.085708

# Initialiser résult
result <- finLeHavre

## Faire l'analyse sur tous les ports obliques
portsAnormaux <-  resume5c %>% 
  select(source_main_port_toponyme)

## Boucler sur chaque port oblique
for (i in 1:length(portsAnormaux$source_main_port_toponyme)) {
  port <- as.character(portsAnormaux[i, ]$source_main_port_toponyme)
  print(port)
  res <- compareOne2Others(port)
  result <- rbind(result,res)
}
#write.table(result, "./outputs/result_based_keep.csv", sep = "\t")
#write.table(result, "./outputs/result_like_postgres.csv", sep = "\t")
write.table(result, "./outputs/result_pour_boxplot.csv", sep = "\t")

## Faire la figure boxplot

boxplotresume <-  result %>%  
  group_by(source_main_port_toponyme) %>%
  summarise(
    surtonnageMoyen = round(mean(ratioMoyenne), 2),
    surtonnageMedian = round(median(ratioMoyenne), 2),
    surtonnageQ3 = round(quantile(ratioMoyenne,probs=0.90), 2),
    soustonnageQ1 = round(quantile(ratioMoyenne,probs=0.10), 2)) %>%
  arrange(surtonnageMoyen)%>%
  filter(surtonnageMoyen > 1.01 | surtonnageMedian!=1)

dataplot <- result %>% 
  inner_join(boxplotresume , by = c("source_main_port_toponyme" )) %>%
  arrange(surtonnageMoyen) %>%
  filter(ratioMoyenne <2)
 
dataplot$source_main_port_toponyme  <- factor(dataplot$source_main_port_toponyme, 
                                                   levels=unique(dataplot$source_main_port_toponyme))

boxplotresume$source_main_port_toponyme  <- factor(unique(boxplotresume$source_main_port_toponyme), 
                                                   levels=unique(boxplotresume$source_main_port_toponyme))


## Sauver dans un fichier en haute résolution - 08.12.2023
install.packages("extrafont")
library(extrafont)
pdf(file = "Curation_fig4.pdf", width = 10, height = 7, family = "Helvetica") # defaults to 7 x 7 inches
postscript("Curation_fig4.eps", width = 10, height = 7, horizontal = FALSE, 
           onefile = FALSE, paper = "special", colormodel = "cmyk", family = "Helvetica")
tiff("Curation_fig4.tiff", height = 7, width = 10, units = 'in', compression = "lzw", res = 300)


# # c(bas, gauche, haut, droite)
# # OMA : augmenter la largeur de la MARGE EXTERNE (oma) sur les bords gauche et droit de 1
# # MAR : reduire la largeur de la MARGE INTERNE (mar), sauf en haut ( + 1.5)
# dev.off()
# par()
# #mar = 5.1 4.1 4.1 2.1 par défaut, et OMA = 0000 par défaut
par(mar = c(9,7,4.1,2.1), oma = c(0,0,0,0))

boxplot(dataplot$ratioMoyenne ~ dataplot$source_main_port_toponyme, 
        ylim=c(-0.25,2),cex.lab =0.8, cex.main = 0.8,cex.axis=0.8,
        ylab="ratio du tonnage moyen du port par rapport \n aux autres pour un même navire" , col="#69b3a2", boxwex=0.4 , 
        las=2, xlab = "", 
        main="Distribution du rapport entre le tonnage moyen d'un navire dans un port 
        et le tonnage moyen de ces mêmes navires ailleurs, 
        ordonnées par rapport moyen (rouge)
        encadrés en bleu par les 1er et 9ème déciles, limites des valeurs anormales")
text(cex=0.7, col="blue", x=boxplotresume$source_main_port_toponyme, 0.15, boxplotresume$surtonnageQ3, xpd=TRUE, srt=0) #insert the text on the graph.
text(cex=0.7, col="red", x=boxplotresume$source_main_port_toponyme, 0, boxplotresume$surtonnageMoyen, xpd=TRUE, srt=0) #insert the text on the graph.
text(cex=0.7, col="blue", x=boxplotresume$source_main_port_toponyme, -0.15, boxplotresume$soustonnageQ1, xpd=TRUE, srt=0) #insert the text on the graph.

# Rajouter une barre horizontale bleue signalant la moyenne
abline(h=1, col="blue")

dev.copy(png,'./outputs/boxplot_ratiotonnage_port_zoom2.png')
dev.off()


## Alternatives d'exports basées sur ggplot2, non utilisées

ggplot(data=df, aes(x=xvar, y=yvar)) + 
  geom_point()
ggsave(path = path, width = width, height = height, device='tiff', dpi=700)

library(Cairo)

Cairo::Cairo(
  7, #length
  10, #width
  file = paste("nameofplot", ".png", sep = ""),
  type = "png", #tiff
  bg = "transparent", #white or transparent depending on your requirement 
  dpi = 300,
  units = "cm" #you can change to pixels etc 
)
plot(p) #p is your graph object 
dev.off()

#########################################################################
## Fin provisoire
def.off()

#boxplot(dataplot$ratioMoyenne ~ dataplot$source_main_port_toponyme, las= 2)



# resume4$ecart_median <- (resume4$maxMediane / resume4$mediane)
# resume4$ecart_moyen <- (resume4$maxPort / resume4$moyenne)
# 
# # max(resume4$ecart_moyen)
# # max(resume4$ecart_median)
# # filter(ecart_moyen !=2500) %>%
#   
# analyse <- resume4%>% 
#   #filter(ecart_median < 200)%>%
#   filter(source_main_port_toponyme %in% portsAnormaux$source_main_port_toponyme) %>%
#    arrange(ecart_median)
# 
# analyse$source_main_port_toponyme <- factor(analyse$source_main_port_toponyme, 
#                                             levels = unique(analyse$source_main_port_toponyme))
# 
# hist(analyse$ecart_median)
# hist(analyse$ecart_moyen)
# 
# resumeAnalyse <- analyse %>% 
#   group_by(source_main_port_toponyme)%>%
#   summarise(n = n(),
#             tauxAugmentationMoyen = mean(ecart_moyen)) %>%
# boxplot(analyse$ecart_moyen ~ analyse$source_main_port_toponyme)
# 
# boxplot(analyse$ecart_median ~ analyse$source_main_port_toponyme) 
#         ylab="Taux de variation entre le tonnage moyen et le tonnage moyen maximal d'un même navire" ,
#         col="#69b3a2", boxwex=0.4 , 
#         las=2, xlab = "", 
#         main="Distribution du tonnage par port oblique \n ordonnés par tonnage moyen \n avec le tonnage moyen (rouge)")
# # Rajouter une barre horizontale bleue signalant la moyenne
# abline(h=1, col="blue")

##############################################################
#Filtrer subset pour conserver les navires intéressants identifiés dans resume
# keep<-  subset %>% 
#   inner_join(resume , by = c("ship_id" )) 
# #16425
# 
# keep %>% 
#   filter(! (source_main_port_toponyme %in% portsAnormaux$source_main_port_toponyme))
# 
# #Pour les navires visitant plus qu'un port oblique, et avec des tonnages différents
# #Calculer par navire, par port visité, le tonnage moyen et le nombre de visites
# analyse <- keep %>% 
#   filter(! (source_main_port_toponyme %in% portsAnormaux$source_main_port_toponyme))%>%
#   group_by(ship_id) %>%
#   summarise(moyenne = round(mean(tonnage)), 
#             n = n(), 
#             min = min(tonnage),
#             max = max(tonnage),
#             mediane = round(median(tonnage)), 
#             q3 = quantile(tonnage,probs=0.75),
#             q1 = quantile(tonnage,probs=0.25))%>%
#   arrange(mediane) 
# 
# analyse1 <- keep %>% 
#   filter( (source_main_port_toponyme %in% portsAnormaux$source_main_port_toponyme[1]))%>%
#   group_by(ship_id) %>%
#   summarise(moyenne1 = round(mean(tonnage)), 
#             n1= n(), 
#             min1 = min(tonnage),
#             max1 = max(tonnage),
#             mediane1 = round(median(tonnage)))
# 
# analyse2 <- keep %>% 
#   filter( (source_main_port_toponyme %in% portsAnormaux$source_main_port_toponyme[2]))%>%
#   group_by(ship_id) %>%
#   summarise(moyenne2 = round(mean(tonnage)), 
#             n2 = n(), 
#             min2 = min(tonnage),
#             max2 = max(tonnage),
#             mediane2 = round(median(tonnage)))
# 
# analyse3 <- keep %>% 
#   filter( (source_main_port_toponyme %in% portsAnormaux$source_main_port_toponyme[3]))%>%
#   group_by(ship_id) %>%
#   summarise(moyenne3 = round(mean(tonnage)), 
#             n3 = n(), 
#             min3 = min(tonnage),
#             max3 = max(tonnage),
#             mediane3 = round(median(tonnage)))
# 
# analyse4 <- keep %>% 
#   filter( (source_main_port_toponyme %in% portsAnormaux$source_main_port_toponyme[4]))%>%
#   group_by(ship_id) %>%
#   summarise(moyenne4 = round(mean(tonnage)), 
#             n4 = n(), 
#             min4 = min(tonnage),
#             max4 = max(tonnage),
#             mediane4 = round(median(tonnage)))
# 
# analysetout <-  analyse %>% 
#   left_join(analyse1 , by = c("ship_id" )) %>% 
#   left_join(analyse2 , by = c("ship_id" )) %>% 
#   left_join(analyse3 , by = c("ship_id" )) %>% 
#   left_join(analyse4 , by = c("ship_id" )) %>%
#   filter( !(is.na(moyenne1) & is.na(moyenne2) & is.na(moyenne3) & is.na(moyenne4)))
#         
# portsAnormaux$source_main_port_toponyme  
# #Le Havre Bordeaux Nantes   Marennes
# fin <- analysetout %>% filter(moyenne > 20) %>%
#   mutate(ratio = moyenne1/moyenne) %>%
#   summarise(tauxmoyenn = mean(ratio, na.rm=TRUE))
# #fin1 : 1.03
# #fin2 : 0.994 / 0.981
# #fin3 : 1.05 / 1.02
# #fin4 : 1.25
# 
# ##Reordonner les levels de ship_class_codage pour correspondre au tri par median
# resume5$source_main_port_toponyme <- factor(unique(resume5$source_main_port_toponyme), 
#                                              levels=unique(resume5$source_main_port_toponyme))
# 
# # Create a vector named "new_order" containing the desired order
# new_order <- with(resume4, reorder(source_main_port_toponyme , moyenne, mean , na.rm=T))
# 
# # c(bas, gauche, haut, droite)
# # OMA : augmenter la largeur de la MARGE EXTERNE (oma) sur les bords gauche et droit de 1
# # MAR : reduire la largeur de la MARGE INTERNE (mar), sauf en haut ( + 1.5)
# dev.off()
# par()
# #mar = 5.1 4.1 4.1 2.1 par défaut, et OMA = 0000 par défaut
# par(mar = c(7,4.1,4.1,2.1), oma = c(2,0,0,0))
# boxplot(resume4$moyenne ~ new_order, ylim=c(-100,500),
#         ylab="tonnage moyen en tonneaux" , col="#69b3a2", boxwex=0.4 , 
#         las=2, xlab = "", 
#         main="Distribution du tonnage par port oblique \n ordonnés par tonnage moyen \n avec le tonnage moyen (rouge)")
# text(cex=0.7, col="red", x=resume$ship_class_codage, -30, resume$mean, xpd=TRUE, srt=0) #insert the text on the graph.
# text(cex=0.7, col="blue", x=resume$ship_class_codage, -75, resume$m, xpd=TRUE, srt=35) #insert the text on the graph.




