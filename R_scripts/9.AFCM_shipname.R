#######################################################################################
##
## 5. Analyse factorielle des correspondances multiples 
## les noms de navires
##
#######################################################################################

setwd("C:/Travail/ULR_owncloud/ANR_PORTIC/Recrutement/Stage_2020/Luis_GONZALES/Travail") 

meslibrairiesR <- "C:/Tools/R4"
# Ajouter meslibrairiesR dans ces chemins : 
.libPaths(c( .libPaths(), meslibrairiesR) )

install.packages("randomForest", meslibrairiesR)
library("FactoMineR")
library(ade4)
library(tidyverse)
library(caret)
library(randomForest)

## Lire les données

rawdata <- read.delim("./Ship_name_cluster_stopwords.csv", header = TRUE, sep = ",", encoding="UTF-8")

colnames(rawdata)
[1] "X"          "pkid"       "ship_id"    "ship_name"  "saint"      "jeune"      "dame"      
[8] "aimable"    "vierge"     "sainte"     "notre"      "demoiselle" "deux"       "bonne"     
[15] "comte"      "belle"      "heureuse"   "prince"

head(rawdata)

rownames(rawdata) <- rawdata$pkid

rawdata[, c(5:18)] <- lapply(rawdata[, c(5:18)], as.factor)

rawdata <- rawdata %>% mutate(ship_id = ifelse(ship_id=="","unknown",ship_id))#5012 unknown
rawdata <- rawdata %>% mutate(saint = ifelse(is.na(saint),-1,saint))
rawdata <- rawdata %>% mutate(jeune = ifelse(is.na(jeune),-1,jeune))
rawdata <- rawdata %>% mutate(dame = ifelse(is.na(dame),-1,dame))
rawdata <- rawdata %>% mutate(aimable = ifelse(is.na(aimable),-1,aimable))
rawdata <- rawdata %>% mutate(vierge = ifelse(is.na(vierge),-1,vierge))
rawdata <- rawdata %>% mutate(sainte = ifelse(is.na(sainte),-1,sainte))
rawdata <- rawdata %>% mutate(notre = ifelse(is.na(notre),-1,notre))
rawdata <- rawdata %>% mutate(demoiselle = ifelse(is.na(demoiselle),-1,demoiselle))
rawdata <- rawdata %>% mutate(deux = ifelse(is.na(deux),-1,deux))
rawdata <- rawdata %>% mutate(bonne = ifelse(is.na(bonne),-1,bonne))
rawdata <- rawdata %>% mutate(comte = ifelse(is.na(comte),-1,comte))
rawdata <- rawdata %>% mutate(belle = ifelse(is.na(belle),-1,belle))
rawdata <- rawdata %>% mutate(heureuse = ifelse(is.na(heureuse),-1,heureuse))
rawdata <- rawdata %>% mutate(prince = ifelse(is.na(prince),-1,prince))

rawdata[, c(5:18)] <- lapply(rawdata[, c(5:18)], as.factor)

levels(rawdata$saint)
rawdata$ship_id <- as.factor(rawdata$ship_id) #12006 levels, Ship_id is a factor
levels(rawdata$ship_id)
summary(rawdata)

sapply(rawdata[, 2:15], function(x) sum(is.na(x)))
which(is.na(rawdata[, 2:15]$saint))
#https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.2041-210X.2011.00172.x



#####################################################
## AFCM
#####################################################

data <- rawdata[, c(5:18)]

#Si le tableau n'était pas déjà un codage disjonctif
library(ade4)

dataafc <- acm.disjonctif(data)

### S'assurer que toutes les variables sont qualitatives (as.factor)
dataafc[, 1:ncol(dataafc)] <- lapply(dataafc[, 1:ncol(dataafc)], as.factor)

head(dataafc)
summary(dataafc)

## Nom des variables présentes dans data
colnames(dataafc)

########################################################################
## Filtrer les modalités rares
########################################################################
summary(data)
summary (dataafc)

data <- dataafc

variablesToremove = c();
for (i  in 1:length(colnames(data) )){
  reponse_0 <- as.integer(unlist(strsplit(summary (data)[i*2-1], ":"))[2]) ;
  reponse_1 <- as.integer(unlist(strsplit(summary (data)[i*2], ":"))[2]);
  #print(paste("i: ", i*2, " 0: ", reponse_0));
  balance <- reponse_0/(reponse_0 + reponse_1) * 100.0;
  ## Les r?ponses rares (moins de 5% des effectifs) sont s?lectionn?es
  if (balance  < 10 | balance > 90) {
    print(paste("colname ", colnames(data)[i], " has unbalanced answers; taux: ", balance));
    variablesToremove <- c(variablesToremove, colnames(data)[i]);
  }	
}

"colname  saint.0  has unbalanced answers; taux:  99.3457759018664"
[1] "colname  saint.1  has unbalanced answers; taux:  96.1458814218822"
[1] "colname  saint.2  has unbalanced answers; taux:  99.1532018729803"
[1] "colname  saint.3  has unbalanced answers; taux:  98.1995647299347"
[1] "colname  saint.4  has unbalanced answers; taux:  99.4024929103739"
[1] "colname  saint.5  has unbalanced answers; taux:  98.2576007386401"
[1] "colname  saint.6  has unbalanced answers; taux:  98.1956077293412"
[1] "colname  saint.7  has unbalanced answers; taux:  96.534986480248"
[1] "colname  saint.8  has unbalanced answers; taux:  98.1929697289455"
[1] "colname  saint.9  has unbalanced answers; taux:  99.9221789883268"


#########################################################################
## AFCM (qui ne marche pas du tout, à cause du déséquilbre notoire des classes)
#########################################################################

res <- dudi.acm(dataafc, scannf = TRUE)

## Règle : 1/C, nombre de variables correspond au seuil pour garder les axes
# C = 14 (on a 14 groupes de variables)
1/14 = 0.0714

#nf = 30


#You can reproduce this result non-interactively with
res <- dudi.acm(df = dataafc, scannf = FALSE, nf = 19)


### Créer un répertoire de graphiques pour l'AFCM
dir.create(file.path(getwd(), './figuresAFCM_shipname'))

## Valeurs propres
res$eig
[1] 1.048307e-02 7.148529e-03 7.147250e-03 7.143179e-03 7.140361e-03 7.112281e-03 7.060809e-03
[8] 6.754401e-03 6.437067e-03


## Inertie (ou variance expliquée associée ? chaque axe)
inertie <- res$eig/sum(res$eig) * 100


## Eboulis des valeurs propres
dev.off()
barplot( inertie[inertie > 0.35] , ylab = "% d'inertie", names.arg = round(inertie[inertie > 0.35], 2))
title("Eboulis des valeurs propres en %")
dev.copy(png,'./figuresAFCM_shipname/eboulis.png')
dev.off()

## Variance cumulée expliquée
inertie_cumulee <- round(cumsum(100 * res$eig/sum(res$eig)), 2)
print(inertie_cumulee)

## Graphique montrant la variance cumulée expliquée
barplot(inertie_cumulee , main="Pourcentage d'inertie cumulée")
dev.copy(png,'./figures/figuresAFCM/inertie.png')
dev.off()

###################################################
## explor
library(explor)

explor(res)

###################################################
## Random forest tree
###################################################

library(randomforest)

colnames(rawdata)
data <- rawdata[, c(2, 3, c(5:18))]

rownames(data) <- rawdata$pkid
colnames(data)

[1] "ship_id"    "saint"      "jeune"      "dame"       "aimable"    "vierge"     "sainte"     "notre"     
[9] "demoiselle" "deux"       "bonne"      "comte"      "belle"      "heureuse"   "prince"  

train <- data %>% sample_frac(0.7) ##2/3 pour l'entrainement
test <- anti_join(data, train, by="pkid") ## 1/3 pour le test

colnames(train)
train<- train[, -1]
#rownames(train) <- train$pkid

test<- test[, -1]
rownames(data) <- data$pkid
data <- data[, -1]

summary(train)
is.factor(train$ship_id)# TRUE
levels(train$ship_id) #~12000
table(factor(train$ship_id))
#(model <- randomForest(ship_id ~ ., data = train[, 2:15], ntree = 100, na.action = na.omit))
train$ship_id <- factor(train$ship_id) ## Relvels since subsetting may have loose some levels


fit <- randomForest(ship_id ~ ., data = train, ntree = 5, na.action = na.omit)

# ship.rf <- randomForest(data[, 2:15], data$ship_id, na.action = na.omit)
# Plante (trop de forets)

ship.rf <- randomForest(data[, 2:15], data$ship_id, na.action = na.omit, ntree = 5)
fit <- randomForest(train[, 3:16], train$ship_id, na.action = na.omit, ntree = 5)

set.seed(123)

ship.rf <- randomForest(train[, 2:15], train$ship_id, ntree = 5, na.action = na.omit)
#mtry 

fit <- ship.rf
print(fit)
ship.rf

hist(fit$oob.times)

## Récupérer les estimations (ou votes) sur les 10 premiers individus (lignes), 10 premiers ship_id
fit$votes[1:10,1:10]
colnames(fit$votes)
rownames(train)
rownames(fit)
resultat <- cbind(train, fit$votes[, 1]) #0000001N
colnames(resultat)[length(colnames(resultat))] = "vote0000001N"
resultat[resultat$ship_id == "0000001N", ]$vote0000001N
resultat[resultat$ship_id == "0000001N", ]$ship_name

rawdata[rawdata$ship_id == "0000001N", ]$ship_name #Endeavour
rawdata[rawdata$ship_id == "0000001N", ]$ship_name #Endeavour

indexc <- grep("0010772N", colnames(fit$votes))
resultat <- cbind(train, fit$votes[, indexc]) #0010772N
colnames(resultat)[length(colnames(resultat))] = "vote"

resultat[resultat$ship_id == "0010772N", ]$vote ## Marie Madelaine





## Classement des variables explicatives 
## en fonction de l'indice gini d'impureté (plus MeanDecreaseGini est élévé, plus la variable est explicative )
fit$importance
varImpPlot(fit)
fit$importance[order(fit$importance[, 1], decreasing = TRUE), ]

##Matrice de confusion
# Il s’agit d’un tableau présentant en ligne les données observées 
# et en colonnes les données prédites par l’algorithme. 
# <==> en colonne, la prédiction de l’algo, et en ligne la modalité réelle.
# On trouve sur sa diagonale le nombre d’individus bien classés 
# c’est-à-dire les individus dont la prédiction de l’algorithme correspond aux données observées.

fit$confusion


## OUT OF BAG ESTIMATE OF ERROR RATE
# chaque arbre est entraîné sur une fraction des data, qui est considérée comme « in-bag ». 
# Ce qui permet à chaque arbre, une fois construit, d’estimer son taux d’erreur 
# sur les données qu’il a laissé « out-of-bag » : 
# plus ce taux est faible, plus le modèle est juste. 
# Ce chiffre à lui seul pourrait servir d’indicateur de performance du modèle.
# Vous pouvez accéder au nombre de fois qu’un individu a été laissé « out of bag » 
# avec model$oob.times
fit$oob.times


## Il s'agit ainsi de la différence entre la proportion de votes pour la classe correcte (i) 
## et la proportion de votes pour la classe sortie majoritaire parmi les autres classes (j ≠ i).
## plus la marge est proche de 1 et plus la confiance accordée à la prédiction est grande
m=margin(fit)

# paramètre mtry : le nombre de variables testées à chaque split. 
# La valeur par défaut étant la racine carrée du nombre de variables. 
# C’est le principal paramètre à modifier, avec ntree