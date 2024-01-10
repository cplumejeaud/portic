# Entête -------------------------------------------------------------------
## Christine Plumejeaud-Perreau, U.M.R 7301 MIGRINTER
## 17 avril 2023
## Script d'analyse des données ICAR Montmorillon
## Programme ICAR (Incertitudes cartographiques)

setwd("C:/Travail/Projets/ANR_PORTIC/Reunions/2023-09-11_NICE_Cloture/Cartes")
getwd()

meslibrairiesR <- "C:/Tools/R/R4"
# Ajouter meslibrairiesR dans ces chemins : 
.libPaths(c( .libPaths(), meslibrairiesR) )


install.packages("tidyverse", meslibrairiesR)
install.packages("mapsf", meslibrairiesR)
install.packages("dplyr", meslibrairiesR)
install.packages("FactoMineR", meslibrairiesR)
install.packages("factoextra", meslibrairiesR)
install.packages("ade4", meslibrairiesR)
install.packages("devEMF", meslibrairiesR)
install.packages("Factoshiny", meslibrairiesR)
install.packages("explor", meslibrairiesR)
install.packages("treemap",meslibrairiesR)
install.packages("RPostgreSQL",meslibrairiesR)
install.packages("hrbrthemes",meslibrairiesR)



library(RPostgreSQL)
library(tidyverse)
library(mapsf)
library(dplyr)  

# library
library(ggplot2)
library(dplyr)
library(hrbrthemes)

# Build dataset with different distributions
data <- data.frame(
  type = c( rep("variable 1", 1000), rep("variable 2", 1000) ),
  value = c( rnorm(1000), rnorm(1000, mean=4) )
)

#requete SQL sur la base
# select ship_class_standardized, tonnage, case when tonnage_unit = 'quintaux' then tonnage::float/24.0 else tonnage::float end as tonnage_converti
# from navigoviz.pointcall p 
# where tonnage is not null and ship_class_standardized in ('Tartane', 'Vaisseau', 'Barque', 'Bateau')
# order by ship_class_standardized;

library(RPostgreSQL)
con <- dbConnect(dbDriver("PostgreSQL"), host='localhost', port='5432', dbname='portic_v10', user='postgres', password='postgres')


#Exécuter la requete sur la base de données et renseigner un dataframe de deux colonnes (identifiant et distance_proche)
data <- dbGetQuery(con, paste("select ship_class_standardized,  case when tonnage_unit = 'quintaux' then tonnage::float/24.0 else tonnage::float end as tonnage_converti
from navigoviz.pointcall p 
where tonnage is not null and ship_class_standardized in ('Tartane', 'Vaisseau', 'Barque', 'Bateau')
order by ship_class_standardized;"))

# Represent it
p <- data %>%
  ggplot( aes(x=tonnage_converti, fill=ship_class_standardized)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080", "#FF5733", "#FFFC33")) +
  theme_ipsum() +
  labs(fill="")


p

## Small multiples


# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/zonination/perceptions/master/probly.csv", header=TRUE, sep=",")
data <- data %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0))

# plot
p <- data %>%
  mutate(ship_class_standardized = fct_reorder(ship_class_standardized, tonnage_converti)) %>%
  ggplot( aes(x=tonnage_converti, color=ship_class_standardized, fill=ship_class_standardized)) +
  geom_histogram(alpha=0.6, binwidth = 5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Assigned Probability (%)") +
  facet_wrap(~ship_class_standardized)

p

## Regarder vaisseau à part

# select ship_class_standardized, tonnage, case when tonnage_unit = 'quintaux' then tonnage::float/24.0 else tonnage::float end as tonnage_converti
# from navigoviz.pointcall p 
# where tonnage is not null and ship_class_standardized in ( 'Vaisseau')
# order by ship_class_standardized;


#Exécuter la requete sur la base de données et renseigner un dataframe de deux colonnes (identifiant et distance_proche)
dataVaisseau <- dbGetQuery(con, paste("select ship_class_standardized,  case when tonnage_unit = 'quintaux' then tonnage::float/24.0 else tonnage::float end as tonnage_converti
from navigoviz.pointcall p 
where tonnage is not null and ship_class_standardized in ( 'Vaisseau')
order by ship_class_standardized;"))

# Draw the boxplot and the histogram 
dev.off()
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(dataVaisseau$tonnage_converti , horizontal=TRUE , ylim=c(0,1000), xaxt="n" , col=rgb(0.2,0.8,0.5,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(dataVaisseau$tonnage_converti , breaks=30 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="Distribution des tonnage pour les chaloupes" , xlab="convertis en tonneaux", xlim=c(0,1000))

#Sauver le graphique
figpath <- './'
dev.copy(png,paste0(figpath, 'histogramme_tonnage_vaisseaux.png'))
dev.off()

dataChaloupe <- dbGetQuery(con, paste("select ship_class_standardized,  case when tonnage_unit = 'quintaux' then tonnage::float/24.0 else tonnage::float end as tonnage_converti
from navigoviz.pointcall p 
where tonnage is not null and ship_class_standardized in ( 'Chaloupe')
order by ship_class_standardized;"))

# Draw the boxplot and the histogram 
dev.off()
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(dataChaloupe$tonnage_converti , horizontal=TRUE , ylim=c(0,80), xaxt="n" , col=rgb(0.2,0.8,0.5,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(dataChaloupe$tonnage_converti , breaks=30 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="Distribution des tonnage pour les chaloupes" , xlab="convertis en tonneaux", xlim=c(0,80))

#Sauver le graphique
figpath <- './'
dev.copy(png,paste0(figpath, 'histogramme_tonnage_chaloupes.png'))
dev.off()

##############################################################################
## Pour curation
# Figure 3 : bar plot des ship

proto_shipclass <- tolower(c("Allège", "Barque", "Bateau", "Bombarde", "Brigantin", "Chasse-marée", 
                             "Chebec", "Corvette", "Félouque", "Frégate", "Goelette", 
                             "Pinque", "Polacre", "Schooner", "Ship", "Sloop", "Tartane", 
                             "Brick", "Vaisseau", "Navire", "Senau", "Chaloupe", "Lougre", "Bisque",
                             "Cotre", "Cutter", "Gabare", "Galiote", "Gondole", "Queche", "Inconnue"))
paste(proto_shipclass , collapse='\',\'')

library(RPostgreSQL)
con <- dbConnect(dbDriver("PostgreSQL"), host='localhost', port='5432', dbname='portic_v10', user='postgres', password='postgres')

df <- dbGetQuery(con, paste("select ship_class_standardized,  count(*)
from navigoviz.pointcall p 
where 
lower(ship_class_standardized) in ('allège','barque','bateau','bombarde','brigantin','chasse-marée','chebec','corvette','félouque','frégate','goelette','pinque','polacre','schooner','ship','sloop','tartane','brick','vaisseau','navire','senau','chaloupe','lougre','bisque','cotre','cutter','gabare','galiote','gondole','queche','inconnue')
group by ship_class_standardized
order by ship_class_standardized;"))

df <- df %>% arrange(desc(count)) %>%
  mutate(ship_class_standardized=factor(ship_class_standardized, levels=ship_class_standardized)) 
# This trick update the factor levels
ggplot(df , aes(x=ship_class_standardized , y=count)) + 
  geom_bar(stat = "identity")

dev.off()

