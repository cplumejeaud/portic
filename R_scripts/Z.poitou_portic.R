###############################################################################
## Christine Plumejeaud-Perreau, U.M.R 7266 LIENSS
## Script d'analyse des congés 1789 délivrés en Poitou
###############################################################################

setwd("C:/Travail/CNRS_mycore/Cours/Cours_M2_stat/Scripts")


meslibrairiesR <- "C:/Tools/R4"

# Ajouter meslibrairiesR dans ces chemins : 
.libPaths(c( .libPaths(), meslibrairiesR) )
library(RPostgreSQL)
library(tidyverse)


## 1. RÃ©cupÃ©rer les donnÃ©es depuis Postgres

con <- dbConnect(dbDriver("PostgreSQL"), host='localhost', port='5432', dbname='portic_v5', user='postgres', password='postgres')
con <- dbConnect(dbDriver("PostgreSQL"), host='localhost', port='5432', dbname='portic_v6', user='postgres', password='postgres')

data_poitou <- dbGetQuery(con, "select * from csv_extract where source_subset = 'Poitou_1789'")
data_all <- dbGetQuery(con, "select * from csv_extract ")
# 108530 obs and 104 variables

a1 <- "source_doc_id, source_suite, source_component, source_main_port_uhgs_id, source_main_port_toponyme"
a2 <- "pointcall_rankfull, data_block_leader_marker, pointcall_action, pointcall_function, pointcall_out_date, pointcall_in_date, outdate_fixed, indate_fixed, net_route_marker, navigo_status"
a3 <- "pointcall, pointcall_uhgs_id, latitude, longitude, pointcall_admiralty, pointcall_province, pointcall_states, pointcall_substates, pointcall_status, shiparea"
a4 <- "homeport_uhgs_id , homeport,  homeport_latitude, homeport_longitude, homeport_admiralty, homeport_province, homeport_states, homeport_substates, homeport_status, homeport_shiparea"
a5 <-  "ship_id , flag, ship_flag_id , ship_name , \"class\" as ship_class, tonnage , tonnage_class , tonnage_unit "
a6 <- " captain_id, captain_name, birthplace, status, citizenship, in_crew"
a7 <- " json_array_length(all_cargos::json) as nb_products, commodity_permanent_coding, commodity_purpose, commodity_standardized, quantity , commodity_permanent_coding2, commodity_purpose2, commodity_standardized2, quantity2, commodity_permanent_coding3, commodity_purpose3, commodity_standardized3, quantity3, commodity_permanent_coding4, commodity_purpose4, commodity_standardized4, quantity4"
a8 <- " tax_concept , q01, q02, q03, ((case when q01 is null then 0 else q01::float*240 end) + (case when q02 is null then 0 else q02::float*12 end) + (case when q03 is null then 0 else q03::float end)) as taxe"
a9 <- "ship_uncertainity, tonnage_uncertainity, flag_uncertainity, homeport_uncertainity, pointcall_uncertainity, captain_uncertainity, cargo_uncertainity, taxe_uncertainity"
a10 <- "state_1787, state_1789"

attributes <- paste("select pkid, record_id", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, sep=",")
clause <- "from navigoviz.pointcall p where  (substring(pointcall_out_date for 4)::int = 1789 OR substring(pointcall_in_date for 4)::int= 1789) "

data <- dbGetQuery(con, paste(attributes, clause))

dim(data)
[1] 31278    83

write.table(data, "./data/portic/data_1789_e.csv", sep = "\t")#13 octobre 2020

con <- dbConnect(dbDriver("PostgreSQL"), host='localhost', port='8005', dbname='portic_v5', user='postgres', password='mesange17')
data <- dbGetQuery(con, "select * from public.csv_extract")
write.table(data, "./data/portic/data_f.csv", sep = "\t")#16 november 2020


clause <- "from navigoviz.pointcall p where  (substring(pointcall_out_date for 4)::int = 1787 OR substring(pointcall_in_date for 4)::int= 1787) "
data <- dbGetQuery(con, paste(attributes, clause))

dim(data)
[1] 75815    83
write.table(data, "./data/portic/data_1787_e.csv", sep = "\t")#13 octobre 2020

q <- "select flag, label from navigoviz.flag_labels"
flags <- dbGetQuery(con, q)


a2 <- "distance_dep_dest_miles, departure_function, destination_function, departure_out_date, destination_in_date, outdate_fixed, indate_fixed"
a3 <- "departure, departure_uhgs_id, departure_latitude, departure_longitude, departure_admiralty, departure_province, departure_state_1789, departure_substates, departure_status, departure_shiparea"
a3bis <- "destination, destination_uhgs_id, destination_latitude, destination_longitude, destination_admiralty, destination_province, destination_states, destination_substates, destination_status, destination_shiparea"
a4 <- "homeport_uhgs_id , homeport,  homeport_latitude, homeport_longitude, homeport_admiralty, homeport_province, homeport_states, homeport_substates, homeport_status, homeport_shiparea"
a5 <-  "ship_id , flag, ship_flag_id , ship_name , \"class\" as ship_class, tonnage , tonnage_class , tonnage_unit "
a6 <- " captain_id, captain_name, birthplace, status, citizenship, in_crew"
a7 <- " json_array_length(all_cargos::json) as nb_products, commodity_permanent_coding, commodity_purpose, commodity_standardized, quantity , commodity_permanent_coding2, commodity_purpose2, commodity_standardized2, quantity2, commodity_permanent_coding3, commodity_purpose3, commodity_standardized3, quantity3, commodity_permanent_coding4, commodity_purpose4, commodity_standardized4, quantity4"
a8 <- " tax_concept , q01, q02, q03, ((case when q01 is null then 0 else q01::float*240 end) + (case when q02 is null then 0 else q02::float*12 end) + (case when q03 is null then 0 else q03::float end)) as taxe"
a9 <- "ship_uncertainity, tonnage_uncertainity, flag_uncertainity, homeport_uncertainity, pointcall_uncertainity, captain_uncertainity, cargo_uncertainity, taxe_uncertainity"
a10 <- "state_1787, state_1789"

attributes <- paste("select id, travel_rank", a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, sep=",")
clause <- "from navigoviz.built_travels p where  (substring(pointcall_out_date for 4)::int = 1789 OR substring(pointcall_in_date for 4)::int= 1789) "
data <- dbGetQuery(con, paste(attributes, clause))


# 2. Lire les données depuis fichier CSV

#Préprocessing dans TD3
pointcalls <- read.table("./data/portic/data_1789_e.csv", sep = "\t")
pointcalls <- read.delim("C:/Travail/CNRS_mycore/Cours/Cours_M2_stat/Scripts/data/portic/data_1789_e.csv")

## Utiliser mutate pour transformer le type d'une variable (par exemple)
## Rq : vu le nombre de variables de   pointcalls cela peut devenir ennuyeux.
## Donc on vous propose mutate_at qui agit sur un ensemble de variables listÃ©es par vars()
## et avec des fonctions listÃ©es dans list()
library(lubridate)
library(tidyverse)
pointcalls <- pointcalls %>%
  mutate_at(vars(pointcall_action, pointcall_function, ship_class, flag, ship_flag_id, tonnage_class, data_block_leader_marker, state_1789 ), list(as.factor))  %>%
  mutate_at(vars(tonnage, taxe, q01, q02, q03, quantity, in_crew, latitude, longitude), list(as.numeric)) %>%
  mutate_at(vars(homeport, homeport_uhgs_id, homeport_admiralty, homeport_states), list(as.factor)) %>%
  mutate_at(vars(source_suite, source_component, source_main_port_uhgs_id, source_main_port_toponyme, tonnage_class, source_suite), list(as.factor)) %>%
  mutate_at(vars(outdate_fixed, indate_fixed), list(ymd))

summary(pointcalls)
dim(pointcalls)# 31278    80

test <- levels(pointcalls$flag)
match(test, flags$flag)
levels(pointcalls$flag) <- flags[match(test, flags$flag),]$label
## a sauver 
write.table(pointcalls, "./data/portic/data_1789_e.csv", sep = "\t")#9 octobre 2020


library(readr)
poitou <- read_delim("./data/portic/etude_1789.csv", ";", escape_double = FALSE, trim_ws = TRUE)



## Filtrer les pointcalls pour ne retenir que ceux inclus dans la liste des source_component du poitou
study <- pointcalls %>% filter (source_component %in% poitou$source_component)
dim(study)
13519    79

sort(unique(study$source_main_port_toponyme))
[1] "Aligre de Marans"        "Ars en RÃ©"               "Beauvoir-sur-Mer"        "ChampagnÃ©-les-Marais"   
[5] "Charente"                "Esnandes"                "Ile de Bouin"            "La Flotte en Rï¿½"        
[9] "La Perrotine"            "La Rochelle"             "La Tranche sur Mer"      "Le Chateau d' OlÃ©ron"   
[13] "Marennes"                "Moricq"                  "Mortagne"                "Noirmoutier"            
[17] "RibÃ©rou Saujon"          "Rochefort"               "Royan"                   "Sables d' Olonne"       
[21] "Saint Denis d' OlÃ©ron"   "Saint Gilles sur Vie"    "Saint Martin de RÃ©"      "Saint Michel en l' Herm"
[25] "Soubise"          


summary(study)


expeditions <- study %>% filter (pointcall_function == 'O') %>% 
  select (source_main_port_toponyme, flag)  %>%
  group_by(source_main_port_toponyme, flag)  %>% 
  summarise(nb_obs = n()) 

expeditionsFlag <- study %>%  filter (pointcall_function == 'O') %>% 
  select (source_main_port_toponyme, flag, pkid )  %>%
  group_by(source_main_port_toponyme, flag)  %>% 
  summarise(nb_obs = n()) 
#commodity_standardized

expeditionsEtrangeresDest <- study %>% filter (is.na(pointcall_admiralty)) %>% 
  select (source_main_port_toponyme, state_1789, flag, commodity_standardized)  %>%
  group_by(source_main_port_toponyme, state_1789,  flag, commodity_standardized)  %>% 
  summarise(nb_obs = n()) 


expeditionsEtrangeresDest <- study %>% filter (is.na(pointcall_admiralty)) %>% 
  select (source_main_port_toponyme, state_1789, pkid)  %>%
  group_by(source_main_port_toponyme, state_1789)  %>% 
  summarise(nb_obs = n()) 

expeditionsEtrangeresDest <- study %>% filter (pointcall_function != 'O') %>%
   filter (is.na(pointcall_admiralty)) %>% 
  select (source_main_port_toponyme, pointcall, pkid)  %>%
  group_by(source_main_port_toponyme, pointcall)  %>% 
  summarise(nb_obs = n()) 


expeditionsEtrangeresDest2 <- study %>% filter (is.na(pointcall_admiralty)) %>% 
  select (source_main_port_toponyme, state_1789, flag, tonnage)  %>%
  group_by(source_main_port_toponyme, state_1789,  flag)  %>% 
  summarise(sum_tonnage = sum(tonnage)) 

departs <- study %>% filter (pointcall_function == 'O') %>% select (source_doc_id, pointcall, pointcall_admiralty,  tonnage, commodity_standardized)
dests <- study %>% filter (pointcall_function != 'O') %>% select (source_doc_id, pointcall, pointcall_admiralty, state_1789, flag)

expeditionsEtrangeresDest3 <- dests %>% 
  filter (is.na(pointcall_admiralty)) %>% 
  left_join(departs, by = c("source_doc_id"))  %>%
  group_by(pointcall.y, pointcall.x, state_1789,  flag)  %>% 
  rename(depart = pointcall.y) %>% 
  rename(destination = pointcall.x) %>% 
  summarise(sum_tonnage = sum(tonnage), nb_ships=n()) 

expeditionsEtrangeresDest3 <- dests %>% filter (pointcall_function == 'O' & pointcall=="Talmont Talmont Saint Hilaire") %>% 
  select (pointcall, state_1789, flag, tonnage)  %>%
  group_by(pointcall, state_1789,  flag)  %>% 
  summarise(sum_tonnage = sum(tonnage)) 


ggplot(data = expeditionsEtrangeresDest3 ,
       mapping = aes(x = depart, y = nb_ships, fill=factor(state_1789))) + 
  geom_bar(stat="identity")+
  labs(x = "ports d'enregistrement du congé",
       y = "nb d'envois",
       title = "Expéditions vers l'étranger depuis le Poitou",
       subtitle = "Quels sont les ports à traffic international",
       caption = "Source : PORTIC 2020 - Navigocorpus (G5 1789)")+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous("Nombre de départs 1789", expand=c(0,0)) + 
  scale_fill_discrete("Pays de destinations") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.copy(png,'./figures/Portic/Poitou_export_vers_etrangers_1789_ports.png')
dev.off()


ggplot(data = expeditionsEtrangeresDest3 ,
       mapping = aes(x = depart, y = nb_ships, fill=factor(destination))) + 
  geom_bar(stat="identity")+
  labs(x = "ports d'enregistrement du congé",
       y = "nb d'envois",
       title = "Expéditions vers l'étranger depuis le Poitou",
       subtitle = "Quels sont les ports à traffic international",
       caption = "Source : PORTIC 2020 - Navigocorpus (G5 1789)")+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous("Nombre de départs 1789", expand=c(0,0)) + 
  scale_fill_discrete("Ports de destinations") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.copy(png,'./figures/Portic/Poitou_export_vers_etrangers_1789_ports.png')
dev.off()



study %>% filter (pointcall_function == 'O' & pointcall!= source_main_port_toponyme) %>% select (source_main_port_toponyme, pointcall)

#########################################################################
## graph
#########################################################################

ggplot(expeditionsEtrangeres ), 
       aes(x = miles_dep_dest, fill = factor(mois))) +
  geom_histogram(bins = 20, color = "grey30") +
  facet_wrap(~factor(mois), ncol = 3)
dev.copy(png,'./figures/Portic/hist_miles_dep_dest_per_month_facets.png')
dev.off()

ggplot(data = expeditionsEtrangeresDest ,
       mapping = aes(x = source_main_port_toponyme, y = nb_obs, fill=factor(state_1789))) + 
  geom_bar(stat="identity")+
  labs(x = "ports d'enregistrement du congé",
       y = "nb d'envois",
       title = "Expéditions vers l'étranger depuis le Poitou",
       subtitle = "Quels sont les ports à traffic international",
       caption = "Source : PORTIC 2020 - Navigocorpus (G5 1789)")+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous("Nombre de départs 1789", expand=c(0,0)) + 
  scale_fill_discrete("Pays de destinations") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.copy(png,'./figures/Portic/Poitou_export_vers_etrangers_1789.png')
dev.off()


ggplot(data = expeditionsEtrangeresDest ,
       mapping = aes(x = source_main_port_toponyme, y = nb_obs, fill=factor(pointcall))) + 
  geom_bar(stat="identity")+
  labs(x = "ports d'enregistrement du congé",
       y = "nb d'envois",
       title = "Expéditions vers l'étranger depuis le Poitou",
       subtitle = "Quels sont les ports à traffic international",
       caption = "Source : PORTIC 2020 - Navigocorpus (G5 1789)")+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous("Nombre de départs 1789", expand=c(0,0)) + 
  scale_fill_discrete("Pays de destinations") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.copy(png,'./figures/Portic/Poitou_export_vers_etrangers_1789_ports.png')
dev.off()

ggplot(data = expeditionsEtrangeresDest2 ,
       mapping = aes(x = source_main_port_toponyme, y=sum_tonnage, fill=factor(state_1789))) + 
  geom_bar(stat="identity")+
  labs(x = "ports d'enregistrement du congé",
       y = "tonnages",
       title = "Expéditions vers l'étranger depuis le Poitou",
       subtitle = "Quels sont les ports à traffic international",
       caption = "Source : PORTIC 2020 - Navigocorpus (G5 1789)")+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous("Somme des tonnages, 1789", expand=c(0,0)) + 
  scale_fill_discrete("Pays de destinations") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.copy(png,'./figures/Portic/Poitou_export_vers_etrangers_1789.png')
dev.off()



ggplot(data = expeditionsFlag ,
       mapping = aes(x = source_main_port_toponyme,fill=factor(flag) )) + 
  geom_bar()+
  labs(x = "ports d'enregistrement du congé",
       y = "nb d'envois",
       title = "Pavillons des sortants du Poitou",
       subtitle = "Quels sont les ports attirant les pavillons étrangers ?",
       caption = "Source : PORTIC 2020 - Navigocorpus (G5 1789)")+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous("Nombre de départs 1789", expand=c(0,0)) + 
  scale_fill_discrete("Pavillons") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.copy(png,'./figures/Portic/Poitou_sorties_pavillon_1789.png')
dev.off()

ggplot(data = travels_rythms %>% filter(!is.na(mois) & !is.na(miles_dep_dest)),
       mapping = aes(x = mois, fill=factor(travel_class))) + 
  geom_bar()+
  labs(x = "Mois 1787 - de Janvier Ã  DÃ©cembre",
       y = "Nombre de dÃ©parts mensuels",
       title = "ActivitÃ© de l'annÃ©e 1787",
       subtitle = "Existe t-il des mois favorables aux sorties en mer ?",
       caption = "Source : PORTIC 2020 - Navigocorpus (G5 et Marseille)")+
  scale_x_discrete(labels=c("Janvier", "FÃ©vrier", "Mars", "Avril", "Mai", "Juin", "Juillet", "Aout", "Septembre", "Octobre", "Novembre", "DÃ©cembre"), expand=c(0,0))+
  scale_y_continuous("Nombre de dÃ©parts mensuels", expand=c(0,0)) + 
  scale_fill_manual("travel_class", values=c("pink","green", "blue"), labels=c("court < 30 miles" ,"moyen [30-200 miles]", "long > 200 miles")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.copy(png,'./figures/Portic/barplot_nombre_depart_per_month_per_travel_class.png')
dev.off()


expeditionsEtrangeres <- study %>% filter (is.na(pointcall_admiralty)) %>% 
  select (source_main_port_toponyme, pointcall_states, commodity_standardized)  %>%
  group_by(source_main_port_toponyme, pointcall_states, commodity_standardized)  %>% 
  summarise(nb_obs = n()) 


study %>% filter (pointcall_function == 'O') %>% 
    select (source_main_port_toponyme, flag, ship_flag_id, commodity_standardized)  %>%
    group_by(source_main_port_toponyme, flag, ship_flag_id, commodity_standardized)  %>% 
    summarise(nb_obs = n()) 


  #arrange(desc(nb_obs))
