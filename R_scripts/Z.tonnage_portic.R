attach(pointcalls)
dim(pointcalls[which(tonnage_unit=="quintaux" & source_suite == "Marseille"), ])
398  31

dim(pointcalls[which(tonnage_unit=="quintaux" & source_suite == "Marseille"), ])

colnames(pointcalls)
unique(pointcalls[which(tonnage_unit=="quintaux" & source_suite == "Marseille"), ]$source_main_port_toponyme)
"Marseille"

unique(pointcalls[which(tonnage_unit=="quintaux" ), ]$source_main_port_toponyme)
"Marseille" "Canet"

pointcalls[which(tonnage_unit=="quintaux" & source_main_port_toponyme=="Canet" ), ]$tonnage_unit

#Tx
398/406*100 à marseille
8/406 * 100 à Canet

v <- match(c("source_doc_id", "pointcall", "pointcall_function", "pointcall_date", "ship_id", "ship_class", "tonnage"), colnames(pointcalls))
extrait <- pointcalls[which(tonnage_unit=="quintaux" & source_main_port_toponyme=="Canet" ),v ]
extrait[order(extrait$source_doc_id), ]

length(pointcalls[which(source_suite == "Marseille" ), ]$tonnage)
13206
length(pointcalls[which(source_suite != "Marseille" ), ]$tonnage)
62609
length(na.omit(pointcalls[which(source_suite == "Marseille" ), ]$tonnage))
1163
length(na.omit(pointcalls[which(source_suite == "Marseille" & tonnage_unit=="quintaux" ), ]$tonnage))
398

summaryBy(tonnage  ~ tonnage_unit, pointcalls[which(source_suite == "Marseille"), ], fun.names=c("moyenne", "ecart-type", "nombre"),
          FUN = function(x) {
            return(c(
              mean(x, na.rm=TRUE), 
              sd(x, na.rm=TRUE), 
              length(na.omit(x))
            ))
          })

max(pointcalls[which(source_suite == "Marseille"), ]$tonnage, na.rm = T)
# En d?taillant la partie sous 100
hist(pointcalls[which(source_suite == "Marseille"), ]$tonnage, main="Distribution des tonnages des navires, \n exprimés en tonneaux",
     xlim = c(0, 500), ylim = c(0, 0.01), 
     breaks=c(0, 20, 50, 100, 150, 250, 600 ))

hist(pointcalls[which(source_suite == "Marseille"), ]$tonnage, main="Distribution des tonnages des navires, \n exprimés en tonneaux",
     xlim = c(0, 500), ylim = c(0, 0.01), 
     breaks=c(0, 8, 18, 28, 44, 86, 126, 170, 250, 600 ))



# Rajouter une barre verticale rouge signalant la moyenne
abline(v=mean(pointcalls[which(source_suite == "Marseille"), ]$tonnage, na.rm = T), lty = 3, col="red")

# Rajouter une barre verticale bleue signalant la m?diane 
abline(v=median(pointcalls[which(source_suite == "Marseille"), ]$tonnage, na.rm = T), lty=2, col="blue")

# Rajouter la courbe de densit? des observations 
lines(density(pointcalls[which(source_suite != "Marseille" & tonnage_unit=="tonneaux"),]$tonnage, na.rm = T), lty=2)

# S?parer les observations en quintaux des observations en tonneaux
lines(density(pointcalls[which(source_suite == "Marseille" & tonnage_unit=="quintaux"), ]$tonnage, na.rm = T), col="red")
lines(density(pointcalls[which(source_suite == "Marseille" & tonnage_unit=="tonneaux"), ]$tonnage, na.rm = T), col="darkgreen")

# Ajouter une l?gende
legend("topright", inset=.05, title="Tonnages à  Marseille",
       c("G5 en tonneaux", "Marseille en tonneaux","Marseille en quintaux"), fill=c("black",  "darkgreen", "red"), horiz=FALSE)
dev.copy(png,'./figures/Portic/hist_Marseille_tonnages.png')
dev.off()
getwd()

dev.copy(png,'./figures/Portic/titi.png')
dev.off()
dev.off()

