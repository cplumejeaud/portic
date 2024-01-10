# portic
Certains codes d'analyse non archivés sur humanum, divers et variés, réalisés dans le programme ANR PORTIC

Data come most of the time of the database (postgres), but someone can get data (the last version) from the API : http://data.portic.fr/ like CSV files. 

## Smogglage.ipynb 

Allows for the analysis of departures from Dunkirk.

Analyse comparée de la distribution des tonnages par pavillon : on remarque bien que tous les smoggleurs partant pour l'angleterre sont notés comme faisant 12 tx

![this kind of boxplot](https://github.com/cplumejeaud/portic/blob/main/Boxplot_Distribution%20des%20tonnages%20en%20fonction%20des%20pavillons%20partis%20de%20Dunkerque_parpavillon.jpg)

## Carto_module12.ipynb and carto_module12_suite.ipynb 

This is for the analysis of product / tonnage sent per destination from Dunkirk.

Diagramme alluvial : 

![Produces this kind of sankey diagram](https://github.com/cplumejeaud/portic/blob/main/sankey_export_product_Dunkerque_1789.png)


