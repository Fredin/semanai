#Codigo de ejemplo introductorio para el uso de la herramienta GDELTtools,
#para el uso de la base de datos GDELT. Se adaptó para analizar las protestas
#a nivel mundial en el 2011. Basado en el ejemplo de Stephen Haptonstahl.

library(GDELTtools)
library(rworldmap)

gdelt.protest <- GetGDELT(start.date="2011-01-01", end.date="2011-12-31",
                           local.folder="~/gdeltdata", filter=list(EventCode="14"))
normed.protest <- NormEventCounts(gdelt.protest, unit.analysis="country.year",
                                         var.name="reports.of.protests")

map.data <- joinCountryData2Map(normed.protest,
                                joinCode="ISO2",
                                nameJoinColumn="country")
mapCountryData(map.data, 
               nameColumnToPlot="reporte.de.protestas",
               mapTitle="Protestas en el año 2011")
