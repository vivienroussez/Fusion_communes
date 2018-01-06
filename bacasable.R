require(tidyverse)
require(spdep)
require(sf)
require(data.table)
require(FactoMineR)
require(cartography)

datacomm <- fread("Sources/Base_communes.csv",sep=";",dec=",",colClasses = c("REG"="chr"))
mapCom <- st_read("Sources/COMMUNE.shp")

filter(mapCom,CODE_DEPT==92) %>% st_geometry() %>% plot()

base <- merge(x=mapCom,y=datacomm,by.x="INSEE_COM",by.y="CODGEO") %>% 
    select(P13_POP,MED13,INSEE_COM)

propSymbolsLayer(base,var="P13_POP",border=NA)
choroLayer(base,var="MED13",border=NA)
