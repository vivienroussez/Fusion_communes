load("Base.RData")

com <- st_read("Sources/com_15.shp")

# On va prendre le fonds généralisé pour gagner un peu de place, à la place du fonds IGN
proj <- st_crs(mapCom)
st_crs(com) <- proj
com <- st_transform(com,3857) # projection en mercator

mapLight <- com
save(base,baseML,mapCom,couple,mapLight"Base.RData")

 m <- st_transform(mapCom, "+proj=longlat +datum=WGS84") %>% select(codgeo,P13_POP)

leaflet() %>% addPolygons(data=m)
plot(st_geometry(com))
