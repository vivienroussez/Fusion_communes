### Programme qui télécharge les fichiers source
### A éxécuter une seule fois à l'installation du projet sur un nouveau matériel

require(tidyverse)

download.file("https://drive.google.com/uc?export=download&id=1Y56k4ADcXehw9Ac07VuFMKKb0U1at78r",destfile = "Sources/Base_communes.csv")
download.file("https://drive.google.com/uc?export=download&id=1oUhBFweSLj-RTSVV5HR3o8jU2iPbGN7C",destfile = "Sources/COMMUNE.dbf")
download.file("https://drive.google.com/uc?export=download&id=1avNoNdtNWqCWL1PNce0-EYRrcBtjDxl0",destfile = "Sources/COMMUNE.prj")
download.file("https://drive.google.com/uc?export=download&id=1nr-UzBQ63-9SCCsLQ_GrtyCufEujU1Tl",destfile = "Sources/COMMUNE.shp")
download.file("https://drive.google.com/uc?export=download&id=1i5vm8w3A1u5N_Urfu8D2Auuhx3r5jSWT",destfile = "Sources/COMMUNE.shx")



# t<- st_read("Sources/COMMUNE.shp")
# plot(st_geometry(t))
