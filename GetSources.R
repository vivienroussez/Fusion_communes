<<<<<<< HEAD
### Programme qui télécharge les fichiers source
### A éxécuter une seule fois à l'installation du projet sur un nouveau matériel

require(tidyverse)

download.file("https://drive.google.com/uc?export=download&id=1Y56k4ADcXehw9Ac07VuFMKKb0U1at78r",destfile = "Sources/Base_communes.csv")
download.file("https://drive.google.com/uc?export=download&id=1KQDhDTkTvHpySsEhmDk7RcmibBfKJ_pR",destfile = "Sources/COMMUNE.dbf")
download.file("https://drive.google.com/uc?export=download&id=1g6Ohx4bI1c4h2KhFNl1TXEz5BYOAEjcK",destfile = "Sources/COMMUNE.prj")
download.file("https://drive.google.com/uc?export=download&id=1UTtSV5h6rXBSnPnvTG5i9izvHa-x_GKU",destfile = "Sources/COMMUNE.shp")
download.file("https://drive.google.com/uc?export=download&id=1QePSRrQNmNCZ52n31M2ufMLD27zsI3aA",destfile = "Sources/COMMUNE.shx")


# Liens locataires propriétaires
download.file("https://drive.google.com/uc?export=download&id=1AGCZ1cFp1M_DwA7T246jMtafuroIrW_G",destfile = "locprop.csv")

# migrations
download.file("https://drive.google.com/uc?export=download&id=17ea5ku4Xkn2s-IIEOpON5F3hqXDJvRZW",destfile = "migrations.csv")

# Résidences secondaires
download.file("https://drive.google.com/uc?export=download&id=1Ww_EexxohhjU0ysvg166bHNWm9S3z9Dr",destfile = "RS.csv"
=======
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
>>>>>>> 18515472661c6303190e5fc4779cbf9c34108e7a
