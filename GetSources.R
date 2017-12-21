### Programme qui télécharge les fichiers source
### A éxécuter une seule fois à l'installation du projet sur un nouveau matériel

require(tidyverse)

download.file("https://drive.google.com/uc?export=download&id=1Y56k4ADcXehw9Ac07VuFMKKb0U1at78r",destfile = "Sources/Base_communes.csv")
download.file("https://drive.google.com/uc?export=download&id=1KQDhDTkTvHpySsEhmDk7RcmibBfKJ_pR",destfile = "Sources/COMMUNE.dbf")
download.file("https://drive.google.com/uc?export=download&id=1g6Ohx4bI1c4h2KhFNl1TXEz5BYOAEjcK",destfile = "Sources/COMMUNE.prj")
download.file("https://drive.google.com/uc?export=download&id=1UTtSV5h6rXBSnPnvTG5i9izvHa-x_GKU",destfile = "Sources/COMMUNE.shp")
download.file("https://drive.google.com/uc?export=download&id=1QePSRrQNmNCZ52n31M2ufMLD27zsI3aA",destfile = "Sources/COMMUNE.shx")
