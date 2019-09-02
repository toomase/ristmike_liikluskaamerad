# Lae alla Viru Väljaku liikluskaamera pildid iga 10-30 sekundi järel.

library(tidyverse)


download_image <- function(x){
  
  # peata laadimine 10-30 sekundiks
  Sys.sleep(runif(n = 1, min = 10, max = 30))
 
  # piltide nimeks anna antud hetke timestamp
  nimi <- round(as.numeric(Sys.time()), 0)
  
  download.file("http://ristmikud.tallinn.ee/last/cam104.jpg", 
                destfile = str_c("data/viru_valjak_raw/", nimi, ".jpg"))
}

map(1:5000, download_image)

## cronjob, et see skript käivitada automaatselt kindlal kuupäeval ja kellaajal
## sisene cronjob vaatesse
# sudo crontab -u rstudio -e

## lisa cronjob vaates järgnevad read
# 45 7 26 8 * cd /home/rstudio/Dropbox/DataScience/R/ristmike_liikluskaamerad && Rscript /home/rstudio/Dropbox/DataScience/R/ristmike_liikluskaamerad/04_salvesta_pildid.R
