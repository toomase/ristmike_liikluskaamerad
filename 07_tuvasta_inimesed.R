# R skript, mis proovib tuvastada, kui inimesed ületavad fooriga ristmiku punase tulega.
# Antud skript tuvastab Viru väljaku ristmiku ületamisi (http://ristmikud.tallinn.ee/last/cam104.jpg)
# eelnevate skriptide abil on maha salvestatud iga 15-60 sekundi järel üks kaader ristmikukaamerast
#   ning tuvastatud, mis tuli on fooris
#   Jalakäiate punase tule korral on failist tehtud erlaid koopia ning
#   pildist on välja lõigatud ainult sebra osa
#   sellelt hakkab antud skript tuvastama, kas mõni inimene on punase tulega sebral
#   
# Objektide tuvastamiseks on kasutatud YOLO mudelit
#   https://pjreddie.com/darknet/yolo/
#   https://heartbeat.fritz.ai/object-detection-in-just-3-lines-of-r-code-using-tiny-yolo-b5a16e50e8a0


library(magick)  # piltide töötlemiseks
library(tidyverse)
library(Rcpp)  # image_darknet_detect tulemuste salvestamiseks 
library(image.darknet)  # tuvasta pildilt objekte YOLO mudeli abil
library(fs)  # failide manipuleerimiseks


# funktsioonid, et salvestada image_darknet_detect tulemused 
# ilma selleta kuvatakse tulemused lihtsalt konsoolis, aga neid pole võimalik maha salvestada
# pärit siit: https://stackoverflow.com/questions/36052902/capture-output-of-system-command-within-a-function-in-r
cppFunction('void redir(){FILE* F=freopen("/tmp/capture.txt","w+",stdout);}')
cppFunction('void resetredir(){FILE* F=freopen("/dev/tty","w+",stdout);}')

# # tuvasta vaba RAM
# # kuna image_darknet_detect kulutab peale mõnda jooksutamist kogu RAM-i ära, siis tuleb selle vabastamiseks
# # teha R sessioonile restart
# # https://stackoverflow.com/questions/6457290/how-to-check-the-amount-of-ram-in-r
# # kuna see ei töödanud väga hästi, siis kasuta teist funkstiooni
# detect_available_memory <- function(){
#   as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern = TRUE))
# }


# tuvasta kasutatud RAM
# kuna image_darknet_detect kulutab peale mõnda jooksutamist kogu RAM-i ära, siis tuleb selle vabastamiseks
# teha R sessioonile restart
# piiriks sea 2000, kui on üle selle, siis restart
# Kui see funktsioon ebaõnnetsub ja annab veateate "Cannot allocate memory",
# siis tuleb anda juurde swap mälu
# See on vajalik kui system() funktsiooni abil jooksutada shell käske, mis terminalis jooksevad kenatsi läbi
# Täpsemalt siin: 
# https://stackoverflow.com/questions/54034171/r-system-cannot-allocate-memory-even-though-the-same-command-can-be-run-from-a?stw=2
# https://aws.amazon.com/premiumsupport/knowledge-center/ec2-memory-swap-file/
# 
detect_used_memory <- function(){
  system("free -m", intern = TRUE)[2] %>% 
    as_tibble() %>% 
    mutate(value = str_squish(value)) %>% 
    separate(value, into = c("group", "total", "used", "free", "shared", "buff", "available"), sep = " ") %>% 
    pull(used) %>% 
    as.numeric(.)
}


# kas pildilt tuvastati mõni inimene?
# kui tuvastati mõni inimene, siis salvestan tulemused maha
predict_person <- function(){
  read_lines("/tmp/capture.txt") %>% 
    as_tibble() %>% 
    filter(str_detect(value, "person")) %>% 
    pull(value) %>% 
    length()
}


# defineeri mudel, mille abil objekte pildilt tuvastada
yolo_tiny_voc <- image_darknet_model(
  type = "detect", 
  model = "tiny-yolo-voc.cfg",
  weights = system.file(package = "image.darknet", "models", "tiny-yolo-voc.weights"),
  labels = system.file(package = "image.darknet", "include", 
                       "darknet", "data", "voc.names"))


predict_person_from_image <- function(x){
  
  path <- list.files("data/viru_valjak/red_light_crop/", full.names = TRUE)[1]
  
  # eralda pildi nimi, et seda kasutada failide kopeerimisel
  nimi <- str_extract(path, "\\d+")
    
  # tuvasta, mis pildil on, ning salvesta ennustused /tmp/capture.txt faili
  redir()
  
  image_darknet_detect(file = path, 
                       object = yolo_tiny_voc,
                       threshold = 0.25)
  
  resetredir()
  
  
  # tee väike paus
  Sys.sleep(runif(n = 1, min = 0.5, max = 1.5))
  
  
  # kui pildilt on midagi tuvastatud, siis salvesta ennustus
  if (predict_person() > 0) {
    
    file_copy("/tmp/capture.txt", str_c("data/viru_valjak/red_light_predict/", nimi, ".txt"))
    
    file_copy("predictions.png", str_c("data/viru_valjak/red_light_predict/", nimi, "_predict.png"))
    
    file_copy(str_c("data/viru_valjak/red_light/", nimi, ".jpg"), 
              str_c("data/viru_valjak/red_light_predict/", nimi, "_raw.jpg"))
  } 
  
  # peale inimeste tuvastamist kustuta toorandmete kaustast pilt ära
  fs::file_delete(path)
  
}



# kui R sessioonis on mälu piisavalt, siis lae uus pilt ja tuvasta sealt inimesed ülekäigu rajal
# kui mälu on liiga vähe, siis tee R sessioonile restart
detect_person_from_road <- function(x){
  # kuna image_darknet_detect kasutab palju mälu, siis tuleb RAM puhastamiseks R sessiooni restarte teha
  # kui vaba RAM on teatud piirist vähem alles, siis ära ennustuse skripti enam jooksuta
  if (detect_used_memory() < 2000) {
    
    predict_person_from_image()
    
  }
}


# jooksuta funktsiooni 10 korda
map(1:10, detect_person_from_road)

## rstudio serveri script, mis käivitub iga minut 27.08
## selle abil valitakse projekti kaust, käivitatakse skript ja tehaks R restart
## siin abi kuidas üles seadea konkreetseks ajaks: https://crontab.guru/#40_14_24_8_*
## selleks, et cronjob logi näha: https://askubuntu.com/questions/56683/where-is-the-cron-crontab-log
## kui cronjob annab veateate No MTA installed, discarding output, siis tee nii: https://askubuntu.com/questions/56683/where-is-the-cron-crontab-log
## selleks, et käivitada skrip ainult siis kui see juba ei käi: https://askubuntu.com/questions/142002/generic-solution-to-prevent-a-long-cron-job-from-running-in-parallel/145999#145999

##sisene cronjob vaatesse
# sudo crontab -u rstudio -e

## lisa cronjob vaates järgnevad read
# * * 27 8 * cd /home/rstudio/Dropbox/DataScience/R/ristmike_liikluskaamerad && run-one Rscript /home/rstudio/Dropbox/DataScience/R/ristmike_liikluskaamerad/07_tuvasta_inimesed.R && rstudio-server restart