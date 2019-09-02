# R skript, mis proovib tuvastada, kui inimesed ületavad fooriga ristmiku punase tulega.
# Antud skript tuvastab Viru väljaku ristmiku ületamisi (http://ristmikud.tallinn.ee/last/cam104.jpg)
# Iga 15-60 sekundi järel salvestatakse maha üks kaader ristmikukaamerast
#   ning tuvastatakse, mis tuli on fooris ning kas jalakäiate punase tule korral on
#   mõni jalakäia ülekäigu rajal
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

# tuvasta vaba RAM
# kuna image_darknet_detect kulutab peale mõnda jooksutamist kogu RAM-i ära, siis tuleb selle vabastamiseks
# teha R sessioonile restart
# https://stackoverflow.com/questions/6457290/how-to-check-the-amount-of-ram-in-r
detect_available_memory <- function(){
  as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern = TRUE))
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
  
  path <- list.files("data/viru_valjak_raw/", full.names = TRUE)[1]
  
  # lae kaamera pilt
  pic <- image_read(path)
  
  ## tuvasta foorituli
  ## selleks lõika välja autode fooritule ruut ning tuvasta selle värvus
  ## kuna päevavalguses see väga hästi ei toiminud, siis ära seda kasuta
  car_light <- pic %>%
    image_crop(geometry_area(width = 10, height = 10, x_off= 1157, y_off = 425)) %>%
    as.raster() %>%
    col2rgb() %>%
    t() %>%  # transpose matrix
    as_tibble() %>%
    rename(r = red, g = green, b = blue) %>%
    summarise_all(mean) %>%
    mutate(light = case_when(r > g ~ "red",
                             g > r & g > 80 ~ "green",
                             TRUE ~ "unknown")) %>%
    pull(light)
  
  # tuvasta jalakäiate foorituli
  person_light <- pic %>% 
    image_crop(geometry_area(width = 8, height = 8, x_off= 222, y_off = 80)) %>% 
    as.raster() %>% 
    col2rgb() %>% 
    t() %>%  # transpose matrix
    as_tibble() %>% 
    rename(r = red, g = green, b = blue) %>% 
    summarise_all(mean) %>% 
    mutate(light = case_when(r > g ~ "red",
                             g > r ~ "green",
                             TRUE ~ "unknown"))  %>% 
    pull(light)
  
  # piltide nimeks anna antud hetke timestamp
  nimi <- str_extract(path, "\\d+")
  
  fs::file_delete(str_c("data/viru_valjak_raw/", nimi, ".jpg"))
  
  # kui jalakäiatel on punane tuli, siis lae pilt alla
  # lõika pildist välja ülekäigu rajalt ruut
  # tuvasta, kas ülekäigu rajal on mõni inimene
  # kui mõni inimene on tuvastatud, siis salvesta ennustuse tulemused maha
  if (person_light == "red" & car_light == "green"){
    
    # salvesta esialgne pilt
    pic %>% 
      image_write(str_c("data/viru_valjak/raw/", nimi, ".jpg"))
    
    # eralda pildist ülekäigu raja osa
    pic %>%
      image_crop(geometry_area(width = 680, height = 680, x_off= 0, y_off = 200)) %>% 
      image_write(str_c("data/viru_valjak/crop/", nimi, ".jpg"))
    
    # tuvasta, mis pildil on, ning salvesta ennustused /tmp/capture.txt faili
    redir()
    
    image_darknet_detect(file = str_c("data/viru_valjak/crop/", nimi, ".jpg"), 
                         object = yolo_tiny_voc,
                         threshold = 0.25)
    
    resetredir()
    
    # kui pildilt on midagi tuvastatud, siis salvesta ennustus
    if (predict_person() > 0) {
      
      file_copy("/tmp/capture.txt", str_c("data/viru_valjak/predict/", nimi, ".txt"))
      
      
      file_copy("predictions.png", str_c("data/viru_valjak/predict/", nimi, ".png"))
      
    } else {
      fs::file_delete(str_c("data/viru_valjak/raw/", nimi, ".jpg"))
    }
  
    
    
  }
  
}


# kui R sessioonis on mälu piisavalt, siis lae uus pilt ja tuvasta sealt inimesed ülekäigu rajal
# kui mälu on liiga vähe, siis tee R sessioonile restart
detect_person_from_road <- function(x){
  # kuna image_darknet_detect kasutab palju mälu, siis tuleb RAM puhastamiseks R sessiooni restarte teha
  # kui vaba RAM on teatud piirist vähem alles, siis tee restart
  if (detect_available_memory() > 700000) {
    
    predict_person_from_image()
    
  } else {
    
    # tee restart ja jooksuta sama skript uuesti
    rstudioapi::restartSession("source('04_tuvasta_objektid_olemasolevast_pildist.R', echo = TRUE)")
    
  }
  
}


# jooksuta funktsiooni korduvalt, et tuvastada punase tulega teed ületavaid inimesi
map(1:100, detect_person_from_road)
