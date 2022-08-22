library(here)
library(data.table)
library(tidyverse)

for (x in 1:47) {
  if(x%in%1:21){
    y<-2020
  }else if(x%in%22:40){
    y<-2021
  }else{
    y<-2022
  }
  file<-paste0("https://www2.census.gov/programs-surveys/demo/datasets/hhp/",y,"/wk",x,"/HPS_Week",stringr::str_pad(x,pad="0",width=2),"_PUF_CSV.zip")
  temp <- tempfile()
  download.file(file,temp)
  
  data <- read.csv(unz(temp, paste0("pulse",y,"_puf_",stringr::str_pad(x,pad="0",width=2),".csv")))
  fwrite(data,here("trunk","raw","Household Pulse Survey Data Tables","puf",paste0("week_",stringr::str_pad(x,pad="0",width=2),".txt")))
  
  data <- read.csv(unz(temp, paste0("pulse",y,"_repwgt_puf_",stringr::str_pad(x,pad="0",width=2),".csv")))
  fwrite(data,here("trunk","raw","Household Pulse Survey Data Tables","repwgt",paste0("week_",stringr::str_pad(x,pad="0",width=2),".txt")))
  
  unzip(zipfile=temp, files=paste0("pulse",y,"_data.dictionary_CSV_",stringr::str_pad(x,pad="0",width=2),".xlsx"),exdir=here("trunk","raw","Household Pulse Survey Data Tables","data dictionary"))
  
  unlink(temp)
}

rm(list=ls())

dir.create(here("trunk","derived","Standard Files","Household Pulse Survey Data Tables"))
df<-rbindlist(map(list.files(here("trunk","raw","Household Pulse Survey Data Tables","puf"),full.names = T),fread),fill=T)
fwrite(df,here("trunk","derived","Standard Files","Household Pulse Survey Data Tables","puf.txt"))

df<-rbindlist(map(list.files(here("trunk","raw","Household Pulse Survey Data Tables","repwgt"),full.names = T),fread),fill=T)
fwrite(df,here("trunk","derived","Standard Files","Household Pulse Survey Data Tables","repwgt.txt"))

rm(list=ls())
