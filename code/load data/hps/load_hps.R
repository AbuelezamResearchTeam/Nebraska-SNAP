library(here)
library(data.table)
library(tidyverse)

dir.create(here("trunk","raw","Household Pulse Survey Data Tables","repwgt"),showWarnings = T)
dir.create(here("trunk","raw","Household Pulse Survey Data Tables","puf"),showWarnings = T)
dir.create(here("trunk","raw","Household Pulse Survey Data Tables","data dictionary"),showWarnings = T)
  #write directories for project to end up in

getOption('timeout')
options(timeout=600)
  #timeout is normally 60 seconds, upping limit for download.file() delays

for (x in 1:47) {
  if(x%in%1:21){
    y<-2020
  }else if(x%in%22:40){
    y<-2021
  }else{
    y<-2022
  }
    #years change in url, so depending on year the survey took place we change these
  
  file<-paste0("https://www2.census.gov/programs-surveys/demo/datasets/hhp/",y,"/wk",x,"/HPS_Week",stringr::str_pad(x,pad="0",width=2),"_PUF_CSV.zip")
  temp <- tempfile()
  download.file(file,temp)
    #we save the zip to a temp location to be drawn on later
  
  data <- read.csv(unz(temp, paste0("pulse",y,"_puf_",stringr::str_pad(x,pad="0",width=2),".csv")))
  fwrite(data,here("trunk","raw","Household Pulse Survey Data Tables","puf",paste0("week_",stringr::str_pad(x,pad="0",width=2),".txt")))
    #we save the puf into puf
  
  data <- read.csv(unz(temp, paste0("pulse",y,"_repwgt_puf_",stringr::str_pad(x,pad="0",width=2),".csv")))
  fwrite(data,here("trunk","raw","Household Pulse Survey Data Tables","repwgt",paste0("week_",stringr::str_pad(x,pad="0",width=2),".txt")))
    #we save the repwgt into repwgt
  
  unzip(zipfile=temp, files=paste0("pulse",y,"_data.dictionary_CSV_",stringr::str_pad(x,pad="0",width=2),".xlsx"),exdir=here("trunk","raw","Household Pulse Survey Data Tables","data dictionary"))
    #we save the data dictionary into data dictionary
  
  unlink(temp)
    #we unlink from temp to clean the space for the following survey
}

rm(list=ls())
  #remove everything in environment, not necessary

dir.create(here("trunk","derived","Standard Files"),showWarnings = F)
dir.create(here("trunk","derived","Standard Files","Household Pulse Survey Data Tables"))
  #create directory for standard files

df<-rbindlist(map(list.files(here("trunk","raw","Household Pulse Survey Data Tables","puf"),full.names = T),fread),fill=T)
fwrite(df,here("trunk","derived","Standard Files","Household Pulse Survey Data Tables","puf.txt"))
  #row binding all surveys to keep in one large dataset

df<-rbindlist(map(list.files(here("trunk","raw","Household Pulse Survey Data Tables","repwgt"),full.names = T),fread),fill=T)
fwrite(df,here("trunk","derived","Standard Files","Household Pulse Survey Data Tables","repwgt.txt"))
  #row binding all repwgt to keep in one large dataset

rm(list=ls())
  #clearing environment
