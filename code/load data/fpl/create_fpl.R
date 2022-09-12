library(data.table)
library(datapasta)
library(tidyverse)
library(magrittr)
library(rvest)

dir.create(here("trunk","raw","FPL"),showWarnings = F)

download.file(url="https://aspe.hhs.gov/sites/default/files/documents/8e7483715719918e8fddb58ab23f606d/Guidelines-1983-2022.xlsx",destfile = here("trunk","raw","FPL","fpls.xlsx"),mode="wb")

dfr<-vector()
for (i in 1:3) {
  df<-readxl::read_excel(here("trunk","raw","FPL","fpls.xlsx"),sheet=i+1)
  df%<>%as.data.table()
  df1<-df[4:43]
  names(df1)<-df[3]%>%as.character()
  df1<-df1[,lapply(.SD,as.numeric),.SDcols=names(df1)[1:10]]
  
  df1[,`:=`(`9 Persons`=`8 Persons`+`Additional $`,
            `10 Persons`=`8 Persons`+2*`Additional $`)]
  
  df1<-
    df1[Year%in%2020:2022]%>%
    pivot_longer(cols=!c("Year","Additional $"),values_to = "FPL",names_to="hh_size")%>%
    mutate(hh_size=gsub(" .*","",hh_size),states=c("all","Alaska","Hawaii")[i])
  dfr%<>%rbind(df1)
}
dfr%<>%data.table()

h<-c(state.name[c(1,3:10,12:50)],"District of Columbia")
dfr%<>%
  mutate(state=case_when(states=="Hawaii"~list("Hawaii"),
                             states=="Alaska"~list("Alaska"),
                             T~list(h)))%>%
  unnest(state)%>%select(!c(states))

fwrite(dfr,here("trunk","raw","FPL","fpl.csv"))

rm(list=ls())
