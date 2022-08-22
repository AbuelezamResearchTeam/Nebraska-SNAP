library(here)
library(data.table)
library(tidyverse)
library(rio)
library(magrittr)

df= rio::import("https://www2.census.gov/programs-surveys/popest/tables/2020-2021/state/totals/NST-EST2021-POP.xlsx")
df%<>%as.data.table()
df1=df[9:59,c(1,3,4)]
names(df1)<-c("state","2020","2021")
  #july 1st estimates of state level pops
df1[,`:=`(state=gsub(".", "", state, fixed=TRUE),
          `2020`=as.numeric(`2020`),
          `2021`=as.numeric(`2021`))]

df1%<>%pivot_longer(!state,names_to="year",values_to = "population")
dir.create(here("trunk","raw","PEP"),showWarnings = F)
fwrite(df1,here("trunk","raw","PEP","pep.txt"))
#source: cdc population estimates
#https://www.census.gov/data/tables/time-series/demo/popest/2020s-state-total.html

rm(list=ls())

