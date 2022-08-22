library(here)
library(data.table)
library(tidyverse)

df<-fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

dir.create(here("trunk","raw","COVID-19"),showWarnings = F)
fwrite(df,here("trunk","raw","COVID-19","covid_state.txt"))
#source: United States COVID-19 Cases and Deaths by State over Time
#https://github.com/nytimes/covid-19-data

rm(list=ls())
