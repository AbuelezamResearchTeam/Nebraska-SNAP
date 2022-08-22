library(here)
library(data.table)
library(tidyverse)

df<-fread("https://healthdata.gov/api/views/g62h-syeh/rows.csv?accessType=DOWNLOAD")

dir.create(here("trunk","raw","COVID-19"),showWarnings = F)
fwrite(df,here("trunk","raw","COVID-19","hosp_imp.txt"))
#source: COVID-19 Reported Patient Impact and Hospital Capacity by State Timeseries
  #https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/g62h-syeh

rm(list=ls())
