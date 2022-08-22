library(here)
library(data.table)
library(tidyverse)
library(magrittr)
library(lubridate)

###load in data & subset cols----
dfp<-fread(here("trunk","derived","Gold Files","Household Pulse Survey Data Tables","hps_final.txt"))

###weight and merge----
####Wrangle Cols----
dfp<-fastDummies::dummy_cols(dfp,select_columns = c("marital_status","race","education","income","age_g"))
x<-tibble(names=names(dfp),
          renames=names(dfp))%>%as.data.table()
x[c(42:71)]$renames<-c("ms_d","ms_m","ms_nm","ms_s","ms_w","ms_na","asian","black","other","white","education_AA","education_BA","education_Grad","education_HS","education_LT_HS","education_Some_College","education_Some_HS","income_NA","income_200k","income_34k","income_49k","income_99k","income_149k","income_74k","income_25k","ag_18to35","ag_36to50","ag_51to65","ag_65to80","ag_80to90")
names(dfp) <- x$renames[match(names(dfp),x$names)]
dfp[,gender_b_M:=case_when(gender_b=="M"~1,T~0)]

cols<-c("food_insec","covid_vax","snap","hispanic",names(dfp)[42:72])
####Aggregate----
df<-dfp[,
        .(.SD*pweight,
          pweight,
          survey_date_end,
          ym=format(survey_date_end, "%Y-%m"),
          state),
        .SDcols=cols]
  #weight relevant vars

rw <- function(x,y) {
  (sum(x,na.rm=T)/sum(y,na.rm=T))*100
}
dfr<-df[,
        lapply(.SD, rw, pweight),
        .SDcols=cols,
        .(survey_date_end,ym,state)][,
                                     lapply(.SD, mean),
                                     .SDcols=cols,
                                     .(ym,state)]%>%
  merge(
    df[snap>0,
       .(food_insec_on_snap_perc=sum(food_insec,na.rm=T)/sum(pweight,na.rm=T)*100),
       .(survey_date_end,ym,state)][,.(food_insec_on_snap_perc=mean(food_insec_on_snap_perc)),.(ym,state)],
      #percent of snap enrollees food insecure
    by=c("ym","state"),all.x=T)

####COVID----
dfc<-fread(here("trunk","raw","COVID-19","covid_state.txt"))
dfc[,`:=`(n_cases=cases-lag(cases),
          n_deaths=deaths-lag(deaths)),state]
  #create new cases/deaths per day

dfc<-
  dfc[,
      .(state,
        date=lubridate::ymd(date),
        ym=format(lubridate::ymd(date), "%Y-%m"),
        n_cases,
        n_deaths
        )
      ][order(date)][,
                     .(n_deaths=sum(n_deaths,na.rm = T),
                       n_cases=sum(n_cases,na.rm = T)),
                     .(ym,state)][,year:=year(ym(ym))]
  #monthly cases and deaths
dfp<-fread(here("trunk","raw","PEP","pep.txt"))

dfc%<>%
  merge(dfp,by=c("state","year"))%>%
  mutate(n_deaths_k=(n_deaths/population)*10^5)%>%
  mutate(n_cases_k=(n_cases/population)*10^5)%>%
  select(state,ym,n_cases_k,n_deaths_k)
  #cases and death rates are NA in 2022 because of lack of 2022 population data
dfr%<>%
  merge(dfc,by=c("state","ym"),all.x=T)
###flag policy intervention----

dfe<-fread(here("trunk","raw","Emergency Allotments","EA_by_month.csv"))
dfe%<>%pivot_longer(!Date)
dfe%<>%as.data.table()
dfe[value==0]$value<-NA
dfe$Date<-mdy(dfe$Date)
dfe<-dfe[!is.na(value)]
dfe=dfe[,.(state=name,ym=format(Date, "%Y-%m"),EA=1)]
dfr%<>%merge(dfe,by=c("state","ym"),all.x=T)
dfr[is.na(EA)]$EA<-0

fwrite(dfr,here("trunk","derived","Analytic Tables","food_security.txt"))
rm(list=ls())
