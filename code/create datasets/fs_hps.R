library(here)
library(data.table)
library(tidyverse)
library(magrittr)
library(lubridate)

###load in data & subset cols----
dfp<-fread(here("trunk","derived","Gold Files","Household Pulse Survey Data Tables","hps_final.txt"))

###weight and merge----
####Wrangle Cols----
dfp <-
  fastDummies::dummy_cols(dfp,
                          select_columns = c("marital_status", "race", "education", "income", "age_g"))
#create dummies
dfp %<>%
  rename(
    ms_d = marital_status_Divorced,
    ms_m = marital_status_Married,
    ms_nm = `marital_status_Never Married`,
    ms_s = marital_status_Separated,
    ms_w = marital_status_Widowed,
    ms_na = marital_status_,
    asian = race_Asian,
    black = race_Black,
    other = race_Other,
    white = race_White,
    education_HS = `education_HS or eq`,
    education_LT_HS = `education_Less than HS`,
    education_Some_College = `education_Some college`,
    education_Some_HS = `education_Some HS`,
    income_25k = `income_Less than $25,000`,
    income_35k = `income_$25,000 - $34,999`,
    income_50k = `income_$35,000 - $49,999`,
    income_75k = `income_50,000 - $74,999`,
    income_100k = `income_$75,000 - $99,999`,
    income_150k = `income_$100,000 - $149,000`,
    income_200k = `income_≥$200,000`,
    ag_18to35 = `age_g_18 to 35`,
    ag_36to50 = `age_g_36 to 50`,
    ag_51to65 = `age_g_51 to 65`,
    ag_66to80 = `age_g_66 to 80`,
    ag_81to90 = `age_g_81 to 90`
  )
#rename dummies

dfp[,gender_b_M := case_when(gender_b == "M" ~ 1, T ~ 0)]
#gender binary
####fpl----
fpl<-fread(here("trunk","raw","FPL","fpl.csv"))
dfp[,Year:=year(survey_date_end)]
dfp%<>%merge(fpl,by=c("state","Year","hh_size"))
dfp%<>%as.data.table()
dfp[,`:=`(
  income_max=case_when(income=="$25,000 - $34,999"~34999,
                       income=="$35,000 - $49,999"~49999,
                       income=="50,000 - $74,999"~74999,
                       income=="$75,000 - $99,999"~99999,
                       income=="$100,000 - $149,000"~149000,
                       )
  )]
y2020<-interval(ymd("20191001"),ymd("20200930"))
y2021<-interval(ymd("20201001"),ymd("20210930"))
y2022<-interval(ymd("20211001"),ymd("20220930"))
  #fiscal year intervals
a<-c("Alaska","Hawaii")
  #alaska an hawaii have different FPLs

dfp[,likely_snap:=case_when(
  (survey_date_end%within%interval(ymd("20191001"),ymd("20220930"))&
     (hh_size%in%1:2&income_25k==1|
        hh_size%in%3:4&(income_25k==1|income_35k==1)|
        hh_size%in%5:7&(income_25k==1|income_35k==1|income_50k==1)|
        hh_size>=8&(income_25k==1|income_35k==1|income_50k==1|income_75k==1))
   !state%in%a&survey_date_end%within%interval(ymd("20191001"),ymd("20220930"))&
     (hh_size%in%1:2&income_25k==1|
        hh_size%in%3:4&(income_25k==1|income_35k==1)|
        hh_size%in%5:7&(income_25k==1|income_35k==1|income_50k==1)|
        hh_size>=8&(income_25k==1|income_35k==1|income_50k==1|income_75k==1)))~1,
  T~0
  )]
  #creating a "likely SNAP" variable which estimates SNAP eligibility based on 2019 household income, household size, and state
    #Fiscal year 2020-2022 have similar enough limits that the groupings are identical given survey bins
    #
  #source: https://www.fns.usda.gov/snap/allotment/COLA
cols <-
  c(
    "food_insec",
    "covid_vax",
    "snap",
    "hispanic",
    "ms_d",
    "ms_m",
    "ms_nm",
    "ms_s",
    "ms_w",
    "ms_na",
    "asian",
    "black",
    "other",
    "white",
    "education_AA",
    "education_BA",
    "education_Grad",
    "education_HS",
    "education_LT_HS",
    "education_Some_College",
    "education_Some_HS",
    "income_",
    "income_200k",
    "income_35k",
    "income_50k",
    "income_100k",
    "income_150k",
    "income_75k",
    "income_25k",
    "ag_18to35",
    "ag_36to50",
    "ag_51to65",
    "ag_66to80",
    "ag_81to90",
    "gender_b_M"
  )

####Aggregate----
df <- dfp[,
          .(.SD * pweight,
            pweight,
            survey_date_end,
            ym = format(survey_date_end, "%Y-%m"),
            state),
          .SDcols = cols]
  #weight relevant vars

rw <- function(x, y) {
  (sum(x, na.rm = T) / sum(y, na.rm = T)) * 100
}
  #function that reweights

dfr <- df[,
          lapply(.SD, rw, pweight),
          .SDcols = cols,
          .(survey_date_end, ym, state)][,
                                         lapply(.SD, mean),
                                         .SDcols = cols,
                                         .(ym, state)] %>%
  merge(df[snap > 0,
           .(food_insec_on_snap_perc = sum(food_insec, na.rm = T) / sum(pweight, na.rm =
                                                                          T) * 100),
           .(survey_date_end, ym, state)][, .(food_insec_on_snap_perc = mean(food_insec_on_snap_perc)), .(ym, state)],
        #percent of snap enrollees food insecure
        by = c("ym", "state"), all.x = T)

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
