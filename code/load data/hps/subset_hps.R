library(here)
library(data.table)
library(lubridate)

#set up----
df<-fread(here("trunk","derived","Standard Files","Household Pulse Survey Data Tables","puf.txt"))
dir.create(here("trunk","derived","Gold Files"),showWarnings = F)
dir.create(here("trunk","derived","Gold Files","Household Pulse Survey Data Tables"),showWarnings = F)


#create gold file----
dfg<-
  df[,.(
  ##Demographics----
    birth_year=TBIRTH_YEAR, 
      # birth year, integer (Year)
    birth_year_imputed=case_when(ABIRTH_YEAR==1~1,
                                 ABIRTH_YEAR==2~0), 
      # binary of whether birth year is imputed, integer (1/0)
    gender_b=case_when(EGENDER==1|is.na(EGENDER)&EGENID_BIRTH==1~"M",
                       EGENDER==2|is.na(EGENDER)&EGENID_BIRTH==2~"F"), 
      # EGENDER runs weeks 1-33, EGENID_BIRTH is assigned gender 34-47, character binary (M/F)
    gender_i=case_when(GENID_DESCRIBE==1~"M",
                       GENID_DESCRIBE==2~"F",
                       GENID_DESCRIBE==3~"T",
                       GENID_DESCRIBE==4~"None"), 
      # gender identity, runs week 34-47, character (M/F/T/None/NA)
    gender_imputed=case_when(AGENDER==1|is.na(AGENDER)&AGENID_BIRTH==1~1,
                             AGENDER==2|is.na(AGENDER)&AGENID_BIRTH==2~0), 
      # AGENDER runs 1-33, AGENID_BIRTH runs 34-47, binary (1/0)
    hispanic=case_when(RHISPANIC==1~0,
                       RHISPANIC==2~1),
      # hispanic origin binary, integer (1/0)
    hispanic_imputed=case_when(AHISPANIC==1~1,
                               AHISPANIC==2~0), 
      # hispanic origin imputed binary, integer (1/0)
    race=case_when(RRACE==1~"White",
                   RRACE==2~"Black",
                   RRACE==3~"Asian",
                   RRACE==4~"Other"), 
      # individual race, character (White/Black/Asian/Other)
    race_imputed=case_when(ARACE==1~1,
                           ARACE==2~0), 
      # race imputed binary, integer (1/0)
    marital_status=case_when(MS==1~"Married",
                             MS==2~"Widowed",
                             MS==3~"Divorced",
                             MS==4~"Separated",
                             MS==5~"Never Married"), 
      # marital status, character (Married/Widowed/Divorced/Separated/Never Married/NA)
    education=case_when(EEDUC==1~"Less than HS",
                        EEDUC==2~"Some HS",
                        EEDUC==3~"HS or eq",
                        EEDUC==4~"Some college",
                        EEDUC==5~"AA",
                        EEDUC==6~"BA",
                        EEDUC==7~"Grad"), 
      # education variable, character (Less than HS/Some HS/HS or eq/Some college/AA/BA/Grad)
    education_imputed=case_when(AEDUC==1~1,
                                AEDUC==2~0),
      # education imputed binary, integer (1/0)
    sex_orientation=case_when(SEXUAL_ORIENTATION==1~"Gay or lesbian",
                              SEXUAL_ORIENTATION==2~"Straight",
                              SEXUAL_ORIENTATION==3~"Bisexual",
                              SEXUAL_ORIENTATION==4~"Other",
                              SEXUAL_ORIENTATION==5~"Unsure"), 
      # sex or. begins week 34, character (Gay or lesbian/Straight/Bisexual/Other/Unsure/NA)  
    hh_size=THHLD_NUMPER, 
      # household size - includes adult and children, integer 1-40
    hh_adults=THHLD_NUMADLT, 
      # household size - adults only, integer 1-40
    hh_kids=THHLD_NUMKID,
      # household size - includes adult and children, integer 1-40
    kids_l5=case_when(KIDS_LT5Y==1~"Yes"), 
      # binary of kids under 5 in hh - begins week 34, integer (1/0)
    kids_5to11=case_when(KIDS_5_11Y==1~"Yes"),
      # binary of kids 5-11 in hh - begins week 34, integer (1/0)
    kids_12to17=case_when(KIDS_12_17Y==1~"Yes"),
      # binary of kids 12-17 in hh - begins week 34, integer (1/0)
    employed=case_when(ANYWORK==1~1,
                       ANYWORK==2~0),
      #binary of whether or not employed, integer (1/0)
    unemployment=case_when(UNEMPPAY%in%c(1:3)~1,
                           UNEMPPAY==4~0),
      #binary of whether or not person received pay for time not working (1/0)
    income=case_when(INCOME==1~"Less than $25,000",
                     INCOME==2~"$25,000 - $34,999",
                     INCOME==3~"$35,000 - $49,999",
                     INCOME==4~"50,000 - $74,999",
                     INCOME==5~"$75,000 - $99,999",
                     INCOME==6~"$100,000 - $149,999",
                     INCOME==7~"$150,000 - $199,999",
                     INCOME==8~"≥$200,000"),
      #categorical 2019 HH income variable
    income_max=case_when(INCOME==1~24999,
                     INCOME==2~34999,
                     INCOME==3~49999,
                     INCOME==4~74999,
                     INCOME==5~99999,
                     INCOME==6~149999,
                     INCOME==7~199999,
                     INCOME==8~200000),
      #max 2019 HH income variable, numeric 
    
  ##COVID----
    covid_vax=case_when(RECVDVACC==1~1,
                        RECVDVACC==2~0),
      # binary of covid vaccine status - begins week 21, integer (1/0)
    covid_doses_i=case_when(DOSES==1|is.na(DOSES)&DOSESRV%in%c(1,2)~"All",
                            DOSES==2|is.na(DOSES)&DOSESRV==3~"Not All"),
      # intention to get vaccine doses - ends week 40, character (All/Not All)
    covid_doses_r=case_when(NUMDOSES==1~"1",
                            NUMDOSES==2~"2",
                            NUMDOSES==3~"3",
                            NUMDOSES==4~">=4"),
      # number of doses - begins week 40, character (1/2/3/>=4/NA)
  ##Food Security----
    snap=case_when(SNAP_YN==1~1,
                   SNAP_YN==2~0), 
      # SNAP binary, integer (1/0/NA)
    food_insec=case_when(CURFOODSUF%in%1:2~0,
                         CURFOODSUF%in%3:4~1), 
      # food insufficient binary, integer (1/0/NA)
    child_food_insec=case_when(CHILDFOOD%in%1:2~1,
                               CHILDFOOD==3~0),
      # child food insufficient binary, integer (1/0/NA)
    food_insec_afford=case_when(FOODRSNRV1==1~1),
      # food insecure due to cost, integer (1/NA)
    food_insec_couldnt_get_out=case_when(FOODRSNRV2==1~1),
      # food insecure due to inability to get out, integer (1/NA)
    food_insec_safety=case_when(FOODRSNRV3==1~1),
      # food insecure due safety concerns or no delivery, integer (1/0/NA)
    food_insec_no_reason=case_when(FOODRSNRV4==1~1),
      # food insecure due to no reason, integer (1/NA)
  ##Weights & Flags----    
    pweight=PWEIGHT,
    hweight=HWEIGHT,
    state=cdlTools::fips(EST_ST,to="Name"),
    week=WEEK,
    scram=SCRAM,
    region=REGION,
    survey_date_end=case_when(
      WEEK==1~ymd("2020-05-05"),
      WEEK==2~ymd("2020-05-12"),
      WEEK==3~ymd("2020-05-19"),
      WEEK==4~ymd("2020-05-26"),
      WEEK==5~ymd("2020-06-02"),
      WEEK==6~ymd("2020-06-09"),
      WEEK==7~ymd("2020-06-16"),
      WEEK==8~ymd("2020-06-23"),
      WEEK==9~ymd("2020-06-30"),
      WEEK==10~ymd("2020-07-07"),
      WEEK==11~ymd("2020-07-14"),
      WEEK==12~ymd("2020-07-21"),
      WEEK==13~ymd("2020-08-31"),
      WEEK==14~ymd("2020-09-14"),
      WEEK==15~ymd("2020-09-28"),
      WEEK==16~ymd("2020-10-12"),
      WEEK==17~ymd("2020-10-26"),
      WEEK==18~ymd("2020-11-09"),
      WEEK==19~ymd("2020-11-23"),
      WEEK==20~ymd("2020-12-09"),
      WEEK==21~ymd("2020-12-21"),
      WEEK==22~ymd("2021-01-18"),
      WEEK==23~ymd("2021-02-01"),
      WEEK==24~ymd("2021-02-15"),
      WEEK==25~ymd("2021-03-01"),
      WEEK==26~ymd("2021-03-15"),
      WEEK==27~ymd("2021-03-29"),
      WEEK==28~ymd("2021-04-26"),
      WEEK==29~ymd("2021-05-10"),
      WEEK==30~ymd("2021-05-24"),
      WEEK==31~ymd("2021-06-07"),
      WEEK==32~ymd("2021-06-21"),
      WEEK==33~ymd("2021-07-05"),
      WEEK==34~ymd("2021-08-02"),
      WEEK==35~ymd("2021-08-16"),
      WEEK==36~ymd("2021-08-30"),
      WEEK==37~ymd("2021-09-13"),
      WEEK==38~ymd("2021-09-27"),
      WEEK==39~ymd("2021-10-11"),
      WEEK==40~ymd("2021-12-13"),
      WEEK==41~ymd("2022-01-10"),
      WEEK==42~ymd("2022-02-07"),
      WEEK==43~ymd("2022-03-14"),
      WEEK==44~ymd("2022-04-11"),
      WEEK==45~ymd("2022-05-09"),
      WEEK==46~ymd("2022-06-13"),
      WEEK==47~ymd("2022-07-11"))
  ##End----
  )]
  ##Age groupings----
dfg[,age:=eeptools::age_calc(ymd(paste0(birth_year,"-1-1")),survey_date_end,units = "years")]
dfg[,age_g:=case_when(round(age)%in%0:17~"0 to 17",
                      round(age)%in%18:35~"18 to 35",
                      round(age)%in%36:50~"36 to 50",
                      round(age)%in%51:65~"51 to 65",
                      round(age)%in%66:80~"66 to 80",
                      round(age)%in%81:90~"81 to 90")]

fwrite(dfg,here("trunk","derived","Gold Files","Household Pulse Survey Data Tables","hps_final.txt"))
  #save recoded df
rm(list=ls())
  #clear environment