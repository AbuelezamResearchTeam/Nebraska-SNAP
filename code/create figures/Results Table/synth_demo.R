library(data.table)
library(tidyverse)
library(magrittr)
library(ggthemes)
library(here)
library(openxlsx)
library(lubridate)
library(Synth)
library(patchwork)
library(flextable)

#load in & set up----
dir.create(here("trunk","analysis","Results"),showWarnings = F)
df<-fread(here("trunk","derived","Analytic Tables","food_security.txt"))

df[,`:=`(unit_var=cdlTools::fips(state,to="FIPS"),
         time_var=(interval(ym("2020-05"),ym(ym)))%/%months(1),
         date=lubridate::ymd(paste0(ym,"-01"))
         )]

#create synths ----
outcomes<-c("n_cases_k","n_deaths_k","food_insec","food_insec_on_snap_perc")
names(df)[c(3,6:39,41:42)]
for (i in 1:3) {
  dp<-dataprep(foo=df,
               predictors=names(df)[c(3,6:39,41:42)],
               dependent=outcomes[i],
               unit.variable="unit_var",
               time.variable = "time_var",
               treatment.identifier = 31,
               controls.identifier = df[unit_var!=31,.N,unit_var]$unit_var,
               time.predictors.prior = c(0:2),
               time.optimize.ssr = c(0:2),
               time.plot = c(0:6)
  )
  assign(paste0(outcomes[i],"_synth"),synth(dp))
  
}


df1=df[unit_var!=31&time_var%in%6,.(unit_var)][order(unit_var)]
df1$weights<-synth$solution.w
df%<>%merge(df1,by="unit_var",all.x=T)
df[,isN:=case_when(unit_var==31~"Nebraska",T~"Synthetic Nebraska")]
df[is.na(weights)]$weights<-1


p1<-
  ggplot(df[,sum(n_cases_k*weights),.(isN,date)])+
  geom_line(aes(x=date,y=V1,col=isN))+
  geom_rect(xmax=lubridate::ymd("2020-11-01"),xmin=lubridate::ymd("2020-08-01"),ymin=-Inf,ymax=Inf,alpha=.05,fill="lightgrey")+
  geom_rect(xmax=Inf,xmin=lubridate::ymd("2021-08-01"),ymin=-Inf,ymax=Inf,alpha=.05,fill="lightgrey")+
  theme_clean()+
  theme(axis.text.x = element_text(angle=45,hjust=1),
        plot.caption=element_text(hjust=0),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.background = element_rect(color = "white"))+
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")

p1

ggsave(p1,file=here("trunk","analysis","Synthetic Control","plot.png"),
       height=7.54,width=9.06,units=c("in"))
knitr::plot_crop(here("trunk","analysis","Synthetic Control","plot.png"))
