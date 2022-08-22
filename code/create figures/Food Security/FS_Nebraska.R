library(data.table)
library(tidyverse)
library(magrittr)
library(ggthemes)
library(here)
library(openxlsx)
library(lubridate)
library(patchwork)
#load in & set up----
dir.create(here("trunk","analysis","Nebraska Trends"),showWarnings = F)
df<-fread(here("trunk","derived","Analytic Tables","food_security.txt"))
#v1----
names(df)[3:4]<-c("food_insec_perc","covid_vax_perc")
v<-c("food_insec_perc","food_insec_on_snap_perc","covid_vax_perc","n_cases_k","n_deaths_k")
df1<-
  df[state=="Nebraska"]%>%
  pivot_longer(cols=v,names_to="var",values_to="val")
df1$var<-factor(df1$var,levels = v)

p1<-df1%>%
  mutate(date=lubridate::ymd(paste0(ym,"-01")))%>%
  ggplot()+
    geom_line(aes(x=date,y=val,group=var))+
    facet_wrap(~var,scales="free",labeller = as_labeller(c(food_insec_perc="Percent Food Insecure",
                                                           food_insec_on_snap_perc="Percent on SNAP Food Insecure",
                                                           covid_vax_perc="Percent Vaccinated",
                                                           n_cases_k="Monthly Case Rate (Per 100k)",
                                                           n_deaths_k="Monthly Mortality Rate (Per 100k)")))+
    geom_rect(xmax=lubridate::ymd("2020-11-01"),xmin=lubridate::ymd("2020-08-01"),ymin=-Inf,ymax=Inf,alpha=.05)+
    geom_rect(xmax=Inf,xmin=lubridate::ymd("2021-08-01"),ymin=-Inf,ymax=Inf,alpha=.05)+
    labs(x="Year-Month",
         y="",
         title="Figure X: Nebraska (2020-2022)")+
    theme_clean()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle=45,hjust=1),
          plot.caption=element_text(hjust=0),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.background = element_rect(color = "white"))+
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")
p1

ggsave(p1,file=here("trunk","analysis","Nebraska Trends","plot.png"),
       height=7.54,width=9.06,units=c("in"))
knitr::plot_crop(here("trunk","analysis","Nebraska Trends","plot.png"))

#v2----

df2<-
  df[,.(state=factor(state,levels=f[c(28,1:27,29:51)]),
        ym,
        food_insec_perc,
        food_insec_on_snap_perc,
        covid_vax_perc,
        n_cases_k,
        n_deaths_k)]%>%
  pivot_longer(cols=v,names_to="var",values_to="val")%>%
  mutate(date=lubridate::ymd(paste0(ym,"-01")))%>%
  mutate(isNeb=case_when(state=="Nebraska"~"Nebraska",T~"Not Nebraska"))

for (i in 1:5) {
  assign(paste0("p",i),
         df2%>%
           mutate(date=lubridate::ymd(paste0(ym,"-01")))%>%
           filter(var==v[i])%>%
           ggplot()+
           geom_line(aes(x=date,y=val,group=state))+
           gghighlight::gghighlight(state=="Nebraska",use_direct_label = F)+
           geom_rect(xmax=lubridate::ymd("2020-11-01"),xmin=lubridate::ymd("2020-08-01"),ymin=-Inf,ymax=Inf,alpha=.05)+
           geom_rect(xmax=Inf,xmin=lubridate::ymd("2021-08-01"),ymin=-Inf,ymax=Inf,alpha=.05)+
           labs(y="",
                x="",
                subtitle=paste0(c("Percent Food Insecure","Percent of SNAP Recipients Food Insecure","Percent Vaccinated","Monthly Case Rate (Per 100k)","Monthly Mortality Rate (Per 100k)")[i]))+
           theme_clean()+
           theme(legend.position = "none",
                 axis.text.x = element_text(angle=45,hjust=1),
                 plot.caption=element_text(hjust=0),
                 plot.title.position = "plot",
                 plot.caption.position = "plot",
                 plot.background = element_rect(color = "white"))+
           scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")
  )
}
p6<-(p1+p2)/(p3+p4)/p5+
  plot_annotation(tag_prefix = 'Fig X.',
                  tag_levels = "A",
                  tag_suffix = ':',
                  title="Figure X: Trends In Outcomes (2020-2022)")& 
  theme(plot.tag.position = c(0, 1.02),
        plot.tag = element_text(size = 8, hjust = 0, vjust = 0),
        plot.subtitle = element_text(size=8))
p6

ggsave(p6,file=here("trunk","analysis","Nebraska Trends","plot2.png"),
       height=7.54,width=9.06,units=c("in"))
knitr::plot_crop(here("trunk","analysis","Nebraska Trends","plot2.png"))

#v3----
df2<-
  df%>%
  pivot_longer(cols=v,names_to="var",values_to="val")%>%
  group_by(state,var)%>%
  mutate(pc=(val-lag(val))/lag(val)*100)

df2$var<-factor(df2$var,levels = v)
df2%<>%
  mutate(is_NE=case_when(state=="Nebraska"~1,
                         T~0))
#fwrite(df2,here("demo.txt"))
for (i in 1:5) {
  assign(paste0("p",i),
         df2%>%
           mutate(date=lubridate::ymd(paste0(ym,"-01")))%>%
           filter(var==v[i])%>%
           ggplot()+
           geom_line(aes(x=date,y=pc,group=state))+
           gghighlight::gghighlight(state=="Nebraska",use_direct_label = F)+
           geom_rect(xmax=lubridate::ymd("2020-11-01"),xmin=lubridate::ymd("2020-08-01"),ymin=-Inf,ymax=Inf,alpha=.05)+
           geom_rect(xmax=Inf,xmin=lubridate::ymd("2021-08-01"),ymin=-Inf,ymax=Inf,alpha=.05)+
           labs(y="",
                x="",
                subtitle=paste0(c("Percent Food Insecure","Percent of SNAP Recipients Food Insecure","Percent Vaccinated","Monthly Case Rate (Per 100k)","Monthly Mortality Rate (Per 100k)")[i]))+
           theme_clean()+
           theme(legend.position = "none",
                 axis.text.x = element_text(angle=45,hjust=1),
                 plot.caption=element_text(hjust=0),
                 plot.title.position = "plot",
                 plot.caption.position = "plot",
                 plot.background = element_rect(color = "white"))+
           scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")
         )
}
p6<-(p1+p2)/(p3+p4)/p5+
  plot_annotation(tag_prefix = 'Fig X.',
                  tag_levels = "A",
                  tag_suffix = ':',
                  title="Figure X: Percent Change Over Month (2020-2022)")& 
  theme(plot.tag.position = c(0, 1.02),
        plot.tag = element_text(size = 8, hjust = 0, vjust = 0),
        plot.subtitle = element_text(size=8))
p6

ggsave(p6,file=here("trunk","analysis","Nebraska Trends","plot1.png"),
       height=7.54,width=9.06,units=c("in"))
knitr::plot_crop(here("trunk","analysis","Nebraska Trends","plot1.png"))
