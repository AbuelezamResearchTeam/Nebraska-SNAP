library(data.table)
library(tidyverse)
library(magrittr)
library(ggthemes)
library(here)
library(openxlsx)
library(lubridate)
####load in and wrangle####
df<-fread(here("trunk","raw","Emergency Allotments","EA_by_month.csv"))
df%<>%pivot_longer(!Date)
df%<>%as.data.table()
df[value==0]$value<-NA
df$Date<-mdy(df$Date)
df1<-df[!is.na(value)]
####time spans#####
dat<-df1 %>%
  group_by(name)  %>%
  mutate(l = interval(Date, lead(Date)) %/% months(1))%>%
  mutate(grp = lag(l, default = FALSE) >1)%>%
  fill(grp)%>%
  mutate(csum=cumsum(grp))%>%
  group_by(csum,name)%>%
  filter(Date == min(Date) | Date == max(Date))%>%
  select(name, Date)%>%
  mutate(s = ifelse(Date == min(Date), "seq_start", "seq_stop"))%>%
  pivot_wider(names_from = s, values_from = Date)
    

####fig 1####
dat%<>%as.data.table()
dat[,isna:=case_when(name=="Nebraska"~"1",T~"0")]
p1<-dat%>%
  ggplot() +
  geom_segment(aes(x = seq_start, xend = seq_stop, y = name, yend = name,col=isna))+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")+
  theme(axis.text.x = element_text(angle=60,hjust=1),
        axis.title = element_blank(),
        panel.grid.minor = element_line(color="lightgrey"),
        panel.grid.major = element_blank(),
        plot.title.position = "plot",
        panel.background = element_rect(fill="white"),
        legend.position = "none")+
  labs(title="Figure X: Emergency Allotment Extensions (2020 - 2022)")+
  scale_colour_manual(values = c("firebrick","navyblue"))

p1
dir.create(here("trunk","analysis","EA Ext Fig"),showWarnings = F)
ggsave(p1,file=here("trunk","analysis","EA Ext Fig","plot.png"),
       height=7.54,width=9.06,units=c("in"))
knitr::plot_crop(here("trunk","analysis","EA Ext Fig","plot.png"))
