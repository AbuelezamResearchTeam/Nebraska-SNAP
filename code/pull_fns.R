library(rvest)
library(tidyverse)

x <- read_html("https://www.fns.usda.gov/disaster/pandemic/covid-19/alaska")

field_contents_links <-  x |> 
  html_elements(".field-contents") |> 
  html_elements("p a")

tibble(
  text = html_text2(field_contents_links),
  link = html_attr(field_contents_links, "href")
)

remotes::install_github("paleolimbot/rbbt")
