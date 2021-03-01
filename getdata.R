
library(dplyr)
library(readxl)
library(httr)
library(lubridate)

URL <- "https://www.dshs.texas.gov/immunize/covid19/COVID-19-Vaccine-Data-by-County.xls"

myfilename <- paste0(here::here(), "/data/", now(), " COVID-19 Vaccine Data by County.xlsx")

download.file(URL, destfile = myfilename)
