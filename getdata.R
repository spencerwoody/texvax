
library(dplyr)
library(readxl)
library(httr)
library(lubridate)

URL <- "https://www.dshs.texas.gov/immunize/covid19/COVID-19-Vaccine-Data-by-County.xls"

myfilename <- paste0(here::here(), "/data/", today(), " COVID-19 Vaccine Data by County.xlsx")

download.file(URL, destfile = myfilename)


URL_zip <- "https://dshs.texas.gov/coronavirus/TexasCOVID19VaccinesbyZIP.xlsx"

myfilename_zip <- paste0(here::here(), "/data_zip/", today(), " COVID-19 Vaccine Data by ZIP.xlsx")

download.file(URL_zip, destfile = myfilename_zip)
