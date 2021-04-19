
library(tidyverse)
library(lubridate)
library(readxl)
library(cowplot)
library(tidycensus)
census_api_key("3deb7c3e77d1747cf53071c077e276d05aa31407", install = TRUE, overwrite = TRUE)
library(rmapzen)
library(sf)
mz_set_tile_host_nextzen(key = ("hxNDKuWbRgetjkLAf_7MUQ"))

theme_set(theme_minimal_grid())

###############################################################################
                                        #             Cross walks             #
###############################################################################

## County <- MSA 
msa <- read_csv("census/msainfo.csv")

msa_fips <- msa %>%
  distinct(msa, county, state, fips) %>%
  glimpse()

## CBG -> ZCTA crosswalk
zip_walk <- read_csv("census/ZCTA_CBG_MASTER_9_25_2020.csv") %>%
  mutate(GEOID10  = str_pad(as.character(GEOID10), 5, "left", "0"),
         ZIP_CODE  = str_pad(as.character(ZIP_CODE), 5, "left", "0"),
         county_fips = str_sub(cbg, 1, 5),
         tract_fips = str_sub(cbg, 1, 11)) 

## ZIP (residential) -> ZCTA
zz <- zip_walk %>% select(ZCTA = GEOID10, ZIP_CODE) %>% distinct()

## ZIP (residential) -> tract
zt <- zip_walk %>% select(FIPS = tract_fips, ZIP_CODE) %>% distinct()

## Tract -> ZCTA
tz <- zip_walk %>% select(FIPS = tract_fips, ZCTA = GEOID10) %>% distinct()

## ZCTA <- county
county <- read_csv("census/counties_basicdata.csv") %>% glimpse()

zip_walk2 <- zip_walk %>%
  distinct(GEOID10, county_fips, PO_NAME, STATE) %>%
  left_join(msa_fips, by = c("county_fips" = "fips"))

## SVI
svi <- read_csv("census/svi_per_zip_TX.csv") %>%
  mutate(ZIP = as.character(ZIP))

glimpse(svi)

svi2 <- svi %>%
  rename(ZIP_CODE = ZIP) 

zz2 <- zz %>%
  left_join(svi2 %>%
            select(ZIP_CODE, SVI),
            by = c("ZCTA" = "ZIP_CODE")) %>%
  filter(!is.na(SVI))

zz3 <- zz %>%
  left_join(svi2 %>%
            select(ZIP_CODE, SVI)## ,
            ## by = c("ZCTA" = "ZIP_CODE")
            ) %>%
  filter(!is.na(SVI))

txsvi <- read_csv("census/Texas-SVI.csv") %>%
  mutate(ST = as.character(ST),
         FIPS = as.character(FIPS)) %>%
  mutate(STCNTY = as.character(STCNTY)) %>% 
  glimpse()

###############################################################################
                                        #    Create SVI data for ZCTA level   #
###############################################################################

tr <- txsvi %>%
  ## filter(COUNTY=="Travis") %>%
  select(ST, STATE, ST_ABBR, STCNTY, COUNTY, FIPS, LOCATION,
         AREA_SQMI, E_TOTPOP, M_TOTPOP,
         contains("RPL_THEME"),
         EP_LIMENG, EP_MINRTY, F_MINRTY, EP_CROWD, EP_POV, F_POV
         ) %>%
  glimpse()
  
trz <- tr %>%
  left_join(tz)

sum(is.na(trz$ZCTA))

trz %>% filter(is.na(ZCTA)) %>% pull(COUNTY)
trz %>% filter(is.na(ZCTA)) %>% glimpse()

glimpse(trz)

?weighted.mean

## trz_summary <- trz %>%
##   group_by(COUNTY, ZCTA) %>%
##   summarize(n = n(),
##             pop = sum(E_TOTPOP),
##             popweight = E_TOTPOP / pop,
##             RPL_THEME1 = weighted.mean(RPL_THEME1),
##             RPL_THEME2 = sum(RPL_THEME2 * popweight),)

trz_weights <- trz_summary <- trz %>%
  group_by(ZCTA) %>%
  summarize(n = n(),
            pop = sum(E_TOTPOP),
            popweight = E_TOTPOP / pop) %>%
  glimpse()

trz_summary <- trz %>%
  filter(!is.na(ZCTA)) %>% 
  left_join(trz_weights) %>%
  ## glimpse() %>% 
  group_by(COUNTY, ZCTA) %>%
  summarize(across(where(is.numeric), ~ weighted.mean(.x, w = popweight)))

  

trz_summary %>% filter(ZCTA == "78725")
trz_summary %>% filter(ZCTA == "78660")
  

trz_summary %>% apply(2, function(x) sum(is.na(x)))

trz_summary %>% filter(is.na(AREA_SQMI))

trz_summary %>% glimpse()

svi <- read_csv("census/svi_per_zip_TX.csv")

glimpse(svi)

svi$ZIP

travis_zips <- trz %>% filter(COUNTY=="Travis") %>% distinct(COUNTY, ZCTA) %>% pull(ZCTA)


acs_wide <- get_acs(geography = "zcta", variables = "B01001_001",
                    state = "TX", geometry = TRUE, cb=F)

acs_wide %>% glimpse()

trz_summary$ZCTA %>% length()
