
library(tidyverse)
library(lubridate)
library(readxl)
library(cowplot)
library(tidycensus)
## census_api_key("3deb7c3e77d1747cf53071c077e276d05aa31407", install = TRUE, overwrite = TRUE)
library(rmapzen)
library(sf)
mz_set_tile_host_nextzen(key = ("hxNDKuWbRgetjkLAf_7MUQ"))
library(ggiraph)

theme_set(theme_minimal_grid())

options(tigris_use_cache = TRUE)

## Diverging color scales
mycols1 <- c("#8c510a", "#01665e", "#f5f5f5")
mycols2 <- c("#c51b7d", "#4d9221", "#f7f7f7")
mycols3 <- c("#b2182b", "#2166ac", "#f7f7f7")
mycols4 <- c("#d73027", "#4575b4", "#ffffbf")
mycols5 <- c("#b2182b", "#4d4d4d", "#ffffff")
mycols6 <- c("#b35806", "#542788", "#f7f7f7")

source("R/clean_zip_data.R")
source("R/get_acs_zip.R")

tsa <- read_csv("census/tsa.csv")
tsa <- tsa %>%
  mutate(county_split = str_split(county, ",")) %>%
  unnest(county_split) %>%
  distinct(tsa_letter, county_split) %>%
  rename(tsa = tsa_letter, county = county_split)

glimpse(tsa)

###############################################################################
                                        #            ZIP-level data           #
###############################################################################

## File names (with and without the "data/" directory)
files_full <- dir("data_zip", full.names = TRUE)
files <- dir("data_zip", full.names = FALSE)

## Dates for each of the files
dates <- files %>% str_sub(1, 10) %>% ymd()

## Names of the sheets of each Excel file
sheet_names <- files_full %>% lapply(excel_sheets)

names(sheet_names) <- dates

sheet_names

## Map
myzz <- read_csv("census/Zip_to_zcta_crosswalk_2020.csv") 

glimpse(myzz)

## Read in data by date

zip_df_list <- vector("list", length(files))

for (k in 1:length(files)) {
  if (k %% 5 == 0) cat(sprintf("Loading file %i out of %i...\n", k, length(files)))
  zip_df_list[[k]] <- clean_zip_data(files_full[k], dates[k], myzz)
}

zip_df <- zip_df_list %>%
  plyr::rbind.fill() %>%
  select(date, everything()) %>% 
  arrange(date, ZCTA)

zip_df <- zip_df %>%
  group_by(ZCTA) %>%
  mutate(one_dose_new = one_dose - lag(one_dose)) %>%
  ## mutate(one_dose_new_av7 =
  ##          stats::filter(
  ##                   one_dose_new, rep(1/7, 7), sides=2
  ##                 )) %>% 
  ungroup()

glimpse(zip_df)

## Most recent data
zip_df_latest <- zip_df %>% filter(date == max(date))

###############################################################################
                                        #             Get ACS data            #
###############################################################################

acs_wide2 <- get_acs_zip()

###############################################################################
                                        #           ZIP, county, tsa          #
###############################################################################

ziponly <- zip_df_latest %>%
  select(ZCTA)

## ZCTA -> county crosswalk 
zc <- read_csv("census/zcta_county_rel_10.txt")

zc <- zc %>%
  mutate(STATE = as.character(STATE)) %>%
  mutate(STATE = str_pad(STATE, 2, "left", "0"))

## Only look at ZCTAs in this dataset
zc_sub <- zc %>%
  filter(STATE == "48") %>% 
  rename(ZCTA = ZCTA5) %>% 
  filter(ZCTA %in% zip_df$ZCTA)

## How many of vax data ZIP codes are in this crosswalk?
mean(ziponly$ZCTA %in% zc_sub$ZCTA)

ziponly %>% filter(!(ZCTA %in% zc_sub$ZCTA))

## Map the ZCTA to the county with the majority most residents
zc_sub_summary  <- zc_sub %>%
  group_by(ZCTA) %>%
  summarize(state_fips = STATE[1],
            county_fips = COUNTY[which.max(ZPOPPCT)],
            ZPOPPCT = max(ZPOPPCT)) %>%
  mutate(county_fips = str_c(state_fips, county_fips))

zc_sub_summary %>% arrange(ZPOPPCT) %>% glimpse()

## Most of these county assignments capture a majority of the ZCTA population
mean(zc_sub_summary$ZPOPPCT > 50)
mean(zc_sub_summary$ZPOPPCT > 80)

## Map of county fips
countyfips <- read_csv("census/all-geocodes-v2019.csv")

glimpse(countyfips)

colnames(countyfips) <- colnames(countyfips) %>%
  tolower() %>% 
  str_replace_all(" ", "_") %>%
  str_replace_all("\\(", "_") %>% 
  str_replace_all("\\)", "") %>%
  str_replace_all("/", "_") %>%
  str_replace_all("__", "_")

countyfips_sub <- countyfips %>%
  mutate(state_code_fips = as.character(state_code_fips)) %>% 
  select(state_fips = state_code_fips,
         county_fips = county_code_fips,
         area_name = area_name_including_legal_statistical_area_description) %>%
  mutate(county_fips = str_c(state_fips, county_fips))

countyfips_sub %>% glimpse()

## Combine ZCTA/County crosswalk
zc_walk <- zc_sub_summary %>%
  left_join(countyfips_sub) %>%
  mutate(county = str_remove(area_name, " County"))

zc_walk %>% glimpse()

## Add in TSA assignments
zc_walk <- zc_walk %>%
  left_join(tsa)

glimpse(zc_walk)

zc_walk$tsa %>% is.na() %>% any()

###############################################################################
                                        #           Combine ZIP data          #
###############################################################################

vote <- read_csv("census/texas-zip-vote-share.csv") %>%
  select(ZCTA = GEOID, dem_share) %>%
  mutate(ZCTA = as.character(ZCTA))

zzsvi <- read_csv("census/zcta-svi-woody.csv") %>%
  rename(SVI = RPL_THEMES) %>%
  mutate(ZCTA = as.character(ZCTA))

msa <- read_csv("census/msainfo.csv")

vax_all <- zip_df %>%
  left_join(acs_wide2 %>%
            rename(ZCTA = GEOID) %>%
            mutate(total_pop = B01001_001E)) %>%
  left_join(zzsvi) %>%
  left_join(vote) %>%
  mutate(coverage = one_dose / (adult_pop+1),
         coverage_total_pop = one_dose / (total_pop+1),
         coverage_under18 = one_dose / (B01001_001E - under18 +1)) %>%
  left_join(zc_walk %>%
            select(-ZPOPPCT, -area_name)) %>% 
  ## left_join(msa %>%
  ##           filter(state == "Texas") %>% 
  ##           rename(COUNTY = county) %>%
  ##           select(msa, state, COUNTY)) %>%
  left_join(msa %>%
            filter(state == "Texas") %>% 
            ## rename(COUNTY = county) %>%
            select(msa, state, county)) %>%
  filter(ZCTA %in% zip_df_latest$ZCTA) %>% 
  select(everything(), geometry)

io <- vax_all %>%
  ## filter(state == "Texas") %>%
  select(date,
         ZCTA,
         PO_NAME,
         county = COUNTY,
         county_fips,
         tsa,
         msa,
         state,
         total_pop = B01001_001E,
         adult_pop,
         adult_pop_65plus = plus65,
         senior_frac,
         under16_pop = under16,
         doses_administered,
         one_dose,
         fully_vaccinated,
         coverage_one_dose_16plus = coverage,
         coverage_one_dose_total_pop = coverage_total_pop,
         dem_share,
         ## dem_share_2party_pres_vote,
         ## state_average_coverage_one_dose = state_average,
         contains("phase"),
         median_household_income = median_incomeE,
         SVI,
         contains("RPL"),
         geometry)

io <- io %>% mutate_if(is.numeric, function(x) round(x, 4)) %>% glimpse()

## Check hostname

myhostname <- system("hostname",intern=T)

if (!(myhostname == "t480s" | str_detect(myhostname, "CNS"))) {
  io %>%
    as.data.frame() %>%
    select(-geometry) %>%
    write_csv(sprintf("map_data/%s_zip_data_processed.csv", today()))
}

## io %>%
##   as.data.frame() %>%
##   select(-geometry) %>%
##   write_csv(sprintf("map_data/%s_zip_data_processed.csv", today()))
