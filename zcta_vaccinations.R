
library(tidyverse)
library(lubridate)
library(readxl)
library(cowplot)
library(tidycensus)
## census_api_key("3deb7c3e77d1747cf53071c077e276d05aa31407", install = TRUE, overwrite = TRUE)
library(rmapzen)
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
         county_fips = str_sub(cbg, 1, 5)) 

## ZIP (residential) -> ZCTA
zz <- zip_walk %>% select(ZCTA = GEOID10, ZIP_CODE) %>% distinct()

## ZCTA <- county
county <- read_csv("census/counties_basicdata.csv") %>% glimpse()

zip_walk2 <- zip_walk %>%
  distinct(GEOID10, county_fips, PO_NAME, STATE) %>%
  left_join(msa_fips, by = c("county_fips" = "fips"))

###############################################################################
                                        #   State- and county-level vax data  #
###############################################################################

state_files_full <- dir("data", full.names = TRUE)

mystatefile <- state_files_full[length(state_files_full)]
mystatefile

state_data <- read_xlsx(mystatefile, sheet = "By County")
state_data[1, ] %>% as.data.frame()

## State-wide average
as.numeric(state_data[1, "People Fully Vaccinated"])/as.numeric(state_data[1, "Population, 16+"])
as.numeric(state_data[1, "People Vaccinated with at least One Dose"])/as.numeric(state_data[1, "Population, 16+"])


state_average <- as.numeric(state_data[1, "People Vaccinated with at least One Dose"])/as.numeric(state_data[1, "Population, 16+"])
state_average

###############################################################################
                                        #      Load in ZIP-level vax data     #
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

## Most recent data
zip_data <- read_xlsx(files_full[length(files_full)], sheet = "By Zip Code")

zip_data %>% glimpse()

## Change column names
colnames(zip_data) <- c("ZIP_CODE", "doses_administered", "one_dose", "fully_vaccinated")

## Convert to numeric...
zip_data <- zip_data %>%
  mutate(doses_administered = as.numeric(doses_administered),
         one_dose = as.numeric(one_dose),
         fully_vaccinated = as.numeric(fully_vaccinated))

## Add in the ZCTA (joined by residential ZIP)
zip_data2 <- zip_data %>%
  left_join(zz)

## Sum vax data across ZCTAs
zip_data3 <- zip_data2 %>%
  ## glimpse() %>% 
  filter(!is.na(ZCTA)) %>%
  group_by(ZCTA) %>%
  summarize(
    doses_administered = sum(doses_administered, na.rm=T),
    one_dose = sum(one_dose, na.rm=T),
    fully_vaccinated = sum(fully_vaccinated, na.rm=T)
  ) %>%
  glimpse()

###############################################################################
                                        #          Pull the ACS data          #
###############################################################################

acs_vars <- load_variables("2019", "acs5")

acs_vars %>% glimpse()

acs_vars %>% distinct(concept)

## write_csv(acs_vars, "acsvars.csv")

## Population data by age
myvars <- c(
  "B01001_001",
  "B01001_003",
  "B01001_004",
  "B01001_005",
  "B01001_006",
  "B01001_027",
  "B01001_028",
  "B01001_029",
  "B01001_030")

acs_vars %>% filter(name %in% myvars)

## acs <- get_acs(geography = "zcta", variables = myvars,
##                 state = "TX", geometry = TRUE)

## Get the ACS data at the ZCTA level 
acs_wide <- get_acs(geography = "zcta", variables = myvars,
                    state = "TX", geometry = TRUE, output = "wide")

glimpse(acs_wide)

## Check for coverage of ZIP codes 
mean(acs_wide$GEOID %in% zip_data$ZIP_CODE)
mean(acs_wide$GEOID %in% zip_data3$ZCTA)
mean(zip_data3$ZCTA %in% acs_wide$GEOID)

## Sum up under-18 population
acs_wide2 <- acs_wide %>%
  mutate(under18 = rowSums(across(contains(myvars[-1]) & !contains("M"))),
         adult_pop = B01001_001E - under18,
         adult_frac = adult_pop/B01001_001E) %>%
  glimpse()

acs_wide2 %>%
  filter(GEOID %in% zip_data3$ZCTA) %>% 
  ggplot() +
  geom_sf(aes(fill=adult_frac,col=adult_frac)) +
  scale_fill_viridis_c() + 
  scale_color_viridis_c()

###############################################################################
                                        #        Join ACS and vax data        #
###############################################################################

## Join 
vax <- acs_wide2 %>%
  left_join(zip_data3, by = c("GEOID" = "ZCTA")) %>%
  filter(adult_pop > 0) %>% 
  mutate(coverage = one_dose / (adult_pop+1)) %>% 
  left_join(zip_walk2 %>%
            ## distinct(GEOID10, PO_NAME, county_fips, county) %>% 
            ## select(GEOID10, S)
            mutate(GEOID10 = as.character(GEOID10)),
            by = c("GEOID" = "GEOID10")) %>%
  glimpse()

vax %>% filter(PO_NAME == "Houston") %>% filter(coverage > 1)

myplot <- vax %>%
  ## filter(county == "Harris") %>% 
  ## filter(PO_NAME == "Houston") %>%
  filter(PO_NAME == "Austin") %>%
  ## filter(coverage<1) %>%
  ## filter(GEOID %in% myzips) %>% 
  ## filter(coverage!=max(coverage)) %>%
  glimpse() %>% 
  ggplot() +
  geom_sf(aes(fill=coverage), size=0.1) +
  scale_fill_viridis_c() + 
  scale_color_viridis_c() +
  ## scale_fill_gradient2(midpoint=state_average) +
  labs(title = "Viridis") + 
  NULL

## myplot

myplot2 <- vax %>%
  ## filter(county == "Harris") %>% 
  ## filter(PO_NAME == "Houston") %>%
  filter(PO_NAME == "Austin") %>%
  ## filter(coverage<1) %>%
  ## filter(GEOID %in% myzips) %>% 
  ## filter(coverage!=max(coverage)) %>%
  glimpse() %>% 
  ggplot() +
  geom_sf(aes(fill=coverage), size=0.1) +
  ## scale_fill_viridis_c() + 
  ## scale_color_viridis_c() +
  scale_fill_gradient2(midpoint=state_average) + 
  NULL +
  labs(title = "Relative to state-average (white)")

## myplot2

## Export certain columns
vax_small <- vax %>%
  mutate(state_average = state_average) %>% 
  select(ZCTA = GEOID,
         PO_NAME,
         county,
         msa,
         state,
         total_pop = B01001_001E,
         adult_pop,
         under18_pop = under18,
         doses_administered,
         one_dose,
         fully_vaccinated,
         coverage_one_dose = coverage,
         state_average_coverage_one_dose = state_average)

glimpse(vax_small)

write_csv(vax_small, sprintf("map_data/%s zip_data_processed.csv", today()))



## Leaflet map
library(mapview)

vax_small2 <- vax_small %>% filter(PO_NAME == "Austin") %>%
  mutate(coverage_one_dose = round(coverage_one_dose * 100, 0))

mapview::mapview(vax_small2, zcol = "coverage_one_dose", at = seq(0, 70, by=10))


