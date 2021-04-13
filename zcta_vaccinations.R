

library(tidyverse)
library(lubridate)
library(readxl)
library(cowplot)
library(tidycensus)
## census_api_key("3deb7c3e77d1747cf53071c077e276d05aa31407", install = TRUE, overwrite = TRUE)
library(rmapzen)
library(sf)
mz_set_tile_host_nextzen(key = ("hxNDKuWbRgetjkLAf_7MUQ"))

theme_set(theme_minimal_grid())

options(tigris_use_cache = TRUE)

mycols1 <- c("#8c510a", "#01665e", "#f5f5f5")
mycols2 <- c("#c51b7d", "#4d9221", "#f7f7f7")
mycols3 <- c("#b2182b", "#2166ac", "#f7f7f7")
mycols4 <- c("#d73027", "#4575b4", "#ffffbf")
mycols5 <- c("#b2182b", "#4d4d4d", "#ffffff")
mycols6 <- c("#b35806", "#542788", "#f7f7f7")

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

###############################################################################
                                        #   State- and county-level vax data  #
###############################################################################

state_files_full <- dir("data", full.names = TRUE)

mystatefile <- state_files_full[length(state_files_full)]
mystatefile

file_date <- mystatefile %>% str_sub(6, 15) %>% ymd()

as_of <- lubridate::stamp("Mar 1, 2021")(file_date-1)

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
  left_join(zz) %>%
  glimpse()

28413

## Unknowns
zip_data2 %>% filter((ZIP_CODE %in% c("Invalid/Unknown"))) %>%  glimpse() %>% pull(one_dose) %>% na.omit() %>% sum()
zip_data2 %>% filter((ZIP_CODE %in% c("Out of State"))) %>%  glimpse() %>% pull(one_dose) %>% na.omit() %>% sum()
zip_data2 %>% filter(!(ZIP_CODE %in% c("Invalid/Unknown", "Out of State"))) %>%  pull(one_dose) %>% na.omit() %>% sum()

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

## Sum vax data across ZCTAs
## zip_data3 <- zip_data2 %>%
##   ## glimpse() %>% 
##   filter(!is.na(ZCTA)) %>%
##   group_by(ZCTA, SVI) %>%
##   summarize(
##     doses_administered = sum(doses_administered, na.rm=T),
##     one_dose = sum(one_dose, na.rm=T),
##     fully_vaccinated = sum(fully_vaccinated, na.rm=T)
##   ) %>%
##   glimpse()

###############################################################################
                                        #          Pull the ACS data          #
###############################################################################

## Check for ACS vars

if (dir() %>% str_detect("acs2019_variables.csv") %>% any()) {
  acs_vars <- read_csv("acs2019_variables.csv")
} else {
  acs_vars <- load_variables("2019", "acs5")
  write_csv(acs_vars, "acs2019_variables.csv")  
}

acs_vars %>% glimpse()

acs_vars %>% distinct(concept)

## Population data by age
myvars <- c(
  "B01001_001",
  "B01001_003",
  "B01001_004",
  "B01001_005",
  "B01001_006", ## male 15-17
  "B01001_027",
  "B01001_028",
  "B01001_029",
  "B01001_030", ## female 15-17
  "B01002_001",
  "B19013_001")

## Seniors
sr <- acs_vars %>% slice(20:25, 44:49) %>% glimpse() %>% pull(name)


## acs_vars %>%
##   mutate(ROWNUM = row_number()) %>% 
##   filter(concept %>% str_detect("SEX BY AGE"),
##          label %>% str_detect("!Male:", negate=TRUE)) %>%
##          View()

acs_vars %>% filter(name %in% myvars)
acs_vars %>% filter(name %in% myvars[c(5, 9)])

## acs <- get_acs(geography = "zcta", variables = myvars,
##                 state = "TX", geometry = TRUE)

## acs_wide18 <- get_acs(geography = "zcta", variables = myvars,
##                       state = "TX", geometry = TRUE, output = "wide", year=2018)

## Get ACS data 
acs_wide <- get_acs(geography = "zcta", variables = myvars,
                    state = "TX", geometry = TRUE, output = "wide")

acs_wide_sr <- get_acs(geography = "zcta", variables = sr,
                    state = "TX", geometry = TRUE, output = "wide")

## Check that ZIP coverage is the same
acs_wide %>% nrow()
acs_wide_sr %>% nrow()

## Caclulate total senior population
acs_wide_sr <- acs_wide_sr %>%
  mutate(plus65 = rowSums(across(contains("B") & contains("E")))) %>%
  glimpse()

## Rename income variable
acs_wide <- acs_wide %>%
  rename(
    median_incomeE = B19013_001E,
    median_incomeM = B19013_001M,
  )

## Check for coverage of ZIP codes 
mean(acs_wide$GEOID %in% zip_data$ZIP_CODE)
mean(acs_wide$GEOID %in% zip_data3$ZCTA)
mean(zip_data3$ZCTA %in% acs_wide$GEOID)

## Sum up under-18 population
acs_wide2 <- acs_wide %>%
  ## mutate(B01001_030E = B01001_030)
  mutate(fifteen_only = round(1/3 * B01001_006E + 1/3 * B01001_030E)) %>% 
  mutate(
    under18 = rowSums(across(## contains(myvars[-c(1, 5, 9)]) &
      ## contains(myvars[-c(1, 5, 9)]) &
      contains(paste0(myvars[-c(1)], "E")) &
      ## contains("fifteen") & 
      !contains("M") &
      !contains("B01002") &
      !contains("B19013") &
      !contains("income"))),
    under16 = rowSums(across(## contains(myvars[-c(1, 5, 9)]) &
      ## contains(myvars[-c(1, 5, 9)]) &
      (contains(paste0(myvars[-c(1, 5, 9)], "E")) |
      contains("fifteen")) & 
      !contains("M") &
      !contains("B01002") &
      !contains("B19013") &
      !contains("income"))),
    ## adult_pop = B01001_001E - under18,
    adult_pop = B01001_001E - under16,
    adult_frac = adult_pop/B01001_001E) %>%
  left_join(acs_wide_sr %>%
            as.data.frame() %>% 
            select(GEOID, plus65)) %>%
  mutate(adults_16_64 = adult_pop - plus65,
         adults_65_plus = plus65) %>% 
  mutate(senior_frac = plus65 / adult_pop,
         nonsenior_frac = adults_16_64 / (adult_pop) ) %>% 
  glimpse()

## acs_wide2 %>%
##   filter(GEOID %in% zip_data3$ZCTA) %>% 
##   ggplot() +
##   geom_sf(aes(fill=adult_frac,col=adult_frac)) +
##   scale_fill_viridis_c() + 
##   scale_color_viridis_c()

###############################################################################
                                        #        Join ACS and vax data        #
###############################################################################

## Join 
vax <- acs_wide2 %>%
  left_join(zip_data3, by = c("GEOID" = "ZCTA")) %>%
  left_join(zz2 %>% distinct(ZCTA, SVI), by = c("GEOID" = "ZCTA")) %>%
  filter(adult_pop > 0) %>% 
  mutate(coverage = one_dose / (adult_pop+1),
         coverage_under18 = one_dose / (B01001_001E - under18 +1)) %>% 
  left_join(zip_walk2 %>%
            filter(!is.na(msa)) %>%
            distinct(GEOID10, msa, .keep_all = TRUE) %>% 
            ## distinct(GEOID10, PO_NAME, county_fips, county) %>% 
            ## select(GEOID10, S)
            mutate(GEOID10 = as.character(GEOID10)),
            by = c("GEOID" = "GEOID10")) %>%
  glimpse()

## Certain places have coverage >1, mostly because of small adult
## populations...
vax %>% filter(coverage > 1) %>% arrange(adult_pop)

## 77030 is the ZIP code for the Texas Medical Center in Houston
## See: https://goo.gl/maps/Us7pcbRVC13n6PjJA
vax %>% filter(PO_NAME == "Houston") %>% filter(coverage > 1)

###############################################################################
                                        #     Join in eligibility numbers     #
###############################################################################

eli <- read_csv("census/tx-zip-codes-vaccine-eligibility-groups.csv")

## Round down eligible population
eli <- eli %>%
  mutate(ZCTA5 = as.character(ZCTA5)) %>%
  mutate_if(is.double, floor) %>%
  glimpse()

## Rename columns
colnames(eli)[-1] <- paste0("phase_", colnames(eli)[-1])

colnames(eli) <- colnames(eli) %>% str_replace("\\+", "plus")

glimpse(eli)

## Attack rate estimates
attack <- read_csv("census/zip_attack_rates_20210329.csv") %>%
  mutate(ZIP = as.character(ZIP))

glimpse(attack)

## Join vaccine data with eligible population
vax <- vax %>%
  left_join(eli %>%
            select(ZCTA5, contains("total")),
            by = c("GEOID" = "ZCTA5")) %>%
  ## left_join(attack,
  ##           by = c("GEOID" = "ZIP")) %>% 
  glimpse()

## Join in attack rates (for Austin area only...)
vax_atx <- vax %>%
  left_join(attack,
            by = c("GEOID" = "ZIP"))

###############################################################################
                                        #           Export ZIP data           #
###############################################################################

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
         state_average_coverage_one_dose = state_average,
         contains("phase"),
         SVI)

glimpse(vax_small)

write_csv(vax_small, sprintf("map_data/%s zip_data_processed.csv", today()))

###############################################################################
                                        #       Get geometry for Austin       #
###############################################################################

## GET ROADS MSA
get_vector_tiles <- function(bbox){
  mz_box=mz_rect(bbox$xmin,bbox$ymin,bbox$xmax,bbox$ymax)
  mz_vector_tiles(mz_box)
}

zip_geom <- vax_sub2
zcta_geom <- st_union(zip_geom$geometry)

load("census/roads.Rdata")

I35_2 <- st_crop(I35, zcta_geom)
US183_2 <- st_crop(US183, zcta_geom)
missing_183_2 <- st_crop(missing_183, zcta_geom)


###############################################################################
                                        #        Make plots for Austin        #
###############################################################################

mycity <- "Austin"

## City averages
city_avg <- vax %>%
  left_join(attack,
            by = c("GEOID" = "ZIP")) %>% 
  filter(PO_NAME == mycity) %>%
  filter(!is.na(attack_rate_mean)) %>% 
  mutate(infected = B01001_001E * attack_rate_mean) %>%
  glimpse() %>% 
  summarize(SVI = mean(SVI),
            total_pop = sum(B01001_001E),
            total_infected = sum(infected),
            infection_rate = total_infected / total_pop,
            adult_pop = sum(adult_pop),
            one_dose = sum(one_dose),
            senior_frac = sum(plus65)/adult_pop,
            nonsenior_frac=sum(adults_16_64)/adult_pop) %>%
  mutate(coverage = one_dose / adult_pop) %>%
  glimpse()


## Subset a city...
vax_sub <- vax %>%
  filter(PO_NAME == mycity)

## Vaccine coverage by income
vax_sub %>%
  ggplot() +
  geom_point(aes(log(median_incomeE), coverage, col = B01002_001E), size=3, alpha=0.8) +
  scale_color_viridis_c("Median age")

## Vaccine coverage by senior fraction
vax_sub %>%
  ggplot() +
  geom_point(aes(senior_frac, coverage, col = SVI), size=3) +
  ## scale_color_viridis_c("Social vulnerability") +
  scale_color_distiller("Social vulnerability", palette="Spectral") +
  scale_y_continuous(labels=scales::label_percent()) +
  scale_x_continuous(labels=scales::label_percent()) +
  labs(x="Percentage of senior residents", y = "Vaccine coverage") +
  guides(color = guide_colorsteps(ticks=TRUE,barwidth = 12, barheight = 0.5)) +
  theme_dark(base_size=16) +
  theme(legend.position = "bottom")

vax_sub %>%
  ggplot() +
  geom_point(aes(SVI, coverage, col = senior_frac), size=3, alpha=0.8) +
  scale_color_viridis_c("Percentage of senior residents", labels=scales::label_percent()) +
  scale_y_continuous(labels=scales::label_percent()) +
  ## scale_x_continuous(labels=scales::label_percent()) + 
  labs(x="Social vulnerability")


## Burden estimates (for Austin)
burden <- read_csv("census/zip_class_new.csv") %>%
  rename(GEOID = ZIP, burden = group) %>%
  mutate(GEOID = as.character(GEOID)) %>% 
  glimpse()

## Join in burden
vax_sub2 <- vax_sub %>%
  left_join(burden) %>%
  left_join(attack,
            by = c("GEOID" = "ZIP")) %>%
  glimpse()

vax_sub2 <- vax_sub2 %>%
  mutate(burden = ifelse(is.na(burden), "low burden", burden))

myplot_highlow <- vax_sub2 %>%
  ## filter(county == "Harris") %>% 
  ## filter(PO_NAME == "Houston") %>%
  filter(PO_NAME == "Austin") %>%
  ## filter(coverage<1) %>%
  ## filter(GEOID %in% myzips) %>% 
  ## filter(coverage!=max(coverage)) %>%
  glimpse() %>% 
  ggplot() +
  geom_sf(aes(fill=burden, label=GEOID## , col=burden
              ), size=0.1) +
  geom_sf(data = I35_2,
          col = "grey10") +
  geom_sf(data = missing_183_2,
          col = "grey10") +
  geom_sf(data = US183_2,
          col = "grey10") +
  ## scale_fill_manual(values=c("purple", "green")) +
  scale_fill_brewer("", palette = "Accent", direction = -1) +
  theme_map() +
  ## annotate(geom="text", x=-97.8, y=30.5, label="US 183", size=5)+
  ## annotate(geom="text", x=-97.68, y=30.5, label="I-35", size=5) + 
  theme(legend.position = "top",
        legend.title = element_text(face = "bold"))

myplot_highlow

ggsave("~/Desktop/burden_map.png")

vax_sub2 %>%
  gg

vax_sub2 %>%
  glimpse() %>% 
  ## filter(coverage>0.2) %>%
  filter(!is.na(SVI)) %>% 
  mutate(SVI_cat = ifelse(SVI < mean(SVI), "low", "high")) %>% 
## group_by(burden) %>%
  group_by(burden) %>% 
  summarize(coverage_mean_adult = sum(one_dose)/sum(adult_pop),
            coverage_mean_total = sum(one_dose)/sum(B01001_001E),
            attack_rate = mean(attack_rate_mean),
            attack_rate_min = min(attack_rate_mean),
            attack_rate_max = max(attack_rate_mean)) %>% glimpse()

myscatter <- vax_sub2 %>%
  ggplot() +
  geom_smooth(aes(SVI, coverage)) +
  geom_point(aes(SVI, coverage,
                 col = attack_rate_mean## , col=B01002_001E
                 ## , col = burden
                 ),
             size = 2.5## , alpha=0.5
             ) +
  ## scale_color_distiller(palette = "Spectral") +
  scale_color_viridis_c("Cumulative infections", option="C", labels=scales::percent) +
  scale_y_continuous(labels=scales::percent) + 
  labs(title = "Vaccine coverage vs. social vulnerability in Austin", 
       y = "Vaccine coverage (% of adult population with at >=1 dose)",
       x = "Social vulnerability index (SVI; higher is more vulnerable)") 

myscatter

ggsave(sprintf("figures/austin/png/%s-vaccine_svi_scatter.png", today()), myscatter,
       width=6.5, height = 6.25, units = "in")
ggsave(sprintf("figures/austin/pdf/%s-vaccine_svi_scatter.pdf", today()), myscatter,
       width=6.5, height = 6.25, units = "in")

vax_sub2 %>% glimpse()

vax_sub2$SVI %>% hist()
vax_sub2$SVI %>% summary()

mynum1 <- vax_sub2 %>%
  filter(SVI <= 0.10962) %>%
  pull(coverage) %>%
  mean()

mynum2 <- vax_sub2 %>%
  filter(SVI >= 0.50333) %>%
  pull(coverage) %>%
  mean()

mynum1
mynum2
mynum1-mynum2

vax_sub2()

vax_sub2$coverage %>% hist()
vax_sub2$coverage %>% summary()

0.50333 - 0.10962

mydfsub <- vax_sub2 %>%
  as.data.frame() %>% 
  select(attack_rate_mean, vaccine_coverage=coverage, SVI, senior_frac) %>%
  ## select(-geometry) %>% 
  as.data.frame() %>%
  glimpse()## %>%
  ## as.matrix()

GGally::ggpairs(mydfsub)

GGally::ggpairs(mydfsub) + theme_grey(base_size=16)

mydfsub <- mydfsub[, -4]

pairs(mydfsub[-is.na(mydfsub[, 1])])

mydfsub[-is.na(mydfsub[, 1])]

source("zcta_features.R")

myscatter2 <- vax_sub2 %>%
  ggplot() +
  geom_smooth(aes(attack_rate_mean, coverage),method="lm") +
  geom_point(aes(attack_rate_mean, coverage,
                 col = SVI## , col=B01002_001E
                 ## , col = burden
                 ),
             size = 2.5## , alpha=0.5
             ) +
  ## scale_color_distiller(palette = "Spectral") +
  scale_color_viridis_c("Social vulnerability",
                        option="C") +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(labels=scales::percent) + 
  labs(## title = "Cumulative infections, vaccine coverage, & social vulnerability in Austin", 
       y = "Vaccine coverage",
       x = "Cumulative infections") +
  guides(color = guide_colorsteps(ticks=TRUE,barwidth = 12, barheight = 0.5)) +
  ## theme_cowplot() + 
  theme(legend.position = "bottom",
        axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"))

myscatter2

summary(lm(coverage ~ senior_frac, data = vax_sub2))
summary(lm(coverage ~ attack_rate_mean, data = vax_sub2))
summary(lm(coverage ~ SVI, data = vax_sub2))
summary(lm(coverage ~ SVI + senior_frac, data = vax_sub2))
summary(lm(coverage ~ senior_frac + SVI , data = vax_sub2))


summary(lm(coverage ~ senior_frac + SVI, data = vax_sub2))
## summary(lm(coverage ~ senior_frac * SVI, data = vax_sub2))
summary(lm(coverage ~ SVI + senior_frac + attack_rate_mean, data = vax_sub2))



mylm <- lm(coverage ~ SVI + senior_frac + attack_rate_mean, data = vax_sub2)

xtable(mylm)

plot(vax_sub2$SVI, vax_sub2$coverage)



ggsave(sprintf("figures/austin/png/%s-vaccine_svi_scatter.png", today()), myscatter2,
       width=6, height=4.5## ,
       ## width=6.75, height = 6.25, units = "in"
       )
ggsave(sprintf("figures/austin/pdf/%s-vaccine_svi_scatter.pdf", today()), myscatter2,
       width=6.75, height = 6.25, units = "in")

source("zcta_features.R")


## myplot2



myplot3 <- vax_sub2 %>%
  ## filter(county == "Harris") %>% 
  ## filter(PO_NAME == "Houston") %>%
  filter(PO_NAME == "Austin") %>%
  ## filter(coverage<1) %>%
  ## filter(GEOID %in% myzips) %>% 
  ## filter(coverage!=max(coverage)) %>%
  glimpse() %>% 
  ggplot() +
  geom_sf(aes(fill=coverage, label=GEOID## , col=burden
              ), size=0.1) +
    ## geom_sf(data = travis_roads1,
  ##         col = "grey30") +
  ## geom_sf(data = zcta_roads %>% filter(kind=="major_road"),
  ##         col = "grey20") + 
  geom_sf(data = I35_2,
          col = "grey10") +
  geom_sf(data = missing_183_2,
          col = "grey10") +
  geom_sf(data = US183_2,
          col = "grey10") +
    ## geom_sf(data = zcta_roads %>% filter(kind=="major_road"),
    ##       col = "grey50") + 
  ## scale_fill_viridis_c() + 
  ## scale_color_viridis_c() +
  ## scale_fill_gradient2(sprintf("Vaccine coverage\n(%% of adult pop. with ≥1 dose)\ncity-average = %2.1f%%", city_avg$coverage * 100),
  ##                      midpoint=city_avg$coverage, labels=scales::percent,
  ##                      high = mycols3[2], low = mycols3[1], mid = mycols3[3]
  ##                      ## , mid="grey90"
  ##                      ## , low="firebrick3",high = "dodgerblue3"
  ##                      ) +
  scale_fill_gradient2(## sprintf("Vaccine coverage"),
    "",
    ## breaks=c(0.3, round(city_avg$coverage, 2), 0.5, 0.7, 0.9),
                       midpoint=city_avg$coverage, labels=scales::label_percent(accuracy=1),
                       high = mycols3[2], low = mycols3[1], mid = mycols3[3]
                       ## , mid="grey90"
                       ## , low="firebrick3",high = "dodgerblue3"
  ) +
##       scale_fill_steps2(    "",
##                     ## breaks=c(0.3, round(city_avg$coverage, 2), 0.5, 0.7, 0.9),
##                     midpoint=city_avg$coverage,## , mid="grey90"
##                     high = mycols3[2], low = mycols3[1], mid = mycols3[3],
##                     ## , low="firebrick3",high = "dodgerblue3"
##                     ## high = mycols1[1], low = mycols1[2], mid = mycols1[3],
##                     ## labels = scales::label_percent(accuracy=1),
##                     nice.breaks=FALSE
##                        ## low="darkorange", high="magenta", mid="grey95"
## ) + 
  NULL +
  ## labs(title = "Vaccine coverage in Austin",
  ##      subtitle = sprintf("Percentage of adult population with at least one dose\nRelative to city-average (%2.1f%%)", city_avg$coverage * 100)) +
  ## guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) +
  ## labs(caption=sprintf("Up to %s\nSource: Texas DSHS", as_of))+
  labs(title="Vaccine coverage")  +
  guides(fill = guide_coloursteps(ticks=TRUE,barwidth = 12, barheight = 0.5)) +
  ## theme_cowplot() +
  theme_map() +
  ## annotate(geom="text", x=-97.8, y=30.5, label="US 183", size=5)+
  ## annotate(geom="text", x=-97.68, y=30.5, label="I-35", size=5) + 
  theme(legend.position = "top",
        legend.title = element_text(face = "bold"))

myplot3

myplot3_svi <- vax_sub2 %>%
  ## filter(county == "Harris") %>% 
  ## filter(PO_NAME == "Houston") %>%
  filter(PO_NAME == "Austin") %>%
  ## filter(coverage<1) %>%
  ## filter(GEOID %in% myzips) %>% 
  ## filter(coverage!=max(coverage)) %>%
  glimpse() %>% 
  ggplot() +
  geom_sf(aes(fill=SVI), size=0.1) +
    geom_sf(data = I35_2,
          col = "grey10") +
  geom_sf(data = missing_183_2,
          col = "grey10") +
  geom_sf(data = US183_2,
          col = "grey10") +
  ## geom_sf(data = travis_roads1,
  ##         col = "grey30") +
  ## geom_sf(data = travis_water3 %>%
  ##           filter(name %>% str_detect("Lake") |
  ##                  name %>% str_detect("River")) ## %>% 
  ##           ## mutate(Area = st_area(geometry) %>% as.numeric()) %>%
  ##           ## filter(Area > 1)
  ##        ,
  ##         aes(geometry = geometry2),
  ##         fill = "lightblue", size = 0.1, col="lightblue")+
  ## scale_fill_viridis_c() + 
  ## scale_color_viridis_c() +
  scale_fill_gradient2(## sprintf("Social vulnerability"),
    "",
                       midpoint=city_avg$SVI,## , mid="grey90"
                       ## , low="firebrick3",high = "dodgerblue3"
                                              ## high = mycols1[1], low = mycols1[2], mid = mycols1[3]
                       low="darkgreen", high="darkviolet", mid="grey95"
                       ) +
    ## scale_fill_gradient2(sprintf("Social vulnerability index\n(higher is more vulnerable)\ncity average = %1.2f", city_avg$SVI), midpoint=city_avg$SVI,## , mid="grey90"
    ##                    ## , low="firebrick3",high = "dodgerblue3"
    ##                                           high = mycols1[1], low = mycols1[2], mid = mycols1[3]
    ##                    low="darkgreen", high="darkviolet", mid="grey95"
    ##                    ) + 
  NULL +
  labs(## title = "Social vulnerability index in Austin",
    ## subtitle = sprintf("SVI by ZIP code\nRelative to city-average (%2.2f; higher is more vulnerable)", city_avg$SVI)
  ) +
  ## guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) +
  labs("\n") + 
  guides(fill = guide_coloursteps(ticks=TRUE,barwidth = 12, barheight = 0.5)) +
  labs(title ="Social vulnerability") + 
  theme_map() +
  ## annotate(geom="text", x=-97.8, y=30.5, label="US 183", size=5)+
  ## annotate(geom="text", x=-97.68, y=30.5, label="I-35", size=5) + 
  ## theme_cowplot() + 
  theme(legend.position = "top",
        legend.title = element_text(face = "bold"))

myplot3_svi

myplot3_svi_mycols <- vax_sub2 %>%
  ## filter(county == "Harris") %>% 
  ## filter(PO_NAME == "Houston") %>%
  filter(PO_NAME == "Austin") %>%
  ## filter(coverage<1) %>%
  ## filter(GEOID %in% myzips) %>% 
  ## filter(coverage!=max(coverage)) %>%
  glimpse() %>% 
  ggplot() +
  geom_sf(aes(fill=SVI), size=0.1) +
    geom_sf(data = I35_2,
          col = "grey10") +
  geom_sf(data = missing_183_2,
          col = "grey10") +
  geom_sf(data = US183_2,
          col = "grey10") +
  ## geom_sf(data = travis_roads1,
  ##         col = "grey30") +
  ## geom_sf(data = travis_water3 %>%
  ##           filter(name %>% str_detect("Lake") |
  ##                  name %>% str_detect("River")) ## %>% 
  ##           ## mutate(Area = st_area(geometry) %>% as.numeric()) %>%
  ##           ## filter(Area > 1)
  ##        ,
  ##         aes(geometry = geometry2),
  ##         fill = "lightblue", size = 0.1, col="lightblue")+
  ## scale_fill_viridis_c() + 
  ## scale_color_viridis_c() +
  scale_fill_gradient2(## sprintf("Social vulnerability"),
    "",
                       midpoint=city_avg$SVI,## , mid="grey90"
                       ## , low="firebrick3",high = "dodgerblue3"
                                              high = mycols1[1], low = mycols1[2], mid = mycols1[3]
                       ## low="darkgreen", high="darkviolet", mid="grey95"
  ) +
##     scale_fill_steps2(    "",
##                     breaks=c(0.2, round(city_avg$infection_rate, 1), 0.4, 0.6, 0.8),
##                        midpoint=city_avg$infection_rate,## , mid="grey90"
##                     ## , low="firebrick3",high = "dodgerblue3"
##                     high = mycols1[1], low = mycols1[2], mid = mycols1[3],
##                     ## labels = scales::label_percent(accuracy=1),
##                     nice.breaks=FALSE
##                        ## low="darkorange", high="magenta", mid="grey95"
## ) + 
    ## scale_fill_gradient2(sprintf("Social vulnerability index\(higher is more vulnerable)\ncity average = %1.2f", city_avg$SVI), midpoint=city_avg$SVI,## , mid="grey90"
    ##                    ## , low="firebrick3",high = "dodgerblue3"
    ##                                           high = mycols1[1], low = mycols1[2], mid = mycols1[3]
    ##                    low="darkgreen", high="darkviolet", mid="grey95"
    ##                    ) + 
  NULL +
  labs(## title = "Social vulnerability index in Austin",
    ## subtitle = sprintf("SVI by ZIP code\nRelative to city-average (%2.2f; higher is more vulnerable)", city_avg$SVI)
  ) +
  ## guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) +
  labs(title="Social vulnerability") + 
  guides(fill = guide_coloursteps(ticks=TRUE,barwidth = 12, barheight = 0.5)) +
  theme_map() + 
  ## theme_cowplot() +
  ##   annotate(geom="text", x=-97.8, y=30.5, label="US 183", size=5)+
  ## annotate(geom="text", x=-97.68, y=30.5, label="I-35", size=5) + 
  theme(legend.position = "top",
        legend.title = element_text(face = "bold"))

myplot3_svi_mycols

myplot3_both <- plot_grid(myplot3, myplot3_svi, align="hv")

myplot3_both

ggsave(sprintf("figures/austin/png/%s-vaccine_coverage_svi_maps.png", today()), myplot3_both,
       width = 11, height = 6.1, units="in")
ggsave(sprintf("figures/austin/pdf/%s-vaccine_coverage_svi_maps.pdf", today()), myplot3_both,
       width = 11, height = 6.1, units="in")

myplot3_burden <- vax_sub2 %>%
  ## filter(county == "Harris") %>% 
  ## filter(PO_NAME == "Houston") %>%
  filter(PO_NAME == "Austin") %>%
  ## filter(coverage<1) %>%
  ## filter(GEOID %in% myzips) %>% 
  ## filter(coverage!=max(coverage)) %>%
  glimpse() %>% 
  ggplot() +
  geom_sf(aes(fill=attack_rate_mean), size=0.1) +
    geom_sf(data = I35_2,
          col = "grey10") +
  geom_sf(data = missing_183_2,
          col = "grey10") +
  geom_sf(data = US183_2,
          col = "grey10") +
  ## geom_sf(data = travis_roads1,
  ##         col = "grey30") +
  ## geom_sf(data = travis_water3 %>%
  ##           filter(name %>% str_detect("Lake") |
  ##                  name %>% str_detect("River")) ## %>% 
  ##           ## mutate(Area = st_area(geometry) %>% as.numeric()) %>%
  ##           ## filter(Area > 1)
  ##        ,
  ##         aes(geometry = geometry2),
  ##         fill = "lightblue", size = 0.1, col="lightblue")+
  ## scale_fill_viridis_c() + 
  ## scale_color_viridis_c() +
  scale_fill_gradient2(## sprintf("Cumulative infections", city_avg$infection_rate * 100),
    "",
                       midpoint=city_avg$infection_rate,## , mid="grey90"
                       ## , low="firebrick3",high = "dodgerblue3"
                       high = mycols2[1], low = mycols2[2], mid = mycols2[3],
                       labels = scales::label_percent(accuracy=1)
                       ## low="darkorange", high="magenta", mid="grey95"
  ) +
##   scale_fill_steps2(    "",
##                     breaks=c(0.2, 0.3, city_avg$infection_rate, 0.4, 0.5, 0.6),
##                        midpoint=city_avg$infection_rate,## , mid="grey90"
##                        ## , low="firebrick3",high = "dodgerblue3"
##                        high = mycols2[1], low = mycols2[2], mid = mycols2[3],
##                     labels = scales::label_percent(accuracy=1),
##                     nice.breaks=FALSE
##                        ## low="darkorange", high="magenta", mid="grey95"
## )+ 
    ## scale_fill_gradient2(sprintf("Cumulative infections\ncity average = %2.1f%%", city_avg$infection_rate * 100), midpoint=city_avg$infection_rate,## , mid="grey90"
    ##                    ## , low="firebrick3",high = "dodgerblue3"
    ##                    high = mycols2[1], low = mycols2[2], mid = mycols2[3],
    ##                    labels = scales::label_percent(accuracy=1)
    ##                    ## low="darkorange", high="magenta", mid="grey95"
    ##                    ) + 
  NULL +
  labs(## title = "Social vulnerability index in Austin",
    ## subtitle = sprintf("SVI by ZIP code\nRelative to city-average (%2.2f; higher is more vulnerable)", city_avg$SVI)
  ) +
  ## guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) +
  labs(title="Cumulative infections") + 
  guides(fill = guide_coloursteps(ticks=TRUE,barwidth = 14, barheight = 0.5)) +
  ## theme_cowplot() +
  theme_map() +
  ## annotate(geom="text", x=-97.8, y=30.5, label="US 183", size=5)+
  ## annotate(geom="text", x=-97.68, y=30.5, label="I-35", size=5) + 
  theme(legend.position = "top",
        legend.title = element_text(face = "bold"),
        legend.title.align = 0)

myplot3_burden


## myplot3_3 <- plot_grid(myplot3, myplot3_svi, myplot3_burden, align="hv", nrow=1)
myplot3_3 <- plot_grid(myplot3_svi, myplot3_burden, myplot3, align="hv", nrow=1)
myplot3_3_mycols <- plot_grid(myplot3_svi_mycols, myplot3_burden, myplot3, align="hv", nrow=1)

myplot3_3_mycols



ggsave(sprintf("figures/austin/png/%s-vaccine_coverage_svi_burden.png", today()), myplot3_3,
       width = 11*3/2, height = 6.1, units="in")
ggsave(sprintf("figures/austin/png/%s-vaccine_coverage_svi_burden.tiff", today()), myplot3_3,
       width = 11*3/2, height = 6.1, units="in")
ggsave(sprintf("figures/austin/png/%s-vaccine_coverage_svi_burden.pdf", today()), myplot3_3,
       width = 11*3/2, height = 6.1, units="in")

ggsave(sprintf("figures/austin/png/%s-vaccine_coverage_svi_burden_new2.png", today()), myplot3_3_mycols,## ,
       width = 10*2800/3000, height = 4, units="in"
       )


ggsave(sprintf("figures/austin/png/%s-vaccine_coverage_svi_burden_new.png", today()), myplot3_3_mycols,
       width = 11*3/2, height = 6.1, units="in")
ggsave(sprintf("figures/austin/png/%s-vaccine_coverage_svi_burden_new.tiff", today()), myplot3_3_mycols,
       width = 11*3/2, height = 6.1, units="in")
ggsave(sprintf("figures/austin/png/%s-vaccine_coverage_svi_burden_new.pdf", today()), myplot3_3_mycols,
       width = 11*3/2, height = 6.1, units="in")

## myplot3_3 <- plot_grid(myplot3, myplot3_svi, myplot3_burden, align="hv", nrow=1)
myplot3_3_tall <- plot_grid(myplot3_svi, myplot3_burden, myplot3, align="hv", ncol=1)
myplot3_3_mycols_tall <- plot_grid(myplot3_svi_mycols, myplot3_burden, myplot3, align="hv", ncol=1)

myplot3_3_tall
myplot3_3_mycols_tall

ggsave(sprintf("figures/austin/png/%s-vaccine_coverage_svi_burden_tall.png", today()), myplot3_3_tall,
              width = 4.5, height = 11, units="in")
ggsave(sprintf("figures/austin/png/%s-vaccine_coverage_svi_burden_tall.tiff", today()), myplot3_3_tall,
              width = 4.5, height = 11, units="in")
ggsave(sprintf("figures/austin/png/%s-vaccine_coverage_svi_burden_tall.pdf", today()), myplot3_3_tall,
              width = 4.5, height = 11, units="in")


ggsave(sprintf("figures/austin/png/%s-vaccine_coverage_svi_burden_tall_new.png", today()), myplot3_3_mycols_tall,
       width = 4.5, height = 11, units="in")
ggsave(sprintf("figures/austin/png/%s-vaccine_coverage_svi_burden_tall_new.tiff", today()), myplot3_3_mycols_tall,
              width = 4.5, height = 11, units="in")
ggsave(sprintf("figures/austin/png/%s-vaccine_coverage_svi_burden_new.pdf", today()), myplot3_3_mycols_tall,
              width = 4.5, height = 11, units="in")



myplot3_sr <- vax_sub2 %>%
  ## filter(county == "Harris") %>% 
  ## filter(PO_NAME == "Houston") %>%
  filter(PO_NAME == "Austin") %>%
  ## filter(coverage<1) %>%
  ## filter(GEOID %in% myzips) %>% 
  ## filter(coverage!=max(coverage)) %>%
  glimpse() %>% 
  ggplot() +
  geom_sf(aes(fill=senior_frac, label=GEOID## , col=burden
              ), size=0.1) +
    ## geom_sf(data = travis_roads1,
  ##         col = "grey30") +
  ## geom_sf(data = zcta_roads %>% filter(kind=="major_road"),
  ##         col = "grey20") + 
  geom_sf(data = I35_2,
          col = "grey10") +
  geom_sf(data = missing_183_2,
          col = "grey10") +
  geom_sf(data = US183_2,
          col = "grey10") +
    ## geom_sf(data = zcta_roads %>% filter(kind=="major_road"),
    ##       col = "grey50") + 
  ## scale_fill_viridis_c() + 
  ## scale_color_viridis_c() +
  ## scale_fill_gradient2(sprintf("Vaccine coverage\n(%% of adult pop. with ≥1 dose)\ncity-average = %2.1f%%", city_avg$coverage * 100),
  ##                      midpoint=city_avg$coverage, labels=scales::percent,
  ##                      high = mycols3[2], low = mycols3[1], mid = mycols3[3]
  ##                      ## , mid="grey90"
  ##                      ## , low="firebrick3",high = "dodgerblue3"
  ##                      ) +
  scale_fill_gradient2(## sprintf("Vaccine coverage"),
    "",
    ## breaks=c(0.3, round(city_avg$coverage, 2), 0.5, 0.7, 0.9),
                       midpoint=city_avg$senior_frac, labels=scales::label_percent(accuracy=1),
                       high = mycols5[2], low = mycols5[1], mid = mycols5[3]
                       ## , mid="grey90"
                       ## , low="firebrick3",high = "dodgerblue3"
  ) +
##       scale_fill_steps2(    "",
##                     ## breaks=c(0.3, round(city_avg$coverage, 2), 0.5, 0.7, 0.9),
##                     midpoint=city_avg$coverage,## , mid="grey90"
##                     high = mycols3[2], low = mycols3[1], mid = mycols3[3],
##                     ## , low="firebrick3",high = "dodgerblue3"
##                     ## high = mycols1[1], low = mycols1[2], mid = mycols1[3],
##                     ## labels = scales::label_percent(accuracy=1),
##                     nice.breaks=FALSE
##                        ## low="darkorange", high="magenta", mid="grey95"
## ) + 
  NULL +
  ## labs(title = "Vaccine coverage in Austin",
  ##      subtitle = sprintf("Percentage of adult population with at least one dose\nRelative to city-average (%2.1f%%)", city_avg$coverage * 100)) +
  ## guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) +
  ## labs(caption=sprintf("Up to %s\nSource: Texas DSHS", as_of))+
  labs(title="Percentage of senior residents")  +
  guides(fill = guide_coloursteps(ticks=TRUE,barwidth = 12, barheight = 0.5)) +
  ## theme_cowplot() +
  theme_map() +
  ## annotate(geom="text", x=-97.8, y=30.5, label="US 183", size=5)+
  ## annotate(geom="text", x=-97.68, y=30.5, label="I-35", size=5) + 
  theme(legend.position = "top",
        legend.title = element_text(face = "bold"))

myplot3_sr

myplot3_nonsr <- vax_sub2 %>%
  ## filter(county == "Harris") %>% 
  ## filter(PO_NAME == "Houston") %>%
  filter(PO_NAME == "Austin") %>%
  ## filter(coverage<1) %>%
  ## filter(GEOID %in% myzips) %>% 
  ## filter(coverage!=max(coverage)) %>%
  glimpse() %>% 
  ggplot() +
  geom_sf(aes(fill=nonsenior_frac, label=GEOID## , col=burden
              ), size=0.1) +
    ## geom_sf(data = travis_roads1,
  ##         col = "grey30") +
  ## geom_sf(data = zcta_roads %>% filter(kind=="major_road"),
  ##         col = "grey20") + 
  geom_sf(data = I35_2,
          col = "grey10") +
  geom_sf(data = missing_183_2,
          col = "grey10") +
  geom_sf(data = US183_2,
          col = "grey10") +
    ## geom_sf(data = zcta_roads %>% filter(kind=="major_road"),
    ##       col = "grey50") + 
  ## scale_fill_viridis_c() + 
  ## scale_color_viridis_c() +
  ## scale_fill_gradient2(sprintf("Vaccine coverage\n(%% of adult pop. with ≥1 dose)\ncity-average = %2.1f%%", mydf$coverage * 100),
  ##                      midpoint=mydf$coverage, labels=scales::percent,
  ##                      high = mycols3[2], low = mycols3[1], mid = mycols3[3]
  ##                      ## , mid="grey90"
  ##                      ## , low="firebrick3",high = "dodgerblue3"
  ##                      ) +
  scale_fill_gradient2(## sprintf("Vaccine coverage"),
    "",
    ## breaks=c(0.3, round(mydf$coverage, 2), 0.5, 0.7, 0.9),
                       midpoint=mydf$nonsenior_frac, labels=scales::label_percent(accuracy=1),
                       high = mycols6[2], low = mycols6[1], mid = mycols6[3]
                       ## , mid="grey90"
                       ## , low="firebrick3",high = "dodgerblue3"
  ) +
##       scale_fill_steps2(    "",
##                     ## breaks=c(0.3, round(mydf$coverage, 2), 0.5, 0.7, 0.9),
##                     midpoint=mydf$coverage,## , mid="grey90"
##                     high = mycols3[2], low = mycols3[1], mid = mycols3[3],
##                     ## , low="firebrick3",high = "dodgerblue3"
##                     ## high = mycols1[1], low = mycols1[2], mid = mycols1[3],
##                     ## labels = scales::label_percent(accuracy=1),
##                     nice.breaks=FALSE
##                        ## low="darkorange", high="magenta", mid="grey95"
## ) + 
  NULL +
  ## labs(title = "Vaccine coverage in Austin",
  ##      subtitle = sprintf("Percentage of adult population with at least one dose\nRelative to city-average (%2.1f%%)", mydf$coverage * 100)) +
  ## guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) +
  ## labs(caption=sprintf("Up to %s\nSource: Texas DSHS", as_of))+
  labs(title="Percentage of nonsenior residents")  +
  guides(fill = guide_coloursteps(ticks=TRUE,barwidth = 12, barheight = 0.5)) +
  ## theme_cowplot() +
  theme_map() +
  ## annotate(geom="text", x=-97.8, y=30.5, label="US 183", size=5)+
  ## annotate(geom="text", x=-97.68, y=30.5, label="I-35", size=5) + 
  theme(legend.position = "top",
        legend.title = element_text(face = "bold"))

myplot3_nonsr


mygrid_sr <- plot_grid(myplot3, myplot3_sr, myplot3_svi_mycols, myplot3_burden)

mygrid_sr

## Theme_mape
myplot3_3map <- plot_grid(myplot3+theme_map()+theme(legend.position = "top"),
                       myplot3_svi+theme_map()+theme(legend.position = "top"),
                       myplot3_burden+theme_map()+theme(legend.position = "top"),
                       align="hv", nrow=1)

myplot3_3map

ggsave(sprintf("figures/austin/png/%s-vaccine_coverage_svi_burden_map.png", today()), myplot3_3map,
       width = 11*3/2, height = 6.1, units="in")

myplot3_3

## 

###############################################################################
                                        #           Extra stuff....           #
###############################################################################

vax_sub2 %>% glimpse()

vax_sub3 <- vax_sub2 %>%
  distinct(GEOID, .keep_all=TRUE) %>% 
  arrange(SVI) %>%
  mutate(SVI_rank = 1:n()) %>%
  ## mutate(GEOID=factor(GEOID)) %>%
  glimpse()

vax_sub3_zip <- vax_sub3 %>% pull(GEOID)

vax_sub3 <- vax_sub3 %>%
  mutate(GEOID = as.character(GEOID)) %>% 
  mutate(GEOID=factor(GEOID, levels=vax_sub3_zip)) %>%
  mutate(
    Area = case_when(
      GEOID=="78701" ~ "Downtown Austin",
      GEOID %in% c("78705", "78712") ~ "UT-Austin Area",
      TRUE ~ "Other"
    ),
    Area = factor(Area, levels = rev(c("Downtown Austin", "UT-Austin Area", "Other")))
  )

vax_sub2 %>%
  ggplot() +
  geom_point(aes((phase_1a_total) / B01001_001E, coverage, col = SVI)) +
  scale_x_continuous(labels=scales::label_percent()) +
  scale_y_continuous(labels=scales::label_percent()) + 
  labs(x="Percentage of pop. in phase 1a", y="Vaccine coverage") +
  scale_color_viridis_c()

bar_coverage <- vax_sub3 %>%
  ggplot() +
  geom_linerange(aes(GEOID, ymin=0, ymax=coverage, col = Area),size=4) +
  ## geom_point(aes(GEOID, coverage, col = Area)) +
  ggthemes::scale_color_colorblind() +
  scale_y_continuous(labels=scales::label_percent()) +
  labs(x = "ZIP code (arranged by increasing SVI)",
       y = "Vaccine coverage (% of adult pop. with ≥1 dose)") +
  theme_minimal_hgrid() +
  theme(axis.text.x = element_text(angle=45, vjust=0.5, family="Inconsolata"),
        legend.position = "top")

bar_attack <- vax_sub3 %>%
  ggplot() +
  geom_linerange(aes(GEOID, ymin=attack_rate_lower, ymax=attack_rate_upper, col = Area),size=1) +
  geom_point(aes(GEOID, attack_rate_mean, col = Area),size=2) +
  scale_y_continuous(labels=scales::label_percent()) + 
  labs(x = "ZIP code (arranged by increasing SVI)",
       y = "Cumulative infections") +
  ggthemes::scale_color_colorblind() +
  theme_minimal_hgrid() + 
  theme(axis.text.x = element_text(angle=45, vjust=0.5, family="Inconsolata"),
        legend.position = "none")

bar_attack

bar_both <- plot_grid(bar_coverage, bar_attack, ncol=1)

ggsave("figures/austin/png/bars.png", bar_both, width=9.5, height=9.5)


vax_sub3 %>%
  ggplot() +
  geom_linerange(aes(GEOID, coverage)) +
  geom_point(aes(GEOID, coverage)) +
  theme(axis.text.x = element_text(angle=45, vjust=0.5, family="Inconsolata"))

vax_sub3 %>%
  ggplot() +
  geom_point(aes(GEOID, SVI))

## myplot4 <- vax_sub %>%
##   ## filter(county == "Harris") %>% 
##   ## filter(PO_NAME == "Houston") %>%
##   filter(PO_NAME == "Austin") %>%
##   ## filter(coverage<1) %>%
##   ## filter(GEOID %in% myzips) %>% 
##   ## filter(coverage!=max(coverage)) %>%
##   glimpse() %>% 
##   ggplot() +
##   geom_sf(aes(fill=coverage), size=0.1) +
##   ## scale_fill_viridis_c() + 
##   ## scale_color_viridis_c() +
##   scale_fill_gradient2("", midpoint=mydf$coverage, labels=scales::percent## , mid="grey90"
##                        ## , low="firebrick3",high = "dodgerblue3"
##                        ) + 
##   NULL +
##   labs(title = "Vaccine coverage in Austin",
##        subtitle = sprintf("Percentage of adult population with at least one dose\nRelative to city-average (%2.1f%%)", mydf$coverage * 100)) +
##   ## guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) +
##   guides(fill = guide_coloursteps(ticks=TRUE,barwidth = 0.5, barheight = 10))





## Leaflet map
## library(mapview)

## vax_small2 <- vax_small %>% filter(PO_NAME == "Austin") %>%
##   mutate(coverage_one_dose = round(coverage_one_dose * 100, 0))

## mapview::mapview(vax_small2, zcol = "coverage_one_dose", at = seq(0, 70, by=10))


# GET ROADS MSA
get_vector_tiles <- function(bbox){
  mz_box=mz_rect(bbox$xmin,bbox$ymin,bbox$xmax,bbox$ymax)
  mz_vector_tiles(mz_box)
}

zip_geom <- vax_sub2
zcta_geom <- st_union(zip_geom$geometry)

load("census/roads.Rdata")

I35_2 <- st_crop(I35, zcta_geom)
US183_2 <- st_crop(US183, zcta_geom)
missing_183_2 <- st_crop(missing_183, zcta_geom)

I35_2 <- st_intersection(I35, zcta_geom)
US183_2 <- st_intersection(US183, zcta_geom)
missing_183_2 <- st_intersection(missing_183, zcta_geom)

zcta_bbox <- st_bbox(zcta_geom)
zcta_vector_tiles <- get_vector_tiles(zcta_bbox)
zcta_roads <- as_sf(zcta_vector_tiles$roads)

RI35<- zcta_roads %>%
  mutate(st_transform(geometry, st_crs(zip_geom))) %>% 
  filter(ref== "I 35" | ref=="I 35;US 190" | ref=="I 35;US 290" | ref=="I 35;US 77") %>% # | ref=="I 35-H Business") %>%
  pull(geometry) %>%
  st_union() %>%
  st_transform(st_crs(zcta_geom)) %>%
  st_intersection(zcta_geom)

US183 = zcta_roads %>%
  mutate(st_transform(geometry, st_crs(zip_geom))) %>% 
  filter(ref=="US 183" | ref=="US 183;FM 20" | ref=="US 183;TX 80" | ref=="US 90;US 183" | ref== "US 183;US 190;US 281" 
         | ref=="US 183;US 190;US 281;FM 580" | ref=="US 183;US 190;US 281;Truck" | ref=="US 183;US 190"  ) %>%
  pull(geometry) %>%
  st_union() %>%
  st_transform(st_crs(zcta_geom)) %>%
  st_intersection(zcta_geom)

missing_183=zcta_roads %>%
  filter(id=="2c2066c3b6fafe2574463154f5cc877d")



missing_183$geometry = st_sfc( c(st_multilinestring(missing_183[[24]][[1]][17:18]), 
                                 st_multilinestring(missing_183[[24]][[1]][20:24])), 
                               crs = st_crs(zip_geom))

myplot32 <- vax2 %>%
  ## filter(county == "Harris") %>% 
  filter(msa %>% str_detect("Austin")) %>%
  ## filter(PO_NAME == "Austin") %>%
  ## filter(coverage<1) %>%
  ## filter(GEOID %in% myzips) %>% 
  ## filter(coverage!=max(coverage)) %>%
  glimpse() %>% 
  ggplot() +
  geom_sf(aes(fill=coverage, label=GEOID## , col=burden
              ), size=0.1) +
    ## geom_sf(data = travis_roads1,
  ##         col = "grey30") +
  ## geom_sf(data = zcta_roads %>% filter(kind=="major_road"),
  ##         col = "grey20") + 
  geom_sf(data = I35,
          col = "grey10",size=2) +
  geom_sf(data = missing_183,
          col = "grey10",size=2) +
  geom_sf(data = US183,
          col = "grey10", size=2) +
    ## geom_sf(data = zcta_roads %>% filter(kind=="major_road"),
    ##       col = "grey50") + 
  ## scale_fill_viridis_c() + 
  ## scale_color_viridis_c() +
  ## scale_fill_gradient2(sprintf("Vaccine coverage\n(%% of adult pop. with ≥1 dose)\ncity-average = %2.1f%%", mydf$coverage * 100),
  ##                      midpoint=mydf$coverage, labels=scales::percent,
  ##                      high = mycols3[2], low = mycols3[1], mid = mycols3[3]
  ##                      ## , mid="grey90"
  ##                      ## , low="firebrick3",high = "dodgerblue3"
  ##                      ) +
    scale_fill_gradient2(sprintf("Vaccine coverage"),
                       midpoint=mydf$coverage, labels=scales::percent,
                       high = mycols3[2], low = mycols3[1], mid = mycols3[3]
                       ## , mid="grey90"
                       ## , low="firebrick3",high = "dodgerblue3"
                       ) + 
  NULL +
  ## labs(title = "Vaccine coverage in Austin",
  ##      subtitle = sprintf("Percentage of adult population with at least one dose\nRelative to city-average (%2.1f%%)", mydf$coverage * 100)) +
  ## guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) +
  ## labs(caption=sprintf("Up to %s\nSource: Texas DSHS", as_of))+
  guides(fill = guide_coloursteps(ticks=TRUE,barwidth = 12, barheight = 0.5)) +
  ## theme_cowplot() +
  theme_map() + 
  theme(legend.position = "top")

myplot32 <- vax2 %>%
  ## filter(county == "Harris") %>% 
  filter(msa %>% str_detect("Austin")) %>%
  ## filter(PO_NAME == "Austin") %>%
  ## filter(coverage<1) %>%
  ## filter(GEOID %in% myzips) %>% 
  ## filter(coverage!=max(coverage)) %>%
  glimpse() %>% 
  ggplot() +
  geom_sf(aes(fill=coverage, label=GEOID## , col=burden
              ), size=0.1)

myplot32
 
myplot <- vax %>%
  ## filter(msa %>% str_detect("Dallas")) %>%
  ## filter(msa %>% str_detect("San Anto")) %>% 
  ## filter(county == "Harris") %>%
  ## filter(PO_NAME == "Dallas") %>%
  ## filter(PO_NAME == "Houston") %>%
  ## filter(coverage<1) %>%
  ## filter(PO_NAME == "Austin") %>%
  ## filter(GEOID %in% myzips) %>% 
  ## filter(coverage!=max(coverage)) %>%
  ggplot() +
  geom_sf(aes(fill=coverage), size=0.1) +
  scale_fill_viridis_c("", labels=scales::percent) + 
  ## scale_color_viridis_c() +
  ## scale_fill_gradient2(midpoint=state_average) +
  labs(title = "Vaccine coverage in Austin",
       subtitle = "Percentage of adult population with at least one dose") +
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) + 
  NULL

myplot

myplot_svi <- vax %>%
  ## filter(county == "Harris") %>% 
  ## filter(PO_NAME == "Houston") %>%
  ## filter(PO_NAME == "Austin") %>%
  ## filter(coverage<1) %>%
  ## filter(GEOID %in% myzips) %>% 
  ## filter(coverage!=max(coverage)) %>%
  glimpse() %>% 
  ggplot() +
  geom_sf(aes(fill=SVI), size=0.1) +
  scale_fill_viridis_c("SVI"## , labels=scales::percent
                       ) + 
  ## scale_color_viridis_c() +
  ## scale_fill_gradient2(midpoint=state_average) +
  labs(title = "Vaccine coverage in Austin",
       subtitle = "Percentage of adult population with at least one dose") +
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10)) + 
  NULL
