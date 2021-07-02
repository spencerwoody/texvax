

source("./zcta_vaccinations_export_nowrite.R")


vax_sub <- vax_all %>% filter(date == max(date)) %>% filter(PO_NAME == "Austin")
vax_sub_time <- vax_all %>% filter(PO_NAME == "Austin")
vax <- vax_all %>% filter(date == max(date))

###############################################################################
                                        #       Get geometry for Austin       #
###############################################################################

load("census/austin-roads-cropped.Rdata")


load("census/estimation_results_France_no0_under20.rda")

## Attack rate estimates
## attack <- read_csv("census/zip_attack_rates_20210329.csv") %>%
##   mutate(ZIP = as.character(ZIP))

attack <- msa_zip_df

glimpse(attack)

## Join in attack rates (for Austin area only...)
vax_sub <- vax_sub %>%
  left_join(attack,
            by = c("ZCTA" = "ZIP"))



###############################################################################
                                        #           Mortality rates           #
###############################################################################

## ZIP1 <- read_csv("hosp/CV19Hospital_ICU_DeID_20210426.csv")
hosp_files <- dir("hosp", full.names = TRUE) %>% sort()
hosp_files
ZIP <- read_xlsx(hosp_files[length(hosp_files)], sheet="Hospitalizations")

## ZIP1 %>% glimpse()
ZIP %>% glimpse()

myzip <- ZIP %>%
  select(ID,
         age = Age,
         date = `Date of Admission`,
         ZIP = `Zip Code of Residence`,
         date_admit = `Date of Admission`,
         date_discharge = `Date of Discharge`,
         discharge_status = `Discharge Status`) %>%
  mutate(ZIP = as.character(ZIP)) %>%
  mutate(discharge_status = toupper(discharge_status)) %>%
  glimpse()

myzip2 <- myzip %>%
  arrange(ID, desc(date_admit)) %>%
  ## distinct(ID, .keep_all=TRUE) %>%
  glimpse()

myzip3 <- myzip %>%
  group_by(ZIP) %>%
  summarize(deaths_65plus =
              sum(str_detect(discharge_status, "EXPIRED") & age >= 65, na.rm=TRUE) +
              sum(str_detect(discharge_status, "DIED") & age >= 65, na.rm=TRUE),
            deaths =
              sum(str_detect(discharge_status, "EXPIRED"), na.rm=TRUE) +
              sum(str_detect(discharge_status, "DIED"), na.rm=TRUE)) %>%
  ungroup() %>%
  arrange(desc(deaths)) %>%
  left_join(myzz %>%
            select(ZIP = ZIP_CODE,
                   PO_NAME, STATE, ZCTA)) %>%
  group_by(ZCTA) %>%
  summarize(deaths = sum(deaths, na.rm=TRUE),
            deaths_65plus = sum(deaths_65plus, na.rm=TRUE),
            n = n()) %>%
  mutate(deaths_65under = deaths - deaths_65plus) %>%
  arrange(desc(deaths))

myzip3$deaths %>% sum()
myzip3$deaths_65plus %>% sum()
myzip3$deaths_65under %>% sum()

myzip <- ZIP %>%
  select(ID,
         age = Age,
         date = `Date of Admission`,
         ZIP = `Zip Code of Residence`,
         date_admit = `Date of Admission`,
         date_discharge = `Date of Discharge`,
         discharge_status = `Discharge Status`) %>%
  mutate(ZIP = as.character(ZIP)) %>%
  mutate(discharge_status = toupper(discharge_status)) %>%
  left_join(myzz %>% select(ZIP = ZIP_CODE, ZCTA)) %>%
  mutate(date_admit = date(date_admit)) %>%
  glimpse()

myzip_admit <- myzip %>%
  mutate(date_admit = date(date_admit)) %>%
  filter(date_admit >= max(date_admit) - 7 * 4) %>%
  group_by(ZCTA## , date_admit
           ) %>%
  summarize(admits=n()) %>%
  glimpse()

vax_sub_deaths <- vax_sub %>%
  left_join(myzip_admit) %>%
  left_join(myzip3 %>% select(-n)) %>%
  mutate(deaths_per_10k = deaths/total_pop*1e4,
         deaths_per_10k_65plus = deaths_65plus / adults_65_plus * 1e4,
         deaths_per_10k_65under = deaths_65under / (total_pop - adults_65_plus) * 1e4,
         deaths_per_100k = deaths_per_10k * 10,
         deaths_per_100k_65plus = deaths_per_10k_65plus * 10,
         deaths_per_100k_65under = deaths_per_10k_65under * 10) %>%
  mutate(admits_imp = ifelse(is.na(admits), 0, admits)) %>%
  mutate(admits_per_10k = admits_imp/total_pop*1e4,
         admits_per_100k = admits_imp/total_pop*1e5) %>%
  glimpse()

vax_sub_deaths$deaths %>% sum()


###############################################################################
                                        #          City-wide averages         #
###############################################################################

city_avg <- vax_sub_deaths %>%
  mutate(infected = B01001_001E * attack_rate_mean) %>%
  summarize(
    SVI = mean(SVI),
    total_pop = sum(B01001_001E),
    total_infected = sum(infected, na.rm=TRUE),
    infection_rate = total_infected / total_pop,
    adult_pop = sum(adult_pop),
    one_dose = sum(one_dose),
    senior_frac = sum(plus65)/adult_pop,
    nonsenior_frac=sum(adults_16_64)/adult_pop,
    total_deaths = sum(deaths),
    total_deaths_65plus = sum(deaths_65plus),
    total_deaths_65under = sum(deaths_65under),
    total_admits=sum(admits_imp),
    city_pop = sum(total_pop),
    city_pop_65plus = sum(adults_65_plus),
    city_pop_65under = city_pop - city_pop_65plus,
    deaths_per_10k_avg = total_deaths/city_pop * 1e4,
    admits_per_10k_avg = total_admits/city_pop * 1e4,
    deaths_per_10k_65plus_avg = total_deaths_65plus / city_pop_65plus * 1e4,
    deaths_per_10k_65under_avg = total_deaths_65under / city_pop_65under * 1e4,
    ) %>%
  mutate(deaths_per_100k_avg = deaths_per_10k_avg * 10,
         admits_per_100k_avg = admits_per_10k_avg * 10,
         deaths_per_100k_65plus_avg = deaths_per_10k_65plus_avg * 10,
         deaths_per_100k_65under_avg = deaths_per_10k_65under_avg * 10,
         coverage = one_dose / adult_pop,
         coverage_all = one_dose / total_pop) %>%
  glimpse()


###############################################################################
                                        #                 Time                #
###############################################################################

vax_early <- vax_all %>% filter(date == min(date))

vax_all2 <- vax_all %>%
  left_join(vax_early %>%
            as.data.frame() %>%
            select(ZCTA, one_dose_early = one_dose)) %>%
  mutate(one_dose_sincefeb = one_dose - one_dose_early) %>%
  mutate(coverage_sincefeb = one_dose_sincefeb/(adult_pop+1))

mysvi <- vax_sub %>%
  select(ZCTA, total_pop, SVI) %>%
  arrange(SVI) %>%
  mutate(total_pop_city = sum(total_pop),
         total_pop_pct = cumsum(total_pop)/sum(total_pop))

mysvi

mysvi %>% filter(total_pop_pct > 0.25) %>% slice(1)
mysvi %>% filter(total_pop_pct > 0.5) %>% slice(1)
mysvi %>% filter(total_pop_pct > 0.75) %>% slice(1)

mypo <-"Austin"

mysvimap <- mysvi %>%
  mutate(
    quantile_cat = case_when(
      total_pop_pct < 0.25 ~ "1st quartile",
      total_pop_pct < 0.5 ~ "2nd quartile",
      total_pop_pct < 0.75 ~ "3rd quartile",
      TRUE ~ "4th quartile"
    )
  )

mydf <- vax_all2 %>%
  filter(date == min(date) | one_dose_new > 0) %>%
  ## filter(PO_NAME == "Austin") %>%
  filter(PO_NAME == mypo) %>%
  ## filter(SVI <= myquants[2] | SVI >= myquants[4]) %>%
  left_join(mysvimap %>% select(ZCTA, SVI_cat = quantile_cat)) %>%
  ## mutate(SVI_cat = case_when(
  ##          SVI <= myquants[2] ~ "1st quartile",
  ##          SVI >= myquants[2] & SVI <= myquants[3] ~ "2nd quartile",
  ##          SVI >= myquants[3] & SVI <= myquants[4] ~ "3rd quartile",
  ##          SVI >= myquants[4] ~ "4th quartile",
  ##          )) %>%
  glimpse() %>%
  ## filter(one_dose_new > 0) %>%
  group_by(SVI_cat, date) %>%
  summarize(coverage_cat = sum(coverage_total_pop * total_pop / sum(total_pop)),
            coverage_sincefeb_cat = sum(coverage_sincefeb * total_pop / sum(total_pop))) %>%
  filter(!is.na(SVI_cat))



mydf_map <- vax_all %>%
  ## filter(PO_NAME == "Austin") %>%
  filter(PO_NAME == mypo) %>%
  ## filter(SVI <= myquants[2] | SVI >= myquants[4]) %>%
  left_join(mysvimap %>% select(ZCTA, SVI_cat = quantile_cat)) ## %>%
  ## mutate(SVI_cat = case_when(
  ##          SVI <= myquants[2] ~ "1st quartile",
  ##          SVI >= myquants[2] & SVI <= myquants[3] ~ "2nd quartile",
  ##          SVI >= myquants[3] & SVI <= myquants[4] ~ "3rd quartile",
  ##          SVI >= myquants[4] ~ "4th quartile",
  ##          ))

###############################################################################
                                        #                Export               #
###############################################################################



myvax_all <- vax_all %>% filter(PO_NAME == "Austin")

## save(atx_roads, vax_sub_deaths, mydf, mysvimap, vax_all2, city_avg, file=sprintf("Rout/%s-map_data.Rdata", today()))
save(atx_roads, vax_sub_deaths, city_avg, myvax_all, mydf, mysvimap, city_avg, file=sprintf("Rout/%s-map_data.Rdata", today()))

## Diverging color scales

myio <- vax_sub_deaths %>%
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
         hosp_deaths_total = deaths,
         hosp_deaths_total_per_100k = deaths_per_100k,
         admits_last_14days = admits_imp,
         admits_last_14days_per_100k = admits_per_100k,
         ## dem_share,
         ## dem_share_2party_pres_vote,
         ## state_average_coverage_one_dose = state_average,
         contains("phase"),
         median_household_income = median_incomeE,
         SVI,
         contains("RPL"),
         geometry)


myio %>%
  as.data.frame() %>%
  select(-geometry) %>%
  glimpse() %>%
  write_csv(sprintf("austin/%s-austin-covid19-vaccinations-burden.csv", today()))

## rm(list=ls())
