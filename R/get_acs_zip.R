


get_acs_zip <- function() {

  if (dir() %>% str_detect("acs2019_variables.csv") %>% any()) {
    acs_vars <- read_csv("acs2019_variables.csv")
  } else {
    acs_vars <- load_variables("2019", "acs5")
    write_csv(acs_vars, "acs2019_variables.csv")  
  }
  
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

  sr <- acs_vars %>% slice(20:25, 44:49) %>% glimpse() %>% pull(name)

  acs_wide <- get_acs(geography = "zcta", variables = myvars,
                      state = "TX", geometry = TRUE, output = "wide")

  acs_wide_sr <- get_acs(geography = "zcta", variables = sr,
                         state = "TX", geometry = TRUE, output = "wide")

  acs_wide_sr <- acs_wide_sr %>%
    mutate(plus65 = rowSums(across(contains("B") & contains("E")))) 

  ## Rename income variable
  acs_wide <- acs_wide %>%
    rename(
      median_incomeE = B19013_001E,
      median_incomeM = B19013_001M,
      )

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
           nonsenior_frac = adults_16_64 / (adult_pop) ) 
}
