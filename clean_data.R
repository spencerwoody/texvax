
library(tidyverse)
library(lubridate)
library(readxl)
library(cowplot)

theme_set(theme_minimal_grid())

###############################################################################
                                        #                Set up               #
###############################################################################

## File names (with and without the "data/" directory)
files_full <- dir("data", full.names = TRUE)
files <- dir("data", full.names = FALSE)

## Dates for each of the files
dates <- files %>% str_sub(1, 10) %>% ymd()

## Names of the sheets of each Excel file
sheet_names <- files_full %>% lapply(excel_sheets)

names(sheet_names) <- dates

sheet_names


###############################################################################
                                        #   Combine county data across dates  #
###############################################################################

countyDfList <- vector("list", length(files))

for (i in 1:length(files)) {

  ## Handle different column names, etc...
  if (i == 1) {
    
    countyDfList[[i]] <- read_xlsx(files_full[i], sheet = "Data") %>%
      mutate(`Population, 16-64\r\n Any Medical Condition` = NA) %>%
      mutate(date = dates[i]) %>%
      rename(`Total Doses Allocated`=`Vaccine Doses Distributed`)
    
  } else if (i == 24) {
    
    countyDfList[[i]] <- read_xlsx(files_full[i], sheet = "By County")
    countyDfList[[i]] <- countyDfList[[i]][, 1:11] %>%
      mutate(date = dates[i])
    
  } else {
    
    countyDfList[[i]] <- read_xlsx(files_full[i], sheet = "By County") %>%
      mutate(date = dates[i])
    
  }

  if (i %in% 2:18) {
    
    countyDfList[[i]] <- countyDfList[[i]] %>%
      rename(`Population, 16-64\r\n Any Medical Condition` =
               `Population, Phase 1B Any Medical Condition`)
    
  }
  
}

countyDf <- countyDfList %>% plyr::rbind.fill()

glimpse(countyDf)

