
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param zip_data 
##' @param zz 
##' @return 
##' @author Spencer Woody
clean_zip_data <- function(zip_file, zip_file_date, zz) {

  if (str_detect(zip_file, "2021-01") | str_detect(zip_file, "2021-02")) {
    zip_data <- read_xlsx(zip_file, sheet = "Export Worksheet")
  } else {
    zip_data <- read_xlsx(zip_file, sheet = "By Zip Code")
  }

  ## zip_data <- read_xlsx(zip_file, sheet = "By Zip Code")

  ## Change column names
  colnames(zip_data) <- c("ZIP_CODE", "doses_administered", "one_dose", "fully_vaccinated")

  ## Convert to numeric...
  suppressWarnings(
    zip_data <- zip_data %>%
    mutate(doses_administered = as.numeric(doses_administered),
           one_dose = as.numeric(one_dose),
           fully_vaccinated = as.numeric(fully_vaccinated))
  )
  
  ## Add in ZCTA
  suppressMessages(
    zip_data2 <- zip_data %>%
      left_join(myzz)
  )

  ## Aggregate across all ZIPs in a ZCTA
  suppressMessages(
    zip_data3 <- zip_data2 %>%
    group_by(ZCTA) %>%
    filter(!is.na(ZCTA)) %>% 
    summarize(
      PO_NAME = ifelse(any(is.null(PO_NAME)),
                       "NA",
                       names(which.max(table(PO_NAME)))),
      ## names(which.max(table(PO_NAME))),#most common PO_NAME
                                                 #for the ZCTA
      doses_administered = sum(doses_administered, na.rm=T),
      one_dose = sum(one_dose, na.rm=T),
      fully_vaccinated = sum(fully_vaccinated, na.rm=T)
    ) %>%
      mutate(date = zip_file_date)
  )

  ## Output
  zip_data3

}
