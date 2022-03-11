pacman::p_load(readr, tidyverse, magrittr, knitr, kableExtra, readstata13, stargazer, pander, 
               captioner, keyring, HMDHFDplus, flextable)


key_list()

# keyring::key_list()
# keyring::key_delete(service = "human-fertility-database", username = "min25@uw.edu")
# keyring::key_delete(service = "human-fertility-database", username = "min25@uw.edu")
# keyring::key_set(service = "human-fertility-database", username = "min25@uw.edu")

library(ISOcodes)
library(HMDHFDplus)

############################################################################### matching the country names 
# HFD country codes
hfdcodes <- getHFDcountries() %>% tibble(ccode = .)

# ISO country codes
isocodes <- ISO_3166_1 %>% tibble() %>% select(ccode = Alpha_3, Name)

# join ISO codes with country names
hfdcodes %<>% left_join(isocodes, by = "ccode")

# there are some countries in the HFD that do not use standard 3 character ISO codes
hfdcodes %>% filter(is.na(Name))

# update those
hfdcodes %<>% 
  mutate(Name = 
           case_when(ccode == "FRATNP" ~  "France",
                     ccode == "DEUTNP" ~  "Germany",
                     ccode == "DEUTE" ~   "East Germany",
                     ccode == "DEUTW" ~   "West Germany",
                     ccode == "GBR_NP" ~  "United Kingdom", 
                     ccode == "GBRTENW" ~ "England and Wales",
                     ccode == "GBR_SCO" ~ "Scotland",
                     ccode == "GBR_NIR" ~ "Northern Ireland",
                     TRUE ~ Name)
  )


hfdcodes %>% 
  kable() %>% 
  kable_styling(bootstrap_options =
                  c("striped", "hover", "condensed", "responsive"), 
                font_size = 12,
                full_width = F, position = "left")


############################################################################### data download
# a function to read HFD for one country and one item
read_hfd_country <- function(CNTRY, item) {
  HMDHFDplus::readHFDweb(
    # the country from the function call
    CNTRY = CNTRY,
    # the item to download
    item = item,
    # the username from this key's record
    username = keyring::key_list("human-fertility-database")$username,
    # the password for this key's record
    password = keyring::key_get(
      service = "human-fertility-database",
      username = keyring::key_list("human-fertility-database")$username
    )
  )
}


# Download a data set iteratively for all named countries using purrr::map()
read_hfd_countries_item <- function(countries, item){
  countries %>%
    # Returns a list of data.frames, adding a column for country code to each
    # the map() function performs a run of Ben's read_hmd_country() 
    #   function for each listed country
    purrr::map_dfr(function(ccode) {
      # the item to read is 1 x 1 death rates
      read_hfd_country(ccode, item) %>%
        # this adds the column "country" storing the country ISO code
        dplyr::mutate(ccode = ccode)
    }) %>%
    # Phil added this to make it a tibble
    tibble() %>% 
    # and add country name
    left_join(hfdcodes, by = "ccode")
}

CNTRIES <- hfdcodes %>% 
  filter(Name %in% c("United States", "Denmark", "Austria")) %>% 
  pull(ccode)

totbirthsRR_USA_LTU_JPN <- read_hfd_countries_item(countries = CNTRIES, item = "totbirthsRR")


totbirthsRR_USA_LTU_JPN %>% 
  mutate(TotalM = Total / 1000000) %>% 
  ggplot( 
    mapping = aes(x = Year, y = TotalM)) +
  geom_line() +
  facet_wrap(~Name, ncol = 1, scales = "free_y") +
  ylab("live births") +
  xlab("year")

