
library(readr)
library(ggplot2)
library(tidyverse)

df <- read_csv(file = "turnover_babushkin.csv")


df %>% 
  mutate(gender = factor(gender, levels = c("m", "f"))) %>%
  ggplot(aes(x = gender, y = anxiety)) +
  geom_boxplot()


df %>% 
  group_by(selfcontrol, independ) %>% 
  summarise(n = n(), .groups = "drop_last")


df %>% 
  group_by(selfcontrol, independ) %>% 
  summarise(n = n()) %>%
  arrange(desc(n))
  



df %>% 
  ggplot(aes(x = selfcontrol, y = independ))+
  geom_point(alpha=.5, position=position_jitter(h=.3, w=.3))



# load required packages
library(HMDHFDplus)
library(keyring)
library(tidyverse)

# get the keys
myKeys <- key_list()

# Set your password for Human Mortality Database (HMD)
# does a key exist? if not, create one
if (key_list(service = "human-mortality-database") %>% nrow() == 0) {
  keyring::key_set(
    service = "human-mortality-database",
    username = "min25@uw.edu"
  )
  # Enter your HMD password in the prompt
}

# Set your password for Human Fertility Database (HFD)
if (key_list(service = "human-fertility-database") %>% nrow() == 0) {
  keyring::key_set(
    service = "human-fertility-database",
    username = "min25@uw.edu"
  )
  # Enter your HFD password in the prompt
}

# Running for a single country with item left NULL lists available series
# for that country and ask for user entry of desired item
HMDHFDplus::readHMDweb(
  CNTRY = "KOR",
  username = keyring::key_list("human-mortality-database")$username,
  password = keyring::key_get(
    service = "human-mortality-database",
    username = keyring::key_list("human-mortality-database")$username
  )
)

key_list()

# Function to download a specified HMD dataset item for a single county
read_hmd_country <- function(CNTRY, item) {
  HMDHFDplus::readHMDweb(
    CNTRY = CNTRY,
    item = item,
    username = keyring::key_list("human-mortality-database")$username,
    password = keyring::key_get(
      service = "human-mortality-database",
      username = keyring::key_list("human-mortality-database")$username
    )
  )
}

# Help function to list the available countries
countries <- HMDHFDplus::getHMDcountries()

# Download a dataset iteratively for all countries using purrr::map()
# In this case, age-specific mortality in 1-year periods x 1-year age groups
# for all 1-year periods available
mx_1x1 <- countries %>%
  # Returns a list of data.frames, adding a column for country code to each
  purrr::map(function(country) {
    read_hmd_country(country, "Mx_1x1") %>%
      dplyr::mutate(country = country)
  }) %>%
  # Combines the data.frames into a single data.frame
  dplyr::bind_rows()


mx_1x1 %>% 
  filter(country %in% c("KOR","JPN","USA")) %>% 
  filter(Year > 2002 & Year < 2019) %>% 
  gather("gender", "value", Female, Male) %>% 
  ggplot(aes(x = Age, y = value, group = Year, color = gender)) +
  geom_line(size = 1)+
  facet_grid(Year~ country) + 
  labs(y = "Mortality from 2003 - 2018")+
  theme_bw()
  


mx_1x1 %>% 
  filter(country %in% c("KOR","JPN","USA")) %>% 
  filter(Year > 2000 & Year < 2019) %>% 
  gather("gender", "value", Female, Male) %>% 
  group_by(country, gender, Age) %>% 
  summarise(mean = mean(value)) %>% 
  ggplot(aes(x = Age, y = mean, group = country, color = country)) +
  geom_line(size = 1)+
  scale_x_continuous(breaks = seq(0, 115, by =10),)+
  facet_wrap(~ gender)+
  labs(y = "Average Mortality from 2003 - 2018")+
  theme_bw()

