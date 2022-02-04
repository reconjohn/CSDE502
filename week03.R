pacman::p_load(animation, captioner, idbr, htmltools, kableExtra, knitr, 
               leaflet, leafem, leafpop, magrittr, mapview, pander, pander, 
               psych, readstata13, rmapshaper, sf, stargazer, tidyverse, 
               tidycensus, tigris)

table_nums <- captioner(prefix = "Table")
figure_nums <- captioner(prefix = "Figure")

# tidycensus::census_api_key(“*****************,” install = TRUE) 
# Your API key has been stored in your .Renviron and can be accessed by 
# Sys.getenv(“CENSUS_API_KEY”)

v2019 <- load_variables(year = 2019, dataset = "acs5", cache = TRUE)

v2019 %>%
  filter(concept == "RACE") %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, position = "left")


# the census variables
census_vars <- c(
  p_denom_race = "B02001_001",
  p_n_white = "B02001_002",
  p_n_afram = "B02001_003",
  p_n_aian = "B02001_004",
  p_n_asian = "B02001_005"
)


# cache tigris data
options(tigris_use_cache = TRUE)

# get the data
ctdat <- get_acs(
  geography = "tract",
  variables = census_vars,
  cache_table = TRUE,
  year = 2019,
  output = "wide",
  state = "WA",
  county = "King",
  geometry = TRUE,
  survey = "acs5"
) # crs 4269

ctdat %<>% st_transform(4326)
# margin of error (MOE), the estimate is represented with the variable name 
# having the terminal character “E” and the MOE is represented with 
# the variable name having the terminal character “M.”



# print a few records
ctdat %>%
  head() %>%
  kable(caption = "Selected census tract variables from the 5-year ACS from 2019 for King County, WA") %>%
  kable_styling(full_width = FALSE, position = "left")

# get the data
ctdatlong <- get_acs(
  geography = "tract",
  variables = census_vars,
  cache_table = TRUE,
  year = 2019,
  output = "tidy",
  state = "WA",
  county = "King",
  geometry = TRUE,
  survey = "acs5"
)

ctdatlong %>%
  head() %>%
  kable(caption = "Selected census tract variables from the 5-year ACS from 2019 for King County, WA (long format)") %>%
  kable_styling(full_width = FALSE, position = "left")


# the same census API key is used
# idb_api_key(Sys.getenv("CENSUS_API_KEY"))

# print the variables as a table
variables5 %>% DT::datatable()
idb_variables()
idb_concepts()


# get data only if necessary
if(!exists("china_india_data")){
  china_india_data <- get_idb(
    # from China and India
    country = c("China", "India"),
    # years 2000 and 2021
    year = c(2000, 2021),
    # age range 0 to 100 years
    age = 0:100,
    # data for both sexes
    sex = c("male", "female")
  )
}

china_india_data %>%
  # multiply male population by -1 to graph to the left of 0
  mutate(pop = ifelse(sex == "Male", pop * -1, pop)) %>%
  # plot with Y as age, color by sex
  ggplot(aes(x = pop, y = as.factor(age), fill = sex)) +
  # column plot
  geom_col(width = 1) +
  # minimal theme with no background annotations
  theme_minimal(base_size = 15) +
  # scale X with labels in the millions
  scale_x_continuous(labels = function(x) paste0(abs(x / 1000000), "m")) +
  # scale Y with breaks & labels every 10 years
  scale_y_discrete(breaks = scales::pretty_breaks(n = 10)) +
  # define the colors
  scale_fill_manual(values = c("red", "gold")) +
  # set the labels
  labs(
    title = "Population structure of\nChina and India, 2000 and 2021",
    x = "Population",
    y = "Age",
    fill = ""
  ) +
  # facet by country and year
  facet_grid(name ~ year)

inf_mort <- get_idb(
  # from China and India
  country = c("China", "India"),
  # years 2000 and 2021
  year = c(2000, 2021),
  # infant mortality
  variables = c("IMR_F", "IMR_M")
) %>% 
  mutate(female = imr_f,
         male = imr_m)

inf_mort %>% 
  pivot_longer(cols = c(female, male), names_to = "sex", values_to = "count") %>% 
  ggplot(aes(x = factor(sex), y = count)) +
  geom_col() +
  facet_grid(name ~ year) +
  labs(x = "year", y = "deaths per 1000 population")


# proportion Black
ctdat %<>%
  mutate(pct_black = (p_n_aframE / p_denom_raceE * 100) %>% round(1))

# a label
labels <- sprintf("%s<br/>%s%s", ctdat$GEOID, ctdat$pct_black, "%") %>% lapply(htmltools::HTML)

bins <- seq(0, 50, by = 10)
pal <- colorBin(
  palette = "Reds",
  domain = ctdat$pct_black,
  bins = bins
)

bins2 <- seq(0, 50, by = 10)
pal2 <- colorBin(
  palette = "Reds",
  domain = ctdat$pct_black,
  bins = bins2
)

# the leaflet map
m <- leaflet(height = "500px") %>%
  # add polygons from tracts
  addPolygons(
    data = ctdat,
    weight = 1,
    fillOpacity = 0.8,
    # fill using the palette
    fillColor = ~ pal(pct_black),
    # highlighting
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    # popup labels
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    position = "bottomright", pal = pal2, values = ctdat$pct_black,
    title = "% African American",
    opacity = 1
  )
m %>% addTiles()


tgtracts_kc_2019 <- tigris::tracts(state = "WA", county = "King", year = 2019)
tgtracts_kc_2019 %<>%  st_transform(4326)

# the leaflet map
m <- leaflet(height = "500") %>%
  addPolygons(
    data = ctdat,
    stroke = TRUE,
    color = "red",
    weight = 7,
    fill = FALSE
  ) %>%
  addPolygons(
    data = tgtracts_kc_2019,
    stroke = TRUE,
    color = "black",
    weight = 2,
    fill = FALSE
  ) %>%
  addLegend(
    colors = c("red", "black"),
    labels = c("tidyverse generalized", "tigris detailed")
  ) %>%
  addTiles() %>% 
  setView(lng = -122.3603, lat = 47.62231, zoom = 13)

m

ctdat_mv <- ctdat %>%
  mutate(afram_white = (p_n_aframE / p_n_whiteE) %>% round(2))

mapview(ctdat_mv,
        zcol = "afram_white",
        popup = popupTable(ctdat_mv,
                           zcol = c("p_n_aframE", "p_n_whiteE")
        )
)


print(ctdat[1,])
st_geometry(ctdat$geometry)[[1]]


ctdat1 <- ctdat %>% head(1)

pts <- st_coordinates(ctdat$geometry[1]) %>% 
  as_tibble()

ggplot() +
  geom_sf(data = ctdat1) +
  geom_point(data = pts, mapping = aes(x = X, y = Y)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# get water
water_kc <- area_water(state = "WA", county = "King", year = 2019)

# erase water from tracts
tract_nowater <- ms_erase(tgtracts_kc_2019, water_kc) %>% 
  filter(GEOID != "53033990100")

# download census variables
# the census variables
census_vars <- c(
  p_denom_race = "B02001_001",
  p_n_white = "B02001_002",
  p_n_afram = "B02001_003",
  p_n_aian = "B02001_004",
  p_n_asian = "B02001_005"
)
# get the data
ctdat_nogeom <- get_acs(
  geography = "tract",
  variables = census_vars,
  cache_table = TRUE,
  year = 2019,
  output = "wide",
  state = "WA",
  county = "King",
  geometry = FALSE,
  survey = "acs5"
)

# join
ctdat_nowater <- tract_nowater %>% left_join(ctdat_nogeom, by = "GEOID")

# write as a GPKG
# an output dir
myDir <- "//udrive.uw.edu/udrive/csde502_winter_2022/week03"
if(!dir.exists(myDir)){
  dir.create(myDir)
}
# write tracts no water
st_write(obj = ctdat_nowater, dsn = file.path(myDir, "ctdat_nowater.gpkg"), layer = "ctdat_nowater", append = TRUE, delete_layer = TRUE, quiet = TRUE)
# write water
st_write(obj = water_kc, dsn = file.path(myDir, "ctdat_nowater.gpkg"), layer = "water_kc", append = TRUE, delete_layer = TRUE, quiet = TRUE)
# write tracts
st_write(obj = tgtracts_kc_2019, dsn = file.path(myDir, "ctdat_nowater.gpkg"), layer = "tgtracts_kc_2019", append = TRUE, delete_layer = TRUE, quiet = TRUE)