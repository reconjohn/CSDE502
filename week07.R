
pacman::p_load(
  tidyverse,
  magrittr,
  knitr,
  kableExtra,
  readstata13,
  haven,
  pdftools,
  curl,
  ggplot2,
  captioner
)



if (!exists("data/AHwave1_v1_haven")) {
  AHwave1_v1_haven <- haven::read_dta("http://staff.washington.edu/phurvitz/csde502_winter_2021/data/AHwave1_v1.dta")
}

# labels is the specific levels while label is the meta data description
attributes(AHwave1_v1_haven$h1gi1m)$labels %>%
  t() %>%
  t()

############################################################################### csv vs. rds difference 
# temp dir
mytempdir <- tempdir()

write.csv(x = AHwave1_v1_haven, file = file.path(mytempdir, "AHwave1_v1_haven.csv"), row.names = FALSE)
saveRDS(object = AHwave1_v1_haven, file = file.path(mytempdir, "AHwave1_v1_haven.RDS"))


AHwave1_v1_haven_csv <- read.csv(file = file.path(mytempdir, "AHwave1_v1_haven.csv"))
is(AHwave1_v1_haven_csv)

AHwave1_v1_haven_csv %>%
  attributes() %>%
  map(~ head(.)) # map function 

AHwave1_v1_haven_csv$h1gi1m %>%
  attributes()

AHwave1_v1_haven_rds <- readRDS(file = file.path(mytempdir, "AHwave1_v1_haven.RDS"))
is(AHwave1_v1_haven_rds)

AHwave1_v1_haven_rds %>%
  attributes() %>%
  map(~ head(.))

AHwave1_v1_haven_rds$h1gi1m %>%
  attributes()

############################################################################### Creating factor variables
AHwave1_v1_haven$h1gh1 %>%
  attributes()

head(AHwave1_v1_haven$h1gh1)

AHwave1_v1_haven$health <- factor(AHwave1_v1_haven$h1gh1)

# look at the first few values of the factor variable
head(AHwave1_v1_haven$health)


# extract the labels from the column attribute
health_levels <- AHwave1_v1_haven$h1gh1 %>%
  attributes() %>%
  extract2("labels") %>%
  names()

# create the factor variable and re-level it in reverse order
AHwave1_v1_haven$health <- factor(AHwave1_v1_haven$h1gh1,
                                  labels = health_levels,
                                  ordered = TRUE
) %>%
  fct_re # reverse factor 

AHwave1_v1_haven$health %>% 
  levels() %>% 
  t() %>% t()

# "raw" variable
(tab_h1gh1 <- AHwave1_v1_haven %>%
    group_by(h1gh1) %>%
    summarise(n = n()))


# factor variable
(tab_health <- AHwave1_v1_haven %>%
    group_by(health) %>%
    summarise(n = n()))

ggplot(data = tab_h1gh1, aes(x = h1gh1, y = n)) +
  geom_bar(stat = "identity") +
  coord_flip()

ggplot(data = tab_health, mapping = aes(x = health, y = n)) +
  geom_bar(stat = "identity") +
  coord_flip()


# filter for Excellent or Very good
y <- AHwave1_v1_haven %>%
  filter(health <= "(2) Very good")

# tabulate
y %>%
  group_by(health) %>%
  summarise(n = n())


x <- AHwave1_v1_haven %>%
  filter(health %>% as.numeric() <= 6)

# tabulate
x %>%
  group_by(health) %>%
  summarise(n = n())


# number of labels
nlabs <- length(unique(AHwave1_v1_haven$h1gi9))

# get the values, "as.numeric()"
obsrace_values <- AHwave1_v1_haven$h1gi9 %>%
  attributes() %>%
  extract2("labels") %>%
  as.numeric()

# get the labels, names()
obsrace_labels <- AHwave1_v1_haven$h1gi9 %>%
  attributes() %>%
  extract2("labels") %>%
  names()

# create the factor
AHwave1_v1_haven$obsrace <- factor(AHwave1_v1_haven$h1gi9,
                                   levels = obsrace_values,
                                   labels = obsrace_labels
)


dat_wb1 <- AHwave1_v1_haven %>%
  filter(obsrace == "(1) White" |
           obsrace == "(2) Black/African American")

dat_wb1 %>%
  group_by(obsrace) %>%
  summarise(n = n())


dat_wb2 <- AHwave1_v1_haven %>%
  filter(str_detect(obsrace, regex("white|black", ignore_case = TRUE))) # string detect 

dat_wb2 %>%
  group_by(obsrace) %>%
  summarise(n = n())


dat_wb3 <- AHwave1_v1_haven %>%
  filter(obsrace %>% as.numeric() %in% c(1, 2))

dat_wb3 %>%
  group_by(obsrace) %>%
  summarise(n = n())


str(AHwave1_v1_haven$health)


write.csv(x = AHwave1_v1_haven, file = file.path(mytempdir, "foo.csv"), row.names = FALSE)
y <- read.csv(file.path(mytempdir, "foo.csv"))
str(y$health)



y$health <- factor(y$health,
                   labels = y$health %>% unique() %>% sort(),
                   ordered = TRUE
)
head(y$health)



write_rds(x = AHwave1_v1_haven, file = file.path(mytempdir, "foo.Rds"))
z <- readRDS(file = file.path(mytempdir, "foo.Rds"))
head(z$health)


############################################################################### Creating attributes
y <- read.csv(file.path(mytempdir, "foo.csv"))

y %>%
  attributes() %>%
  map(~ head(.))


attributes(y)$label <- "National Longitudinal Study of Adolescent to Adult Health (Add Health), 1994-2000 with some variable additions"

y %>%
  attributes() %>%
  extract("label")

# label for health
attributes(y$health)$label <- "General health from public Add Health Wave 1 Section 3 question 1"

# labels for health
healthlevels <- unique(y$health) %>%
  sort()

# values for health
attributes(y$health)$levels <- healthlevels

# create the factor "health" with appropriate levels, order it and reverse the levels
#   so that Excellent is highest
y %<>% mutate(
  health =
    factor(health, levels = healthlevels, ordered = TRUE) %>%
    fct_rev()
)


# label for obsrace
attributes(y$obsrace)$label <- "Interviewer observation of race from public Add Health Wave 1 Section 1 question 9"

# obsrace levels
obsracelevels <- unique(y$obsrace) %>%
  sort()

# values  for obsrace
attributes(y$obsrace)$levels <- obsracelevels

# create a factor
y %<>% mutate(
  obsrace = factor(obsrace, levels = obsracelevels)
)

y$health %>% attributes()

head(y$health)

y$obsrace %>% attributes()
head(y$obsrace)


############################################################################### tabulation 

# download and unzip the larger data set
myUrl <- "http://staff.washington.edu/phurvitz/csde502_winter_2021/data/21600-0001-Data.dta.zip"

# zip file in $temp -- basename gets just the file name from the URL and not the URL path;
#   file.path stitches the tempdir() path to the file name
zipfile <- file.path(mytempdir, basename(myUrl))

# dta file in $temp
dtafile <- tools::file_path_sans_ext(zipfile)

# check if the dta file exists
if (!file.exists(dtafile)) {
  # if the dta file doesn't exist, check for the zip file
  # check if the zip file exists, download if necessary
  if (!file.exists(zipfile)) {
    curl::curl_download(url = myUrl, destfile = zipfile)
  }
  # unzip the downloaded zip file
  if (file.exists(zipfile)) {
    unzip(zipfile = zipfile, exdir = mytempdir)
  }
}

# if the data set has not been read, read it in
if (!exists("ahcomplete")) {
  ahcomplete <- haven::read_dta(dtafile)
}
# lowercase column names
colnames(ahcomplete) %<>% str_to_lower()


# create a data frame of the variable names and labels
ahcomplete_metadata <- bind_cols(
  varname = colnames(ahcomplete),
  varlabel = ahcomplete %>% map(~ attributes(.)$label) %>% unlist()
)

# print the table with DT::datatable for interactive display
DT::datatable(ahcomplete_metadata)


attributes(ahcomplete$h1gh59a)$labels
attributes(ahcomplete$h1gh59b)$labels
attributes(ahcomplete$h1gh60)$labels



# make the data frame
htwt <- ahcomplete %>%
  # select columns
  select(
    feet = h1gh59a,
    inches = h1gh59b,
    weight_lb = h1gh60,
    health = h1gh1,
    obsrace = h1gi9
  ) %>%
  # filter for valid values
  filter(feet < 90 & inches < 90 & weight_lb < 900) %>%
  # calculate metric units and BMI
  mutate(
    height_m = (feet * 12 + inches) / 39.37008,
    weight_kg = weight_lb / 2.205,
    BMI = weight_kg / height_m^2
  )

# factor: get values and labels
healthvals <- htwt$health %>%
  attributes() %>%
  extract2("labels") %>%
  as.numeric()

healthlabs <- htwt$health %>%
  attributes() %>%
  extract2("labels") %>%
  names()

racevals <- htwt$obsrace %>%
  attributes() %>%
  extract2("labels") %>%
  as.numeric()

racelabs <- htwt$obsrace %>%
  attributes() %>%
  extract2("labels") %>%
  names()

# package the data frame up
htwt %<>%
  mutate(
    health = factor(health,
                    levels = healthvals,
                    labels = healthlabs
    ),
    obsrace = factor(obsrace,
                     levels = racevals,
                     labels = racelabs
    )
  )

############################################################################### Making categorical variable 
# get the 5th & 85th percentile
bmibreaks <- quantile(x = htwt$BMI, probs = c(0.05, 0.85))
ggplot(htwt, aes(x = BMI)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = bmibreaks)


htwt %<>%
  mutate(bmiclass = cut(
    x = BMI,
    breaks = c(min(BMI), bmibreaks, max(BMI)),
    labels = c("underweight", "normal", "overweight"),
    include.lowest = TRUE
  ) %>%
    factor(ordered = TRUE))


############################################################################### Making a table 

# base R
table(htwt$bmiclass, useNA = "ifany")


# tidyR
htwt %>%
  group_by(bmiclass) %>%
  summarise(n = n()) %>%
  kable() %>%
  kable_styling(full_width = FALSE, position = "left", 
                bootstrap_options = c("striped", "hover", "condensed", "responsive"))

htwt %>%
  group_by(obsrace) %>%
  summarise(n = n()) %>%
  kable() %>%
  kable_styling(
    full_width = FALSE, position = "left",
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  )

round(prop.table(table(htwt$bmiclass)), 2)

round(prop.table(table(htwt$bmiclass)) * 100, 0)


htwt %>%
  group_by(obsrace) %>%
  summarise(n = n()) %>%
  mutate(`%` = n / sum(n) * 100) %>%
  mutate(`%` = `%` %>% round(1)) %>%
  kable() %>%
  kable_styling(
    full_width = FALSE, position = "left",
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  )


htwt %>%
  group_by(
    obsrace,
    bmiclass
  ) %>%
  summarise(n = n(), .groups = "drop_last") %>% 
  # be careful with "drop_last" allowing proportional value per group
  # if you want to ungroup, use "drop" 
  mutate(`%` = n / sum(n) * 100) %>%
  mutate(`%` = `%` %>% round(1)) %>%
  kable() %>%
  kable_styling(
    full_width = FALSE, position = "left",
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  )


# create the row groupings
(obsrace <- htwt %>%
    group_by(
      obsrace,
      bmiclass
    ) %>%
    summarise(n = n(), .groups = "drop_last") %>% 
    group_by(obsrace) %>% 
    summarise(n = n()) %>% 
    deframe()) # remove dataframe


# super row creation 
htwt %>%
  group_by(
    obsrace,
    bmiclass
  ) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(`%` = n / sum(n) * 100) %>%
  mutate(`%` = `%` %>% round(1)) %>% 
  kable %>% 
  kable_styling(
    full_width = FALSE, position = "left",
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  ) %>% 
  pack_rows(index = obsrace) 


htwt %>%
  group_by(
    bmiclass,
    obsrace
  ) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(`%` = n / sum(n) * 100) %>%
  mutate(`%` = `%` %>% round(1)) %>%
  kable() %>%
  kable_styling(full_width = FALSE, position = "left",
                bootstrap_options = c("striped", "hover", "condensed", "responsive")
  )



bmi_race <- htwt %>%
  group_by(
    obsrace,
    bmiclass
  ) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(`%` = n / sum(n) * 100) %>%
  filter(!str_detect(obsrace, regex("refused|know", ignore_case = TRUE)))

ggplot(data = bmi_race, mapping = aes(x = bmiclass, y = `%`)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_grid(~obsrace) +
  xlab("BMI class")


ggplot(data = bmi_race, mapping = aes(x = obsrace, y = `%`, fill = bmiclass)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_discrete(name = "BMI class") +
  xlab("observed race")

