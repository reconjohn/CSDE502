# remotes::install_github("ihmeuw-demographics/hierarchyUtils")
# remotes::install_github("ihmeuw-demographics/demCore")


library(sqldf) # to use sql syntax with data frames
library(knitr) # knitr for kable tables
library(kableExtra) # pretty tables
library(sf) # simple features (GIS)
library(leaflet) # nice maps
library(tools) # md5sum
library(tidyverse)
library(magrittr)
library(demCore)
library(scales)
is(demCore::thailand_data)

str(demCore::thailand_data)
dim(demCore::thailand_data$population)

demCore::thailand_data$population %>%
  head() %>%
  kable() %>%
  kable_styling(
    bootstrap_options =
      c("striped", "hover", "condensed", "responsive"),
    font_size = 12,
    full_width = F, position = "left"
  )

############################################################################### create population projection using CCMPP
is(thailand_initial_estimates)
names(demCore::thailand_initial_estimates)
lapply(demCore::thailand_initial_estimates, FUN = function(x) (is(x)))
str(demCore::thailand_initial_estimates)

demCore::thailand_initial_estimates$srb %>% 
  kable() %>% 
  kable_styling(
    bootstrap_options =
      c("striped", "hover", "condensed", "responsive"),
    font_size = 12,
    full_width = F, position = "left"
  )    


# the settings for this run of ccmpp
thailand_settings <- list(
  # start of each calendar year, i.e., year_start
  years = seq(1960, 1995, 5),
  # use both sexes
  sexes = c("female", "male"),
  # ages being projected
  ages = seq(0, 80, 5),
  # ages for which mortality parameter estimates are available
  ages_mortality = seq(0, 85, 5),
  # assumed female fertility ages
  ages_asfr = seq(15, 45, 5)
)


thailand_population <- ccmpp(
  inputs = demCore::thailand_initial_estimates,
  settings = thailand_settings
)


############################################################################### creating a factor from two variables
thailand_population %<>%
  mutate(ageclass = factor(str_c(age_start, age_end, sep = " to ") %>%
                             fct_reorder(., age_start)))



# max label
maxlev <- length(levels(thailand_population$ageclass))

# current max level
currmaxlev <- levels(thailand_population$ageclass)[maxlev]

# substitute
updatemaxlev <- currmaxlev %>% 
  str_replace(pattern = "to Inf",
              replacement = "plus")

# update
levels(thailand_population$ageclass)[maxlev] <- updatemaxlev


ggplot(data = thailand_population,
       mapping = aes(x = year, y = value)) +
  geom_line() +
  # this stratifies the graph by sex and age class
  facet_grid(ageclass ~ sex, scales = "free_y", # remove spaces 
             labeller = labeller(age = thailand_population$ageclass)) +
  # do not use a constant Y scale otherwise the older age classes would look flat.
  # and show the Y axis values with commas
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  # change the axis labels
  xlab("Year") +
  ylab("Population")


############################################################################### health data cleaning 

# unzip the file
if(file.exists("data/AHwave1_v1.dta.zip") & !file.exists("data/AHwave1_v1.dta")){
  unzip(zipfile = "data/AHwave1_v1.dta.zip", exdir = "data")
}

## read the data by haven
AHwave1_v1_haven <- haven::read_dta(file = "data/AHwave1_v1.dta")

str(AHwave1_v1_haven$imonth)
head(AHwave1_v1_haven$bio_sex)
attributes(AHwave1_v1_haven$h1gi1m)


AHwave1_v1_haven_metadata <- bind_cols(
  # variable name
  varname = colnames(AHwave1_v1_haven),
  # label
  varlabel = lapply(AHwave1_v1_haven, function(x) attributes(x)$label) %>% 
    unlist(),
  # format
  varformat = lapply(AHwave1_v1_haven, function(x) attributes(x)$format.stata) %>%
    unlist(),
  # values
  varvalues = lapply(AHwave1_v1_haven, function(x) attributes(x)$labels) %>% 
    # names the variable label vector 
    lapply(., function(x) names(x)) %>% 
    # as character
    as.character() %>% 
    # remove the c() construction
    str_remove_all("^c\\(|\\)$")
)

DT::datatable(AHwave1_v1_haven_metadata)


## read the data by readstata13
AHwave1_v1_rs13 <- readstata13::read.dta13(file = "data/AHwave1_v1.dta")

# read the data
AHwave1_v1_rs13 <- readstata13::read.dta13(file = "data/AHwave1_v1.dta", generate.factors = TRUE, nonint.factors = TRUE)

AHwave1_v1_rs13_metadata <- bind_cols(
  varname = colnames(AHwave1_v1_rs13),
  varlabel = attributes(AHwave1_v1_rs13)$var.labels,
  varformat = attributes(AHwave1_v1_rs13)$formats
)

# value ranges; need to do this separately because those variables with no value labels were not accounted for
varvalues <- bind_cols(
  varname = names(attributes(AHwave1_v1_rs13)$label.table) %>% tolower,
  vals = attributes(AHwave1_v1_rs13)$label.table %>% 
    lapply(., function(x) names(x)) %>% 
    as.character() %>% 
    str_remove_all("^c\\(|\\)$"))

# join
AHwave1_v1_rs13_metadata %<>% 
  left_join(varvalues, by = "varname")

DT::datatable(AHwave1_v1_rs13_metadata)


head(x = AHwave1_v1_rs13$imonth, n = 6)

AHwave1_v1_rs13 %>% 
  head(10) %>% 
  filter(imonth == "(6) June") %>% 
  select(aid, imonth, iday)
levels(AHwave1_v1_rs13$imonth) %>% t() %>% t()


head(AHwave1_v1_haven$imonth)


AHwave1_v1_haven %>% 
  head(10) %>% 
  filter(imonth == 6) %>% 
  select(aid, imonth, iday)

# unzip and read in the larger data set
if(file.exists("data/21600-0001-Data.dta.zip") & !file.exists("data/21600-0001-Data.dta")){
  unzip(zipfile = "data/21600-0001-Data.dta.zip", exdir = "data")
}

# because this is a big file we might want to check if it has been read
if(!exists("data_21600_0001")){
  data_21600_0001 <- haven::read_dta(file = "data/21600-0001-Data.dta")
}

# dimensions of the two
dim(AHwave1_v1_haven)

dim(data_21600_0001)

# lowercase the column names
colnames(data_21600_0001) %<>% str_to_lower()

# select() some columns of the same name
dat <- data_21600_0001 %>% 
  select(colnames(AHwave1_v1_haven))

# identical?
identical(dat, AHwave1_v1_haven)


# a generic(?) function to generate metadata for a Stata file read by haven::read_dta()
# x is a data frame from haven::read_dta
f_haven_stata_metadata <- function(x){
  # variable names
  varname <- colnames(x)
  # labels
  varlabel <- x %>% 
    lapply(., function(x) attributes(x)$label) %>% 
    unlist()
  # format
  varformat <- x %>% 
    lapply(., function(x) attributes(x)$format.stata) %>%
    unlist()
  # values
  varvalues <- x %>% 
    lapply(., function(x) attributes(x)$labels) %>% 
    # names the variable label vector
    lapply(., function(x) names(x)) %>% 
    # as character
    as.character() %>% 
    # remove the c() construction
    str_remove_all("^c\\(|\\)$")  
  
  bind_cols(varname = varname, 
            varlabel = varlabel, 
            varformat = varformat,
            varvalues = varvalues)
}

# generate the metadata
data_21600_0001_metadata <- f_haven_stata_metadata(data_21600_0001)

# print the metadata table as a DT::datatable
DT::datatable(data_21600_0001_metadata)



# a function to get matching strings in a PDF, ignore case
f_pdf_str_match <- function(x, pat, ignore.case = TRUE){
  # convert the PDF to text
  mytext <- pdf_text(x)
  # pattern
  if(ignore.case){
    mypat <- regex(pat, ignore_case = TRUE)
  } else {
    mypat <- pat
  }
  # match strings = pages
  pages <- str_which(string = mytext, pattern = mypat)
  if(length(pages) == 0){
    return(data.frame(fname = x, pat, page_num = as.integer(NA), ignore.case))
  }
  # create a data frame
  data.frame(fname = x, pat, page_num = pages, ignore.case)
}

# a list of my PDFs
mypdfs <- list.files(path = "data/metadata/Wave1_InHome_Codebooks", pattern = "*.pdf$", full.names = TRUE)

# an empty data frame
x <- NULL

# run each one
for(i in mypdfs){
  x <- rbind(x, f_pdf_str_match(i, "black", ignore.case = TRUE))
}

# ignore NAs
x %>% filter(!is.na(page_num))

