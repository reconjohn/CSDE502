


# if (!interactive()) {
#   fnamepath <- as.character(sys.call(1))[2]
# }
# dirname(rstudioapi::getActiveDocumentContext()$path) # To set current folder as your wd.



pacman::p_load(demogR, demography, magrittr, knitr, kableExtra, readstata13, captioner,tigris, sf, tidyverse)

f_compare <- function(x, y){
  # either missing?
  if(nargs() != 2)
    return("invalid number of arguments")
  # numeric?
  if(!is.numeric(x) | !is.numeric(y)){
    return(sprintf("%s or %s is not numeric.", x, y))
  }
  # comparisons follow
  if(x > y){
    return(sprintf("%s is greater than %s", x, y))
  } 
  if(x < y) {
    return(sprintf("%s is less than %s", x, y))
  }
  if(x == y){
    return(sprintf("%s equals %s", x, y))
  }
}

f_compare(1)

f_compare(1, 2)


f_readfile <- function(fname){
  if(!file.exists(fname)){
    warning(paste(fname, "does not exist!"))
    return()
  } else {
    read.csv(fname)
  }
}

f_readfile("foobar.txt")



f_readfile <- function(fname){
  if(!file.exists(fname)){
    warning(paste(fname, "does not exist!"))
    return(invisible())
  } else {
    read.csv(fname)
  }
}

f_readfile("foobar.txt")


# declare a few variables
x <- 1
y <- "hello"

# a simple function
f <- function(x){
  # create a local variable
  y <- x + 2
  # another function inside this function
  g <- function(x){
    x * 3
  }
  # what variables are in this environment?
  print("----------")
  print("objects in this function's environment:")
  print(ls())
  # what is in the global env?
  print("----------")
  print("objects in the global environment:")
  print(ls(envir = .GlobalEnv))
  # return the output of the function
  print("----------")
  y
}

f(1)


# a function to show how we can assign
g <- function(x){
  # code for a bunch of complicated operations
  # ...
  # create environment "foo"
  if(!exists("foo")){
    message("make foo")
    assign(x = "foo", value = new.env(), envir = .GlobalEnv)
  }    
  # generates an intermediate data frame named "bar"
  bar <- head(iris)
  # save to the foo env
  assign(x = "bar", value = bar, envir = foo)
  # more code to do more complicated stuff
  # ...
  foobar <- head(cars)
  # also assign 
  assign(x = "foobar", value = foobar, envir = foo)
  # yet more complicated stuff here
  # ...
}


# run the function
g()


# what is in environment "foo"?
ls(envir = foo)


print(foo$bar)

print(foo$foobar)


ls(envir = .GlobalEnv)

# take the first 5 state names
states_5 <- head(state.name, 5)

# iterate over those
for (i in 1:length(states_5)){
  s <- states_5[i]
  message(paste0(i, ": ", s))
}

# initialize a value to hold a sum
mySum <- 0

# create a data frame of 5 rows
y <- iris %>% head(5)

# loop
for(x in 1:nrow(y)){
  message(paste("iteration:", x))
  # get the sepal length for this iteration
  sl <- y$Sepal.Length[x]
  # add to make a cumulative sum
  mySum <- mySum + sl
  # calculate the mean
  myMean <- mySum / x
  message(paste0("  sepal length = ", sl, 
                 "; cumulative sum = ", mySum, 
                 "; mean = ", myMean))
  
}




# make it reproducible
set.seed(5)

# create a list
L <- list(
  v1 = rnorm(n = 10, mean = 1),
  v2 = rpois(n = 20, lambda = 5),
  v3 = runif(n = 25, min = 0, max = 10)
)

# run the loop
system.time(
  for(i in L){
    print(mean(i))
  }
)

system.time(
  Lmean <- lapply(X = L, FUN = mean)
)




# the tigris download function for counties
f_county <- function(state_name, year = 2019){
  # this downloads a single county
  counties(state = state_name, year)
}

# the map_dfr() functionality is wrapped in here
get_counties <- function(year = 2019) {
  # the output is a data frame. input (.x) is the state name, the function (.f) is the county download
  map_dfr(
    # input is the built in vector "state.name"
    .x = state.name,
    # each iteration runs the f_county() function over the iteration's state
    .f = function(x) {
      f_county(state_name = x, year = year)
    }
  )
}

# run the function
all_counties <- get_counties()

# export to GPKG
myTmpDir <- tempdir()
st_write(obj = all_counties, dsn = file.path(myTmpDir, "counties.gpkg"), layer = "us_counties_2019", delete_dsn = TRUE)




mtcars %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .x)) %>%
  map(summary) %>%
  map_dbl("r.squared")


tempdir()
getwd()


# create the population
# 1 indicates female and 0 indicates male
pop <- c(rep(1, 5e4 * 3 / 5), rep(0, 5e4 * 2 / 5))

# initialize a vector
F <- NULL

# run the bootstrap
for (i in seq(from = 1, to = 5000, by = 1)){
  # sample once
  s <- sample(x = pop, size = 100, replace = TRUE)
  # calculate percent female
  p <- sum(s) / length(s)
  # concatenate the result with the running result
  F <- c(F, p)
}

# mean and standard deviation of the bootstrap
mean(F)

1.96*sd(F)/sqrt(length(F)) + mean(F)

# 95% CI
ci_95 <- Rmisc::CI(x = F, ci = 0.95)

# plot with 95 % CI
plot(density(F), main = "")
abline(v = ci_95, col=c(2,1,2))



# load the Goodman data from the demogR package
data(goodman)

## default type="kf", Venezuela data
vlt <- with(goodman, life.table(x=age, nKx=ven.nKx, nDx=ven.nDx))

## US life table
ult <- with(goodman, life.table(x=age, nKx=usa.nKx, nDx=usa.nDx))

## Madagascar life table
mlt <- with(goodman, life.table(x=age, nKx=mad.nKx, nDx=mad.nDx))

# some values for the text
vlx35 <- vlt %>% filter(x == 35) %>% pull(lx) %>% round(2)
vlx80 <- vlt %>% filter(x == 80) %>% pull(lx) %>% round(2)
ulx35 <- ult %>% filter(x == 35) %>% pull(lx) %>% round(2)
ulx80 <- ult %>% filter(x == 80) %>% pull(lx) %>% round(2)
mlx35 <- mlt %>% filter(x == 35) %>% pull(lx) %>% round(2)
mlx80 <- mlt %>% filter(x == 80) %>% pull(lx) %>% round(2)

# combine these
lt <- bind_rows(vlt, ult, mlt)

lt %>% kable() %>% 
  kable_styling(bootstrap_options =
                  c("striped", "hover", "condensed", "responsive"), 
                full_width = F, position = "left", font_size = 12, fixed_thead = T) %>% 
  pack_rows(group_label = "Venezuela", start_row = 1, end_row = 19) %>% 
  pack_rows(group_label = "USA", start_row = 20, end_row = 38) %>% 
  pack_rows(group_label = "Madagascar", start_row = 39, end_row = 57)

lt %<>% mutate(
  country = c(rep("VEN", 19), rep("USA", 19), rep("MDG", 19))
)
ggplot(data = lt, mapping = aes(x = x, y = lx, col = country))+
  geom_line() +
  xlab("age") +
  ylab("probability of surviving to age on X axis")


france.lt <- lifetable(fr.mort)
plot(france.lt)
lt1990 <- print(lifetable(fr.mort,year=1990))

# create the life table
lt_france_2005 <- lifetable(fr.mort,year=2005)

# print using the native interface
print(lt_france_2005)


# I looked at the individual vectors output from the lifetable function. These are the ones we want:
positions <- c(1,3:10)

# use lapply() across some of the list objects to
#   get the object (a vector), that is the anonymous "function(x) x"
# use "do.call" to do something to each of those vectors. that something is a "cbind"
# turn it into a data frame and then a tibble
lt_france_2005_df <- do.call(cbind, lapply(lt_france_2005[positions], function(x) x)) %>% 
  data.frame %>% tibble()

# set the column names from the names of the vectors
colnames(lt_france_2005_df) <- names(lt_france_2005)[positions]

# print a nice table
lt_france_2005_df %>% kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, position = "left", font_size = 12, )
