
library(readr)

df <- read_csv(file = "turnover_babushkin.csv")


unique(df$hire_source) %>% 
  sort()

df$profession %>% table() %>% as.data.frame()


df$age <- round(df$age,0)
prop.table(table(df$age)) %>% as.data.frame()


df %>% 
  select(selfcontrol,anxiety, gender) %>% 
  gather("key","value",selfcontrol,anxiety) %>% 
  mutate(gender = factor(gender)) %>% 
  group_by(gender,key) %>% 
  summarise(count = n(),
            mean = mean(value),
            sd = sd(value))


df %>% 
  select(independ,profession) %>% 
  group_by(profession) %>% 
  summarise(count = n(),
            mean = mean(independ),
            sd = sd(independ)) %>% 
  arrange(desc(mean))
