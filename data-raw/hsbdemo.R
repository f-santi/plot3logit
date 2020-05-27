
library(tidyverse)
library(haven)

# source:
# https://select-statistics.co.uk/blog/analysing-outcomes-with-multiple-categories/
# https://stats.idre.ucla.edu/stat/data/hsbdemo.dta
read_dta('data-raw/hsbdemo.dta') %>%
  mutate_if(is.labelled, as_factor) -> hsbdemo

save(hsbdemo, file = 'data/hsbdemo.RData')



