
library(tidyverse)
library(haven)


read_dta('data-raw/USvote2016.dta') %>%
  # Reencode from file of "stata"
  mutate_if(is.labelled, as_factor) %>%
  # Preliminary variable selection
  select(
    case_identifier, presvote16post_2016, vote_for_against_2016,
    birthyr_baseline, gender_baseline, race_baseline, educ_baseline,
    faminc_baseline
  ) %>%
  # Birth year
  mutate(birthyr = as.integer(as.character(birthyr_baseline))) %>%
  # Race
  mutate(race = fct_lump_min(race_baseline, 100)) %>%
  # Vote
  mutate(vote = fct_lump_n(presvote16post_2016, 2)) %>%
  mutate(vote = fct_recode(vote,
    'Clinton' = 'Hillary Clinton',
    'Trump' = 'Donald Trump'
  )) %>%
  # Vote against / in favour
  mutate(against = fct_recode(vote_for_against_2016,
    'no' = 'Vote in favor',
    'yes' = 'Vote against opponent'
  )) %>%
  mutate(against = factor(against, levels = c('no', 'yes'))) %>%
  # Rename variables
  rename(
    idcode = case_identifier, famincome = faminc_baseline,
    educ = educ_baseline, gender = gender_baseline
  ) %>%
  # Variable selection
  select(idcode, vote, race, educ, gender, birthyr, famincome) -> USvote2016


# Save
save(USvote2016, file = 'data/USvote2016.RData')



