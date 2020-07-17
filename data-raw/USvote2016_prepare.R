
library(tidyverse)
library(haven)


read_dta('data-raw/USvote2016.dta') %>%
  # Reencode from file of "stata"
  mutate(across(where(is.labelled), as_factor)) %>%
  # Preliminary variable selection
  select(
    case_identifier, presvote16post_2016, vote_for_against_2016,
    birthyr_baseline, gender_baseline, race_baseline, educ_2016,
    faminc_baseline
  ) %>%
  # Vote against / in favour
  mutate(against = fct_recode(vote_for_against_2016,
    'no' = 'Vote in favor',
    'yes' = 'Vote against opponent'
  )) %>%
  mutate(against = factor(against, levels = c('no', 'yes'))) %>%
  # Birth year
  mutate(birthyr = as.integer(as.character(birthyr_baseline))) %>%
  mutate(birthyr = cut(
    x = birthyr,
    breaks = c(1920, 1940, 1950, 1960, 1970, 1980, 2000),
    dig.lab = 10, right = FALSE
  )) %>%
  # Education
  mutate(educ = fct_recode(educ_2016,
    'No high school' = 'No HS',
    'High school grad.' = 'High school graduate',
    'Some college' = 'Some college',
    '2-year college' = '2-year',
    '4-year college' = '4-year',
    'Post-grad' = 'Post-grad',
    NULL = 'Skipped',
    NULL = 'Not Asked'
  )) %>%
  #mutate(educ = fct_relevel(educ, '2-year college')) %>%
  # Family income
  mutate(famincome = fct_collapse(faminc_baseline,
    '[0; 30,000)' = c('Less than $10,000', '$10,000 - $19,999', '$20,000 - $29,999'),
    '[30,000; 60,000)' = c('$30,000 - $39,999', '$40,000 - $49,999', '$50,000 - $59,999'),
    '[60,000; 100,000)' = c('$60,000 - $69,999', '$70,000 - $79,999', '$80,000 - $99,999'),
    '[100,000; 150,000)' = c('$100,000 - $119,999', '$120,000 - $149,999'),
    '[150,000; Inf)' = c(
      '$150,000 or more', '$150,000 - $199,999', '$200,000 - $249,999',
      '$250,000 - $349,999', '$350,000 - $499,999', '$500,000 or more'
    ),
    'NA' = c('Skipped', 'Not Asked', 'Prefer not to say')
  )) %>%
  mutate(famincome = fct_recode(famincome, NULL = 'NA')) %>%
  # Race
  mutate(race = fct_lump_min(race_baseline, 100)) %>%
  # Vote
  mutate(vote = fct_lump_n(presvote16post_2016, 2)) %>%
  mutate(vote = fct_recode(vote,
    'Clinton' = 'Hillary Clinton',
    'Trump' = 'Donald Trump'
  )) %>%
  # Identifier
  mutate(idcode = as.integer(case_identifier)) %>%
  # Rename variables
  rename(gender = gender_baseline) %>%
  # Variable selection
  select(idcode, vote, race, educ, gender, birthyr, famincome) -> USvote2016


# Save
save(USvote2016, file = 'data/USvote2016.RData')



