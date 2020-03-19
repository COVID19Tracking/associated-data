library(tidycensus)
library(tidyverse)
library(datasets)

#add state info from base R

states <- tibble(state_name = state.name,
                 state = state.abb)

#get state-level estimates

st <- get_estimates('state','population') %>%
  pivot_wider(names_from = variable,values_from=value) %>%
  #rename columns
  rename(state_name = 1,
         geo_id = 2,
         population = 3,
         pop_density = 4) %>%
  left_join(states,by='state_name') %>%
  #fix PR and DC
  mutate(state = ifelse(state_name == 'Puerto Rico','PR',
                      ifelse(state_name == 'District of Columbia','DC',state))) %>%
  #fix column order
  select(state,everything())

write.table(st,'us_census_2018_population_estimates_states.csv',quote = FALSE,row.names = FALSE,sep=',')

#get county level estimates

ct <- get_estimates('county','population') %>%
  pivot_wider(names_from = variable,values_from=value) %>%
  rename(county = 1,
         geo_id = 2,
         population = 3,
         pop_density = 4) %>%
  separate(county,into=c('county','state_name'),sep=', ') %>%
  mutate(county = trimws(str_replace_all(county,fixed(' County'),''))) %>%
  left_join(states,by='state_name') %>%
  mutate(state = ifelse(state_name == 'Puerto Rico','PR',
                        ifelse(state_name == 'District of Columbia','DC',state))) %>%
  #fix column order
  select(state,everything())

write.table(ct,'us_census_2018_population_estimates_counties.csv',quote = FALSE,row.names = FALSE,sep=',')

#get it by age

ct2 <- get_estimates('state','characteristics',breakdown = 'AGEGROUP',breakdown_labels = TRUE)

#get all possible agegroups
agegroup_possible <- ct2 %>% distinct() %>% pull(AGEGROUP)

#make list of agegroups to filter to

ages <- c( "Age 0 to 4 years", "Age 5 to 9 years",
  "Age 10 to 14 years", "Age 15 to 19 years", "Age 20 to 24 years",
  "Age 25 to 29 years", "Age 30 to 34 years", "Age 35 to 39 years",
  "Age 40 to 44 years", "Age 45 to 49 years", "Age 50 to 54 years",
  "Age 55 to 59 years", "Age 60 to 64 years", "Age 65 to 69 years",
  "Age 70 to 74 years", "Age 75 to 79 years", "Age 80 to 84 years",
  "Age 85 years and older")

#risk data by age from WHO

risks <- tibble(agegroup = ages,
                cfr_risk = c(0,0,.002,.002,.002,.002,.002,.002,.004,.004,.013,.013,.036,.036,.08,.08,.148,.148))

ct3 <- ct2 %>%
  rename(geo_id = 1,
         state_name = 2,
         value = 3,
         agegroup = 4) %>%
  #filter down to key age buckets
  filter(agegroup %in% ages) %>%
  rename(population = value) %>%
  left_join(states,by='state_name') %>%
  group_by(state) %>%
  mutate(pct_pop = population / sum(population),
         agegroup = as.character(agegroup)) %>%
  ungroup() %>%
  mutate(state = ifelse(state_name == 'Puerto Rico','PR',
                             ifelse(state_name == 'District of Columbia','DC',state))) %>%
  select(state,state_name,everything())

write.table(ct3,'us_census_2018_population_estimates_states_agegroups.csv',quote = FALSE,row.names = FALSE,sep=',')

#add in risks

ct3_adjusted <- ct3 %>%
  left_join(risks,by='agegroup') %>%
  mutate(risk_level_10 = (population * .1) * cfr_risk,
         risk_level_20 = (population * .2) * cfr_risk,
         risk_level_50 = (population * .5) * cfr_risk,
         risk_level_70 = (population * .75) * cfr_risk)

ct3_adjusted_summarized <- ct3_adjusted %>%
  group_by(state,state_name) %>%
  summarize(state_pop = sum(population),
            risk_level_10 = round(sum(risk_level_10),0),
            risk_level_20 = round(sum(risk_level_20),0),
            risk_level_50 = round(sum(risk_level_50),0),
            risk_level_70 = round(sum(risk_level_70),0),
            risk_pct_10 = risk_level_10 / state_pop,
            risk_pct_20 = risk_level_20 / state_pop,
            risk_pct_50 = risk_level_50 / state_pop,
            risk_pct_70 = risk_level_70 / state_pop) %>%
  ungroup() %>%
  arrange(desc(risk_pct_70))

sum(ct3_adjusted$risk_level_70)

sum(ct3_adjusted$risk_level_10)

write.table(ct3_adjusted,'us_census_2018_population_estimates_states_agegroups_risk.csv',quote = FALSE,row.names = FALSE,sep = ',')
write.table(ct3_adjusted_summarized,'us_census_2018_population_estimates_states_agegroups_risk_summarize.csv',quote = FALSE,row.names = FALSE,sep = ',')

