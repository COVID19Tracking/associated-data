library(tidycensus)
library(tidyverse)
library(datasets)

#add state info from base R

states <- tibble(state_name = state.name,
                 state = state.abb)

#get the variables

acs_vars <- load_variables(2018, "acs1/profile")

#clean up the different and relevant health insurance variables

health_insurance <- acs_vars %>%
  filter(str_detect(label,'health insurance')==TRUE)

labels <- health_insurance %>%
  mutate(estimate_type = ifelse(str_starts(label,'Percent Estimate')==TRUE,'percent_estimate','population_estimate') ,
         coverage_type = str_replace_all(label,'.*!!',''),
         clean_label = str_replace(label,'.*Civilian noninstitutionalized population',''),
         age_group = ifelse(str_detect(clean_label,'years')==FALSE,'overall',str_replace_all(clean_label,'!!.*','')),
         labor_force = ifelse(str_detect(clean_label,'labor force')== FALSE,NA,
                              ifelse(str_detect(clean_label,'Not in labor force')==TRUE,'Not in labor force','In labor force')),
         employed = ifelse(str_detect(clean_label,'mployed')==FALSE,NA,
                           ifelse(str_detect(clean_label,'Unemployed')==TRUE,'Unemployed','Employed'))

  ) %>%
  select(-clean_label) %>%
  mutate(concept = str_to_title(concept)) %>%
  rename(variable = name)

#get a list of variables to call

codes <- labels$variable

#call the API

hi2 <- get_acs('state',variables = codes,cache_table = TRUE,year = 2018)

#join the data from the API with the label and state data and clean it

hi3 <- hi2 %>%
  left_join(labels,by='variable') %>%
  rename(margin_of_error = moe) %>%
  rename(geo_id = GEOID,
         state_name = NAME,
         acs_variable = variable) %>%
  left_join(states,by='state_name') %>%
  select(geo_id,state,state_name,everything())

#output to CSV

write.table(hi3,'acs_2018_health_insurance_coverage_estimates.csv',row.names = FALSE,quote = FALSE,sep=',')
