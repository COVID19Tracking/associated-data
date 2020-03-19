library(tidyverse)
library(datasets)
library(curl)

tidycols <- function(df) {
  require(snakecase)

  dfnames <- colnames(df)

  dfnames <- to_snake_case(dfnames,sep_out = "_")

  dfnames <- tolower(gsub(" ","_",dfnames))

  dfnames <- gsub(".","_",dfnames,fixed=TRUE)

  dfnames <- gsub("/","_per_",dfnames,fixed=TRUE)

  colnames(df) <- dfnames

  return(df)
}

#add state info from base R

states <- tibble(state_name = state.name,
                 state = state.abb)

#file downloaded from https://www.kff.org/other/state-indicator/beds-by-ownership/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Total%22,%22sort%22:%22asc%22%7D
#make sure to point it to the right folder where you downloaded the file
file <- 'kff_raw_data.csv'

#read in file and transform it

hbeds <- read_csv(file,skip = 2) %>%
  tidycols() %>%
  rename(state_name = location) %>%
  #add in state info
  left_join(states,by='state_name') %>%
  #reorganize columns
  select(state,state_name,everything()) %>%
  #switch data columns to numeric type
  mutate_at(3:6,as.numeric) %>%
  #fix DC
  mutate(state = ifelse(state_name == 'District of Columbia','DC',state)) %>%
  arrange(state) %>%
  #get rid of random extra rows from CSV file
  filter(!is.na(state)) %>%
  print()

#output cleaned data

write.table(hbeds,'kff_usa_hospital_beds_per_capita_2018.csv',sep = ',',quote = FALSE,row.names = FALSE)
