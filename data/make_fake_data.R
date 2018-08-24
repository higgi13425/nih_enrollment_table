library(magrittr)
library(tidyverse)
library(keyring)
library(charlatan)
library(fs)
library(readxl)
library(writexl)
library(REDCapR)
library(redcapAPI)
library(RCurl)
library(glue)

# generate data for fake database in training REDCap
# using assorted generators and lists of names
record_id <- 1:500

#from https://github.com/hadley/data-baby-names
firstnames <- read_csv("data/baby-names-us.csv")
firstnames %>%  filter(year == 2008) %>% pull(name) %>% 
  sample(size=500) ->
first_name

#from Census http://www2.census.gov/topics/genealogy/2010surnames/
lastnames <- read_excel("data/us_surnames.xlsx")
lastnames %>%  
    pull(name) %>% 
    sample(size=500) %>% 
    tolower() %>% 
    tools::toTitleCase(.) ->
last_name

#street names from 
# https://catalog.data.gov/dataset/street-names-37fec/resource/d655cc82-c98f-450a-b9bb-63521c870503
street_names <- read_csv("data/Street_Names.csv") %>% 
  filter(FullStreetName > "A ST") %>% 
  select(FullStreetName) %>% 
  pull(FullStreetName) %>% 
  tolower() %>% 
  tools::toTitleCase(.) 

address <- paste0(round(runif(500)*98 +1)," ",
                  sample(street_names, size =500))

telephone <- for(i in 1:500){
  phone[i] <- collapse(sample(2:9,10, replace = TRUE))
  }
# note still leaves in a few non-public area codes like
# 555, 950, 958, 959, 976

year <- round(runif(500)*70 + 1930)
month <- round(runif(500)*11 +1)
day <- round(runif(500)*27 +1)
dob <- as.Date(paste0(year,"-", month, "-", day))

#age <- round(as.numeric(Sys.Date() - (dob))/365)
# not needed as auto-calculated by REDCap

ethnicity <-runif(500) 
ethnicity <- case_when(ethnicity < 0.05 ~ 0, 
                ethnicity < 0.98 ~ 1,
                TRUE ~ 2)

race <-runif(500) 
race <- case_when(race<0.04 ~ 1, 
                  race<0.15 ~ 3,
                  race<0.85 ~ 4,
                  race<0.98 ~ 5,
                  TRUE ~ 6)
sex <-runif(500) 
sex <- case_when(sex < 0.53 ~ 0, 
                  TRUE ~ 1)

height <- rnorm(500, mean= 173, sd =10) %>% round(1)
 
weight <- rnorm(500, mean= 112, sd =25) %>%
          as.integer()

fake_data <- tibble(record_id, first_name, last_name, address,
        telephone,  dob,  ethnicity, race,
        sex, height, weight)
#fake_data$bmi <- round(fake_data$weight*10000/
#                    (fake_data$height^2), 1)
# not needed as bmi is auto-calculated by REDCap

fake_data$email <- paste0(tolower(fake_data$first_name),".",
                tolower(fake_data$last_name), "@aol.com")

#now write to csv file for upload to REDCap
write_csv(fake_data,"data/fake.csv")

