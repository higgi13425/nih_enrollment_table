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
# using the charlatan package and list of names
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
fake_data$email <- paste0(tolower(fake_data$first_name),".",
                tolower(fake_data$last_name), "@aol.com")
write_csv(fake_data,"data/fake.csv")

# SET UP TOKEN in ADVANCE
# Note: to set up hidden token in keyring
# first run 
# keyring::key_set(service = "your_service", 
#                   username = "your_username")
# and, when prompted with a dialog box
# enter your API token with no quotes around it
# Once this is done,
# subsequent calls to keyring::key_get(
#           service = "your_service", 
#           username = "your_username")
# will provide the token that you can assign to an object
# and the token will remain secret


# example for testing that works from Oklahoma
# uses REDCapR - this works fast
ok_url   <- "https://bbmc.ouhsc.edu/redcap/api/"
ok_token <- "9A81268476645C4E5F03428B8AC3AA7B"
ok_df <- redcap_read(redcap_uri=ok_url, 
                     token=ok_token)$data

#test with training dbase of fake data
# connects, reads something, but get HTML in one column
# works if connected via Ethernet
# works with WiFi and VPN - faster
train_url <- 'https://redcap-t-a.umms.med.umich.edu/api/'
train_token <- "1C4CCB1234A55E711EDF57B0D8E24F01"
train_df <- redcap_read(redcap_uri=train_url, 
                        token=train_token)$data

# REDCapR using umich prod data - 
# works with Ethernet connection
# uses REDCapR
prod_url <- 'https://redcap-p-a.umms.med.umich.edu/api/'
prod_token <- keyring::key_get(
  service = "REDCap IBD Database API Token", 
  username = "REDCap IBD Database API Token")
prod_df <- redcap_read(redcap_uri = prod_url, 
                       token = prod_token)$data

# test httr with Oklahoma API
main_post <- httr::POST(
  url = ok_url,
  body = list(
    token = ok_token,
    content = "record",
    format = "csv",
    rawOrLabel = "label",
    exportCheckboxLabel = TRUE)
)

#source("dataclean_helpers.R") #do this once
main_df <- post_to_df(main_post)


# test httr with training API - still one icky column
main_post <- httr::POST(
  url = "https://redcap-t-a.umms.med.umich.edu/api/",
  body = list(
    token="1C4CCB1234A55E711EDF57B0D8E24F01",
    content = "record",
    format = "csv",
    rawOrLabel = "label",
    exportCheckboxLabel = TRUE)
)

#curl -X POST -H "Cache-Control: no-cache" -F "token=1C4CCB1234A55E711EDF57B0D8E24F01" -F "content=record" -F "format=csv" "https://redcap-t-a.umms.med.umich.edu/api/">redcap-t-a_api.txt

main_df <- post_to_df(main_post)

#try httr approach 1
source("dataclean_helpers.R")

main_post <- httr::POST(
  url = ok_url,
  body = list(
    token = ok_token,
    content = "record",
    format = "csv",
    rawOrLabel = "label",
    exportCheckboxLabel = TRUE)
)

main_df <- post_to_df(main_post)

baseline_df <- subset(main_df)

# try httr approach 2 - works with ok_url, ok_token
# does not work with train or prod

baseline_post <- httr::POST(
  url = prod_url,
  body = list(
    token = prod_token,
    content = "record",
    format = "csv",
    forms = "demographics, baseline_data",
    fields = c("study_id"),
    events = "baseline_visit_arm_1",
    rawOrLabel = "label",
    exportCheckboxLabel = TRUE)
)

baseline_df <- post_to_df(baseline_post)
baseline_df %<>% select(sex:ethnicity) %>% 
  select(-marital_status)
# now ready to process to NIH Enrollment Table


# prepare to export from production
# fails
prod_url <- 'https://redcap-p-a.umms.med.umich.edu/api/'
prod_token <- keyring::key_get(
  service = "REDCap IBD Database API Token", 
  username = "REDCap IBD Database API Token")
prod_rcon <- redcapConnection(prod_url, prod_token)
mydata <- exportRecords(rcon=prod_rcon, 
                        fields = c("record_id",
                                   "name_first","name_last","address", 
                                   "telephone", "email",  "dob", "age")) 

# export fake data from training db
# set up for training database
# with redcapAPI
# hangs, very slow
train_url <- 'https://redcap-t-a.umms.med.umich.edu/api/'
train_token <- "1C4CCB1234A55E711EDF57B0D8E24F01"
train_rcon <- redcapConnection(train_url, train_token)
records <- exportRecords(rcon=train_rcon, 
                         factors=FALSE, labels=TRUE,
                         dates=FALSE, survey=FALSE, dag=TRUE,
                         batch.size=1)


# from OK - this version works
# but pretty slow
ok_url   <- "https://bbmc.ouhsc.edu/redcap/api/"
ok_token <- "9A81268476645C4E5F03428B8AC3AA7B"
ok_rcon <- redcapConnection(ok_url, ok_token)
records <- exportRecords(rcon=ok_rcon, 
                         factors=FALSE, labels=TRUE,
                         dates=FALSE, survey=FALSE, dag=TRUE,
                         batch.size=1)

# test redcapAPI with Oklahoma setup on production data
# this version fails with Wifi,
# works with Ethernet
# slow
prod_url <- 'https://redcap-p-a.umms.med.umich.edu/api/'
prod_token <- keyring::key_get(
  service = "REDCap IBD Database API Token", 
  username = "REDCap IBD Database API Token")
prod_rcon <- redcapConnection(prod_url, prod_token)
prod_data <- exportRecords(rcon=prod_rcon, 
                           factors=FALSE, labels=TRUE,
                           dates=FALSE, survey=FALSE, dag=TRUE,
                           batch.size=1)

# test redcapAPI with Oklahoma setup on training data
# works with VPN vpn.med.umich.edu
# set up with Cisco and wifi - but slow
# works with Ethernet
library(redcapAPI)
train_data <- exportRecords(rcon=train_rcon, 
                            factors=FALSE, labels=TRUE,
                            dates=FALSE, survey=FALSE, dag=TRUE,
                            batch.size=1)
#another httr approach from
# redcapR
# https://cran.r-project.org/web/packages/REDCapR/vignettes/TroubleshootingApiCalls.html
redcap_uri <- "https://bbmc.ouhsc.edu/redcap/api/"
token      <- "9A81268476645C4E5F03428B8AC3AA7B"

raw_text <- RCurl::postForm(
  uri                         = redcap_uri
  , token                     = token
  , content                   = 'record'
  , format                    = 'csv'
  , type                      = 'flat'
  , rawOrLabel                = 'raw'
  , exportDataAccessGroups    = 'true'
  , .opts                     = RCurl::curlOptions(ssl.verifypeer=FALSE)
)

# raw but works
# now try with production data
raw_text <- RCurl::postForm(
  uri                         = prod_url
  , token                     = prod_token
  , content                   = 'record'
  , format                    = 'csv'
  , type                      = 'flat'
  , rawOrLabel                = 'raw'
  , exportDataAccessGroups    = 'true'
  , .opts                     = RCurl::curlOptions(ssl.verifypeer=FALSE)
)

#note to self
# for looking at secure keys
# key_list() will give whole list of service, username
# key_list()[41,1] will give service for row 41
# key_list()[41,2] will give username for row 41
# keyring::key_get(service = key_list()[41,1], 
#                 username = key_list()[41,2])
# will challenge you for password for keychain- if entered
# will give you the key for row 41