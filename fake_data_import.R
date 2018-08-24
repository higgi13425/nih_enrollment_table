library(magrittr)
library(tidyverse)
library(keyring)
library(REDCapR)
library(redcapAPI)
library(readxl)
library(writexl)

# to import production data from REDCap,
# you need a API token from your local
# REDCap administrator - email/call them.
# note that for training data you can provide
# API token for yourself without having to ask an admin


# SET UP API TOKEN in ADVANCE
# Note: to set up hidden token in keyring
# first run 
# keyring::key_set(service = "REDCap_database_name", 
#                   username = "your_username")
# and, when prompted with a dialog box
# enter your API token with no quotes around it
# Once this is done,
# subsequent calls to keyring::key_get(
#           service = "REDCap_database_name", 
#           username = "your_username")
# will provide the token that you can assign to an object
# and the token will remain secret
# for example
prod_url <- 'https://redcap-p-a.umms.med.umich.edu/api/'
prod_token <- keyring::key_get(
          service = "REDCap IBD Database API Token", 
          username = "REDCap IBD Database API Token")
prod_rcon <- redcapConnection(prod_url, prod_token)

# note to actually export UMHS data, need to be on campus with 
# Ethernet connection, or wireless with VPN


# REDCapR using umich production data - 
# works if on VPN, 
# works well using REDCapR

#all fields
prod_df <- redcap_read(
  redcap_uri = prod_url,
  token = prod_token
)$data

# REDCapR using umich training data - 
# works if on VPN, 
# works well
# using REDCapR
# set up for training database
train_url <- 'https://redcap-t-a.umms.med.umich.edu/api/'
train_token <- "1C4CCB1234A55E711EDF57B0D8E24F01"
train_rcon <- redcapConnection(train_url, train_token)

#all fields
train_df <- redcap_read(
  redcap_uri = train_url,
  token = train_token
)$data

#specific fields
train_df <- redcap_read(
  redcap_uri = train_url,
  token = train_token, 
  fields = c('race', 'ethnicity', 'sex')
)$data

# REDCapR example for testing that works from Oklahoma
ok_url   <- "https://bbmc.ouhsc.edu/redcap/api/"
ok_token <- "9A81268476645C4E5F03428B8AC3AA7B"
ok_rcon <- redcapConnection(ok_url, ok_token)
ok_df <- redcap_read(
  redcap_uri = ok_url,
  token = ok_token
)$data



#redcapAPI seems to require specifying fields
# now attempts with redcapAPI
prod_url <- 'https://redcap-p-a.umms.med.umich.edu/api/'
prod_token <- keyring::key_get(
  service = "REDCap IBD Database API Token", 
  username = "REDCap IBD Database API Token")
prod_rcon <- redcapConnection(prod_url, prod_token)
prod_data <- exportRecords(prod_rcon, fields = c(
  "subject_id",
  "first_name", "last_name", "address_1", "telephone_1", "email",
  "birthdate", "gi_dr"
))


# set up for training database
train_url <- 'https://redcap-t-a.umms.med.umich.edu/api/'
train_token <- "1C4CCB1234A55E711EDF57B0D8E24F01"
train_rcon <- redcapConnection(train_url, train_token)
train_data <- exportRecords(train_rcon, fields = c(
  "record_id",
  "first_name", "last_name", "address", "telephone", "email",
  "dob", "age", "ethnicity", "race", "sex", "height", "weight"),
factors = TRUE)

# note to actually export UMHS data, need to be on campus with 
# Ethernet connection, or wireless with VPN

# set up for open Oklahoma REDCap fake database
ok_url   <- "https://bbmc.ouhsc.edu/redcap/api/"
ok_token <- "9A81268476645C4E5F03428B8AC3AA7B"
ok_rcon <- redcapConnection(ok_url, ok_token)
ok_data <- exportRecords(ok_rcon,
  factors = TRUE,
  labels = TRUE)

# in general, to pull in records
mydata <- exportRecords(rcon)

# test redcapAPI with Oklahoma setup
ok_data <- exportRecords(ok_rcon, fields = c(
  "record_id",
  "name_first", "name_last", "address", "telephone", "email",
  "dob", "age"
))


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

main_df <- post_to_df(main_post)

#try httr approach 1
source("dataclean_helpers.R")

main_post <- httr::POST(
  url = url,
  body = list(
    token = token,
    content = "record",
    format = "csv",
    rawOrLabel = "label",
    exportCheckboxLabel = TRUE)
)

main_df <- post_to_df(main_post)

baseline_df <- subset(main_df)

# try httr approach 2

baseline_post <- httr::POST(
  url = url,
  body = list(
    token = token,
    content = "record",
    format = "csv",
    forms = "demographics, baseline_data",
    fields = c("study_id"),
    events = "baseline_visit_arm_1",
    rawOrLabel = "label",
    exportCheckboxLabel = TRUE)
)

baseline_df <- post_to_df(baseline_post)
baseline_df %<>% select(sex:ethnicity) %>% select(-marital_status)
# now ready to process to NIH Enrollment Table


#note to self
# for looking at secure keys
# key_list() will give whole list of service, username
# key_list()[41,1] will give service for row 41
# key_list()[41,2] will give username for row 41
# keyring::key_get(service = key_list()[41,1], 
#                 username = key_list()[41,2])
# will challenge you for password for keychain- if entered
# will give you the key for row 41