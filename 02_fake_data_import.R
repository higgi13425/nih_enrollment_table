library(magrittr)
library(tidyverse)
library(keyring)
library(REDCapR)


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
# only 5 records
ok_url   <- "https://bbmc.ouhsc.edu/redcap/api/"
ok_token <- "9A81268476645C4E5F03428B8AC3AA7B"
ok_df <- redcap_read(redcap_uri=ok_url, 
                     token=ok_token)$data

# fake clinical data hosted at Oklahoma by William H. Beasley

fake_df <- REDCapR::redcap_read_oneshot(
  redcap_uri = "https://bbmc.ouhsc.edu/redcap/api/",  
  token      = "F304DEC3793FECC3B6DEEFF66302CAD3"
)$data

# test with training dbase of fake data at UMichigan
# only works if connected via Ethernet or 
# on WiFi if already connected via VPN 
train_url <- 'https://redcap-t-a.umms.med.umich.edu/api/'
train_token <- "1C4CCB1234A55E711EDF57B0D8E24F01"
train_df <- redcap_read(redcap_uri=train_url, 
                        token=train_token)$data

# REDCapR using umich prod data - 
# works with Ethernet connection
# or WiFi after VPN connection
# uses REDCapR
prod_url <- 'https://redcap-p-a.umms.med.umich.edu/api/'
prod_token <- keyring::key_get(
  service = "REDCap IBD Database API Token", 
  username = "REDCap IBD Database API Token")
prod_df <- redcap_read(redcap_uri = prod_url, 
                       token = prod_token)$data


# Assorted other approaches -----------------------------------------------


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
# redcapAPI approach
# needs VPN for WiFi
# or ethernet on campus for UMichigan
library(redcapAPI)
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
# needs VPN for WiFi
# or on campus ethernet connection at UMichigan
library(redcapAPI)
train_url <- 'https://redcap-t-a.umms.med.umich.edu/api/'
train_token <- "1C4CCB1234A55E711EDF57B0D8E24F01"
train_rcon <- redcapConnection(train_url, train_token)
records <- exportRecords(rcon=train_rcon, 
                         factors=FALSE, labels=TRUE,
                         dates=FALSE, survey=FALSE, dag=TRUE,
                         batch.size=1)


# from OK - this version works
# with redcapapi
# but pretty slow compared to REDCapR
library(redcapAPI)
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

library(redcapAPI)
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
# set up with VPN and wifi - but slow
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