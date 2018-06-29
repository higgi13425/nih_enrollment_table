library(tidyverse)
library(janitor)
library(flextable)
library(here)
library(writexl)
library(magrittr)
library(knitr)
library(rmarkdown)
library(officer)

ibd <- read_csv("IBDDatabank2015_DATA_2018-06-29_0901.csv")

ibd %>% 
  filter(redcap_event_name == "baseline_visit_arm_1") %>% 
  select(sex, race___0:ethnicity) %>% 
  mutate(race  =  case_when(
             .$race___0 == 1 ~ "White",
             .$race___1 == 1 ~ "Black or African-American",
             .$race___2 == 1 ~ "Asian",
             .$race___3 == 1 ~ "Native Hawaiian or Other Pacific Islander",
             .$race___4 == 1 ~ "American Indian or Alaska Native",
             .$race___5 == 1 ~ "More Than One Race",
             .$race___999 == 1 ~ "Unknown or Not Reported",
             TRUE ~ "Unknown or Not Reported")) %>% 
  select(sex, race, ethnicity) %>% 
  mutate(ethnic_cat = case_when(
    .$ethnicity == 1 ~ "Hispanic or Latino",
    .$ethnicity == 0 ~ "Not Hispanic or Latino",
    TRUE ~ "Unknown/Not Reported Ethnicity")) %>% 
  select(sex, race, ethnic_cat) %>% 
  mutate(sex2 = case_when(
    .$sex == 1 ~ "Male",
    .$sex == 0 ~ "Female",
    TRUE ~ "Female")) %>% 
  select(sex2, race, ethnic_cat) ->
ibd
      
ibd_table <- ibd %>% 
  tabyl(race, sex2, ethnic_cat) %>% 
  reduce(left_join, by = "race")

names(ibd_table) <-(c("race", "female.Hispanic", 
                      "male.Hispanic", 
        "female.NotHispanic", "male.NotHispanic",
        "female.Unknown", "male.Unknown"))

# kludge - add rows for missing Race
ai <- c(NA, NA,NA,NA,NA,NA, NA)
nh <- c(NA,NA,NA,NA,NA,NA, NA)
names(ai) <-(c("race", "female.Hispanic",
                      "male.Hispanic",
                      "female.NotHispanic", "male.NotHispanic",
                      "female.Unknown", "male.Unknown"))
names(nh) <-(c("race", "female.Hispanic",
               "male.Hispanic",
               "female.NotHispanic", "male.NotHispanic",
               "female.Unknown", "male.Unknown"))
#now bind together
ibd_table <- bind_rows(ibd_table, ai, nh)
ibd_table[6,1] <- "American Indian or Alaska Native"
ibd_table[7,1] <- "Native Hawaiian or Other Pacific Islander"
ibd_table[6, 2:7] <- 0
ibd_table[7, 2:7] <- 0

# add margin totals
# convert race col to rownames to make numbers into a matrix
m <- as.matrix(ibd_table[ ,-1])
rownames(m) <- ibd_table$race
ibd_table2 <- addmargins(m, FUN=c(Total=sum), quiet = T)
ibd_table <- rownames_to_column(as.data.frame(ibd_table2), "Racial Categories")

#arrange without total
ibd_table3 <- ibd_table[1:7,] %>% arrange(`Racial Categories`)

ibd_table <- as.data.frame(rbind(ibd_table3, ibd_table[8,]))

write_xlsx(ibd_table, here("ibd_table.xlsx"))

#make flextable
myft <- ibd_table %>% 
  regulartable() %>% 
  theme_booktabs() %>% 
  set_formatter_type(fmt_double = "%0.0f") %>% 
  add_header(`Racial Categories` = "Racial Categories",
             female.Hispanic = "Hispanic or Latino",
             male.Hispanic = "Hispanic or Latino",
             female.NotHispanic = "Not Hispanic or Latino",
             male.NotHispanic = "Not Hispanic or Latino",
             female.Unknown = "Unknown/Not Reported Ethnicity",
             male.Unknown = "Unknown/Not Reported Ethnicity",
             Total = "Total") %>% 
  add_header(`Racial Categories` = "Racial Categories",
             female.Hispanic = "Ethnic Categories",
             male.Hispanic = "Ethnic Categories",
             female.NotHispanic = "Ethnic Categories",
             male.NotHispanic = "Ethnic Categories",
             female.Unknown = "Ethnic Categories",
             male.Unknown = "Ethnic Categories",
             Total = "Total") %>% 
  set_header_labels(`Racial Categories` = "Racial Categories",
                    female.Hispanic = "Female",
                    male.Hispanic = "Male",
                    female.NotHispanic = "Female",
                    male.NotHispanic = "Male",
                    female.Unknown = "Female",
                    male.Unknown = "Male",
                    Total = "Total") %>% 
  merge_h(part= "header") %>% 
  merge_v(part= "header") %>% 
  align(align = "center", part = "all") %>% 
  autofit()

myft
  
# save as word document
doc <- read_docx()  
doc <- body_add_flextable(doc, value = myft)
print(doc, target = "/Users/peterhiggins/Documents/Rcode/nih_enrollment_table/word_table.docx")

# save as ppt slide
ppt <- read_pptx()
ppt <- add_slide(ppt, layout = "Title and Content",
                 master = "Office Theme")
ppt <- ph_with_flextable(ppt, value = myft, type= "body")
print(ppt, target = "/Users/peterhiggins/Documents/Rcode/nih_enrollment_table/ppt_slide.pptx")
