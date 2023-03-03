### AES2013-22 ###
### Finding and analysing data ###
### Original: 4 July 2018 ###
### Revised: 10 February 2022 ###


## Clean environment ##
rm(list=ls())

## Find and access data using dataverse R package and ADA
## Set environment variables ##

library("dataverse")

# Dataverse environment functions
Sys.setenv("DATAVERSE_SERVER" = "dataverse.ada.edu.au")  
Sys.setenv("DATAVERSE_KEY" = "INSERT_YOUR_DV_API_KEY_HERE")

############
## 2022 data
############

## GET AND USE DATAVERSE SPSS DATA THAT IS IN A ZIP FILE ##

# install the httr package
#install.packages("httr")

# load the httr library
library(httr)
library(haven)
library(foreign)

# define the URL of the zip file you want to download
zip_url <- "https://dataverse.ada.edu.au/api/access/datafile/18015"

# download the zip file
response <- GET(zip_url)

# write the zip file to your local machine
writeBin(content(response, "raw"), "datafile.zip")

# extract the contents of the zip file
unzip("datafile.zip", exdir = ".")

aes2022spss2 <- read.spss(file="aes22_unrestricted_v2.sav", to.data.frame = TRUE, use.value.labels = TRUE, max.value.labels = 30)
aes2022spss2_nolab <- read.spss(file="aes22_unrestricted_v2.sav", to.data.frame = TRUE, use.value.labels = FALSE, max.value.labels = 30)
aes2022spss2_haven <- read_sav(file="aes22_unrestricted_v2.sav")

aes2022spss2 <- as.tibble(aes2022spss2)
aes_cols <- as.tibble(colnames(aes2022spss2))

aes_demog_tibble <- aes2022spss2 %>%
  select(ID, H1:H6, dem_agehedu:dem_region)

aes_demog_tibble

## Tabulate sex and state
library(dplyr)

gendertable <- aes_demog_tibble %>%
  group_by(dem_gender) %>%
  summarize(count = n())
gendertable

rm(gen_statetable)
gen_statetable <- aes_demog_tibble %>%
  count(dem_region, dem_gender)
gen_statetable

## INTEROPERATING - Display the data

# read in the binary file using the foreign package
library("foreign")

table(aes2022spss2$A1)

#install.pacakges("ggplot2")
library(ggplot2)

p2022 <- ggplot(aes2022spss2, aes(dem_gender)) +
  geom_bar(fill = "#0073C2FF")
print(p2022)


# Define a tibble with the agreed terms or categories
# result_tibble is the output of the "get_classification..." function
concept_agreed_terms <- concepts_in_class_tibble
#sexp_agreed_terms <- result_tibble

# Define a tibble with a column that needs to be checked
rm(my_tibble)
#my_tibble <- tibble(id = c("01", "02", "03", "04"), sex = c("1","2","3","2"))
my_tibble <- aes_demog_tibble
colnames(my_tibble)

# Check if the values in the sex column of my_tibble are consistent with the agreed terms
# Return a tibble as the result
# Append the result to the original tibble
consistent_values <- tibble(as.numeric(my_tibble$H1) %in% concept_agreed_terms$notation)
my_tibble <- my_tibble %>% mutate(sexpcodetf = consistent_values)
colnames(my_tibble)

# Print the result
my_tibble <- my_tibble %>% rename(sex_code_correct = sexpcodetf)
my_tibble
colnames(my_tibble)

# Tabulate the result - which genders were classified using correct notation.
# NOTE: WE NEED TO CHECK THAT THE NOTATIONS MATCH - MALE = 1 FEMALE =2 in both 
# THis only checks if the notation/code EXISTS in the classification

#sex_code_consistency
my_tibble %>%
  group_by(sex_code_correct) %>%
  summarize(n = n())

gen_sexcode_tibble1 <- my_tibble %>%
  count(sex_code_correct, H1)
gen_sexcode_tibble1

gen_sexcode_tibble2 <- my_tibble %>%
  count(sexpcodetf, as.numeric(dem_gender))
gen_sexcode_tibble2
