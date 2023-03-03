## CLEAN CODE VERSION - RVA VOCABS USE

## SOURCE: read_rva_vocabs_test_24Feb.R

## Get the content of the IRISS scheme from RVA

## THIS CODE ENABLES USERS TO SELECT AND EXTRACT CLASSIFICATIONS FROM THE ARDC 
## RESEARCH VOCABULARIES AUSTRALIA, IMPORT THE CLASSIFICATION TO AN TIDYVERSE TIBBLE
## OR LIST, AND THEN COMPARE EXISTING VARIABLES AGAINST THE CLASSIFICATION

# Note that the functions make use of SKOS terminology as the classifications are
# stored as SKOS in RVA. So at times there is a need to move between ABS and RVA terminology
# Terms used (need to verify these:
# ABS                RVA              Definition
# Classification     Concept scheme   INSERT DEFNS HERE
# Category           TopConcept       A category within a classification
# Code               Notation         The code used to denote the category (e.g M for male)
# Category label     PrefLabel        The name given to a category in a classification (e.g. "Male")
# Code identifier    CodeID           The persistent identifier assigned to the concept

################################
## THE FUNCTIONS AVAILABLE IN THIS PACKAGE ARE AS FOLLOWS ##
################################

## 1. READ CLASSIFICATIONS FROM RVA
## 2. EXTRACT CONCEPT SCHEMES (CLASSIFICATIONS) INTO A TIBBLE
## 3. REQUEST THE NAME OF AN abs:classification THE USER WISHES TO USE
## 3. DETERMINE THE NUMBER OF skos:CONCEPTS (abs:CATEGORIES) IN A CLASSIFICATION
## 4. EXTRACT THE SET OF skos:CONCEPTS - abs:categories - INTO A TIBBLE

################################
## PREPARATION - CLEAN ENVIRONMENT AND IMPORT DEPENDENCY PACKAGES
################################

## Clean environment ##
rm(list=ls())

#INSTALL ANY MISSING PACKAGES - REMOVE COMMENTS TO INSTALL
#install.packages("rjson")

# LOAD RELEVANT LIBRARIES
library("tidyverse")
library("rjson")
library("dplyr")
library("httr")
library("tibble")
#library("jsonlite")

source("https://sebastiansauer.github.io/Rcode/find_funs.R")
find_funs("filter")

###################################
## 1. READ CLASSIFICATIONS FROM RVA
###################################

## Get the full IRISS Census concept scheme from RVA
# Give the input file name to the function.
##JSONLITE VERSION
#detach("jsonlite")
censusconcepts_result <- fromJSON("https://demo.vocabs.ardc.edu.au/repository/api/lda/ands-nc/a-subset-of-the-2021-abs-census-data-dictionary/3/conceptscheme.json?_view=all")
##RJSON VERSION
#censusconcepts_result <- fromJSON(file = "https://demo.vocabs.ardc.edu.au/repository/api/lda/ands-nc/a-subset-of-the-2021-abs-census-data-dictionary/3/conceptscheme.json?_view=all")

# Move the full concept scheme results to a tibble
census_conceptschemes_tibble <- enframe(unlist(censusconcepts_result))
census_conceptschemes_tibble

# Get just the names of the concepts ("_About")
#census_conceptschemes_tibble2 <- filter(census_conceptschemes_tibble, name == "result.items._about")
census_conceptschemes_tibble2 <- filter(census_conceptschemes_tibble, grepl('result.items._about', name))
census_conceptschemes_tibble2

###################################
## 2. EXTRACT CONCEPT SCHEMES (CLASSIFICATIONS) INTO A TIBBLE
###################################

### EXTRACT THE SET OF CONCEPTSCHEMES - CLASSIFICATIONS
library(tidyverse)

# Create an example tibble of URLs
urls <- tibble(url = census_conceptschemes_tibble2$value)

# Define a function to extract the final section of a URL
extract_classification_notation <- function(url) {
  url %>%
    str_extract("/[^/]+$") %>%
    str_remove("/")
}

# Apply the function to each URL in the tibble using mutate()
# Apply the function to each URL in the tibble using mutate()
classifications_list_tibble <- urls %>%
  mutate(cl_notation = extract_classification_notation(url))

# View the resulting tibble
classifications_list_tibble 


#################################
## 3. Request the name of an abs:classification the user wishes to use
## DEV NOTE: Current version
## Dev note: Still needs some work ##
# Input: list of classifications
## Find out which classification a user wants
## This code joins the classification name to the URL to select the right classification
#################################


# Function to prompt the user for the classification required
# Takes as input the list of classifications (classifications_list_tibble2) from Step 2 above
# and passes it to "class_options" to loop through all of the currently available classifications

ask_for_classification <- function() {
  cat("IRISS Classification use service")
  cat("The following classifications are available.")
  # Print the options and ask the user to select one
  cat("Please select an option:\n")
  for (i in seq_along(class_options)) {
    cat(i, ". ", class_options[i], "\n")
  }
  # Get the user input (note - RStudio tries to return existing R object names
  # need to fix this!!)
  class_input_name <- readline(prompt="Please enter the four letter code (e.g. SEXP) for the classification you want to import: ")
  print(paste("You have selected", class_input_name))
  return(class_input_name)
}


# Extract the vector of classifications from the classifications_list_tibble

class_options <- classifications_list_tibble$cl_notation

## CHECKS ON RESPONSE VALIDITY - TO BE ADDED
## Convert the response to an integer
#response <- as.integer(response)
#
## Check if the response is valid
#if (!is.na(response) && response %in% seq_along(class_options)) {
#  cat("You selected", class_options[response], "\n")
#} else {
#  cat("Invalid selection\n")
#}

## Request the name of the classification
classification_name <- toupper(ask_for_classification())

# Pass the requested name to create the URL for the vocabulary
vocab_url <- paste0("https://demo.vocabs.ardc.edu.au/repository/api/lda/ands-nc/a-subset-of-the-2021-abs-census-data-dictionary/3/resource?uri=http://example.com/census/",classification_name,"/")
print(vocab_url)



##############################
## 4a. DETERMINE THE NUMBER OF skos:CONCEPTS (abs:CATEGORIES) IN A CLASSIFICATION
## 4b. EXTRACT THE CONCEPTS IN THE CLASSIFICATION TO A TIBBLE
##############################

# This code count the number of top concepts in the classification, 
# to set the size of the loop to parse the classification

# DEVELOPMENT NOTE: This should PREFERABLY extract the set of skos:concept 
# "notations" (abs:codes) from a skos:conceptscheme (abs:classification)

## Get a specific skos:conceptscheme (abs:classification) from the RVA Census 2021 concept scheme
# In this case, the example abs:classification used is "AGE10P" - 10 year age groups
# This needs to be adapted to enable users to select the classification


# Create the URL for the vocabulary

## Request the name of the classification
classification_name <- toupper(ask_for_classification())

# Pass the requested name to create the URL for the vocabulary
vocab_url <- paste0("https://demo.vocabs.ardc.edu.au/repository/api/lda/ands-nc/a-subset-of-the-2021-abs-census-data-dictionary/3/concept/topConcepts.json?scheme=http://example.com/census/",classification_name)
# IF EXAMPLE NEEDED - set vocab_url to "AGE10P"
#vocab_url <- paste0("https://demo.vocabs.ardc.edu.au/repository/api/lda/ands-nc/a-subset-of-the-2021-abs-census-data-dictionary/3/resource?uri=http://example.com/census/",classification_name,"/")

# Confirm the URL is correct on screen
print(vocab_url)

##
## CREATE A TIBBLE WITH THE LIST OF ALL CATEGORIES IN THE CLASSIFICATION
##
library(tidyverse)

# read in the JSON file
# Get the top concepts from the classification URL - vocab_url
vocab_url
# Append the view all to get all the concepts in the vocab in JSON
vocab_concepts_url <- paste0(vocab_url,"&_view=all")
vocab_concepts_url
# Pass the JSON into an R object
json_data <- fromJSON(vocab_concepts_url, flatten = TRUE)
json_data

# extract the relevant information using tidyverse functions
concepts_in_class_tibble <- as_tibble(json_data$result$items %>%
  select(notation, identifier, prefLabel._value, prefLabel._lang))
concepts_in_class_tibble

# Determine the number of abs:categories in the abs:classification (count the number of rows)
class_num_topconcepts_rows <- nrow(concepts_in_class_tibble)
print(class_num_topconcepts_rows)




# ##LIKELY SUPERSEDED CODE
# ##CHECK AND CONFIRM
# ## FUNCTION TO RETRIEVE TIBBLE
# ## This version of the function returns notation, label and id as a tibble
# get_classification_code_tibble <- function(current_name,result_tibble) {
#   #  concept_result2 = GET(paste0("https://demo.vocabs.ardc.edu.au/repository/api/lda/ands-nc/a-subset-of-the-2021-abs-census-data-dictionary/3/resource?uri=http://example.com/census/SEXP/", current_name))
#   concept_result2 = GET(paste0(vocab_url, current_name))
#   cr2_content <- content(concept_result2)
#   code_notation <- cr2_content[["result"]][["primaryTopic"]][["notation"]]
#   code_label_value <- cr2_content[["result"]][["primaryTopic"]][["prefLabel"]][["_value"]]
#   code_identifier <- cr2_content[["result"]][["primaryTopic"]][["identifier"]]
#   #  return(list(notation = code_notation, label_value = code_label_value, identifier = code_identifier))
#   #  result_tibble <- create_tibble(code_notation, code_label_value, code_identifier)
#   #  result_tibble <- add_row_to_tibble(code_tibble_shell,code_notation, code_label_value, code_identifier)
#   print(result_tibble)
#   end_result_tibble <- add_row_to_tibble(result_tibble,code_notation, code_label_value, code_identifier)
#   print(end_result_tibble)
#   return(end_result_tibble)
# }
# ##
# 
# ## LOOP VERSION 2 ##
# # Uses tibble function
# ## Need to figure out how many codes in the list, instead of returning a column of name_values
# 
# # Create an empty tibble with 0 rows and 3 columns
# code_tibble_shell <- tibble(codenot = character(), codeid = character(), codelabel = character())
# 
# ## Initialise empty tibbles (code_tibble and result_tibble) before extracting list
# result_tibble <- code_tibble_shell
# rm(codetibble)
# 
# 
# #Test codetibble
# current_name <- 1
# vocab_url
# current_name
# codetibble <- get_classification_code_tibble(current_name,code_tibble_shell)
# codetibble
# rm(codetibble)
# 
# 
# ## CODELIST example - if a list is required##
# # preference is for a tibble as output
# current_name <- 2
# codelist <- get_classification_code_info(current_name)
# cr2_content
# print(code_notation)
# print(code_label_value)
# print(code_identifier)
# 
# codelist
# 
# rm(codelist)
#
##END SUPERSEDED
##############################


####################
### Code to compare a variable's categories with codes in a census classification
# This works with a sample tibble ("my_tibble")
# Need to read in real data e.g. AUSSA or AES
####################

library(dplyr)

# Define a tibble with the agreed terms or categories
# result_tibble is the output of the "get_classification..." function
concept_agreed_terms <- concepts_in_class_tibble
#sexp_agreed_terms <- result_tibble

# Define a tibble with a column that needs to be checked
my_tibble <- tibble(id = c("01", "02", "03", "04"), sex = c("1","2","3","2"))
colnames(my_tibble)

# Check if the values in the sex column of my_tibble are consistent with the agreed terms
# Return a tibble as the result
# Append the result to the original tibble
consistent_values <- tibble(my_tibble$sex %in% concept_agreed_terms$notation)
my_tibble <- my_tibble %>% mutate(sexpcodetf = consistent_values)
colnames(my_tibble)

# Print the result
my_tibble <- my_tibble %>% rename(sex_code_correct = sexpcodetf)
my_tibble

# Tabulate the result
#sex_code_consistency
my_tibble %>%
  group_by(sexpcodetf$`my_tibble$sex %in% sexp_agreed_terms$col1`) %>%
  summarize(n = n())


## Now check against AES

## Go to "test_readrvavocabs_withaes.r"

