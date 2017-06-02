##### INTRODUCTION AND INSTRUCTIONS #####
# Master thesis "Public corruption and the municipal bond market" by Felix Gräber
# If you want to run the code on your machine, change working directory and install packages as needed
#########################################

getwd()
setwd("C:/Users/graeb/OneDrive/Uni/_Masterarbeit/Data/R Projekt")
#install.packages("readr")
#install.packages("plyr")
#install.packages("ggplot2")
#install.packages("psych")
#install.packages("dplyr")
#install.packages("car")
#install.packages("tm")
#install.packages("stringr")
#install.packages("fuzzyjoin")
#install.packages("data.table")
library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
library(psych)
library(car)
library(tm)
library(stringr)
library(fuzzyjoin)
library(data.table)

packages <- c("plyr", "dplyr", "readr", "ggplot2", "psych")
invisible(lapply(packages, require, character.only = TRUE))
load.packages <- function(packages) {
    for (i in 1:length(packages)) {
        package <- as.character(packages[i])
        install.packages(package)
        library(package)
    }
}
load.packages(packages)

remove.bond.issuers <- function(keywords, bond_issuers) {
    for (i in 1:length(keywords)) {
        keyword <- as.character(keywords[i])
        bond_issuers_removed <- c(bond_issuers_removed, unique(bond_issuers[grepl(keyword, bond_issuers$iq_issuer)]))
        bond_issuers <- bond_issuers[!grepl(keyword, bond_issuers$iq_issuer),]
    }
}

#####################################
##### 1. DATA IMPORT & CLEANING #####
#####################################

##### 1.1 GOVERNMENT DATA #####
## Data: "Statistics on revenue, expenditure, debt, and assets (cash and security holdings) for governments. There are statistics for the 50 state areas and the District of Columbia, as well as a national summary."
government <- read_fwf(
    file = "Fin_GID_2014.txt",
    skip = 4,
    fwf_widths(c(14, 64, 35, 5, 5, 9, 2, 7, 2, 2, 2, 4, 2)))
colnames(government) = c("id_code", "id_name", "county_name", "fips", "fips_place_code", "population", "population_year", "enrollment", "enrollment_year", "function_code_for_special_districts", "school_level_code", "fiscal_year_ending", "survey_year")
raw_data_government <- government

##### 1.1.1 Decompose ID #####
# digit 1-2: state code
government["fips_state_code"] <- substr(government$id_code, 1, 2)
# digit 3: type of government 
government["type_of_government"] <- substr(government$id_code, 3, 3) # 0 = state, 1 = county, 2 = city, 3 = township, 4 = special district, 5 = independent school district
# digit 4-6: county or county-type area
government["fips_county_code"] <- substr(government$id_code, 4, 6)
# digit 7-9: unit identifier
government["unit_identifier"] <- substr(government$id_code, 7, 9)
# digit 10-14: should be 00000 to indicate that the unit is not part of another government
government["check_var_multiple_governments"] <- substr(government$id_code, 10, 14)

##### 1.1.2 Remove entries and columns #####
# remove all individual units = 3,4 or 5 since we're only interested in states, counties and cities
government <- government[!government$type_of_government %in% c(3, 4, 5),]
# since only school districts have enrollment and school information, we can drop the columns "Enrollment", "Enrollment_Year" and "School_Level_Code"
# "Function_Code_for_Special_Districts" is only for special districts, can be dropped
government <- government[, !(names(government) %in% c("enrollment", "enrollment_year", "function_code_for_special_districts", "school_level_code"))]
# All remaining items are not part of another government (=00000). Column can be removed
table(government$check_var_multiple_governments)
government <- government[, !(names(government) %in% "check_var_multiple_governments")]

##### 1.1.3 Clean FIPS codes #####
# some FIPS codes are obviously wrong; replace by correct values
government$fips <- as.numeric(government$fips)
government$id_code <- as.numeric(government$id_code)
# FIPS code is 2 (state code of Alaska); correct one is 02275
government[which(government$id_code == 2203600100000), "fips"] <- 02275

##### 1.1.4 Reformat data #####
# reformatting variables as appropriate
government$fips_county_code <- as.numeric(government$fips_county_code)
government$fips_place_code <- as.numeric(government$fips_place_code)
government$fips_state_code <- as.numeric(government$fips_state_code)
government$population_year <- as.numeric(government$population_year)
government$fiscal_year_ending <- as.factor(government$fiscal_year_ending) # fiscal year ending is a date, formatted as MDD --> factor variable
government$id_code <- as.numeric(government$id_code)
government$type_of_government <- as.factor(government$type_of_government)

##### 1.1.5 Subsetting government into state, county and city dataframes
government_state <- government[government$type_of_government == 0,]
government_county <- government[government$type_of_government == 1,]
government_city <- government[government$type_of_government == 2,]

government_matching_data <- rbind(government_city, government_county)

##### 1.2 MAPPING DATA #####
##### 1.2.1 FIPS to District table #####
mapping_fips_to_districts <- read.csv(file = "MAPPING_FIPS_to_District.csv", header = T, sep = ";", encoding = "UTF-8")
colnames(mapping_fips_to_districts) <- c("state", "county", "state_code", "county_code", "fips", "district", "numfips")
# load mapping list
# mapping_states_to_districts <- read.csv(file = "MAPPING_States_DistrictCourts_StateCodes.csv", header = T, sep = ";", encoding = "UTF-8")
# colnames(mapping_states_to_districts) = c("District_Courts", "State_Code", "State_Name", "Abbrevation")

##### 1.2.1.1 Cleaning #####
str(mapping_fips_to_districts)
mapping_fips_to_districts$county <- as.character(mapping_fips_to_districts$county)
mapping_fips_to_districts$county_code <- as.numeric(mapping_fips_to_districts$county_code)
# Shannon County, SD changed its name to Oglala Lakota County, SD in 2015. My mapping table was apparently outdated. Due to this name change, the FIPS changed from 46113 to 46102
mapping_fips_to_districts$fips[mapping_fips_to_districts$fips == 46113] <- 46102
mapping_fips_to_districts$numfips[mapping_fips_to_districts$fips == 46102] <- 46102
mapping_fips_to_districts$county[mapping_fips_to_districts$fips == 46102] <- "Oglala Lakota"
# Wade Hampton Census Area, A changed its name to Kusilvak Census Area, A in 2015. FIPS changed from 02270 to 02158
mapping_fips_to_districts$fips[mapping_fips_to_districts$fips == 02270] <- 02158
mapping_fips_to_districts$numfips[mapping_fips_to_districts$fips == 02158] <- 2158
mapping_fips_to_districts$county[mapping_fips_to_districts$fips == 02158] <- "Kusilvak"
# My mapping table has mapped 1 FIPS code twice. Remove it:
mapping_fips_to_districts <- mapping_fips_to_districts[!mapping_fips_to_districts$county == "Skagway-Yakutat",]

##### 1.3 MERGING #####
# merging of states to judicial districts not possible, since first

government_state <- merge(government_state, mapping_fips_to_districts, by = "state_code")
government_state <- government_state[!duplicated(government_state[, c("state_code")]),] # reduce to unique states
government_county <- merge(government_county, mapping_fips_to_districts, by = "fips")
government_city <- merge(government_city, mapping_fips_to_districts, by = "fips")


##### 1.4 CORRUPTION DATA #####
# Describing the activities of the Public Integrity Section from 1998 to 2015. 
# Also provides statistics on the nationwide federal effort against public corruption over the previous decade
corruption <- read.csv(file = "Public_Corruption_Data.csv", header = T, sep = ";", encoding = "UTF-8")
colnames(corruption) <- c("district_code", "district", "1998","1999","2000","2001","2002","2003","2004","2005","2006", "2007", "2008","2009","2010","2011","2012","2013", "2014","2015")
# add total count
total <- NULL
for (row in 1:nrow(corruption)) {
    total <- c(total, sum(corruption[row, 3:20], na.rm = T))
}
corruption$total <- as.numeric(total) # checksum = sum, all good

##### 1.5 BOND ISSUER FILE #####
# The bond issuer file is a list of all bond issuers. We need to clean from it every issuer who isn't a state, county or city.
bond_issuers <- read.csv(file = "./Transaction_Data/MSRB_cusip9_issuer_newfields.csv", header = T, sep = ";", encoding = "UTF-8", stringsAsFactors = F) # 1048575 elements

##### 1.5.1 Remove irrelevant issuers #####
bond_issuers$iq_issuer <- tolower(bond_issuers$iq_issuer)
# remove specific elements from the bond issuer file based on keyword detection
source("Custom_Functions.R")
v_keywords_for_exclusion <- c("school", "township", "special district", "educational", "hospital", "water", "utility", "building", "special", "fire", "tax", "corp", "housing", "college", "health", "career", "nursing")
bond_issuers <- remove.bond.issuers(v_keywords_for_exclusion, bond_issuers)

# development agencies are somewhat related to cities (improving infrastructure & co.); keep them, but add dummy variable 
bond_issuers$dummy_redevelopment <- 0
bond_issuers$dummy_redevelopment[grepl("development", bond_issuers$iq_issuer)] <- 1

# subsample unique issuers based on iq_issuer_ciqid
bond_issuers_unique <- bond_issuers[!duplicated(bond_issuers[, c("iq_issuer_ciqid")]),]

# subsample to test keywords for exlusion 
test <- bond_issuers[grepl("airport", bond_issuers$iq_issuer),]
test <- test[!duplicated(test[, c("iq_issuer_ciqid")]),]

##############################################################################################################
#
# Have another look at "corp" as a keyword!
#
##############################################################################################################


##### 1.5.2 Visual analysis #####
# Analyze the remaining issuers for keywords to exclude from the dataset. Create wordcloud for visual analysis
generate.word.cloud(bond_issuers_unique$iq_issuer, 50)

# Seems to be OK for now; further diggin later on!

##### 1.6 MATCHING BOND_ISSUER <--> GOVERNMENT DATA #####
bond_issuers_open <- bond_issuers_unique
# Preperation: add all columns that will be feature-engineered later
bond_issuers_open$county <- NA
bond_issuers_open$state <- NA
bond_issuers_open$city <- NA
##### 1.6.1 Match all states where # of FIPS == 1 #####
v_states_with_single_fips <- c("Alaska", "Arizona", "Colorado", "Connecticut", "Delaware", "Hawaii", "Idaho", "Kansas", "Maine", "Maryland", "Massachusetts", "Minnesota", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "North Dakota", "Oregon", "Puerto Rico", "Rhode Island", "South Carolina", "South Dakota", "Utah", "Vermont", "Washington, DC (District of Columbia)", "Wyoming")
v_states_with_single_fips <- tolower(v_states_with_single_fips)
for (i in 1:length(bond_issuers_open$iq_issuer)) {
    for (j in 1:length(v_states_with_single_fips)) {
        if (grepl(v_states_with_single_fips[j], bond_issuers_open$iq_issuer[i])) {
            bond_issuers_open$state[i] <- v_states_with_single_fips[j]
        }
    }
}
bond_issuers_merged <- subset(bond_issuers_open, (!is.na(bond_issuers_open$state)))
government_state$state <- tolower(government_state$state)
bond_issuers_merged <- join(bond_issuers_merged, government_state, by = "state", type = "left")
bond_issuers_open <- subset(bond_issuers_open, is.na(bond_issuers_open$state))

##### 1.6.2 Add state info with # of fips > 1 where possible based on keyword matching #####
v_list_of_states <- tolower(as.character(government_city$state[!duplicated(government_city$state)]))
for (i in 1:length(bond_issuers_open$iq_issuer)) {
    for (j in 1:length(v_list_of_states)) {
        if (grepl(v_list_of_states[j], bond_issuers_open$iq_issuer[i])) {
            bond_issuers_open$state[i] <- v_list_of_states[j]
        }
    }
}
  
bond_issuers_open$city <- bond_issuers_open$iq_issuer
# Remove common english words like "of", "from", ... containing no information of value for the matching
v_remove_words <- c(stopwords("english"), "&", "city", ",")
bond_issuers_open$city <- removeWords(bond_issuers_open$city, v_remove_words)
bond_issuers_open$city <- removeWords(bond_issuers_open$city, v_list_of_states)
##### 1.6.3. Match all elements where state == state && county == county
# first, create subset with all elements containing the word county, b/c sometimes a city has the same name as a county
bond_issuers_with_county <- bond_issuers_open[grepl("county", bond_issuers_open$iq_issuer),]

# extract counties based on keyword matching on "county"
v_list_of_counties <- tolower(as.character(government_city$county[!duplicated(government_city$county)]))
for (i in 1:length(bond_issuers_with_county$iq_issuer)) {
    for (j in 1:length(v_list_of_counties)) {
        if (grepl(v_list_of_counties[j], bond_issuers_with_county$city[i])) {
            bond_issuers_with_county$county[i] <- v_list_of_counties[j]
        }
    }
}

a1 <- subset(bond_issuers_with_county, (!is.na(bond_issuers_with_county$state)))
a1 <- subset(a1, (!is.na(a1$county)))

a <- join(a1, government_county, by = c("state", "county"),type= "left")
a <- a[!duplicated(a[, c("cusip")]),]

bond_issuers_merged <- rbind(bond_issuers_merged, a)
######################################
######################################
######################################
######################################
######################################
######################################
# Warum unterschiedliche # Spalten? 
# renaming und cleaning
# 


for (i in 1:length(bond_issuers_unique$city)) {
    if (grepl("county", bond_issuers_unique$city[i])) {
        bond_issuers_unique$county[i] <- bond_issuers_unique$city[i]
    }
}
bond_issuers_unique$county <- sapply(strsplit(bond_issuers_unique$county, "county"), `[`, 1)

##### 1.5.4 Create Excel File #####
# To handle the fuzzy matching, we use a excel plug-in. Therefore, a csv file is created
write.csv(bond_issuers, file = "bond_issuers_cleaned.csv")

####################################
##### 2. DESCRIPTIVE ANALSYSIS #####
####################################

##### 2.1 Exploratory analysis #####
# check if sum of population of counties of alaska == population of alaska
government_state$Population[government_state$State_Code == "02"]
sum(government_county$Population[government_county$State_Code.y == 02]) + sum(government_city$Population[government_city$State_Code.y == 02])
# NOPE; WHY????

describe.by(government$Population, government$State_Code)

ggplot(government_county, aes(x = reorder(District), y = Population, fill = State_Code)) +
geom_bar(stat = "identity") +
coord_flip() +
theme(legend.position = "none")


# 1.2: US DISTRICT COURT DISTRICTS
court_districts <- read.csv(file = "List_US_District_Court_District.csv", header = F, encoding = "UTF-8")

mapping_states_to_district_court_districts <- read.csv(file = "MAPPING_States_to_District_Court_Districts.csv", header = F, encoding = "UTF-8", sep = ";")

colnames(mapping_states_to_district_court_districts) <- c("District_Court_District", "State_Code")

hist(as.numeric(government$`Type of Government`), breaks = 10)


tail(government)
summary(government$`Individual Unit`)



#################
##### Stuff #####
#################

# merging both frames based on State_Code (digits 1-2 in "ID_Code" in government == State Code)
mapping_states_to_state_codes <- read.csv(file = "MAPPING_States_to_State_Codes.csv", header = T, encoding = "UTF-8", sep = ";")
mapping_states_to_state_codes$State.Code[1:10] <- c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09") # !! transform "1" -> "01", otherwise merging conflict ... 
colnames(mapping_states_to_state_codes) = c("State_Name", "Abbrevation", "State_Code")
government["State_Code"] <- substr(government$ID_Code, 1, 2)
government <- merge(government, mapping_states_to_state_codes)






# To improve fuzzy matching precision, I try to extract as much information out of the iq_issuer
a <- str_split_fixed(bond_issuers_unique$iq_issuer, ",", 2)
bond_issuers_unique$iq_issuer_city <- a[, 1]
bond_issuers_unique$iq_issuer_state <- a[, 2]
# Remove common english words like "of", "from", ... containing no information of value for the matching
bond_issuers_unique$iq_issuer_city <- removeWords(bond_issuers_unique$iq_issuer_city, v_remove_words)
############needed? 
bond_issuers_unique$county <- sapply(strsplit(bond_issuers_unique$county, "county"), `[`, 1)
merge_result <- subset(bond_issuers_unique, (!is.na(bond_issuers_unique$county)))
merge_result$county <- removeWords(merge_result$county, " ")
government_county$County <- tolower(government_county$County)



for (i in 1:length(merge_result$state)) {
    for (j in 1:length(government_county$State)) {
        if (merge_result$state == government_county$State) {
            a <- stringdist_join(merge_result, government_county, distance_col = "distance", max_dist = 1)
        }
    }
}




for (i in 1:length(merge_result$county)) {
    if (grepl(merge_result$c, bond_issuers_unique$city[i])) {
        bond_issuers_unique$county[i] <- bond_issuers_unique$city[i]
    }
}