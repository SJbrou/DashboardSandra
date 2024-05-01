library(shinydashboard)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)
library(ggplot2)
library(networkD3)
library(knitr)
library(googlesheets4)
library(lubridate)
### Check for files
files <- list.files(path = "data", pattern = "fmelders", full.names = TRUE)

# Load the first CSV file with specified column classes
afmelders <- read.table(files[1], sep = ";", header = TRUE, skip = 0, fill = TRUE, quote = "")

# Loop through the remaining files, if any, and combine dataframes
if (length(files) > 1) {
  for (i in 2:length(files)) {
    afmelders_new <- read.table(files[i], sep = ";", header = FALSE, col.names = mynames, skip = 1)
    afmelders <- rbind(afmelders, afmelders_new)
  }
}



























##############################
# Data ordenen
##############################


# Datum Tijd definieren
library(lubridate)
library(hms)

# Assuming "Trainingsdatum" is the name of the column containing the date-time strings
afmelders$Trainingsdag <- format(dmy_hms(afmelders$Trainingsdatum), "%d-%m-%y")
afmelders$Trainingstijd <- format(dmy_hms(afmelders$Trainingsdatum), "%H:%M")

# Assuming "Trainingsdatum" and "Afmelddatum" are the names of the columns containing the date-time strings
afmelders$Trainingsdatum <- dmy_hms(afmelders$Trainingsdatum)
afmelders$Afmelddatum <- dmy_hms(afmelders$Afmelddatum)

# Calculate the time difference
afmelders$TimeDifference <- as.hms(afmelders$Trainingsdatum - afmelders$Afmelddatum)

# Format the time difference as hours:minutes
afmelders$delta <- sprintf("%02d:%02d", hour(afmelders$TimeDifference), minute(afmelders$TimeDifference))

afmelders <- subset(afmelders, select = -c(Trainingsdatum, TimeDifference))


##############################
# Lestype categoriseren
##############################

# Sample list for categorization

# Sample data frame with names to match and names to replace
categorie_df <- data.frame(
  OriginalName = c(
    "\"Advanced core & balance\"",
    "\"Advanced OLY\"",
    "\"Advanced raw strength\"",
    "\"Afspraak Karen\"",
    "\"Afspraak Manja\"",
    "\"Basic Barbell Strength and Core\"",
    "\"basic core & balance\"",
    "\"Beginnerscursus \"",
    "Conditioning",
    "\"Conditioning WOD\"",
    "\"Core & balans\"",
    "\"Crossfit & boksen\"",
    "Evenement",
    "Gymnastics",
    "\"Hero workout \"",
    "\"KERST WOD\"",
    "Kids",
    "Masters",
    "Mobility",
    "\"Murph / Team Murph\"",
    "OLY",
    "\"OLY basic\"",
    "\"Ouder en kind WOD\"",
    "\"Stink & Drink\"",
    "Strength",
    "Strongfit",
    "\"Team/Duo WOD\"",
    "Teens",
    "\"Triathlon Bike\"",
    "\"Triathlon running\"",
    "WOD",
    "Yoga"),
  Replacement = c(
    "Core & balans",
    "OLY",
    "Strength",
    "Overig",
    "Overig",
    "Strength",
    "Core & balans",
    "Overig",
    "Conditioning",
    "WOD",
    "Core & balans",
    "Overig",
    "Overig",
    "Overig",
    "WOD",
    "Overig",
    "KidsTeens",
    "Masters",
    "Overig",
    "Overig",
    "OLY",
    "OLY",
    "Overig",
    "Overig",
    "Strength",
    "Strongfit",
    "WOD",
    "KidsTeens",
    "Overig",
    "Overig",
    "WOD",
    "Overig"),
  stringsAsFactors = FALSE
)

# Assuming "Lestype" is the column to be categorized in your data frame
afmelders$Lestype <- ifelse(afmelders$Lestype %in% categorie_df$OriginalName,
                            categorie_df$Replacement[match(afmelders$Lestype, categorie_df$OriginalName)],
                            afmelders$Lestype)

rm(categorie_df)
# Now "Lestype" has been replaced with the corresponding Replacement values
