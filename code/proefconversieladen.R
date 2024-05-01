rm(list = ls())
install_and_load_packages <- function(packages = c("ggplot2", "kableExtra", "readxl", "tibble", "tidyr", "tidyverse", "plotly", "dplyr", "networkD3")) {
  # Check if packages are not installed
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if (length(new_packages) > 0) {
    # Install missing packages
    install.packages(new_packages, dependencies = TRUE)
  }
  
  # Load the packages
  invisible(lapply(packages, library, character.only = TRUE))
}

# Call the function without providing a list (it will use the default list)
install_and_load_packages()



# Load CSV file with specified column classes
mynames <- c("Registratienummer", "Naam", "Rooster", "Eventtype", "Datum", "Email", "Telefoon", "Geconverteerd", "Lidmaatschap", "BekendVia")
proef <- read.table("data/zproefdata/ProeflesconversieAlle.csv", sep = ";", header = FALSE, col.names = mynames, skip = 1, fill = TRUE, quote = "")

proef <- proef %>%
  select(Registratienummer, Naam, Datum, Geconverteerd, Lidmaatschap, BekendVia)
rm(mynames)

# Load CSV file with specified column classes
mynames <- c("Registratienummer",	"Voornaam",	"Achternaam",	"Tussenvoegsel",	"Emailadres",	"Telefoonnummer",	"AantalLopendeAbbo",	"OpgezegdOfVerlengd",	"Product",	"Einddatum",	"NieuwProductGestart", "GestartPer", "RedenOpzegging",	"Toelichting")
opzeg <- read.table("data/zproefdata/Opzeggingen2023.csv", sep = ";", header = FALSE, col.names = mynames, skip = 1)
opzeg <- opzeg %>%
  select(Registratienummer, Voornaam, Tussenvoegsel, Achternaam, AantalLopendeAbbo, OpgezegdOfVerlengd, Product, Einddatum, RedenOpzegging, Toelichting) %>%
  rename(Productomschrijving = Product)
rm(mynames)

# Filteren Opzeggen
opzeg <- opzeg %>%
  filter(AantalLopendeAbbo == 0)


# PRODUCT FILTEREN OP REGEX
# Alle unieke productomschrijvingen, unieke abbo
unique_abbo <- unique(opzeg$Productomschrijving)
#   unique_abbo <- unique(subset(data, Producttype == "Abonnement")$Productomschrijving)
# Filteren, Define patterns to remove
patterns_to_remove <- c(
  " / \\d{2}\\.\\d{2} /",
  " / \\d{1}\\.\\d{2} /",
  " / \\d{3}\\.\\d{2} /",
  "Reguliere lessen - ",
  "Gesprekken - ",
  "Crossfit - ",
  "BFNL - High five ",
  "BFNL - BFNL ",
  "Abonnementen - ",
  "\\b\\d+ maand\\(en\\)\\b",
  "- gesprek of PT ",
  "\\(",
  "\\)",
  " incl Hyrox, Yoga, Rumble",
  " 2021",
  ", gesprek of PT",
  "-  gesprek of PT"
)
# Apply gsub with a loop for each pattern
for (pattern in patterns_to_remove) {
  unique_abbo <- trimws(gsub(pattern, "", unique_abbo))
}

# New abbo list "Product"
opzeg$Product <- opzeg$Productomschrijving  # Create a new column
for (pattern in patterns_to_remove) {
  opzeg$Product <- gsub(pattern, "", opzeg$Product)
}
opzeg$Product <- trimws(opzeg$Product)  


opzeg <- opzeg %>%
  mutate(
    Product = ifelse(grepl("1x  per week", Product), "1x per week", Product)
  )

# CATEGORIEEN DEFINIEREN

# Load CSV file with the list of values and corresponding categories
lijst <- read.table("data/zproefdata/categorieen.csv", sep = ";", header = TRUE)
# Create a data frame for data_category_list
data_category_list <- data.frame(
  value = c(lijst$value, lijst$valueRittenkaart, lijst$valueOverig, lijst$valueKassa),
  categorie = c(lijst$categorie, lijst$categorieRittenkaart, lijst$categorieOverig, lijst$categorieKassa)
)
data_category_list[] <- lapply(data_category_list, function(x) ifelse(nzchar(x), x, NA)) # Remove "" values
data_category_list[] <- lapply(data_category_list, function(x) ifelse(!is.na(x), x, NA)) # Remove "NA" values
data_category_list <- na.omit(data_category_list) # Remove rows with NAs
# Merge the data dataframe with the data_category_list dataframe
opzeg <- merge(opzeg, data_category_list, by.x = "Product", by.y = "value", all.x = TRUE)
opzeg$categorie <- ifelse(is.na(opzeg$categorie), as.character(opzeg$Product), as.character(opzeg$categorie))

# data$categorie filteren op pt en fitaid
opzeg <- opzeg %>%
  mutate(
    categorie = case_when(
      grepl("personal training", categorie, ignore.case = TRUE) ~ "PT",
      grepl("Fit[- ]?Aid", categorie, ignore.case = TRUE) ~ "FitAid",
      TRUE ~ as.character(categorie)
    )
  )

rm(pattern, a, patterns_to_remove, unique_abbo, lijst)

opzeg <- opzeg %>%
  mutate(
    categorie = ifelse(Product == "Ski marathon - Evenement", "Event", 
                       ifelse(Product == "Traject - Beginnerscursus: \"Wat is conditie?\"", "Traject", 
                              ifelse(Product == "Traject - Beginnerscursus 8 januari", "Traject", 
                                     ifelse(Product == "Module - Creeer balans", "Traject", 
                                            ifelse(Product == "Trajecten - Barbell en dumbell strength", "Traject", categorie)
                                     )
                              )
                       )
    )
  )


## Error met de start 2x per week?
opzeg <- opzeg %>%
  mutate(
    categorie = ifelse(Product == "De Start 2x per week", "2x per week", categorie)
  )


##
##
#
##



### ### ### ### ### ###
library(dplyr)
library(lubridate)

# Date conversion for 'proef'
proef$Maand <- as.Date(proef$Datum, format = "%d/%m/%Y")
proef$Maand <- format(proef$Maand, "%Y-%m")

# Date conversion for 'opzeg'
# Convert opzeg%Einddatum to Date format
opzeg <- opzeg %>%
  mutate(Einddatum = dmy(Einddatum))

# Extract the month in yyyy-mm format
opzeg <- opzeg %>%
  mutate(EindMaand = format(Einddatum, "%Y-%m"))


# Remove specified columns
opzeg <- opzeg %>%
  select(-AantalLopendeAbbo, -OpgezegdOfVerlengd)

rm(pattern, patterns_to_remove, unique_abbo)

#

#

#




