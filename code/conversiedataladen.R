
# Load CSV file with specified column classes
mynames <- c("Registratienummer", "Naam", "Rooster", "Eventtype", "Datum", "Email", "Telefoon", "Geconverteerd", "Lidmaatschap", "BekendVia")


### Check for files
files <- list.files(path = "data", pattern = "oeflesconversie", full.names = TRUE)

# Load the first CSV file with specified column classes
proef <- read.table(files[1], sep = ";", header = FALSE, col.names = mynames, skip = 1, fill = TRUE, quote = "")

# Loop through the remaining files, if any, and combine dataframes
if (length(files) > 1) {
  for (i in 2:length(files)) {
    proef_new <- read.table(files[i], sep = ";", header = FALSE, col.names = mynames, skip = 1, fill = TRUE, quote = "")
    proef <- rbind(proef, proef_new)
  }
}



proef <- proef %>%
  select(Registratienummer, Naam, Datum, Geconverteerd, Lidmaatschap, BekendVia)
rm(mynames)

# Load CSV file with specified column classes
mynames <- c("Registratienummer",	"Voornaam",	"Achternaam",	"Tussenvoegsel",	"Emailadres",	"Telefoonnummer",	"AantalLopendeAbbo",	"OpgezegdOfVerlengd",	"Product",	"Einddatum",	"NieuwProductGestart", "GestartPer", "RedenOpzegging",	"Toelichting")

### Check for files
files <- list.files(path = "data", pattern = "pzeggingen", full.names = TRUE)

# Load the first CSV file with specified column classes
opzeg <- read.table(files[1], sep = ";", header = FALSE, col.names = mynames, skip = 1)

# Loop through the remaining files, if any, and combine dataframes
if (length(files) > 1) {
  for (i in 2:length(files)) {
    opzeg_new <- read.table(files[i], sep = ";", header = FALSE, col.names = mynames, skip = 1)
    opzeg <- rbind(opzeg, opzeg_new)
  }
}

opzeg <- opzeg %>%
  select(Registratienummer, Voornaam, Tussenvoegsel, Achternaam, AantalLopendeAbbo, OpgezegdOfVerlengd, Product, Einddatum, RedenOpzegging, Toelichting) %>%
  rename(Productomschrijving = Product)
rm(mynames, opzeg1, opzeg2, opzeg3)





# Filteren Opzeggen
opzeg$Productomschrijving <- gsub("\\d+\\ maand\\(en\\)", "", opzeg$Productomschrijving)
opzeg$Productomschrijving <- gsub("\\s*/\\s*\\d+\\.\\d+\\s*/\\s*", "", opzeg$Productomschrijving)


# New abbo list "Product"

patterns_to_remove <- read.csv("data/regex.csv", header = TRUE, sep=";")
patterns_to_remove <- patterns_to_remove$regex



opzeg$Product <- opzeg$Productomschrijving
for (pattern in patterns_to_remove) {
  opzeg$Product <- gsub(pattern, "", opzeg$Product)
}

opzeg <- opzeg %>%
  mutate(
    Product = ifelse(grepl("1x  per week", Product), "1x per week", Product)
  )




####


## Configuratie categorieen, producten
configuratiecsv <- read.table("data/configuratie.csv", header = TRUE, sep = ";", fill = TRUE, quote = "", na.strings = "")

data_category_list <- data.frame(
  value = configuratiecsv$Productnaam,
  categorie = configuratiecsv$Categorie
)
data_category_list[] <- lapply(data_category_list, function(x) ifelse(nzchar(x), x, NA))
data_category_list[] <- lapply(data_category_list, function(x) ifelse(!is.na(x), x, NA))
data_category_list <- na.omit(data_category_list)

opzeg <- merge(opzeg, data_category_list, by.x = "Product", by.y = "value", all.x = TRUE)
opzeg$categorie <- ifelse(is.na(opzeg$categorie), as.character(opzeg$Product), as.character(opzeg$categorie))


# data$categorie filteren op pt en fitaid
opzeg <- opzeg %>%
  mutate(
    categorie = case_when(
      grepl("personal training", categorie, ignore.case = TRUE) ~ "PT",
      grepl("Fit[- ]?Aid", categorie, ignore.case = TRUE) ~ "Overig",
      TRUE ~ as.character(categorie)
    )
  )

opzeg <- opzeg %>%
  mutate(
    categorie = ifelse(Product == "Ski marathon - Evenement", "Event", 
                       ifelse(Product == "Traject - Beginnerscursus: \"Wat is conditie?\"", "Traject", 
                              ifelse(Product == "Traject - Beginnerscursus 8 januari", "Traject", 
                                     ifelse(Product == "Module - Creeer balans", "Traject", 
                                            ifelse(Product == "Trajecten - Barbell en dumbell strength", "Traject",
                                                   ifelse(Product == "Module - Creeer de kracht van je leven", "Traject" ,categorie))
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

# Display the modified dataframe
# print(opzeg)


opzeg_all <- opzeg
opzeg <- opzeg %>%
  filter(AantalLopendeAbbo == 0)


# Remove specified columns
opzeg <- opzeg %>%
  select(-AantalLopendeAbbo, -OpgezegdOfVerlengd)

rm(pattern, patterns_to_remove, data_category_list, configuratiecsv, files, opzeg_new, i)




unique(opzeg$categorie)
