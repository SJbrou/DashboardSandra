library(tidyr)
library(dplyr)
library(plotly)
library(ggplot2)
library(networkD3)
library(knitr)
library(lubridate)





facturen_laden <- function() {
  # Check for files
  files <- list.files(path = "data", pattern = "facturen", full.names = TRUE)
  
  # Initialize an empty data frame to store the combined data
  data <- data.frame()
  
  # Load the first CSV file with specified column classes
  data1 <- read.csv(files[1], sep = ";", header = TRUE, skip = 3)
  # data2 <- read.csv(files[2], sep = ";", header = TRUE, skip = 3)
  
  # Loop through the remaining files, if any, and combine data frames
  # if (length(files) > 1) {
  #  for (i in 2:length(files)) {
  #    data_new <- read.csv(files[i], sep = ";", header = TRUE, skip = 3)
  #    data <- rbind(data, data_new)
  #  }
  #}
  
  data <- data1
  #data <- rbind(data1, data2)
  ## Verwijderen waar niet betaald is
  data <- subset(data, Betaalstatus == "Voldaan" | Betaalstatus == "Gecrediteerd")
  #data1 <- subset(data1, Betaalstatus == "Voldaan" | Betaalstatus == "Gecrediteerd")
  #data2 <- subset(data2, Betaalstatus == "Voldaan" | Betaalstatus == "Gecrediteerd")
  
  
  ## Data juist definieren
  data <- data %>%
    select(Registratienummer, Voornaam, Tussenvoegsel, Achternaam, Factuurdatum, Factuurbedrag, Besteldatum, Betaaldatum, Betaalmethode, Betaalstatus, Producttype, Productomschrijving)
  
  ## Factuurbedrag converteren naar numeriek, comma's en punten goed verwerken
  data$Factuurbedrag <- as.numeric(gsub(",", ".", gsub("\\.", "", data$Factuurbedrag)))
  
  ## Datum naar juiste format converteren
  data$BestelMaand <- as.Date(gsub("-", "/", data$Besteldatum), format = "%d/%m/%Y")
  data$BetaalMaand <- as.Date(gsub("-", "/", data$Betaaldatum), format = "%d/%m/%Y")
  data$BestelMaand <- format(data$BestelMaand, "%Y-%m")
  data$BetaalMaand <- format(data$BetaalMaand, "%Y-%m")
  
  # data_new <- subset(data_new, Betaalstatus == "Voldaan" | Betaalstatus == "Gecrediteerd")
  ## Regex
  if (!file.exists("data/regex.csv")) {
    regexcsv <- data.frame(
      regex = c("Value1", "Value2")
    )
    write.csv(regexcsv, "data/regex.csv", sep = ";", row.names = FALSE)
    rm(regexcsv)
  } else {
    patterns_to_remove <- read.csv("data/regex.csv", header = TRUE, sep=";")
    patterns_to_remove <- patterns_to_remove$regex
  }
  
  
  data$Product <- data$Productomschrijving
  for (pattern in patterns_to_remove) {
    data$Product <- gsub(pattern, "", data$Product)
  }
  
  
  data <- data %>%
    mutate(
      Product = ifelse(grepl("1x  per week", Product), "1x per week", Product)
    )
  
  rm(pattern, patterns_to_remove)
  ##########################################################################################
  # Categoriseren 
  ##########################################################################################
  
  ## Configuratie categorieen, producten
  if (!file.exists("data/configuratie.csv")) {
    configuratie <- data.frame(
      Productnaam = unique(data$Product),
      Categorie = rep("", length(unique(data$Product))),
      Product = rep("", length(unique(data$Product))),
      Maanden = rep("", length(unique(data$Product))),
      Credits = rep("", length(unique(data$Product)))
    )
    write.csv2(configuratie, "data/configuratie.csv", sep = ",", row.names = FALSE)
    rm(configuratie)
  } else {
    configuratiecsv <- read.csv("data/configuratie.csv", header = TRUE, sep = ";", quote = "", na.strings = "")
  }
  
  data_category_list <- data.frame(
    value = configuratiecsv$Productnaam,
    categorie = configuratiecsv$Categorie
  )
  data_category_list[] <- lapply(data_category_list, function(x) ifelse(nzchar(x), x, NA))
  data_category_list[] <- lapply(data_category_list, function(x) ifelse(!is.na(x), x, NA))
  data_category_list <- na.omit(data_category_list)
  
  matching_indices <- match(data$Product, data_category_list$value)
  
  # Update data$categorie with corresponding values from data_category_list$categorie
  data$categorie[!is.na(matching_indices)] <- data_category_list$categorie[matching_indices[!is.na(matching_indices)]]
  data$categorie <- ifelse(is.na(data$categorie), as.character(data$Product), as.character(data$categorie))
  
  
  # data$categorie filteren op pt en fitaid
  data <- data %>%
    mutate(
      categorie = case_when(
        grepl("personal training", categorie, ignore.case = TRUE) ~ "PT",
        grepl("Fit[- ]?Aid", categorie, ignore.case = TRUE) ~ "Overig",
        grepl("3x per week ", categorie, ignore.case = TRUE) ~ "3x per week",
        grepl("4x per week ", categorie, ignore.case = TRUE) ~ "4x per week",
        grepl("1 PT per week\nt/m 31-12-2024", categorie, ignore.case = TRUE) ~ "PT",
        grepl("Trajecten - beginnerscursus ", categorie, ignore.case = TRUE) ~ "Traject",
        grepl("Trajecten - beginnerscursus", categorie, ignore.case = TRUE) ~ "Traject",
        grepl("Traject - Lose Weight stay fit", categorie, ignore.case = TRUE) ~ "Traject",
        grepl("Traject - Creeer balans", categorie, ignore.case = TRUE) ~ "Traject",
        grepl("Unlimited- April \ntot 19 april", categorie, ignore.case = TRUE) ~ "Energy Fit",
        grepl("Module - Creeer balans ", categorie, ignore.case = TRUE) ~ "Traject",
        grepl("Module - Creëer de kracht van je leven", categorie, ignore.case = TRUE) ~ "Traject",
        
        
        TRUE ~ as.character(categorie)
      )
    )
  
  data <- data %>%
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
  
  
  ## Product aanpassen voor Rittenkaarten en Trajecten
  rm(data_category_list)  # Remove previous data_category_list if exists
  data_category_list <- data.frame(
    value = configuratiecsv$Productnaam,
    productnaam = configuratiecsv$Product
  )
  
  data_category_list <- data.frame(
    value = configuratiecsv$Productnaam,
    prodnaam = configuratiecsv$Product
  )
  data_category_list[] <- lapply(data_category_list, function(x) ifelse(nzchar(x), x, NA))
  data_category_list[] <- lapply(data_category_list, function(x) ifelse(!is.na(x), x, NA))
  data_category_list <- na.omit(data_category_list)
  
  matching_indices <- match(data$Product, data_category_list$value)
  
  # Update data$categorie with corresponding values from data_category_list$categorie
  data$Product[!is.na(matching_indices)] <- data_category_list$prodnaam[matching_indices[!is.na(matching_indices)]]
  
  
  
  
  ### Trajecten
  data$Product[data$Product == "Trajecten - beginnerscursus"] <- "Beginnerscursus"
  data$Product[data$Product == "Traject - Lose weight stay fit"] <- "Lose weight"
  data$Product[data$Product == "Module - Creëer de kracht van je leven"] <- "Kracht van je leven"
  data$Product[data$Product == "Trajecten - beginnerscursus"] <- "Beginnerscursus"
  
  
  
  
  return(data)
}

data <- facturen_laden()