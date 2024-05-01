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
  files <- list.files(path = "data", pattern = "Facturen", full.names = TRUE)
  
  
  
  # Load the first CSV file with specified column classes
  data <- read.table(files[1], sep = ";", header = TRUE, skip = 3, fill = TRUE)
  
  # Loop through the remaining files, if any, and combine dataframes
  if (length(files) > 1) {
    for (i in 2:length(files)) {
      data_new <- read.table(files[i], sep = ";", header = TRUE, skip = 3, fill = TRUE)
      data <- rbind(data, data_new)
    }
  }
  
  ## Data juist definieren
  data <- data %>%
    select(Registratienummer, Voornaam, Tussenvoegsel, Achternaam, Factuurdatum, Factuurbedrag, Besteldatum, Betaaldatum, Betaalmethode, Betaalstatus, Producttype, Productomschrijving)
  
  
  
  data_PID <<- data %>%
    select(Registratienummer, Voornaam, Tussenvoegsel, Achternaam) %>%
    distinct()
  
  
  
  ## Factuurbedrag converteren naar numeriek, comma's en punten goed verwerken
  data$Factuurbedrag <- as.numeric(gsub(",", ".", gsub("\\.", "", data$Factuurbedrag)))
  
  ## Datum naar juiste format converteren
  data$BestelMaand <- as.Date(gsub("-", "/", data$Besteldatum), format = "%d/%m/%Y")
  data$BetaalMaand <- as.Date(gsub("-", "/", data$Betaaldatum), format = "%d/%m/%Y")
  data$BestelMaand <- format(data$BestelMaand, "%Y-%m")
  data$BetaalMaand <- format(data$BetaalMaand, "%Y-%m")
  
  ## Verwijderen waar niet betaald is
  data <- subset(data, Betaalstatus == "Voldaan" | Betaalstatus == "Gecrediteerd")
  
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
    write.csv2(configuratie, "data/configuratie.csv", sep = ";", row.names = FALSE)
    rm(configuratie)
  } else {
    configuratiecsv <- read.table("data/configuratie.csv", header = TRUE, sep = ";", fill = TRUE, quote = "", na.strings = "")
  }
  
  data_category_list <- data.frame(
    value = configuratiecsv$Productnaam,
    categorie = configuratiecsv$Categorie
  )
  data_category_list[] <- lapply(data_category_list, function(x) ifelse(nzchar(x), x, NA))
  data_category_list[] <- lapply(data_category_list, function(x) ifelse(!is.na(x), x, NA))
  data_category_list <- na.omit(data_category_list)
  
  data <- merge(data, data_category_list, by.x = "Product", by.y = "value", all.x = TRUE)
  data$categorie <- ifelse(is.na(data$categorie), as.character(data$Product), as.character(data$categorie))
  
  
  # data$categorie filteren op pt en fitaid
  data <- data %>%
    mutate(
      categorie = case_when(
        grepl("personal training", categorie, ignore.case = TRUE) ~ "PT",
        grepl("Fit[- ]?Aid", categorie, ignore.case = TRUE) ~ "Overig",
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
  ## Product aanpassen voor Rittenkaarten en Trajecten
  rm(data_category_list)  # Remove previous data_category_list if exists
  data_category_list <- data.frame(
    value = configuratiecsv$Productnaam,
    productnaam = configuratiecsv$Product
  )
  
  # Remove empty values and NAs
  data_category_list <- data_category_list %>%
    filter(value != "", !is.na(value)) %>%
    filter(productnaam != "", !is.na(productnaam))
  
  # Left join with data
  data <- merge(data, data_category_list, by.x = "Product", by.y = "value", all.x = TRUE)
  data$Product <- ifelse(!is.na(data$productnaam), as.character(data$productnaam), as.character(data$Product))
  data <- subset(data, select = -productnaam)
  
  
  ## Opschonen workspace
  rm(pattern, patterns_to_remove, configuratiecsv, data_category_list)