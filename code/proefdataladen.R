install_and_load_packages <- function(packages = c("ggplot2", "kableExtra", "readxl", "tibble", "tidyr", "tidyverse", "plotly", "dplyr", "networkD3", "lubridate")) {
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



## facturen_laden
# DATA LADEN
facturen_laden <- function(filteren = TRUE) {
  print("if filteren = TRUE, the data will be categorised")
  
  # Load the first CSV file with specified column classes
  data <- read.csv("data/zproefdata/Facturen2023.csv", sep = ";", header = TRUE, skip = 3, fill = TRUE)
  
  # Select relevant columns
  data <- data %>%
    select(Registratienummer, Voornaam, Tussenvoegsel, Achternaam, Factuurdatum, Factuurbedrag, Besteldatum, Betaaldatum, Betaalmethode, Betaalstatus, Producttype, Productomschrijving)
  
  ###############################
  # verwerking van de factuurdata
  ###############################
  
  # Factuurbedrag conversion of euro to numeric
  data$Factuurbedrag <- as.numeric(gsub(",", ".", gsub("\\.", "", data$Factuurbedrag)))
  
  # Date conversion
  data$BestelMaand <- as.Date(data$Besteldatum, format = "%d/%m/%Y")
  data$BetaalMaand <- as.Date(data$Betaaldatum, format = "%d/%m/%Y")
  data$BestelMaand <- format(data$BestelMaand, "%Y-%m")
  data$BetaalMaand <- format(data$BetaalMaand, "%Y-%m")
  
  # Verwijderen waar niet betaald is  
  nietbetaald <- nrow(data)                         # tellen hoeveel rijen er zijn
  data <- subset(data, Betaalstatus == "Voldaan" | Betaalstatus == "Gecrediteerd")   # filteren op betaalstatus
  nietbetaald <- (nietbetaald - nrow(data))         # berekenen hoeveel rijen verwijderd zijn
  
  data_PID <<- data %>%
    select(Registratienummer, Voornaam, Tussenvoegsel, Achternaam) %>%
    distinct()
  
  data <- data %>%
    select(-Voornaam, -Tussenvoegsel, -Achternaam, -Factuurdatum, -Besteldatum, -Betaalmethode, -Betaaldatum, -Betaalstatus)  
  
  ##########################################################################################
  # Categoriseren 
  ##########################################################################################
  
  if (filteren) {
    # PRODUCT FILTEREN OP REGEX
    # Alle unieke productomschrijvingen, unieke abbo
    unique_abbo <- unique(data$Productomschrijving)
    #   unique_abbo <- unique(subset(data, Producttype == "Abonnement")$Productomschrijving)
    # Filteren, Define patterns to remove
    patterns_to_remove <- c(
      "Abonnementen \\d{4} - ",
      "Abonnementen - ",
      "Crossfit \\d{4} - ",
      "CrossFit Energy - ",
      " / \\b\\w+ \\d{4}\\b",
      " - \\b\\w+ \\d{4}\\b",
      " / DOWNGRADE",
      " / UPGRADE",
      " / VERLENGING",
      ", gesprek of PT",
      "Reguliere lessen - ",
      "Maand abonnement - ",
      "Crossfit - ",
      "BFNL - ",
      "  gesprek of PT",
      "Gesprekken - ",
      "Crossfit CFE - ",
      "Crossfit 2021 CFE - ",
      " - NLG",
      " - 1 maand",
      " (+Hyrox, Yoga, Rumble)",
      " 2021",
      " - coach abonnement",
      #  "Jaarbetaling - ",
      #  " jaar betaling",
      #  "Jaarbetaling",
      " - incl. next life gym",
      " all-in",
      " CFE",
      "Crossfit Energy - ",
      "2024 - "
    )
    # Apply gsub with a loop for each pattern
    for (pattern in patterns_to_remove) {
      unique_abbo <- trimws(gsub(pattern, "", unique_abbo))
    }
    # New abbo list "Product"
    data$Product <- data$Productomschrijving  # Create a new column
    for (pattern in patterns_to_remove) {
      data$Product <- gsub(pattern, "", data$Product)
    }
    
    data <- data %>%
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
    data <- merge(data, data_category_list, by.x = "Product", by.y = "value", all.x = TRUE)
    data$categorie <- ifelse(is.na(data$categorie), as.character(data$Product), as.character(data$categorie))
    
    # data$categorie filteren op pt en fitaid
    data <- data %>%
      mutate(
        categorie = case_when(
          grepl("personal training", categorie, ignore.case = TRUE) ~ "PT",
          grepl("Fit[- ]?Aid", categorie, ignore.case = TRUE) ~ "FitAid",
          TRUE ~ as.character(categorie)
        )
      )
    
    rm(pattern, a, patterns_to_remove, unique_abbo, lijst)
    
    data <- data %>%
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
    
    #############################################
    #############################################
  }
  
  data <- data %>%
    select(Registratienummer, Producttype, Productomschrijving, Product, categorie, BestelMaand, BetaalMaand, Factuurbedrag)
  
  return(list(data = data, data_PID = data_PID))
}

# Example usage:
result <- facturen_laden()
data <- result$data
data_PID <- result$data_PID
rm(result)


