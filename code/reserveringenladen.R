rm(list = ls())

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
reserveringen_laden <- function(files = "data/Overzicht reserveringen2024.csv") {
  # facturen_laden <- function(filteren = TRUE, files = ("data/Facturen2023.csv")) {

  
  # Load the first CSV file with specified column classes
  data <- read.table(files[1], sep = ";", header = TRUE, skip = 0, fill = TRUE)
  
  # Loop through the remaining files, if any, and combine dataframes
  if (length(files) > 1) {
    for (i in 2:length(files)) {
      data_new <- read.table(files[i], sep = ";", header = TRUE, skip = 3, fill = TRUE)
      data <- rbind(data, data_new)
    }
  }
  
  # Rename empty column headers
  data <- data %>%
    rename(Dag = Datum, Tijd = X, Datum = X.1)
  
  # Convert Datum to dd-mm-yyyy
  # Replace Dutch month names with numeric equivalents
  data$Datum <- gsub("januari", "01", data$Datum)
  data$Datum <- gsub("februari", "02", data$Datum)
  data$Datum <- gsub("maart", "03", data$Datum)
  data$Datum <- gsub("april", "04", data$Datum)
  data$Datum <- gsub("mei", "05", data$Datum)
  data$Datum <- gsub("juni", "06", data$Datum)
  data$Datum <- gsub("juli", "07", data$Datum)
  data$Datum <- gsub("augustus", "08", data$Datum)
  data$Datum <- gsub("september", "09", data$Datum)
  data$Datum <- gsub("oktober", "10", data$Datum)
  data$Datum <- gsub("november", "11", data$Datum)
  data$Datum <- gsub("december", "12", data$Datum)
  # Convert Datum to dd-mm-yyyy
  data$Datum <- as.Date(data$Datum, format = "%d %m %Y")
  data$Datum <- format(data$Datum, "%Y-%m-%d")
  return(data)
}
data <- reserveringen_laden()