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
library(readxl)
library(chron)


izettle_laden <- function() {
  library(readxl)
  
  ### Check for files
  # List files matching the pattern in the "data" directory
  files <- list.files(path = "data", pattern = "Zettle", full.names = TRUE)
  
  # Load the first Excel file with specified column classes
  datai <- read_xlsx(files[1], skip = 6, col_names = FALSE)
  
  # Loop through the remaining files, if any, and combine dataframes
  if (length(files) > 1) {
    for (i in 2:length(files)) {
      datai_new <- read_xlsx(files[i], skip = 6, col_names = FALSE)
      datai <- rbind(datai, datai_new)
    }
  }
  
# Juiste colommen selecteren
my_col_names <- (c("Datum", "Tijd", "a", "b", "Naam", "c", "d", "Aantal", "PrijszKorting", "Korting", "Prijs", "e", "f", "Kostprijs", "g"))
colnames(datai) <- my_col_names


# Tijd definieren
datai$JaarMaand <- format(datai$Datum, "%Y-%m")
datai$Datum <- format(datai$Datum, "%Y-%m-%d")
datai$Tijd <- format(datai$Tijd, "%H:%M")
  ## Data juist definieren
  datai <- datai %>%
    select(Datum, JaarMaand, Tijd, Naam, Aantal, PrijszKorting, Korting, Prijs, Kostprijs)
  
# Categorieen definieren
#  barverkoop - kleding - supplementen - eiwit - overig
  
  configuratiecsv <- read.table("data/izettleconfiguratie.csv", header = TRUE, sep = ";", fill = TRUE, quote = "", na.strings = "")
  data_category_list <- data.frame(
    value = configuratiecsv$Productnaam,
    categorie = configuratiecsv$Categorie
  )
  data_category_list[] <- lapply(data_category_list, function(x) ifelse(nzchar(x), x, NA))
  data_category_list[] <- lapply(data_category_list, function(x) ifelse(!is.na(x), x, NA))
  data_category_list <- na.omit(data_category_list)
  
  datai <- merge(datai, data_category_list, by.x = "Naam", by.y = "value", all.x = TRUE)
  datai$categorie <- ifelse(is.na(datai$categorie), as.character(datai$Naam), as.character(datai$categorie)) 
  
  
  
  return(datai)
}