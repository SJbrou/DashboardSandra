# aantallen.R
# Function to generate Aantallen related outputs

## Aantallen per maand per categorie line plot
plot_aantal_pmpc_line <- function(data) {
  aantal <- data %>%
    filter(data$Producttype == "Abonnement") %>%
    group_by(BestelMaand, categorie) %>%
    summarise(OrdersCount = n(), .groups = 'drop') %>%
    pivot_wider(names_from = BestelMaand, values_from = OrdersCount, values_fill = 0)
  # Convert omzet to long format for plotting
  aantal_long <- pivot_longer(aantal, cols = -categorie, names_to = "BestelMaand", values_to = "OrdersCount")
  aantal_long <- aantal_long %>%
    filter(!(BestelMaand %in% c("2022-11", "2022-12")))
  # Plot
  plot <- plot_ly(aantal_long, x = ~BestelMaand, y = ~OrdersCount, type = "scatter", mode = "lines+markers",
                  color = ~categorie, colors = "Set1", text = ~paste(categorie, ": ", OrdersCount),
                  hoverinfo = "text") %>%
    layout(xaxis = list(title = "Maand"),
           yaxis = list(title = "Aantal"))
  return(plot)
}



## Aantallen per maand per categorie bar plot
plot_aantal_pmpc_bar <- function(data) {
  aantal <- data %>%
    filter(data$Producttype == "Abonnement") %>%
    group_by(BestelMaand) %>%
    summarise(OrdersCount = n())
  
  # Plot
  plot <- plot_ly(aantal, x = ~BestelMaand, y = ~OrdersCount, type = "bar",
                  hoverinfo = "text", color = ~Producttype, colors = "Set1", marker = list(line = list(width = 1)),
                  text = ~paste("Abonnement", ": ", OrdersCount), textposition = "inside") %>%
    layout(xaxis = list(title = "Maand"),
           yaxis = list(title = "Aantal"),
           barmode = 'stack')
  return(plot)
}



plot_aantal_rittenkaart_pmpc_bar <- function(data) {
  aantal <- data %>%
    filter(Producttype == "Rittenkaart") %>%  # Filter data where Producttype is "Rittenkaart"
    group_by(BestelMaand, Product) %>%
    summarise(OrdersCount = n(), .groups = 'drop') %>%
    pivot_wider(names_from = BestelMaand, values_from = OrdersCount, values_fill = 0)
  
  # Convert aantal to long format for plotting
  aantal_long <- pivot_longer(aantal, cols = -Product, names_to = "BestelMaand", values_to = "OrdersCount")
  aantal_long <- aantal_long %>%
    filter(!(BestelMaand %in% c("2022-11", "2022-12")))
  
  # Plot
  plot <- plot_ly(aantal_long, x = ~BestelMaand, y = ~OrdersCount, type = "bar",
                  hoverinfo = "text", color = ~Product, colors = "Set1", marker = list(line = list(width = 1)),
                  text = ~paste(Product, ": ", OrdersCount), textposition = "inside") %>%
    layout(xaxis = list(title = "Maand"),
           yaxis = list(title = "Aantal"),
           barmode = 'stack')
  
  return(plot)
}




## Aantallen per maand per traject bar plot
plot_aantal_traject_pmpc_bar <- function(data) {
  aantal <- data %>%
    filter(categorie == "Traject") %>%  # Filter data where categorie is "Traject"
    group_by(BestelMaand, Product) %>%
    summarise(OrdersCount = n(), .groups = 'drop') %>%
    pivot_wider(names_from = BestelMaand, values_from = OrdersCount, values_fill = 0) %>%
    pivot_longer(cols = -Product, names_to = "BestelMaand", values_to = "OrdersCount") %>%
    filter(!(BestelMaand %in% c("2022-11", "2022-12")))
  
  # Plot
  plot <- plot_ly(aantal, x = ~BestelMaand, y = ~OrdersCount, type = "bar",
                  hoverinfo = "text", color = ~Product, colors = "Set1", marker = list(line = list(width = 1)),
                  text = ~paste(Product, ": ", OrdersCount), textposition = "inside") %>%
    layout(xaxis = list(title = "Maand"),
           yaxis = list(title = "Aantal"),
           barmode = 'stack',
           legend = list(orientation = "h", y = 1.1, x = 0.6))  # Adjust y position to move the legend to the top
  
  
  return(plot)
}


plot_aanatal_piechart <- function(data) {
  aantal <- data %>%
    filter(Producttype == "Abonnement") %>%
    group_by(BestelMaand, categorie) %>%
    summarise(OrdersCount = n(), .groups = 'drop') %>%
    pivot_wider(names_from = BestelMaand, values_from = OrdersCount, values_fill = 0)
  
  
  # Assuming 'aantal' is the summarized dataframe
  most_recent_month <- tail(colnames(aantal), 1)
  
  # Filter the dataframe for the most recent month
  most_recent_month_data <- aantal %>%
    select(categorie, !!most_recent_month)
  
  # Create a pie chart
  plot <- (ggplot(most_recent_month_data, aes(x = "", y = !!sym(most_recent_month), fill = categorie)) +
             geom_bar(stat = "identity", width = 1) +
             coord_polar("y", start = 0) +
             labs(title = "Verdeling van de abonnementen",
                  x = NULL,
                  y = NULL,
                  fill = "Abonnement") +
             theme_void()) # Adjust color palette as needed
  return(plot)
}

