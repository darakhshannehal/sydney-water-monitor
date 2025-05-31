
# Load necessary libraries
library(tidyverse)
library(leaflet)           # Interactive maps
library(leaflet.extras)
library(shiny)             # Shiny web application framework
library(shinyBS)           # Bootstrap components for Shiny
library(shinythemes)       # Themes for Shiny
library(rsconnect)         # Deploying to shinyapps.io

# Load Data
water_quality <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-20/water_quality.csv"
)
weather <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-20/weather.csv"
)

# Check for Missing values & Remove NA in 'enterococci_cfu_100ml'
water_quality <- water_quality %>%
  filter(!is.na(enterococci_cfu_100ml)) %>%
  distinct(.keep_all = TRUE)  # Remove duplicates

# Log-transform enterococci levels due to skewness
water_quality <- water_quality %>%
  mutate(log_enterococci = log10(enterococci_cfu_100ml + 1))  # Adding 1 to avoid log(0)

# Categorize contamination risk levels
water_qual <- water_quality %>%
  mutate(
    risk_level = case_when(
      enterococci_cfu_100ml < 35 ~ "Safe",
      enterococci_cfu_100ml >= 35 & enterococci_cfu_100ml < 104 ~ "Moderate",
      enterococci_cfu_100ml >= 104 ~ "Unsafe"
    ),
    risk_level = factor(risk_level, levels = c("Safe", "Moderate", "Unsafe"))
  ) %>%
  select(date, longitude, latitude, swim_site, enterococci_cfu_100ml, risk_level)

# Define color palette for risk levels
color_palette <- colorFactor(
  palette = c("forestgreen", "gold", "firebrick"),
  levels = c("Safe", "Moderate", "Unsafe")  # Ensure order matches color sequence
)

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel(
    tags$div(
      tags$h2("🌊 Sydney Beach Water Monitor", id = "title")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      tags$div(
        class = "sidebar-description",
        HTML("Use these filters to assess <b>Enterococci</b> contamination <b>risk levels</b> at Sydney swim sites.")
      ),
      br(),
      
      tags$div(class = "sidebar-label", style = "border: 1px solid forestgreen; border-radius: 8px; padding: 8px; margin-bottom: 5px;", "🟢 Safe: Low contamination"),
      tags$div(class = "sidebar-label", style = "border: 1px solid gold; border-radius: 8px; padding: 8px; margin-bottom: 5px;", "🟡 Moderate: Above threshold"),
      tags$div(class = "sidebar-label", style = "border: 1px solid firebrick; border-radius: 8px; padding: 8px; margin-bottom: 5px;", "🔴 Unsafe: High contamination"),
      
      br(),
      
      selectInput("year_filter", "Year:", choices = sort(unique(format(water_qual$date, "%Y")))),
      selectInput("month_filter", "Month:", choices = month.name),
      selectInput("risk_filter", "Risk Level:", choices = c("Safe", "Moderate", "Unsafe"), selected = c("Unsafe"), multiple = TRUE)
    ),
    
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  filtered_data <- reactive({
    req(input$year_filter, input$month_filter, input$risk_filter)
    water_qual %>%
      filter(
        format(date, "%Y") == input$year_filter,
        format(date, "%B") == input$month_filter,
        risk_level %in% input$risk_filter
      )
  })
  
  # Render interactive map
  output$map <- renderLeaflet({
    if (nrow(filtered_data()) == 0) {
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = 151.2093, lat = -33.8688, zoom = 9)  # Default location (Sydney)
    } else {
      leaflet(filtered_data()) %>%
        addSearchOSM() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(
          lng = mean(filtered_data()$longitude, na.rm = TRUE),
          lat = mean(filtered_data()$latitude, na.rm = TRUE),
          zoom = 9.5
        ) %>%
        addCircleMarkers(
          lng = ~longitude, lat = ~latitude,
          radius = 5,
          color = "grey",
          fillColor = ~color_palette(risk_level),
          fillOpacity = 0.4,
          stroke = TRUE,
          weight = 0.7,
          label = ~swim_site,
          labelOptions = labelOptions(
            style = list("font-size" = "14px", "font-weight" = "bold")
          )
        )
    }
  })
}

# Run the application
shinyApp(ui, server)
