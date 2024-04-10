# Libraries
library(shiny)
library(leaflet) 
library(sf)
library(dplyr)
library(scales)
library(RColorBrewer)
 

# Data
PSE_TEST <- st_read("../../Data/PSE.shp")
Equity_TEST <- st_read("../../Data/Equity_RSE.shp")
PSE2023_TEST<-st_read("../../Data/PSE2023.shp")

# UI
ui <- fluidPage(
  titlePanel("Seattle Energy Project"),
  mainPanel(
    tabsetPanel(
      tabPanel("Overview", "Overview content will be placed here."),
      tabPanel("GIS Analysis", 
               tabsetPanel(
                 tabPanel("Emissions Per Account", 
                          fluidPage(
                            selectInput("calYearEmissions", "Select Year:", choices = sort(unique(PSE$CAL_YEAR))),
                            leafletOutput("emissionsMap"),
                            verbatimTextOutput("emissionsInfo") # Text box for emissions info
                          )
                 ),
                 tabPanel("Gas Usage Per Account",
                          fluidPage(
                            selectInput("calYearGasUsage", "Select Year:", choices = sort(unique(PSE$CAL_YEAR))),
                            leafletOutput("gasUsageMap"),
                            verbatimTextOutput("gasUsageInfo") # Text box for gas usage info
                          )
                 ),
                 tabPanel("Reduced Emissions",
                          fluidPage(
                            fluidRow(
                              HTML("<br/>"),
                              tags$b(textOutput("graphTitle1")),
                              HTML("<br/>"),
                              leafletOutput("emissions2023"),
                              HTML("<br/>"),
                              tags$b(textOutput("graphTitle2")),
                              HTML("<br/>"),
                              leafletOutput("reducedEPAmap")
                        
                            )
                          )
               )
      )
    )
  )
))

# Server
server <- function(input, output, session) {
  
  # Filter PSE data based on selected year for Emissions
  filteredDataEmissions <- reactive({
    PSE %>%
      filter(CAL_YEAR == input$calYearEmissions) %>%
      na.omit()
  })
  
  # Filter PSE data based on selected year for Gas Usage
  filteredDataGasUsage <- reactive({
    PSE %>%
      filter(CAL_YEAR == input$calYearGasUsage) %>%
      na.omit()
  })
  
  # Descriptive text for the Emissions Per Account tab
  output$emissionsInfo <- renderText({
    paste0("The RSE Quintiles are divided as follows:\n",
           "0th-20th percentile (Lowest Equity Priority)\n",
           "20th-40th percentile (Second Lowest Equity Priority)\n",
           "40th-60th percentile (Middle Equity Priority)\n",
           "60th-80th percentile (Second Highest Equity Priority)\n",
           "80th-100th percentile (Highest Equity Priority)\n\n",
           "The Emissions Per Account color scale is divided into three quantile bins representing:",
           "\n0% - 33% (green), \n33% - 67% (yellow), \n67% - 100% (red).")
  })
  
  # Descriptive text for the Gas Usage Per Account tab
  output$gasUsageInfo <- renderText({
    paste0("The RSE Quintiles are divided as follows:\n",
           "0th-20th percentile (Lowest Equity Priority)\n",
           "20th-40th percentile (Second Lowest Equity Priority)\n",
           "40th-60th percentile (Middle Equity Priority)\n",
           "60th-80th percentile (Second Highest Equity Priority)\n",
           "80th-100th percentile (Highest Equity Priority)\n\n",
           "The Gas Usage Per Account color scale is divided into three quantile bins representing:",
           "\n0% - 33% (green), \n33% - 67% (yellow), \n67% - 100% (red).")
  })
  
  output$graphTitle1 <- renderText({
    "Emissions Per Account in 2023 \n"
  })
  
  output$graphTitle2 <- renderText({
    "Emissions Per Account Reduced by 39% \n"
  })
  
  output$reductionInfo<-renderText({
    paste0("")
  })
  
  # Map for Emissions Per Account
  output$emissionsMap <- renderLeaflet({
    data <- filteredDataEmissions()
    emissionsPalette <- colorQuantile(c("green", "yellow", "red"),
                                      domain = data$`Emissions Per Account`,
                                      n = 3)
    
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~emissionsPalette(`Emissions Per Account`),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = ~paste0("RSE Quintile: ", RSE_Quintile),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>%
      addLegend(pal = emissionsPalette, 
                values = ~`Emissions Per Account`,
                opacity = 0.7, 
                title = "Emissions Per Account",
                position = "bottomright")
  })
  
  # Map for Gas Usage Per Account
  output$gasUsageMap <- renderLeaflet({
    data <- filteredDataGasUsage()
    gasUsagePalette <- colorQuantile(c("green", "yellow", "red"),
                                     domain = data$`Gas Usage Per Account`,
                                     n = 3)
    
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~gasUsagePalette(`Gas Usage Per Account`),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = ~paste0("RSE Quintile: ", RSE_Quintile),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>%
      addLegend(pal = gasUsagePalette, 
                values = ~`Gas Usage Per Account`,
                opacity = 0.7, 
                title = "Gas Usage Per Account",
                position = "bottomright")
  })
  
  
  #reduced emissions by proportion
  output$reducedEPAmap <- renderLeaflet({
    emissionsPalette <- colorFactor(palette = c("#2dc937", "#99c140", "#e7b416", "#db7b2b", "#cc3232"),
                                    domain = PSE2023$Emissions_Percentile)
    emissionsReducedPalette <- colorFactor(palette = c("#2dc937", "#99c140", "#e7b416", "#db7b2b", "#cc3232"),
                                           domain = PSE2023$Emissions_Percentile_Reduced)
    leaflet(PSE2023) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~emissionsReducedPalette(Emissions_Percentile_Reduced),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = ~paste0("RSE Quintile: ", RSE_Quintile),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>%
      addLegend(
        pal = emissionsPalette,
        values = ~Emissions_Percentile_Reduced,
        opacity = 0.7,
        title = "Emissions Per Account",
        position = "bottomright",
        labels = labels)
  })
  
  output$emissions2023 <- renderLeaflet({
    emissionsPalette <- colorFactor(palette = c("#2dc937", "#99c140", "#e7b416", "#db7b2b", "#cc3232"),
                                    domain = PSE2023$Emissions_Percentile)

    leaflet(PSE2023) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~emissionsPalette(Emissions_Percentile),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = ~paste0("RSE Quintile: ", RSE_Quintile),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>%
      addLegend(
        pal = emissionsPalette,
        values = ~Emissions_Percentile,
        opacity = 0.7,
        title = "Emissions Per Account",
        position = "bottomright",
        labels = labels)
  })
  
 
}

# Run Application
shinyApp(ui, server)


