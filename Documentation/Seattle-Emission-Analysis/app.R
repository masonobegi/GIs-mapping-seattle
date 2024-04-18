# Libraries
library(shiny)
library(leaflet) 
library(sf)
library(dplyr)
library(scales)
library(RColorBrewer)
library(ggplot2)
library(car)
library(tools)
 

# Data
PSE_TEST <- st_read("../../Data/PSE.shp")
Equity_TEST <- st_read("../../Data/Equity_RSE.shp")
PSE2023_TEST<-st_read("../../Data/PSE2023.shp")

#Masons data and cleaning
data2022 <- read.csv("../../Data/Performance_Ranges_by_Building_Type_2022.csv")
data2021 <- read.csv("../../Data/Performance_Ranges_by_Building_Type_2021.csv")
data2020 <- read.csv("../../Data/Performance_Ranges_by_Building_Type_2020.csv")
data2019 <- read.csv("../../Data/Performance_Ranges_By_Building_Type_2019.csv")
data2018 <- read.csv("../../Data/Performance_Ranges_By_Building_Type_2018.csv")
data2017 <- read.csv("../../Data/Performance_Ranges_by_Building_Type_2017.csv")
data2016 <- read.csv("../../Data/Performance_Ranges_By_Building_Type_2016.csv")

data2017 <- select(data2017, -c(Low.Outlier.Cutoff, High.Outlier.Cutoff))
data2022 <- na.omit(data2022)
data2021 <- na.omit(data2021)
data2020 <- na.omit(data2020)
data2019 <- na.omit(data2019)
data2018 <- na.omit(data2018)
data2017 <- na.omit(data2017)
data2016 <- na.omit(data2016)

#We are renaming all the columns names for all the years so they are all uniform so we can join them later. The order does not matter right now, we are going with the order of how they are in their respective files.

colnames(data2016) <- c("BuildingType", "Percentile_25th_EUI", "Median_EUI", "Percentile_75th_EUI", "Median_EUI_WN", "Median_Source_EUI", "Median_ES_Score", "Number_of_Buildings", "Total_GFA", "Median_GFA", "Median_Year_Built", "Percent_Electricity", "Percent_Gas", "Percent_Steam", "Percent_Other_Fuel")

colnames(data2017) <- c("BuildingType", "Percentile_25th_EUI", "Median_EUI", "Percentile_75th_EUI", "Median_EUI_WN", "Median_Source_EUI", "Median_ES_Score", "Number_of_Buildings", "Number_of_Buildings_with_ES_Score", "Total_GFA", "Median_GFA", "Median_Year_Built", "Percent_Electricity", "Percent_Gas", "Percent_Steam", "Percent_Other_Fuel")

colnames(data2018) <- c("BuildingType", "Number_of_Buildings", "Percentile_25th_EUI", "Median_EUI", "Percentile_75th_EUI", "Average_Site_EUI", "Average_Site_EUI_WN", "Median_EUI", "Median_ES_Score", "Total_GFA", "Median_GFA", "Median_Year_Built", "Not applies")

colnames(data2019) <- c("BuildingType", "Number_of_Buildings", "Percentile_25th_EUI", "Median_EUI", "Percentile_75th_EUI", "Median_EUI_WN", "Average_Site_EUI", "Average_Site_EUI_WN", "Median_Source_EUI", "Median_ES_Score", "Average_ES_Score", "Total_GFA", "Median_GFA", "Median_Year_Built", "Average_GHGSF", "MedianGHGSF")

colnames(data2020) <- c("BuildingType", "Number_of_Buildings", "Percentile_25th_EUI", "Median_EUI", "Percentile_75th_EUI", "Median_EUI_WN", "Average_Site_EUI", "Average_Site_EUI_WN", "Median_Source_EUI", "Median_ES_Score", "Average_ES_Score", "Total_GFA", "Median_GFA", "Median_Year_Built", "Average_GHGSF", "MedianGHGSF")

colnames(data2021) <- c("BuildingType", "Number_of_Buildings", "Percentile_25th_EUI", "Median_EUI", "Percentile_75th_EUI", "Median_EUI_WN", "Average_Site_EUI", "Average_Site_EUI_WN", "Median_Source_EUI", "Median_ES_Score", "Average_ES_Score", "Total_GFA", "Median_GFA", "Median_Year_Built", "Average_GHGSF", "MedianGHGSF")

colnames(data2022) <- c("BuildingType", "Number_of_Buildings", "Percentile_25th_EUI", "Median_EUI", "Percentile_75th_EUI", "Median_EUI_WN", "Average_Site_EUI", "Average_Site_EUI_WN", "Median_Source_EUI", "Median_ES_Score", "Average_ES_Score", "Total_GFA", "Median_GFA", "Median_Year_Built", "Average_GHGSF", "MedianGHGSF")

#we are doign 3 checks to see if every column in every year is in 2022, 2016 and 2018 respecitvely as I was having issues with incompatible dataset before
data2016 <- data2016[, colnames(data2016) %in% colnames(data2022)]
data2017 <- data2017[, colnames(data2017) %in% colnames(data2022)]
data2018 <- data2018[, colnames(data2018) %in% colnames(data2022)]
data2019 <- data2019[, colnames(data2019) %in% colnames(data2022)]
data2020 <- data2020[, colnames(data2020) %in% colnames(data2022)]
data2021 <- data2021[, colnames(data2021) %in% colnames(data2022)]

data2016 <- data2016[, colnames(data2016) %in% colnames(data2016)]
data2017 <- data2017[, colnames(data2017) %in% colnames(data2016)]
data2018 <- data2018[, colnames(data2018) %in% colnames(data2016)]
data2019 <- data2019[, colnames(data2019) %in% colnames(data2016)]
data2020 <- data2020[, colnames(data2020) %in% colnames(data2016)]
data2021 <- data2021[, colnames(data2021) %in% colnames(data2016)]
data2022 <- data2022[, colnames(data2022) %in% colnames(data2016)]

data2016 <- data2016[, colnames(data2016) %in% colnames(data2018)]
data2017 <- data2017[, colnames(data2017) %in% colnames(data2018)]
data2018 <- data2018[, colnames(data2018) %in% colnames(data2018)]
data2019 <- data2019[, colnames(data2019) %in% colnames(data2018)]
data2020 <- data2020[, colnames(data2020) %in% colnames(data2018)]
data2021 <- data2021[, colnames(data2021) %in% colnames(data2018)]
data2022 <- data2022[, colnames(data2022) %in% colnames(data2018)]

#we are adding a year column to every dataset 
library(dplyr)
data2022 <- mutate(data2022, Year = 2022)
data2021 <- mutate(data2021, Year = 2021)
data2020 <- mutate(data2020, Year = 2020)
data2019 <- mutate(data2019, Year = 2019)
data2018 <- mutate(data2018, Year = 2018)
data2017 <- mutate(data2017, Year = 2017)
data2016 <- mutate(data2016, Year = 2016)

#we are now going to combine all our datasets
combined_data <- bind_rows(
  mutate(data2016, Year = 2016),
  mutate(data2017, Year = 2017),
  mutate(data2018, Year = 2018),
  mutate(data2019, Year = 2019),
  mutate(data2020, Year = 2020),
  mutate(data2021, Year = 2021),
  mutate(data2022, Year = 2022)
)
combined_data<- combined_data[combined_data$BuildingType != "Citywide", ]

#Connor using Kalin's data for Grocery models
#Loading in .csv files
Benchmarking2016 <- read.csv("../../Data/2016_Building_Energy_Benchmarking.csv", header = TRUE)
Benchmarking2017 <- read.csv("../../Data/2017_Building_Energy_Benchmarking.csv", header = TRUE)
Benchmarking2018 <- read.csv("../../Data/2018_Building_Energy_Benchmarking.csv", header = TRUE)
Benchmarking2019 <- read.csv("../../Data/2019_Building_Energy_Benchmarking.csv", header = TRUE)
Benchmarking2020 <- read.csv("../../Data/2020_Building_Energy_Benchmarking.csv", header = TRUE)
Benchmarking2021 <- read.csv("../../Data/2021_Building_Energy_Benchmarking.csv", header = TRUE)
Benchmarking2022 <- read.csv("../../Data/2022_Building_Energy_Benchmarking.csv", header = TRUE)

#Removing columns that are not available for year over year analysis
Bench2016 <- subset(Benchmarking2016, select = -c(Comments,Outlier,DefaultData) )
Bench2017 <- subset(Benchmarking2017, select = -c(Outlier,DefaultData) )
Bench2018 <- subset(Benchmarking2018, select = -c(Compliance.Issue, EPA.Building.Sub.Type.Name) )
Bench2019 <- subset(Benchmarking2019, select = -c(ComplianceIssue) )
Bench2020 <- subset(Benchmarking2020, select = -c(ComplianceIssue) )
Bench2021 <- subset(Benchmarking2021, select = -c(ComplianceIssue) )
Bench2022 <- subset(Benchmarking2022, select = -c(ComplianceIssue) )

#create current EUI models for building types
current_sum_EUI_2022 <- Bench2022 %>%
  filter(!is.na(SourceEUI.kBtu.sf.)) %>%
  group_by(LargestPropertyUseType) %>% 
  summarize(count = n(), Median_Source_EUI = median(SourceEUI.kBtu.sf., na.rm = TRUE)) %>%
  filter(count > 10) %>%
  rename(BuildingType = LargestPropertyUseType) %>%
  mutate(Year = 2022)

current_sum_EUI_2021 <- Bench2021 %>%
  filter(!is.na(SourceEUI.kBtu.sf.)) %>%
  group_by(LargestPropertyUseType) %>% 
  summarize(count = n(), Median_Source_EUI = median(SourceEUI.kBtu.sf., na.rm = TRUE)) %>%
  filter(count > 10) %>%
  rename(BuildingType = LargestPropertyUseType) %>%
  mutate(Year = 2021)

current_sum_EUI_2020 <- Bench2020 %>%
  filter(!is.na(SourceEUI.kBtu.sf.)) %>%
  group_by(LargestPropertyUseType) %>% 
  summarize(count = n(), Median_Source_EUI = median(SourceEUI.kBtu.sf., na.rm = TRUE)) %>%
  filter(count > 10) %>%
  rename(BuildingType = LargestPropertyUseType) %>%
  mutate(Year = 2020)

current_sum_EUI_2019 <- Bench2019 %>%
  filter(!is.na(SourceEUI.kBtu.sf.)) %>%
  group_by(LargestPropertyUseType) %>% 
  summarize(count = n(), Median_Source_EUI = median(SourceEUI.kBtu.sf., na.rm = TRUE)) %>%
  filter(count > 10) %>%
  rename(BuildingType = LargestPropertyUseType) %>%
  mutate(Year = 2019)

current_sum_EUI_2018 <- Bench2018 %>%
  filter(!is.na(SourceEUI.kBtu.sf.)) %>%
  group_by(Largest.Property.Use.Type) %>% 
  summarize(count = n(), Median_Source_EUI = median(SourceEUI.kBtu.sf., na.rm = TRUE)) %>%
  filter(count > 10) %>%
  rename(BuildingType = Largest.Property.Use.Type) %>%
  mutate(Year = 2018)

current_sum_EUI_2017 <- Bench2017 %>%
  filter(!is.na(SourceEUI.kBtu.sf.)) %>%
  group_by(LargestPropertyUseType) %>% 
  summarize(count = n(), Median_Source_EUI = median(SourceEUI.kBtu.sf., na.rm = TRUE)) %>%
  filter(count > 10) %>%
  rename(BuildingType = LargestPropertyUseType) %>%
  mutate(Year = 2017)

current_sum_EUI_2016 <- Bench2016 %>%
  filter(!is.na(SourceEUI.kBtu.sf.)) %>%
  group_by(LargestPropertyUseType) %>% 
  summarize(count = n(), Median_Source_EUI = median(SourceEUI.kBtu.sf., na.rm = TRUE)) %>%
  filter(count > 10) %>%
  rename(BuildingType = LargestPropertyUseType) %>%
  mutate(Year = 2016)

#Combined data to be used in models
combined_sum_EUI <- bind_rows(current_sum_EUI_2022, current_sum_EUI_2021, current_sum_EUI_2020, current_sum_EUI_2019, current_sum_EUI_2018, current_sum_EUI_2017, current_sum_EUI_2016)

current_combined_stores <- combined_sum_EUI %>% 
  filter(BuildingType %in% c("Supermarket", "Supermarket/Grocery Store"))

reduced_sum_EUI_2022 = current_sum_EUI_2022 %>%
  mutate(Median_Source_EUI = ifelse(BuildingType == "Supermarket/Grocery Store", 
                                    Median_Source_EUI * 0.6, 
                                    Median_Source_EUI))

reduced_combined_stores = current_combined_stores %>%
  mutate(Median_Source_EUI = ifelse(Year == 2022, 
                                    Median_Source_EUI * 0.6, 
                                    Median_Source_EUI))


# UI
ui <- fluidPage(
  titlePanel("Seattle Energy Project"),
  mainPanel(
    tabsetPanel(
      tabPanel("Overview", "Seattle, like many cities around the world are looking at how to reduce their emissions. This project looks at the problem through 2 lenses, geospatial analysis and forecasting. Geospatial wise, this project looks at whether factors such as socioeconomic disparity play a factor in what parts of the city have higher emissions. Forecasting wise, this project tries to look at overall trends from the entire city of Seattle and it's building types. In the conclusion of this project, there will be a clear idea of whether or not socioeconomic factors, types of buildings or another factor is a major contributor to emissions in Seattle."),
      tabPanel("GIS Analysis", 
               tabsetPanel(
                 tabPanel("Natural Gas Emissions Per Account", 
                          fluidPage(
                            selectInput("calYearEmissions", "Select Year:", choices = sort(unique(PSE$CAL_YEAR))),
                            leafletOutput("emissionsMap"),
                            verbatimTextOutput("emissionsInfo") # Text box for emissions info
                          )
                 ),
                 tabPanel("Natural Gas Usage Per Account",
                          fluidPage(
                            selectInput("calYearGasUsage", "Select Year:", choices = sort(unique(PSE$CAL_YEAR))),
                            leafletOutput("gasUsageMap"),
                            verbatimTextOutput("gasUsageInfo") # Text box for gas usage info
                          )
                 ),
                 tabPanel("Reduced Natural Gas Emissions",
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
    ), 
    tabPanel("Median EUI Models", 
             fluidPage(
               selectInput("regression_selection", "Select Building Type", choice = c("general", "commercial", "industrial", "residential")), 
               plotOutput("Regression_Model"), 
               textOutput("Durbin")
             )
          ),
    tabPanel("Grocery Store Recommendations", 
             fluidPage(
               selectInput("model_selection", "Select Grocery EUI Data", choice = c("current", "reduced")), 
               plotOutput("building_model"),
               plotOutput("time_model")
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
    "Natural Gas Emissions Per Account (MTCO2e) in 2023 \n"
  })
  
  output$graphTitle2 <- renderText({
    "Natural Gas Emissions Per Account Reduced (MTCO2e) by 39% \n"
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
                title = "Natural Gas Emissions Per Account (MTCO2e)",
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
                title = "Natural Gas Usage Per Account (MMBtu)",
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
  
  
  #Masons work starts here 
  #looks like we lost data somewhere here but we will figure that out later
  commercial_data <- combined_data %>% filter(grepl("Grocery|Hospital|K-12|Office|Medical|Mixed|Other|Retail|Worship", BuildingType))
  
  industrial_data <- combined_data %>% filter(grepl("Distribution|Warehouse", BuildingType))
  
  residential_data <- combined_data %>% filter(grepl("High-rise|Low-rise|Mid-rise|Residence|Senior|Multifamily", BuildingType))
  
  industrial_data_year <- industrial_data %>% 
    group_by(Year) %>%
    filter(Year != 2020) %>%
    summarize(Average_Median_EUI = mean(Median_EUI))
  
  residential_data_year <- residential_data %>% 
    group_by(Year) %>%
    filter(Year != 2020) %>%
    summarize(Average_Median_EUI = mean(Median_EUI))
  
  
  commercial_data_year <- commercial_data %>% 
    group_by(Year) %>%
    filter(Year != 2020) %>%
    summarize(Average_Median_EUI = mean(Median_EUI))
  
  general_data_year <- combined_data %>%
    group_by(Year) %>% 
    summarize(Average_Median_EUI = mean(Median_EUI)) %>%
    filter(Year != 2020)
  
  
  industrial_forecast <- auto.arima(industrial_data_year$Average_Median_EUI)
  residential_forecast <- auto.arima(residential_data_year$Average_Median_EUI)
  commercial_forecast <- auto.arima(commercial_data_year$Average_Median_EUI)
  general_forecast <- auto.arima(general_data_year$Average_Median_EUI)
  
  forecast_residential <- forecast(residential_forecast, h = 6)
  forecast_industrial <- forecast(industrial_forecast, h = 6)
  forecast_commercial <- forecast(commercial_forecast, h = 6)
  forecast_general <- forecast(general_forecast, h = 6)
  
  forecast_residential_df <- data.frame(Year = c(2022,2023, 2024, 2025, 2026, 2027), Forecasted_EUI = forecast_residential$mean)
  forecast_industrial_df <-data.frame(Year = c(2022,2023, 2024, 2025, 2026, 2027),Forecasted_EUI = forecast_industrial$mean)
  forecast_commercial_df <-data.frame(Year = c(2022,2023, 2024, 2025, 2026, 2027),Forecasted_EUI = forecast_commercial$mean)
  forecast_general_df <- data.frame(Year = c(2022,2023, 2024, 2025, 2026, 2027),Forecasted_EUI = forecast_general$mean)
  
  
  
  output$Regression_Model <- renderPlot({
    req(input$regression_selection)
    
    name_pred <- paste0("predicted_eui_", toString(input$regression_selection))
    name_data <- paste0(toString(input$regression_selection), "_data_year")
    name_fore <-paste0("forecast_", toString(input$regression_selection), "_df")
    
    
    ggplot() +
      scale_x_continuous(breaks = seq(2016, 2027,by = 1)) +
      geom_line(data = get(name_data), aes(x = Year, y = Average_Median_EUI, color = "Actual"), show.legend = TRUE) + 
      geom_line(data = get(name_fore), aes(x = Year, y = Forecasted_EUI, color = "Forecasted"), show.legend = TRUE) + 
      scale_color_manual(name = "Lines", values = c("Actual" = "black", "Forecasted" = "red")) + 
      labs(title = paste0(toTitleCase(input$regression_selection), " Median EUI by Year"), x = "Year", y = paste0(toTitleCase(input$regression_selection), " Median EUI"))
    
  })
  
  output$Durbin <- renderText({
    req(input$regression_selection)
    name_durbin <- paste0(toString(input$regression_selection), "_forecast")
    durb <- durbinWatsonTest(as.numeric(residuals(get(name_durbin))))
    paste0("Durbin Watson Test value: ",toString(durb))
  })
  
  
  output$building_model <- renderPlot({
    req(input$model_selection)
    
    name_data <- paste0(toString(input$model_selection), "_sum_EUI_2022")
    
    top_quan2022 = quantile(get(name_data)$Median_Source_EUI,0.75)
    high_impact_buildings_2022 = get(name_data) %>%
      filter(Median_Source_EUI >= top_quan2022) %>%
      arrange(desc(Median_Source_EUI))
    
    ggplot(high_impact_buildings_2022, aes(x = reorder(BuildingType, -Median_Source_EUI), y = Median_Source_EUI)) +
      geom_bar(stat = "identity", fill = ifelse(high_impact_buildings_2022$BuildingType == "Supermarket/Grocery Store", "red", "dodgerblue3")) +
      labs(
        x = NULL,
        y = "Median Source EUI",
        title = "Top 25% Building Types Based on Median Source EUI",
        subtitle = "EUI: Energy use intensity measured per unit of floor area",
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        plot.caption = element_text(hjust = 0.5, size = 8)
      ) +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$time_model <- renderPlot({
    req(input$model_selection)
    
    name_data <- paste0(toString(input$model_selection), "_combined_stores")
    
    ggplot(get(name_data), aes(x = Year, y = Median_Source_EUI)) +
      geom_line() +
      geom_point() +
      labs(x = "Year", y = "Median Source EUI", title = "Supermarket/Grocery Store Median Source EUI 2016-2022") +
      scale_x_continuous(breaks = unique(reduced_combined_stores$Year))
    
    
  })
 
}

# Run Application
shinyApp(ui, server)


