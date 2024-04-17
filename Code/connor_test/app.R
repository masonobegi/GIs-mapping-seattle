
library(shiny)
library(dplyr)
library(ggplot2)


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

combined_sum_EUI <- bind_rows(current_sum_EUI_2022, current_sum_EUI_2021, current_sum_EUI_2020, current_sum_EUI_2019, current_sum_EUI_2018, current_sum_EUI_2017, current_sum_EUI_2016)

current_combined_stores <- combined_sum_EUI %>% 
  filter(BuildingType %in% c("Supermarket", "Supermarket/Grocery Store"))






reduced_sum_EUI_2022 = sum_EUI_2022 %>%
  mutate(Median_Source_EUI = ifelse(BuildingType == "Supermarket/Grocery Store", 
                                    Median_Source_EUI * 0.6, 
                                    Median_Source_EUI))

reduced_combined_stores = current_combined_stores %>%
  mutate(Median_Source_EUI = ifelse(Year == 2022, 
                                    Median_Source_EUI * 0.6, 
                                    Median_Source_EUI))







ui <- fluidPage(

    titlePanel("Connor Test Shiny App"),
    tabPanel("Grocery Store Reccomendations", 
             fluidPage(
               selectInput("model_selection", "Select Grocery EUI Data", choice = c("current", "reduced")), 
               plotOutput("building_model"),
               plotOutput("time_model")
             )
    )
)


server <- function(input, output, session) {


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

# Run the application 
shinyApp(ui = ui, server = server)
