##### Attaching all the library 

library(dplyr)
library(maps)
library(leaflet)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(readxl)
library(tidyr)


## attaching the data 
pv <- read_xlsx("~/Downloads/PV_Bangalore.xlsx")
pv_num <- pv %>% mutate(across(c(lat, long, type, active_1), as.numeric))
attach(pv_num)


#user interface for the dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Geographic Points on Map"),  # Header of the dashboard
  dashboardSidebar( # Sidebar with menu items
    sidebarMenu(
      menuItem("Map", tabName = "Map", icon = icon("map")), # Menu item for the map tab
      menuItem("Multiple Choice", tabName = "Plot", icon = icon("check-square")),# Menu item for the plot tab
      menuItem("Age & Gender", tabName = "Demographics", icon = icon("users")),# Menu item for the demographics tab
      menuItem("Data", tabName = "Data", icon = icon("data"))# Menu item for the data tab
    )
  ), # Body options under fluidflow
  dashboardBody(
    tabItems(
      tabItem("Map", # Content for the map tab
              fluidRow(column(width = 10,
                              selectInput(inputId = "what_type",
                                          label = "Select type:",
                                          choices = c("Physically Active" = "1", "Physically Inactive" = "2", "Place used for Physical Activity" = "3")))),
              leafletOutput("mymap")# Output to render the Leaflet map
      )
      ,
      # tab for the plot
      tabItem("Plot", # Content for the plot tab
              selectInput(inputId= "plot_select", 
                          "Select a Plot", 
                          choices = c("Physically Active" = "plot", "Physically Inactive" = "plot_2", "Place used for Physical Activity" = "plot_3")),
              fluidRow(
                column(width = 12,
                       plotOutput("plot", height = 400, width = "100%"),# Output to render the plot for Physically Active
                       plotOutput("plot_2", height = 400, width = "100%"),# Output to render the plot for Physically Inactive
                       plotOutput("plot_3", height = 400, width = "100%") # Output to render the plot for Place used for Physical Activity
                )
              )
      ),
      # tab item for the demographics, while selecting from choices
      tabItem("Demographics",
              selectInput(inputId= "plot_select_demo", "Select Plot:", choices = c("Age", "Gender"))
              ,
              fluidRow(
                column(width = 12, # Output to render the demographic plot
                       plotOutput("plotting_demo")
                )
              )
      )
      
      ,
      tabItem("Data",# Content for the data tab
              fluidPage(
                h1("Data"),
                dataTableOutput("Data")  # Output to render the data table
                
              )
              
      )
    )
  )
)



server <- function(input, output, session){
  
  #Aggregating the all the values options which corresponds to the multiple choice categories with "TYPE" Column 
  # first step to filter the data, perform the count with pivot_longer while mutating accross the columns we want starts_with
  # Second step would be to summarise all the entries with the multiple choice options and then graphing with ggplot option
  pv_num <- pv_num %>%
    mutate(across(starts_with("active_"), as.numeric))
  
  pv_num_long <- pv_num %>%
    pivot_longer(cols = starts_with("active_"),
                 names_to = "mc_options",
                 values_to = "count") %>%
    filter(count == "1")  # only keep rows with count == "1"
  
  # group by multiple choice options and count the occurrences
  pv_num_agg_type_1 <- pv_num_long %>%
    group_by(mc_options) %>%
    summarise(n = n())
  # using if, else for taking plot tab items from UI, whilst using rendering command
  output$plot <- renderPlot({
    if (input$plot_select == "plot") {
      # group by multiple choice options and count the occurrences
      pv_num_agg_type_1 <- pv_num_long %>%
        # renaming all the mc_columns as case_when to rename the values to character 
        mutate(mc_options = case_when(
          mc_options == "active_1" ~ "walking",
          mc_options == "active_2" ~ "cycling",
          mc_options == "active_3" ~ "sports",
          mc_options == "active_4" ~ "working",
          mc_options == "active_5" ~ "playing",
          mc_options == "active_6" ~ "other",
          TRUE ~ mc_options
        )) %>% # map the different options to their corresponding labels
        group_by(mc_options) %>%
        summarise(n = n())
      
      ggplot(pv_num_agg_type_1, aes(x = mc_options, y = n)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(title = "Count of Multiple Choice Answers",
             x = "Multiple Choice Options",
             y = "Count")
    } else if (input$plot_select == "plot_2") {
      pv_num <- pv_num %>%
        mutate(across(starts_with("inactive_"), as.numeric))
      
      pv_num_long <- pv_num %>%
        pivot_longer(cols = starts_with("inactive_"),
                     names_to = "mc_options",
                     values_to = "count") %>%
        filter(count == "1")  # only keep rows with count == "1"
      
      # group by multiple choice options and count the occurrences 
      pv_num_agg_type_2 <- pv_num_long %>%
       # renaming all multiple choice with the case when_ to rename the inactive to characters
         mutate(mc_options = case_when(
          mc_options == "inactive_1" ~ "too dangerous",
          mc_options == "inactive_2" ~ "Too loud",
          mc_options == "inactive_3" ~ "no equipment",
          mc_options == "inactive_4" ~ "other",
          TRUE ~ mc_options
        )) %>%
        group_by(mc_options) %>%
        summarise(n = n())
      
      ggplot(pv_num_agg_type_2, aes(x = mc_options, y = n)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(title = "Count of Multiple Choice Answers",
             x = "Multiple Choice Options",
             y = "Count")
    }
    ## This is the filter option for the type 3, which corresponds to the only multiple choice of the develope
    else if (input$plot_select == "plot_3") {
      pv_num <- pv_num %>%
        # Convert develope to the numeric for aggregating
        mutate(across(starts_with("develop_"), as.numeric))
      
      
      ## pivotting over all the column with develope choices which has "1" variable
      pv_num_long <- pv_num %>%
        pivot_longer(cols = starts_with("develop_"),
                     names_to = "mc_options",
                     values_to = "count") %>%
        filter(count == "1")  # only keep rows with count == "1"
      
      # group by multiple choice options and count the occurrences with if else command # Create a bar plot to visualize the count of multiple choice answers
      pv_num_agg_type_3 <- pv_num_long %>%
        mutate(mc_options = case_when(
          mc_options == "develop_1" ~ "walking",
          mc_options == "develop_2" ~ "cycling",
          mc_options == "develop_3" ~ "sports",
          mc_options == "develop_4" ~ "working",
          mc_options == "develop_5" ~ "playing",
          mc_options == "develop_6" ~ "other",
          TRUE ~ mc_options
        )) %>%
        group_by(mc_options) %>%
        summarise(n = n())
      ## ggplot for the count option with filter "1"
      ggplot(pv_num_agg_type_3, aes(x = mc_options, y = n)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(title = "Count of Multiple Choice Answers",
             x = "Multiple Choice Options",
             y = "Count")
    }
  })
  
  
  
  ## Popup content on the map navigation used if, else to filter the option from the type of choice 
  # Generate popup content based on the selected 'what_type' value
  popup_content <- reactive({
    if (input$what_type == "1") {
      paste("This is a place where I like to be physically active ")
      
    } else if (input$what_type == "2") {
      paste("This is a place where I do not like to be physically active")
    } else if (input$what_type == "3") {
      paste("This is a place that could be used for physical activity")
    }
  })
  
  
 ## Rendering of the map to make it accessible for the interface with filtering the content of choice 
  output$mymap <- renderLeaflet({leaflet(pv_num %>%
                                           dplyr::filter(
                                             ### what column we want, input taking from the UI while making slection from drop box
                                             type == input$what_type # Filter data based on the selected type from UI dropdown
                                           ))%>%
      addTiles() %>% 
      addMarkers(
        lat = ~lat, lng = ~long,
        # cluster option to make the map more visually accessible
        clusterOptions = markerClusterOptions(),# Enable clustering for markers
        popup = ~paste(popup_content(), # Define popup content using reactive values
                       "<div>",
                       "<br>",
                       "<img src='", photo_URL, "' width='150' />",
                       "</div>"), options = popupOptions(maxWidth = "150"))
    
  })
  
  ### Output plot for the gender and age column with if and else command to specify the categories
  output$plotting_demo <- renderPlot({
    if (input$plot_select_demo == "Age") {
      # Create a bar plot of age distribution
      ggplot(pv_num, aes(x = age)) +
        geom_bar(binwidth = 5, fill = "steelblue") +
        labs(title = "Count of Respondents by Age",
             x = "Age",
             y = "Count")
      # Check if the selected plot option is "Gender"
    } else if (input$plot_select_demo == "Gender") {
      # Create a bar plot of gender distribution
      ggplot(pv_num, aes(x = gender)) +
        geom_bar(fill = "steelblue") +
        labs(title = "Count of Responses by Gender",
             x = "Gender",
             y = "Count")
    }
  })
  ### Data is attaching the our excel data from the environment # Render the data table
  output$Data <- renderDataTable(pv_num)
}

shinyApp(ui, server)

