# Loading the Packages -------------------------------------

#Setting the required packages
pkgs <- c("shiny", "shinydashboard", "shinyWidgets",
          "RMySQL", "tidyverse",
          "plotly", "caret",
          "dplyr", "lubridate",
          "DT", "knitr", "kableExtra"
)


for(pkg in pkgs){
  if(!(pkg %in% rownames(installed.packages()))){
    install.packages(pkg, dependencies = TRUE)
  }
  lapply(pkg, FUN = function(X) {
    do.call("require", list(X))
  })
}

# Source the UI Function -------------------------------------
# source("ui.R")

# Data -------------------------------------------------------
## obtain the data
con <- dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', 
                 dbname='dataanalytics2018', 
                 host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

## query the attributes
query_data <- paste0("SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM")

yr_2006 <- dbGetQuery(con, paste(query_data, "yr_2006"))
yr_2007 <- dbGetQuery(con, paste(query_data, "yr_2007"))
yr_2008 <- dbGetQuery(con, paste(query_data, "yr_2008"))
yr_2009 <- dbGetQuery(con, paste(query_data, "yr_2009"))
yr_2010 <- dbGetQuery(con, paste(query_data, "yr_2010"))
rm(query_data)

number <- 300
submeter_data <- bind_rows(head(yr_2007, number), head(yr_2008, number), head(yr_2009, number), head(yr_2010, number))
rm(yr_2006, yr_2007, yr_2008, yr_2009, yr_2010)

## combine the two attributes and convert the data type
submeter_data <- submeter_data %>% 
  mutate(datetime = as.POSIXct(paste(Date, Time, sep = "T"), tz = "Europe/Paris")) %>% 
  select(datetime, starts_with("Sub_"))

# UI Function -------------------------------------
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Sub-metering"),
  # Dashboard Side Bar -------------------------------------
  dashboardSidebar(
    sidebarMenu(id = "tabs", 
                menuItem("Overview", icon = icon("table"), tabName = "overview"),
                menuItem("Visualization", icon = icon("palette"), startExpanded = TRUE,
                         menuSubItem("Barplots", icon = icon("bar-chart-o"), tabName = "barplots"),
                         menuSubItem("Heatmaps", icon = icon("fire"), tabName = "heatmaps"),
                         menuSubItem("Lines", icon = icon("chart-line"), tabName = "lines")
                )
                # , menuItem("Models", icon = icon("cog"), tabName = "models",
                #          menuSubItem("Regression & Classification", tabName = "models1")
                # )
    )
  ),
  # Dashboard Body -------------------------------------
  dashboardBody(
    tabItems(
      tabItem(
        # Overview content -------------------------------------------------
        tabName = "overview",
        fluidRow(
          tabBox(width = NULL,
                 tabPanel("Smart Home", 
                          column(width = 4,
                                 fluidRow(
                                   br(),
                                   h3(style="text-align:justify", "The smart home category has huge potential and has led to a number of
                                          organizations planning to address this market"), br(), 
                                   tags$ul(
                                     tags$li("Businesses from ", strong(style="font-size: 20px;color:DodgerBlue", "diverse"), " industries are investigating the opportunities"), hr(),
                                     tags$li("The market is expected to ", strong(style="font-size: 20px;color:DarkRed", "expand rapidly"), " as more brands see the potential"), hr(),
                                     tags$li("Companies as diverse as home retailers, insurance providers, utilities and tech
                                          companies ", strong(style="font-size: 20px;color:YellowGreen", "all interested"), " in Smart Home"), hr(),
                                     tags$li("Revenue in the Smart Home market amounts to ", strong(style="font-size: 20px;color:RoyalBlue", "US$90,968m"), " in 2020 and 
                                                 it is expected to show an annual growth rate of 15.0%")
                                   )
                                 )
                          ), 
                          column(width = 8, br(), br(), br(),
                                 img(src='smart_home.png', align = 'right')
                          )
                 ),
                 tabPanel("Sub-meters", 
                          column(width = 4,
                                 fluidRow(
                                   br(), br(),
                                   strong("Electrical submetering"), " involves the installation power meters (also called power monitors, electrical meters, or energy monitors) 
                                   that can measure energy usage after it reaches the primary utility meter.", br(), br(),
                                   "Submetering offers the ability to ", strong("monitor energy usage"), " for: ",
                                   tags$ol(
                                     tags$li("individual tenants"), 
                                     tags$li("departments"), 
                                     tags$li("pieces of equipment"), 
                                     tags$li("other loads") 
                                   ),
                                   "to account for their actual energy usage."
                                 )
                          ), 
                          column(width = 8, br(), br(),
                                 img(src='submeter.jpg', align = 'right', width = 680)
                          )),
                 tabPanel("Benefits", 
                          column(width = 5,
                                 fluidRow(
                                   br(), br(),
                                   tags$ul(
                                     tags$li("Accurate energy monitoring, real-time energy consumption"),
                                     tags$li("Granular in-depth review of facility energy data"),
                                     tags$li("Better informed to make decisions that can help optimize energy performance"),
                                     tags$li("Ability to record actual energy usage (no estimates)"),
                                     tags$li("Comparison of usage across similar facilities over time"), 
                                     tags$li("Ability to identify and eliminate wasted energy"), 
                                     tags$li("Early access to maintenance issues for repair before critical equipment fails")
                                   )
                                 )
                          ), 
                          column(width = 7, br(), br(),
                                 img(src='benefits.jpg', align = 'right', width = 550)
                          )),
                 tabPanel("Challenges", 
                          br(),
                          h3(style="text-align:left", "But a number of questions remain"),
                          column(width = 4,
                                 fluidRow(
                                   br(), br(),
                                   infoBox(width = 12, color = "blue", title = "", icon = icon("dice-one"),
                                       subtitle = h4("Are consumers ready for the smart home?")
                                   ),
                                   infoBox(width = 12, color = "aqua", title = "", icon = icon("dice-two"),
                                       subtitle = h4("What elements are key?")
                                   ),
                                   infoBox(width = 12, color = "navy", title = "", icon = icon("dice-three"),
                                           subtitle = h4("The main barriers to adoption?")
                                   ),
                                   infoBox(width = 12, color = "lime", title = "", icon = icon("dice-four"),
                                           subtitle = h4("Which brands are trusted to deliver the smart home?")
                                   )
                                 )
                          ), 
                          column(width = 8, br(), br(),
                                 img(src='companies.png', align = 'right', height = 400)
                          ))
          )
        )
      ),
      # Barplot - Visualization content -------------------------------------------------
      tabItem(
        tabName = "barplots",
        fluidRow(
          tabBox(width = NULL,
                 tabPanel("Data",
                          br(),
                          column(width = 4,
                                 box(width = NULL,
                                     title = "Input", status = "success", solidHeader = TRUE,
                                     pickerInput("submeter_bar", "Submeter location:", 
                                                 choices = c("Kitchen" = "Sub_metering_1", "Laundry room" = "Sub_metering_2", "Water-heater/Air-conditioner" = "Sub_metering_3"),
                                                 multiple = TRUE, selected = c("Kitchen", "Laundry room", "Water-heater/Air-conditioner"), options = list(`actions-box` = TRUE)),
                                     dateRangeInput("dates_bar", "Date range:", 
                                                    start = min(submeter_data$datetime), end = max(submeter_data$datetime)),
                                     selectInput("position_bar", "Barplot type:", 
                                                 choices = c("Grouped bar chart" = "dodge", "Stacked bar chart" = "fill"), 
                                                 selected = c("Grouped bar chart")),
                                     selectInput("freq_bar", "Time period:", 
                                                 choices = c("Year", "Month", "Weekday", "Day"), selected = "Year")
                                 )
                          ),
                          column(width = 8,
                                 box(width = NULL,
                                     title = "Input", status = "success", solidHeader = TRUE,
                                     DT::DTOutput("table_bar")
                                 )
                          )
                          ),
                 tabPanel("Barplot", br(),
                          plotlyOutput("plot_bar")
                          )
          )
        )
      ),
      # Heatmap - Visualization content -------------------------------------------------
      tabItem(
        tabName = "heatmaps",
        fluidRow(
          tabBox(width = NULL,
                 tabPanel("Data"
                          
                 ),
                 tabPanel("Heatmap"
                          
                 )
          )
        )
      ),
      # Lines - Visualization content -------------------------------------------------
      tabItem(
        tabName = "lines",
        fluidRow(
          tabBox(width = NULL,
                 tabPanel("Data"
                          
                 ),
                 tabPanel("Lines"
                          
                 )
          )
        )
      )
    )
  )
)
# Server Function -------------------------------------
server <- function(input, output, session) {
  # Barplot - filter the database ----------------------------------------------------
  submeter_data_barplot <- reactive({
    submeter_data %>% 
      filter(datetime >= input$dates_bar[1], datetime <= input$dates_bar[2]) %>% 
      select(datetime, input$submeter_bar)
  })
  
  # Barplot - show the database ------------------------------------------------------
  output$table_bar <- DT::renderDataTable({
    head(submeter_data_barplot(), 25)
  })
  
  # Barplot - plot barplot ------------------------------------------------------
  output$plot_bar <- renderPlotly({
    if(input$freq_bar == "Year"){
      p <- submeter_data_barplot() %>% 
        group_by(year = year(datetime)) %>%  
        summarize_at(input$submeter_bar, sum) %>% 
        pivot_longer(-year, names_to = "Sub_metering", values_to = "Watt_hour") %>%
        ggplot(aes(year, Watt_hour, fill = Sub_metering))
      
    }else if(input$freq_bar == "Month"){
      p <- submeter_data_barplot() %>% 
        group_by(month = month(datetime, label = TRUE)) %>%  
        summarize_at(input$submeter_bar, sum) %>% 
        pivot_longer(-month, names_to = "Sub_metering", values_to = "Watt_hour") %>%
        ggplot(aes(month, Watt_hour, fill = Sub_metering))
      
    }else if(input$freq_bar == "Weekday"){
      p <- submeter_data_barplot() %>% 
        group_by(weekday = weekdays(datetime)) %>%  
        summarize_at(input$submeter_bar, sum) %>% 
        pivot_longer(-weekday, names_to = "Sub_metering", values_to = "Watt_hour") %>%
        ggplot(aes(weekday, Watt_hour, fill = Sub_metering))
      
    }else if(input$freq_bar == "Day"){
      p <- submeter_data_barplot() %>% 
        group_by(day = day(datetime)) %>%  
        summarize_at(input$submeter_bar, sum) %>% 
        pivot_longer(-day, names_to = "Sub_metering", values_to = "Watt_hour") %>%
        ggplot(aes(day, Watt_hour, fill = Sub_metering))
    }
    
    p <- p + geom_bar(stat = "identity", position = input$position_bar, colour = "black") + 
      labs(
        x = "", 
        y = "Watt-hour of active energy"
      ) + 
      scale_fill_discrete(
        name = "Sub-metering",
        breaks = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
        labels = c("Kitchen", "Laundry", "Heater")
      )
    p
  })
}

# Call the App -------------------------------------
shinyApp(ui = ui, server = server)