library(shiny)
library(tidyverse)
library(shinythemes)
library(plotly)
library(sjPlot)

# I prepared my data in the data_preparation.R file and saved the csv in the app folder
# I'm reading in the data and renaming the variables so they will look better in the
# plot and data tables

data_master <- read_csv("final_data.csv") %>% 
  select(-X1, -country_code) %>% 
  rename(Year = year, Country = country_name, `Female managers` = female_managers,
         `Male managers` = male_managers, `Female hours` = female_hours,
         `Male hours` = male_hours, `Female part-time` = female_pt,
         `Male part-time` = male_pt, `Female LFPR` = lfpr_female,
         `Family-benefits spending` = family_benefits_exp, `Female tertiary students` = per_female_tertiary,
         `Maternity leave` = mandatory_leave_days, `Service sector employment` = employment_services,
         `EPL score` = epl_score, `Female legislators` = female_politicians)

# Creating a vector of nice variable lables for user-selection in plots, tables, and regressions

variable_options <- c("Female employment in management" = "Female managers",
                      "Male employment in management" = "Male managers",
                      "Weekly hours for women" = "Female hours",
                      "Weekly hours for men" = "Male hours",
                      "Women working part-time" = "Female part-time",
                      "Men working part-time" = "Male part-time",
                      "Female labor force participation rate" = "Female LFPR",
                      "Public spending on family benefits" = "Family-benefits spending",
                      "Female share of tertiary students" = "Female tertiary students",
                      "Mandatory paid maternity leave" = "Maternity leave",
                      "Share of persons employed in the service sector" = "Service sector employment",
                      "Employment protection legislation score" = "EPL score", 
                      "Female representation in government" = "Female legislators")


# Creating a vector of definitions for these variables

variable_defintions <- c("the percentage of women in management positions" = "Female managers",
                         "the percentage of men in management positions" = "Male managers",
                         "the average hours per week worked by female managers" = "Female hours",
                         "the average hours per week worked by male managers" = "Male hours",
                         "the percentage of female managers who work part-time" = "Female part-time",
                         "the percentage of male managers who work part-time" = "Male part-time",
                         "the sum of all employed women divided by the working age population" = "Female LFPR",
                         "public spending on family benefits as a percentage of GDP" = "Family-benefits spending",
                         "the percentage of all students enrolled in tertiary education who are female" = "Female tertiary students",
                         "the number of days of paid time-off to which a female employee is entitled to in order to take care of a newborn child" = "Maternity leave",
                         "the number of persons working in the service sector as a percentage of total employment" = "Service sector employment",
                         "a country's score on the OECD employment protection legislation index, which ranges from 1 - 6" = "EPL score",
                         "the percentage of legislative seats held by women" = "Female legislators")

# Creating country choices for drop-down menu

country_options <- data_master %>% select(Country) %>% distinct %>% pull(Country)

# Creating variable choices for my plot

plot_options <- c("Female employment in management" = "Female managers",
                  "Male employment in management" = "Male managers",
                  "Female labor force participation rate" = "Female LFPR",
                  "Public spending on family benefits" = "Family-benefits spending",
                  "Female share of tertiary students" = "Female tertiary students",
                  "Share of persons employed in the service sector" = "Service sector employment",
                  "Employment protection legislation score" = "EPL score",
                  "Female representation in government" = "Female legislators")

# Creating variable choices for the x-axis of my regression

regression_options <- c("Female labor force participation rate" = "Female LFPR",
                        "Public spending on family benefits" = "Family-benefits spending",
                        "Female share of tertiary students" = "Female tertiary students",
                        "Share of persons employed in the service sector" = "Service sector employment",
                        "Employment protection legislation score" = "EPL score",
                        "Female representation in government" = "Female legislators")

# I am using a navbar layout for my shiny app so I can have multiple distinct sub-components 
# (each with their own sidebar, tabsets, etc)

ui <- fluidPage(theme = shinytheme("flatly"),
   
   navbarPage("",

   tabPanel("Overview",
            htmlOutput("overview")),
   
   # Data table
   
   tabPanel("Data Table",
            sidebarLayout(
              sidebarPanel(
                selectInput(inputId = "country_table",
                            label = "Choose country to display",
                            choices = country_options,
                            selected = "United States"),
                br(),
                sliderInput(inputId = "table_years",
                            label = "Select year range",
                            min = 2008, max = 2017, value = c(2010, 2017), step = 1),
                br(),
                selectizeInput(inputId = "table_variable",
                            label = "Select up to 5 variables",
                            choices = variable_options,
                            multiple = TRUE,
                            options = list(maxItems = 5),
                            selected = variable_options[1]),
                htmlOutput("define_variables_table")),
              
              mainPanel(
                DT::dataTableOutput(outputId = "table1")))),
   
   # Plot 
   
   tabPanel("Data Visualization",
            sidebarLayout(
              sidebarPanel(
                selectInput(inputID = "y",
                            label = "Select a variable to display on the y-axis",
                            choices = plot_options,
                            selected = plot_options[1]),
                htmlOutput("define_variables_y"),
                br(),
                selectizeInput(inputId = "plot_country",
                            label = "Select up to 16 countries",
                            choices = country_options,
                            multiple = TRUE,
                            options = list(maxItems = 16),
                            selected = country_options[1])),
          
              mainPanel(
                  h3("Instructions"),
                  p("Select up to 16 countries and a variable to view the scatter plot", br(), "Hover over each point for details")), 
                plotlyOutput("scatterplot"))),
   
   # Regression
   
   tabPanel("Modeling the Data",
            sidebarLayout(
              sidebarPanel(
                selectInput(inputId = "x_regression",
                            label = "Select a variable to plot on the x-axis",
                            choices = regression_options,
                            selected = regression_options[6]),
                htmlOutput("define_reg_variable")),
              
              mainPane(
                h3("Instructions"),
                p("Select an x variable to plot against the percentage of women in management positions.  See below for a model summary and interpretation."),
                plotOutput("regression_plot"),
                br(),
                htmlOutput("regression_stats"))))))


# Define server 

server <- function(input, output) {
   
  # Reactive that filters table data based on user selection
  
  table_data <- reactive({
    req(input$table_variable, input$table_years[1], input$table_years[2])
    data_master %>% 
      filter(Year >= input$table_years[1], Year <= input$table_years[2]) %>% 
      subset(select = c(Country, Year, input$table_variable)) })
  
  # Reactive that subsets the countries to plot in the scatterplot
  
  plot_countries <- reactive({ 
    req(input$plot_country)
    data_master %>%
      filter(Country %in% input$plot_country) })
  
  output$overview <- renderUI({
    HTML(paste(
      h3("ABOUT THIS APP"),
      p("This interactive application can be used to evaluate the percentage of women in management positions across 14 different countries from 2008 to 2017."),
      p("Click through the above tabs to explore the data in a table, scatterplot, and linear regression model. These different lenses are intended to highlight the cross-national variations in the percentage of women managers as well as its interactions with several other social indicators.  
        The data also helps communicate changes in crime trends from the tumultuous 1990s period through President Vladimir Putin's first two terms and the first half of Dmitry Medvedev's presidency."),
      h3("SOURCES"), 
      p("European Union Labor Force Survey (EU LFS):", 
        br(),   
        "The EU LFS is conducted in the 28 Member States of the European Union, 2 candidate countries, and 3 countries of the European Free Trade Association (EFTA). The data collection covers the years from 1983 onwards. For this project, I was analyzing the LFS microdata for the countries in the EU-15.",
        br(),
        "To learn more about this data, click ",
        tags$a(href = "https://ec.europa.eu/eurostat/web/microdata/european-union-labour-force-survey",
               "here.")),
      p("Current Population Survey (CPS):",
        br(),
        "The CPS, conducted jointly by the U.S. Census Bureau and the U.S. Bureau of Labor Statistics (BLS), is the primary source of labor force statistics for the U.S. population. For this project, I was analyzing the occupational data included in the Annual Social and Economic (ASEC) supplement to the CPS.",
        br(),
        "To learn more about this data, click ",
        tags$a(href = "https://www.census.gov/programs-surveys/cps.html",
               "here.")),
      p("Social Indicators:",
        br(),
        "I pulled from several different datasets to explore social indicators related to female occupational outcomes.",
        br(),
        "My primary sources were the ",
        tags$a(href = "https://data.worldbank.org/",
               "World Bank Open DataBank"),
        ", the United Nations' ",
        tags$a(href = "http://hdr.undp.org/en/data",
               "Human Development Data"),
        ", the ",
        tags$a(href = "http://data.uis.unesco.org/",
               "UNESCO Institute for Statistics data"),
        ", and the ",
        tags$a(href = "https://data.oecd.org/",
               "OECD databases."))))
  })
  
  # Data table output.
  
  output$table1 <- DT::renderDataTable({
    DT::datatable(data = table_data()) 
  })
  
  # Scatter plot output

  output$scatterplot <- renderPlotly({
    ggplotly(ggplot(data = plot_countries(), aes_string(x = "Year", y = input$y, color = "Country")) + 
               geom_point(alpha = 0.8) + geom_line(aes(color = "Country", group = "Country")) +
               labs(x = "Year", 
                    y = names(crime_options[which(plot_options == input$y)]))) %>% 
      config(displayModeBar = FALSE) })
  
  # Correlation plot output
  
  output$regression_plot <- renderPlot({
    ggplot(data = master_data, aes_string(x = input$x_regression, y = "Female managers")) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm") +
      labs(x = names(regression_options[which(regrssion_options == input$x_regression)]), 
           y = "Female Managers")
  })
  
  # Reactive HTML table output for regreassion results
  
  reg_table <-  reactive({
    model <- paste0("Female managers", "  ~ ", input$x_regression)    
    table <- tab_model(lm(model, data = master_data)) })
  
  # HTML table from regression results output
  
  output$regression_stats <- renderPrint({
    HTML(h4("Model Summary"))
    reg_table()
  })
  
  # Create a variable descriptor for the data table
  
  output$define_variables_table <- renderUI({
    for(i in input$table_variable) {
      if(is.null(input$table_variable[i])) {
        return(NULL)
      } else {
        HTML(paste("Where",
                   str_to_lower(names(variable_options[which(variable_options == input$table_variable[i])])),
                   " is defined as ",
                   names(variable_definitions[which(variable_defintions == input$table_variable[i])])))
      }
    }
  })
  
  # Create a variable descriptor for the scatter plot
  
  output$define_variables_y <- renderUI({
    HTML(paste("Where ",
               str_to_lower(names(plot_options[which(plot_options == input$y)])),
               " is defined as ",
               names(variable_definitions[which(variable_definitions == input$y)])))  
  })
  
  # Create a y variable descriptor for the reg plot
  
  output$define_reg_variable <- renderUI({
    HTML(paste("Where ",
               str_to_lower(names(regression_options[which(regression_options == input$x_regression)])),
               " is defined as ",
               names(variable_definitions[which(variable_definitions == input$x_regression)])))  
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

