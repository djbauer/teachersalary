### Setup

library(shiny)
library(shinythemes)
library(tidyverse)
library(DT) # for the DataTable output... pretty cool!

source("teacherdata.R") # run the script to load and clean the data. the teacherdata.R file must appear in the same directory as the current script (app.R); the 
# downloaded .csv file must appear in a subdirectory called 'data'.

### Define UI for application
ui <- navbarPage("Salary Summarizer",
      theme = shinytheme("united"),
      
      tabPanel("Overview",
        sidebarPanel(
          h2("Synopsis"),
          p("This tool will provide you with summary information about teacher salary in the La Crosse, WI School District from the 2018 - 2019 contract year.") 
        ),
        mainPanel(
          h1("Teacher Salary Summarizer",
          h3("School District of La Crosse, WI")),  
          h2("Overview"),
          p("The raw data were acquired from a",
          (a("public database", href = "https://publicstaffreports.dpi.wi.gov/PubStaffReport/Public/PublicReport/AllStaffReport")),
          ("maintained by the state of Wisconsin. The filters included:")),
          tags$ul(
            tags$li("Year: 2018 - 2019"),
            tags$li("Hiring Agency: 2849 - La Crosse School District"),
            tags$li("Assignment Position: 53 - Teacher")
            ),
          p("The public database contains many variables beyond those presented here; please refer to the source material for further exploration."),
          h2("Directions"),
          p("Click on the Tool tab in the nagivation bar above to access the summarizer. Alter the input variables in the sidebar as desired 
          to view a summary of the teacher salary data for the combined variables.")
        )),

      tabPanel("Tool", 
        sidebarPanel(
          h2("Input Variables"),
          checkboxGroupInput("gender",
                            h4("Gender"),
                            choices = list(
                              "Female",
                              "Male"),
                            selected = c("Female", "Male")
          ),
          checkboxGroupInput("race", 
                            h4("Race/Ethnicity"),
                            choices = list(
                              "American Indian or Alaska Native",
                              "Asian",
                              "Black or African American",
                              "Hispanic/Latino",
                              "Native Hawaiian or Other Pacific Islander",
                              "Two or More Races",
                              "White"),
                            selected = c("American Indian or Alaska Native",
                                         "Asian",
                                         "Black or African American",
                                         "Hispanic/Latino",
                                         "Native Hawaiian or Other Pacific Islander",
                                         "Two or More Races",
                                         "White")
         ),
         checkboxGroupInput("degree",
                            h4("Degree"),
                            choices = list(
                              "Bachelors",
                              "Masters",
                              "Doctoral",
                              "Other"),
                            selected = c("Bachelors",
                                         "Masters",
                                         "Doctoral",
                                         "Other")
         ),
         sliderInput("experience",
                     h4("Experience (Years)"),
                     min = min(teach$Experience),
                     max = max(teach$Experience),
                     value = c(min(teach$Experience), max(teach$Experience))
         ),
         sliderInput("age",
                     h4("Age (Years)"),
                     min = min(teach$Age),
                     max = max(teach$Age),
                     value = c(min(teach$Age), max(teach$Age))
         )
        ),
                              
        mainPanel(
          h2("Salary Summary of La Crosse, WI District Teachers"),
          h4("The boxplot below displays the salary for each individual (jittered purple dots) in the data set, filtered 
             by the input variables. The thick black line in the center of the gray box represents the median salary (50th %ile) for 
             this group of individuals. The thin black lines on the left and right sides of the gray box represent the 1st 
             and 3rd quartile salaries (25th and 75th %iles, respectively). The dashed black lines extend from the 
             gray box to the extreme values in the data set and therefore provide the full range of salaries for the 
             selected group. Values are in 2018 US Dollars ($)"),
          plotOutput("plot"),
          hr(),
          h2("Raw Data"),
          p("These are the raw data used to generate the above summary based on the selected input variables. 
            Rows are automatically ordered alphabetically by last name; however, I removed the name columns. 
            Click on the small gray arrows next to a variable name to reorder the data by that variable."),
          DTOutput("rawdata")
          )
        )
      )

### Define server logic
server <- function(input, output) {
    
    output$rawdata <- renderDT ({
      teach %>%
        filter(Gender %in% input$gender) %>%
        filter(`Race/Ethnicity` %in% input$race) %>%
        filter(Degree %in% input$degree) %>%
        filter(Experience >= input$experience[1]) %>%
        filter(Experience <= input$experience[2]) %>%
        filter(Age >= input$age[1]) %>%
        filter(Age <= input$age[2])
    })
    
    filtered <- reactive ({
      teach %>%
        filter(Gender %in% input$gender) %>%
        filter(`Race/Ethnicity` %in% input$race) %>%
        filter(Degree %in% input$degree) %>%
        filter(Experience >= input$experience[1]) %>%
        filter(Experience <= input$experience[2]) %>%
        filter(Age >= input$age[1]) %>%
        filter(Age <= input$age[2])
    })
    
    output$plot <- renderPlot({
      
      boxplot(filtered()$Salary,
              frame = FALSE,
              axes = TRUE,
              range = 0,
              staplewex = 1,
              col = "gray",
              horizontal = TRUE,
              lwd = 3
              )
      
      stripchart(filtered()$Salary,
                 method = "jitter",
                 add = TRUE,
                 pch = 21,
                 cex = 2,
                 col = 'black',
                 bg = rgb(0.7,0.1,1, alpha = 0.5)
                 )
      
      text(x = fivenum(filtered()$Salary),
           labels = fivenum(filtered()$Salary),
           y = 1.25,
           cex = 1.5
           )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
