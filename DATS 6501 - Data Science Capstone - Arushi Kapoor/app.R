source("Visualizations.R")
source("Tweets.R")

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)


### Header ###

header <- dashboardHeader(title = span("Gender Gap: Data Science & Machine Learning", style = "font-family: Roboto+Condensed; font-weight: bold"), 
                          titleWidth = 650)

### Sidebar ###

sidebar <- dashboardSidebar(   
  sidebarMenu(id = "mysidebar",
              menuItem("Exploratory Data Analysis", tabName = "eda"),
              menuItem("Dissecting the Pay Gap", tabName = "pay_gap",
                       menuSubItem("Overall Compensation", tabName = "oc"),
                       menuSubItem("Age", tabName = "age"),
                       menuSubItem("Education Level", tabName = "ed"),
                       menuSubItem("Job Title", tabName = "jt"),
                       menuSubItem("Country of Residence", tabName = "nat"),
                       menuSubItem("Programming Languages", tabName = "plang")
              ),
              menuItem("Exploring Twitter", tabName = "tw_an")
              
  ))

### Body ###

body <- dashboardBody(tags$head(#includeCSS("styles.css"), 
                                tags$style(HTML(".main-sidebar { font-size: 18px; }"),
                                           (".content-wrapper { background-color: #fff; }")),
                                tags$script('$(".navbar-custom-menu").on("click",function()
                                {$(window).trigger("resize");})')),
                      tabItems(
                        tabItem(tabName = "eda",
                                h2("Examining Gender Distributions", align = "center"),
                                fluidRow(
                                  column(width = 12, align="center",
                                         fluidRow(
                                           column(width = 12,
                                                  plotOutput("g_resp", height = 500, width = 650))),
                                         fluidRow(br()),
                                         fluidRow(
                                           column(width = 12,
                                                  plotOutput("g_age", height = 550, width = 1000))),
                                         fluidRow(br()),
                                         fluidRow(
                                           column(width = 12,
                                                  plotOutput("g_educ", height = 550, width = 1000))),
                                         fluidRow(br()),
                                         fluidRow(
                                           column(width = 12,
                                                  plotOutput("g_job", height = 550))),
                                         fluidRow(br()),
                                         fluidRow(
                                           column(width = 12,
                                                  plotOutput("g_code", height = 550))),
                                         fluidRow(br()),
                                         fluidRow(
                                           column(width = 12,
                                                  plotOutput("g_nat", height = 550))),
                                         fluidRow(br()),
                                         fluidRow(
                                           column(width = 12,
                                                  plotOutput("g_compg", height = 550)))
                                         
                                  )
                                )
                                
                        ),
                        tabItem(tabName = "oc",
                                h2("Examining the Gender Pay Gap", align = "center",
                                   fluidRow(
                                     column(width = 12,
                                            fluidRow(br()),
                                            fluidRow(
                                              column(width = 12, align = "center",
                                                     plotOutput("g_comp", height = 400, width = 750)),
                                              fluidRow(br()), 
                                              fluidRow(br()),
                                              fluidRow(br()),
                                              column(width = 12, align = "center", 
                                                     plotOutput("g_comp_pc", height = 400, width = 750)))
                                     )))), 
                        tabItem(tabName = "age",
                                            h2("Pay Gap by Age", align = "center"),
                                            fluidRow(
                                              column(width = 12, align="center",
                                                     plotOutput("pg_age", height = 1250)))
                                     ),
                        tabItem(tabName = "ed",
                                h2("Pay Gap by Education", align = "center"),
                                fluidRow(
                                  column(width = 12, align="center",
                                         plotOutput("pg_educ", height = 1250)))
                        ), 
                        tabItem(tabName = "jt",
                                h2("Pay Gap by Job Title", align = "center"),
                                fluidRow(
                                  column(width = 12, align="center",
                                         plotOutput("pg_jt", height = 1250)),
                                  fluidRow(br()), 
                                  fluidRow(br()),
                                  column(width = 12, align="center",
                                         plotOutput("pg_jt_s", height = 800)))
                        ), 
                        tabItem(tabName = "nat",
                                h2("Pay Gap by Country of Residence", align = "center"),
                                fluidRow(
                                  column(width = 12, align="center",
                                         plotOutput("pg_nat", height = 1250)))
                        ), 
                        tabItem(tabName = "plang",
                                h2("Pay Gap by Programming Languages", align = "center",
                                fluidRow(
                                  column(width = 12, 
                                         fluidRow(br()),
                                         fluidRow(
                                           column(width = 6, 
                                                  plotOutput("pg_python", height = 900)),
                                           column(width = 6,
                                                  plotOutput("pg_r", height = 900)), 
                                           column(width = 6,
                                                  plotOutput("pg_sql", height = 900)),
                                           column(width = 6,
                                                  plotOutput("pg_java", height = 900)),
                                           column(width = 6,
                                                  plotOutput("pg_c", height = 900)), 
                                           column(width = 6,
                                                  plotOutput("pg_cpl", height = 900)))
                                
                        )
                        
                                   ))),
                        tabItem(tabName = "tw_an",
                                h2("Live Twitter Sentiment: A Brief Analysis", align = "center",
                                   fluidRow(
                                     column(width = 12,
                                            h3("Exploring Tweets"),
                                            fluidRow(br()),
                                            fluidRow(
                                              column(width = 12, align="center",
                                                     plotOutput("tplot", height = 500, width = 700)),
                                              fluidRow(br()),
                                              fluidRow(br()),
                                              column(width = 12, align="center",
                                                     plotOutput("splot1", height = 400, width = 1200)),
                                              fluidRow(br()),
                                              fluidRow(br()),
                                              column(width = 12, align="center",
                                                     plotOutput("splot2", height = 1000)))
                                     ))))))


#####UI#####

ui <- dashboardPage(title="Gender Gap", skin = "green", header, sidebar, body)

### Server ###

server <- function(input, output) { 
  
  #Number of Respondents
  output$g_resp <- renderPlot({
    resp
  })
  
  #Gender Distribution by Age
  output$g_age <- renderPlot({
    g_age
  })
  
  #Gender Distribution by Education Levels
  output$g_educ <- renderPlot({
    g_educ
    
  })
  
  #Gender Distribution by Job Titles
  output$g_job <- renderPlot({
    g_job
  })
  
  #Gender Distribution by Years of Coding Experience
  output$g_code <- renderPlot({
    g_code
  })
  
  #Gender Distribution by Countries (Top 10)
  output$g_nat <- renderPlot({
    g_nat
  })
  
  #Gender Distribution by Compensation Groups
  output$g_compg <- renderPlot({
    g_compg
  })
  
  #Difference in Average Compensation in USD
  output$g_comp <- renderPlot({
    g_comp
    
  })
  
  #Percent Difference in Average Compensation
  output$g_comp_pc <- renderPlot({
    g_comp_pc
    
  })
  
  #Pay Gap by Age
  output$pg_age <- renderPlot({
    diff_pc_age / grid.arrange(g3)
    
  })
  
  #Pay Gap by Education
  output$pg_educ <- renderPlot({
    diff_pc_educ2 / grid.arrange(g4)
    
  })
  
  #Pay Gap by Job Title
  output$pg_jt <- renderPlot({
    diff_pc_jt / grid.arrange(g5)
    
  })
  
  #Pay Gap Severity by Job Titles
  output$pg_jt_s <- renderPlot({
    pg_jt_s
    
  })
  
  #Pay Gap by Country of Residence
  output$pg_nat <- renderPlot({
    diff_pc_na / grid.arrange(g7)
    
  })
  
  #Pay Gap by Python
  output$pg_python <- renderPlot({
    diff_pc_py / grid.arrange(g6)

  })
  
  #Pay Gap by R
  output$pg_r <- renderPlot({
    diff_pc_r / grid.arrange(g8)
    
  })
  
  #Pay Gap by SQL
  output$pg_sql <- renderPlot({
    diff_pc_sql / grid.arrange(g9)
    
  })
  
  #Pay Gap by Java
  output$pg_java <- renderPlot({
    diff_pc_java / grid.arrange(g10)
    
  })
  
  #Pay Gap by C
  output$pg_c <- renderPlot({
    diff_pc_c / grid.arrange(g11)
    
  })
  
  #Pay Gap by C++
  output$pg_cpl <- renderPlot({
    diff_pc_cpl / grid.arrange(g12)
    
  })
  
  
  #Tweets Density Plot
  output$tplot <- renderPlot({
    t.plot
    
  })
  
  
  #Sentiment Analysis
  output$splot1 <- renderPlot({
    sent.plot1
    
  })
  
  #Words
  output$splot2 <- renderPlot({
    sent.plot2
    
  })
  
  
  
  
}


shinyApp(ui, server)
