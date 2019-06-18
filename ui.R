library(shiny)


shinyUI(navbarPage("Computation of a Portfolio:", id = "inTab",
                   tabPanel("Using real market data", value = 1, 
                            # Application title
                            headerPanel("Optimal Portfolio selection"),
                            # Application's left Panel
                            sidebarPanel(
                              #we define the style for the error messages
                              tags$style(HTML("
                                              .shiny-output-error-validation {
                                              color: red;
                                              margin-top:15px;
                                              }
                                              ")), 
                              h4("Choose the Stocks to put in your Portfolio"),
                              selectInput("Exchange", "Exchange", c("ASX"="ASX")),
                              uiOutput("stockChoice"),
                              h4("Indicate the Risk Free Rate"),
                              numericInput("Risk_Free", "Risk Free Rate (%) :", 1),
                              h4("Indicate the source you want to use for the expected returns of the stocks"),
                              radioButtons("choice1", "Source of the expected returns", c("Randomly generated"= 1, "Historical data"= 2)),
                              h4("Do you want to authorize short selling ?"),
                              radioButtons("shortChoice1", "Short selling", c("Yes"= "yes", "No"= "no")),
                              h4("Indicate the period of time you want to use for historical data"),
                              dateRangeInput("dateRange", "Period to consider for historical data", start = Sys.Date()-(5*365), end = Sys.Date(), min = "2000-01-01", max = Sys.Date()),
                              actionButton("do1", "launch"),
                              #error messages, 1 for number of stocks chosen, 2 to ensure that a Rf value is chosen, 3 to make sure that all the expected returns inputs are filled
                              textOutput("alert1"),
                              textOutput("alert2"),
                              textOutput("alert4")
                            ),
                            
                            # Shows the main panel with the efficient frontier and the main information about the generated portfolio
                            mainPanel(
                              conditionalPanel("input.do1",
                                               h2("Report"),
                                               hr(),
                                               h3("Portfolio's Location on the Efficient Frontier"),
                                               plotOutput("results")
                              ),
                              conditionalPanel("output.results",
                                               p("Source of data : Yahoo Finance"),
                                               p(textOutput("dateRange1", inline = TRUE)),
                                               p(textOutput("shortSelling1", inline = TRUE)),
                                               sliderInput("RiskFreeProp1", "Percentage of Risk Free Asset in your complete Portfolio", -100, 100, 1, width = '100%'),
                                               h3("Computation's Results"),
                                               br(),
                                               h4("Complete Portfolio"),
                                               p(textOutput("expRet1"), textOutput("Risk1")),
                                               h4("Minimum Variance Portfolio"),
                                               p(textOutput("minExpRet1"), textOutput("minRisk1")),
                                               h4("Optimal Portfolio"),
                                               p(textOutput("optExpRet1"), textOutput("optRisk1")),
                                               br(),
                                               h3("Portfolios' Compositions"),
                                               br(),
                                               dataTableOutput("assets1")
                              )
                            )
                   ),
                   tabPanel("Using my data", value = 2, 
                            tags$style(HTML("
                                            p {
                                            text-align:justify;
                                            }
                                            ")),
                            headerPanel("Optimal Portfolio selection"),
                            sidebarPanel(
                              h4("Notes: "),
                              p("In this part you can input an Excel file containing your own data.
             This file must follow the model shown in the documentation and contain all the information necessary for the computation."),
                              p("This includes:"),
                              p("- Stocks' names"),
                              p("- Stocks' Expected Returns"),
                              p("- Stocks' variances"),
                              p("- Stocks' correlation coefficients"),
                              p("- The Risk Free Rate"),
                              br(),
                              h4("Please Input your data"),
                              p("Format supported: .xls, .xlsx"),
                              fileInput("inputData", "Upload your data", multiple = FALSE, accept = NULL),
                              h4("Do you want to authorize short selling ?"),
                              p("If short selling is authorized you might have assets with negative weigths."),
                              p("On the other hand, if you don't allow short selling, some assets might have a 
                              weight of 0 in your portfolio as they are inefficient."),
                              radioButtons("shortChoice2", "Short selling", c("Yes"= "yes", "No"= "no")),
                              actionButton("do2", "launch"),
                              textOutput("alertFile")
                            ),
                            mainPanel(
                             
                              conditionalPanel("input.do2",
                                               h2("Report"),
                                               hr(),
                                               h3("Portfolio's Location on the Efficient Frontier"),
                                               plotOutput("results2")
                              ),
                              
                              conditionalPanel("output.results2",
                                               textOutput("shortSelling2", inline = TRUE),
                                               sliderInput("RiskFreeProp2", "Percentage of Risk Free Asset in your complete Portfolio", -100, 100, 1, width = '100%'),
                                               h3("Computation's Results"),
                                               br(),
                                               h4("Complete Portfolio"),
                                               p(textOutput("expRet2"), textOutput("Risk2")),
                                               h4("Minimum Variance Portfolio"),
                                               p(textOutput("minExpRet2"), textOutput("minRisk2")),
                                               h4("Optimal Portfolio"),
                                               p(textOutput("optExpRet2"), textOutput("optRisk2")),
                                               br(),
                                               h3("Portfolios' Compositions"),
                                               br(),
                                               dataTableOutput("assets2")
                              )
                              
                            )
                   )
                   
))