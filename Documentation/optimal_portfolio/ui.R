library(shiny)

 
shinyUI(pageWithSidebar(
  
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
    selectInput("Stocks", 
                "Stocks:", 
                c("AAC"="AAC","AAD"="AAD","ABC"="ABC","ABP"="ABP","AGI"="AGI","AIO"="AIO","ALL"="ALL","AMC"="AMC","AMP"="AMP","ANN"="ANN","ANZ"="ANZ","APA"="APA","APN"="APN","ARI"="ARI","ASX"="ASX","AWC"="AWC","AWE"="AWE","BEN"="BEN","BGA"="BGA","BHP"="BHP","BKN"="BKN","BLD"="BLD","BOQ"="BOQ","BPT"="BPT","BRG"="BRG","BSL"="BSL","BWP"="BWP","BXB"="BXB","CAB"="CAB","CBA"="CBA","CCL"="CCL","CCP"="CCP","CDD"="CDD","CGF"="CGF","CHC"="CHC","CMW"="CMW","COH"="COH","CPU"="CPU"
),
                selected = c("AAC","AAD","ABC","ABP"), multiple = TRUE
  ),
    h4("Indicate the Risk Free Rate"),
    numericInput("Risk_Free","Risk Free Rate (%) :",1),
    
    h4("Indicate the source you want to use for the expected returns of the stocks"),
    radioButtons("choice","Source of the expected returns",c("Randomly generated"= 1, "Historical data"= 2,"Inputs"= 3)),
    
    #Panel which appears only when the "Inputs" option is chosen
    conditionalPanel("input.choice == 3",h4("Please input your Expected Returns"),uiOutput("InputExpRet")),

    actionButton("do","launch"),

    #error messages, 1 for number of stocks chosen, 2 to ensure that a Rf value is chosen, 3 to make sure that all the expected returns inputs are filled
    textOutput("alert1"),
    textOutput("alert2"),
    textOutput("alert3"),
    
    #Panel which appears once the computation is done and the efficient frontier is plotted. It gives the user the possibility to chose the percentage of risk free asset in his portfolio
    conditionalPanel("output.results",h4("Choose the proportion of Risk Free Asset in your complete Porfolio",style="margin-top:30px"),
    sliderInput("RiskFreeProp", "Percentage of Risk Free Asset", -100, 100, 1))
            
    
  ),
  
  
  
  # Shows the main panel with the efficient frontier and the main information about the generated portfolio
  mainPanel(
    plotOutput("results"),
    conditionalPanel("output.results",h3("Your Complete Portfolio",style="margin-top:10px"),
                                         textOutput("expRet"), textOutput("Risk"),tableOutput("assets"))
  
)))