library(shiny)
library(Quandl)
library(quadprog)
library(XLConnect)


shinyServer(function(input, output, clientData, session) {
  observe({
    #we verify that we are in the first tab
    if(input$inTab == 1){
      #list of stocks' codes
      if (input$Exchange == "ASX"){
        stockChoice <- c()
        stock_datacode <- c()
        stock_names <- c("AAC", "AAD", "ABC", "ABP", "AGI", "AIO", "ALL", "AMC", "AMP", "ANN", "ANZ", "AOG", "APA", "APN", "ARI", "ASX", "AWC", "AWE", "BEN", "BGA", "BHP", "BKN", "BLD", "BOQ", "BPT", "BRG", "BSL", "BWP", "BXB", "CAB", "CBA", "CCL", "CCP", "CDD", "CGF", "CMW", "COH", "CPU")
        for (i in 1:length(stock_names)){
          stockChoice[stock_names[i]] <- stock_names[i]
          #list of stocks'code used to retrieve the data from Quandl
          stock_datacode[i] <- paste0("YAHOO/ASX_", stock_names[i], "_AX")
        }
        output$stockChoice <- renderUI({
          selectInput("Stocks", 
                      "Stocks:", 
                      c("AAC"="AAC", "AAD"="AAD", "ABC"="ABC", "ABP"="ABP", "AGI"="AGI", "AIO"="AIO", "ALL"="ALL", "AMC"="AMC", "AMP"="AMP", "ANN"="ANN", "ANZ"="ANZ", "APA"="APA", "APN"="APN", "ARI"="ARI", "ASX"="ASX", "AWC"="AWC", "AWE"="AWE", "BEN"="BEN", "BGA"="BGA", "BHP"="BHP", "BKN"="BKN", "BLD"="BLD", "BOQ"="BOQ", "BPT"="BPT", "BRG"="BRG", "BSL"="BSL", "BWP"="BWP", "BXB"="BXB", "CAB"="CAB", "CBA"="CBA", "CCL"="CCL", "CCP"="CCP", "CDD"="CDD", "CGF"="CGF", "CHC"="CHC", "CMW"="CMW", "COH"="COH", "CPU"="CPU"),
                      selected = stock_names[1:4], multiple = TRUE )
        })
      }
      #random Expected Returns generation
      stock_expectReturn <- c(0.08 + 0.25 * rnorm(length(stock_names), 0, 0.4))
      #in this section we observe all the inputs happening on the left panel of the page and display the error messages accordingly
      #we check that at least two stocks are selected
      check1 <- reactive({ 
        validate(
          need(length(input$Stocks)>=2, "- Please select at least two Stocks")
        )
      })
      output$alert1 <- renderText(check1())
      #we check that the Risk free rate is indicated
      check2 <- reactive({ 
        validate(
          need(!is.na(input$Risk_Free), "- Please indicate the risk free rate")
        )
      })
      output$alert2 <- renderText(check2())
      #we check that the range for the dates longer than 5 years
      check4 <- reactive({ 
        validate(
          need(difftime(input$dateRange[2], input$dateRange[1], "years") > 4, "- The period to consider for historical data must be at least 5 years")
        )
      })
      output$alert3 <- renderText(check1())
      #we check that the Risk free rate is indicated
      check2 <- reactive({ 
        validate(
          need(!is.na(input$Exchange), "- Please select a Stock Exchange")
        )
      })
      output$alert3 <- renderText(check3())
      #in this part we define which action to perform once the launch button is pressed
      observeEvent(input$do1,{
        #if less than two stocks are selected then the program doesn't do anything, if the risk free rate isn't indicated then the program doesn't do anything, if the "Inputs" option is selected and not all expected returns are indicated then the program doesn't do anything
        if(length(input$Stocks) < 2){
          
        }else if(is.na(input$Risk_Free)){
        
        }else if(is.na(input$Exchange)){
          
        }else if(difftime(input$dateRange[2], input$dateRange[1], "years") < 5){
          
        }else{
          # if the form is completely filled the pregram then proceeds with the following
          fixedDateRange <- input$dateRange
          fixedShortChoice <- input$shortChoice1
          index <- match(input$Stocks, stock_names)
          data <- stock_datacode[index]
          Rf <- input$Risk_Free / 100
          names <-stock_names[index]
          #we retrieve the data for each selected stock from Quandl
          mydata <<- getData(data, start = input$dateRange[1], end = input$dateRange[2])
          #we calculate the arithmetic returns for each stocks
          myReturns <<- getMonthlyReturns(mydata)
          #we calculate the covariance matrix of all the selected stocks
          cov_matrix <<- getCovMatr(myReturns)
          #we define the expected returns to use in the computation according to the user's choice
          if(input$choice1 == 1){
            Expt_Ret <- stock_expectReturn[index]
          }else if(input$choice1 == 2){
            Expt_Ret <- getHistoricalReturn(getYearlyReturns(mydata))
          }
          #we calculate the optimal portfolio for the the stocks selected
          Optimal <<- OptimalPortfolio(Expt_Ret, cov_matrix, Rf,input$shortChoice1)
          #we calculate the Minimum Variance Portfolio
          MinVar <<- getMinVarPort(Expt_Ret, cov_matrix, input$shortChoice1)
          #we calculate the efficient portfolio
          Effi <<- getEfficientPort(Expt_Ret, cov_matrix, input$shortChoice1, MinVar[[2]])
          #we compute several portfolios on the efficient frontier according to the two fund separation theorem
          Frontier <<- getEffFrontier(MinVar[[1]], Effi[[1]], Expt_Ret, cov_matrix, input$shortChoice1)
          #we calculate the Capital Allocation Line
          CAL <<- getCapitalAssetLine(Optimal[[2]], Rf, Optimal[[3]])
          #here we observe risk free asset proportion's input and update the graph accordingly
          observe({
            propor <- input$RiskFreeProp1 / 100
            comp_portfolio <- getCompletePortfolio(propor, Rf, Optimal[[2]], Optimal[[3]], Optimal[[1]], Expt_Ret, cov_matrix)
            output$results <- renderPlot({
              #we draw the efficient frontier
              plot(Frontier[[2]], Frontier[[1]], type="l", xlim=c(min(0, min(Frontier[[2]])), max(Frontier[[2]])), ylim = c(min(0, min(Frontier[[1]])), max(Frontier[[1]])), main = "Efficient Frontier", xlab = "Global Risk", ylab = "Expected Return", lwd = 2, xaxs = "i", yaxs = "i")
              #we draw the optimal portfolio
              points(Optimal[[3]], Optimal[[2]], pch=21, bg="blue", cex=1.3)
              #we draw the Minimum Variance Portfolio
              points(MinVar[[3]], MinVar[[2]], pch=21, bg="red", cex=1.3)
              #we draw the risk free asset
              points(0, Rf, pch=21, bg="green", cex=1.3)
              #we draw capital allocation line
              lines(CAL[[1]], CAL[[2]], col="green")
              #we draw the complete portfolio
              points(comp_portfolio[[3]], comp_portfolio[[2]], pch=21, bg="black", cex=1.3)
              #we draw a legend for the graph
              legend(x = "bottomright", c("Optimal Portfolio of Risky Assets", "Minimum Variance Portfolio", "Risk Free Rate", "Complete Portfolio", "Capital Allocation Line"), pch = c(19, 19, 19, 19, NA), lty = c(0, 0, 0, 0, 1), col = c("blue", "red", "green", "black", "green"))
            })
            #we display the complete portfolio's expected return
            output$expRet1 <- renderText({
              paste("Expected Return :", round(comp_portfolio[[2]] * 100, 2), "%")
            })
            #we display the complete portfolio's risk
            output$Risk1 <- renderText({
              paste("Global Risk :", round(comp_portfolio[[3]] * 100, 2), "%")
            })
            #we a table summarizing all the assets present in the complete portfolio, their weight and individual expected return
            table1 <- reactive({
              table1 <- data.frame(Stocks = c("T-Bill", names), Expt_Ret = c(Rf * 100, round(Expt_Ret * 100, 2)), Variance = c(0, round(diag(cov_matrix) * 100, 2)), Weights1 = as.character(c(round(propor * 100, 2), round(comp_portfolio[[1]] * 100, 2))), Weights2 = as.character(c(0, round(MinVar[[1]] * 100, 2))), Weights1 = as.character(c(0, round(Optimal[[1]] * 100, 2))))
              colnames(table1) <- c("Stock", "Expt_Ret(%)", "Var(%)", "Weight in the Complete Port.(%)", "Weight in the Min Var Port.(%)", "Weight in the Optimal Port.(%)")
              table1
            })
            
            output$assets1 <- renderDataTable({
              table1()
            })
            output$dateRange1 <- renderText({
              paste("Period :", fixedDateRange[1], "to", fixedDateRange[2])
            })
            output$shortSelling1 <- renderText({
              paste("Short Selling :", fixedShortChoice)
            })
            output$minExpRet1 <- renderText({
              paste("Expected Return :", round(MinVar[[2]] * 100, 2), "%")
            })
            #we display the complete portfolio's risk
            output$minRisk1 <- renderText({
              paste("Global Risk :", round(MinVar[[3]] * 100, 2), "%")
            })
            output$optExpRet1 <- renderText({
              paste("Expected Return :", round(Optimal[[2]] * 100, 2), "%")
            })
            #we display the complete portfolio's risk
            output$optRisk1 <- renderText({
              paste("Global Risk :", round(Optimal[[3]] * 100, 2), "%")
            })
          })
        }
      })
    }else if(input$inTab == 2){
      checkInput <- reactive({ 
        validate(need(!is.null(input$inputData), "Please Input your data")
        )
      })
      output$alertFile <- renderText(checkInput())
      if(!is.null(input$inputData)){
        data <- readWorksheet(loadWorkbook(input$inputData$datapath), sheet = 1, header = TRUE)
        corr <- readWorksheet(loadWorkbook(input$inputData$datapath), sheet = 2, header = TRUE)
        stock_names <- data[, 1]
        AllStock_expectReturn <- data[, 2]/100
        stock_variance <- data[, 3]/100
        cov_matrix <- matrix(nrow = length(AllStock_expectReturn) - 1)
        for(i in 2:length(stock_variance)){
          cov <- c()
          for (j in 2:length(stock_variance)){
            cov <- append(cov, sqrt(stock_variance[i]) * sqrt(stock_variance[j]))
          }
          col <- diag(cov) %*% corr[, i]
          cov_matrix <- cbind(cov_matrix, col)
        }
        cov_matrix <- cov_matrix[, 2:ncol(cov_matrix)]
        observeEvent(input$do2, {
          fixedShortChoice <- input$shortChoice2
          Rf <- AllStock_expectReturn[1]
          stock_expectReturn <- AllStock_expectReturn[2:length(AllStock_expectReturn)]
          Optimal <<- OptimalPortfolio(stock_expectReturn, cov_matrix, Rf, input$shortChoice2)
          #we calculate the Minimum Variance Portfolio
          MinVar <<- getMinVarPort(stock_expectReturn, cov_matrix, input$shortChoice2)
          #We calculate the efficient portfolià
          Effi <<- getEfficientPort(stock_expectReturn, cov_matrix, input$shortChoice2, MinVar[[2]])
          #we compute several portfolios on the efficient frontier according to the two fund separation theorem
          Frontier <<- getEffFrontier(MinVar[[1]], Effi[[1]], stock_expectReturn, cov_matrix, input$shortChoice2)
          #we calculate the Capital Allocation Line
          CAL <<- getCapitalAssetLine(Optimal[[2]], Rf, Optimal[[3]])
          observe({
            propor <- input$RiskFreeProp2 / 100
            comp_portfolio <- getCompletePortfolio(propor, Rf, Optimal[[2]], Optimal[[3]], Optimal[[1]], stock_expectReturn, cov_matrix)
            output$results2 <- renderPlot({
              #we draw the efficient frontier
              plot(Frontier[[2]], Frontier[[1]], type="l", xlim=c(min(0, min(Frontier[[2]])), max(Frontier[[2]])), ylim = c(min(0, min(Frontier[[1]])), max(Frontier[[1]])), main = "Efficient Frontier", xlab = "Global Risk", ylab = "Expected Return", lwd = 2, xaxs = "i", yaxs = "i")
              #we draw the optimal portfolio
              points(Optimal[[3]], Optimal[[2]], pch=21, bg="blue", cex=1.3)
              #we draw the Minimum Variance Portfolio
              points(MinVar[[3]], MinVar[[2]], pch=21, bg="red", cex=1.3)
              #we draw the risk free asset
              points(0, Rf, pch=21, bg="green", cex=1.3)
              #we draw capital allocation line
              lines(CAL[[1]], CAL[[2]], col="green")
              #we draw the complete portfolio
              points(comp_portfolio[[3]], comp_portfolio[[2]], pch=21, bg="black", cex=1.3)
              #we draw a legend for the graph
              legend(x = "bottomright", c("Optimal Portfolio of Risky Assets", "Minimum Variance Portfolio", "Risk Free Rate", "Complete Portfolio", "Capital Allocation Line"), pch = c(19, 19, 19, 19, NA), lty = c(0, 0, 0, 0, 1), col = c("blue", "red", "green", "black", "green"))
            })
            #we display the complete portfolio's expected return
            output$expRet2 <- renderText({
              paste("Expected Return :", round(comp_portfolio[[2]] * 100, 2), "%")
            })
            #we display the complete portfolio's risk
            output$Risk2 <- renderText({
              paste("Global Risk :", round(comp_portfolio[[3]] * 100, 2), "%")
            })
            #we a table summarizing all the assets present in the complete portfolio, their weight and individual expected return
            table2 <- reactive({
              table2 <- data.frame(Stocks = stock_names, Expt_Ret = c(round(AllStock_expectReturn * 100, 2)), Variance = c(0, round(diag(cov_matrix) * 100, 2)), Weights1 = as.character(c(round(propor * 100, 2), round(comp_portfolio[[1]] * 100, 2))), Weights2 = as.character(c(0, round(MinVar[[1]] * 100, 2))), Weights3 = as.character(c(0, round(Optimal[[1]] * 100, 2))))
              colnames(table2) <- c("Stock", "Expt_Ret(%)", "Var(%)", "Weight in the Complete Port.(%)", "Weight in the Min Var Port.(%)", "Weight in the Optimal Port.(%)")
              table2
            })
            output$assets2 <- renderDataTable({
              table2()
            })
            output$shortSelling2 <- renderText({
              paste("Short Selling :", fixedShortChoice)
            })
            output$minExpRet2 <- renderText({
              paste("Expected Return :", round(MinVar[[2]] * 100, 2), "%")
            })
            #we display the complete portfolio's risk
            output$minRisk2 <- renderText({
              paste("Global Risk :", round(MinVar[[3]] * 100, 2), "%")
            })
            output$optExpRet2 <- renderText({
              paste("Expected Return :", round(Optimal[[2]] * 100, 2), "%")
            })
            #we display the complete portfolio's risk
            output$optRisk2 <- renderText({
              paste("Global Risk :", round(Optimal[[3]] * 100, 2), "%")
            })
          })
        })
      }
    }
  })
})