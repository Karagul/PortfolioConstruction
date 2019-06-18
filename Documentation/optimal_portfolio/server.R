library(shiny)
library(Quandl)


shinyServer(function(input, output, clientData, session) {
  
  #list of stocks' codes
  stock_names <- c("AAC","AAD","ABC","ABP","AGI","AGL","AHG","AHY","AIO","ALL","ALQ","AMC","AMP","ANN","ANZ","AOG","APA","APN","ARB","ARI","AST","ASX","AWC","AWE","AZJ","BEN","BGA","BHP","BKN","BLD","BOQ","BPT","BRG","BSL","BWP","BXB","CAB","CAR","CBA","CCL","CCP","CDD","CGF","CHC","CIM","CMW","COH","CPU"
)
  #list of stocks'code used to retrieve the data from Quandl
  stock_datacode <- c("YAHOO/ASX_AAC_AX","YAHOO/ASX_AAD_AX","YAHOO/ASX_ABC_AX","YAHOO/ASX_ABP_AX","YAHOO/ASX_AGI_AX","YAHOO/ASX_AGL_AX","YAHOO/ASX_AHG_AX","YAHOO/ASX_AHY_AX","YAHOO/ASX_AIO_AX","YAHOO/ASX_ALL_AX","YAHOO/ASX_ALQ_AX","YAHOO/ASX_AMC_AX","YAHOO/ASX_AMP_AX","YAHOO/ASX_ANN_AX","YAHOO/ASX_ANZ_AX","YAHOO/ASX_AOG_AX","YAHOO/ASX_APA_AX","YAHOO/ASX_APN_AX","YAHOO/ASX_ARB_AX","YAHOO/ASX_ARI_AX","YAHOO/ASX_AST_AX","YAHOO/ASX_ASX_AX","YAHOO/ASX_AWC_AX","YAHOO/ASX_AWE_AX","YAHOO/ASX_AZJ_AX","YAHOO/ASX_BEN_AX","YAHOO/ASX_BGA_AX","YAHOO/ASX_BHP_AX","YAHOO/ASX_BKN_AX","YAHOO/ASX_BLD_AX","YAHOO/ASX_BOQ_AX","YAHOO/ASX_BPT_AX","YAHOO/ASX_BRG_AX","YAHOO/ASX_BSL_AX","YAHOO/ASX_BWP_AX","YAHOO/ASX_BXB_AX","YAHOO/ASX_CAB_AX","YAHOO/ASX_CAR_AX","YAHOO/ASX_CBA_AX","YAHOO/ASX_CCL_AX","YAHOO/ASX_CCP_AX","YAHOO/ASX_CDD_AX","YAHOO/ASX_CGF_AX","YAHOO/ASX_CHC_AX","YAHOO/ASX_CIM_AX","YAHOO/ASX_CMW_AX","YAHOO/ASX_COH_AX","YAHOO/ASX_CPU_AX"
)
  #random Expected Returns generation
  stock_expectReturn <- c(0.08+0.25*rnorm(length(stock_names),0,0.4))
  
  inputs_values <- c()
  
  #in this section we observe all the inputs happening on the left panel of the page and display the error messages accordingly
  observe({
    
    #this enables to display a number of numericInput equals to the number of stocks selected in the case where the user inputs the expected returns by himself
    output$InputExpRet <- renderUI({
      inputs_list <- list()
      for (i in 1:length(input$Stocks)){
        inputs_list[[i]] <- numericInput(paste0("asset",i),paste(input$Stocks[i],"'s Expected Return(%) :"),NULL)
      }
      inputs_list
    })
    
    
    #we check that at least two stocks are selected
    check1 <- reactive({ 
      validate(
        need(length(input$Stocks)>=2, "Please select at least two Stocks")
      )
    })
    output$alert1 <- renderText(check1())
    
    #we check that the Risk free rate is indicated
    check2 <- reactive({ 
      validate(
        need(!is.na(input$Risk_Free), "Please indicate the risk free rate")
      )
    })
    output$alert2 <- renderText(check2())
    
    #we check that all the expected returns are inputted
    check3 <- reactive({ 
      if(input$choice == 3){
        for (i in 1:length(input$Stocks)){
          inputs_values[i] <<- as.numeric(input[[paste0("asset",i)]])
        }
        validate(
          need(is.na(match(NA,inputs_values[1:length(input$Stocks)])), "Please Indicate all the Expected Returns")
        )
      }
    })
    output$alert3 <- renderText(check3())
    
  })
  
  
  #in this part we define which action to perform once the launch button is pressed
  observeEvent(input$do,{
    
    #if less than two stocks are selected then the program doesn't do anything, if the risk free rate isn't indicated then the program doesn't do anything, if the "Inputs" option is selected and not all expected returns are indicated then the program doesn't do anything
    if(length(input$Stocks)<2){
    
    }else if(is.na(input$Risk_Free)){
      
    }else if(input$choice==3 && !is.na(match(NA,inputs_values[1:length(input$Stocks)]))){
      
    }else{
      # if the form is completely filled the pregram then proceeds with the following
      index <- match(input$Stocks,stock_names)
      data <- stock_datacode[index]
      Rf <- input$Risk_Free/100
      
      names <-stock_names[index]
      
      #we retrieve the data for each selected stock from Quandl
      mydata <<- getData(data)
      
      #we calculate the arithmetic returns for each stocks
      myReturns <<- getMonthlyReturns(mydata)
      
      #we calculate the covariance matrix of all the selected stocks
      cov_matrix <<- getCovMatr(myReturns)
      
      #we define the expected returns to use in the computation according to the user's choice
      if(input$choice==1){
        Expt_Ret <- stock_expectReturn[index]
      }else if(input$choice==2){
        Expt_Ret <- getHistoricalReturn(getYearlyReturns(mydata))
      }else if(input$choice==3){
        Expt_Ret <- inputs_values[1:length(input$Stocks)]/100
      }
      
      #we calculate the optimal portfolio for the the stock selected
      Optimal <<- OptimalPortfolio(Expt_Ret,cov_matrix,Rf)
      
      #we calculate the Minimum Variance Portfolio
      MinVar <<- getMinVarPort(Expt_Ret,cov_matrix)
      
      #we calculate the efficient portfolio
      Effi <<- getEfficientPort(Expt_Ret,cov_matrix)
      
      #we compute several portfolios on the efficient frontier according to the two fund separation theorem
      Frontier <<- getEffFrontier(MinVar[[1]],Effi[[1]],Expt_Ret,cov_matrix)
      
      #we calculate the Capital Allocation Line
      CAL <<- getCapitalAssetLine(Optimal[[2]],Rf,Optimal[[3]])
      
      #here we observe risk free asset proportion's input and update the graph accordingly
      observe({
        propor <- input$RiskFreeProp/100
        comp_portfolio <- getCompletePortfolio(propor,Rf,Optimal[[2]],Optimal[[3]],Optimal[[1]], Expt_Ret, cov_matrix)
        output$results <- renderPlot({
          
          #we draw the efficient frontier
          plot(Frontier[[2]],Frontier[[1]],type="l",xlim=c(min(0,min(Frontier[[2]])),max(Frontier[[2]])),ylim = c(min(0,min(Frontier[[1]])),max(Frontier[[1]])),main="Efficient Frontier",xlab="Global Risk",ylab="Expected Return",lwd=2,xaxs="i",yaxs="i")
          
          #we draw the optimal portfolio
          points(Optimal[[3]],Optimal[[2]],pch=21,bg="blue",cex=1.3)
          
          #we draw the Minimum Variance Portfolio
          points(MinVar[[3]],MinVar[[2]],pch=21,bg="red",cex=1.3)
          
          #we draw the risk free asset
          points(0,Rf,pch=21,bg="green",cex=1.3)
          
          #we draw capital allocation line
          lines(CAL[[1]],CAL[[2]],col="green")
          
          #we draw the complete portfolio
          points(comp_portfolio[[3]],comp_portfolio[[2]],pch=21,bg="black",cex=1.3)
          
          #we draw a legend for the graph
          legend(x="bottomright",c("Optimal Portfolio of Risky Assets","Minimum Variance Portfolio","Risk Free Rate","Complete Portfolio","Capital Allocation Line"),pch=c(19,19,19,19,NA),lty=c(0,0,0,0,1),col=c("blue","red","green","black","green"))
          
        })
        
        #we display the complete portfolio's expected return
        output$expRet <- renderText({
          paste("Expected Return :",round(comp_portfolio[[2]]*100,2),"%")
        })
        
        #we display the complete portfolio's risk
        output$Risk <- renderText({
          paste("Global Risk :",round(comp_portfolio[[3]]*100,2),"%")
        })
        
        #we a table summarizing all the assets present in the complete portfolio, their weight and individual expected return
        stocks_col <- c("T-Bill",names)
        weights_col <- c(round(propor*100,2),round(comp_portfolio[[1]]*100,2))
        Exp_Ret_col <- c(Rf*100,round(Expt_Ret*100,2))
        
        table <- reactive({data.frame(Stocks=c("T-Bill",names), Weights =as.character(c(round(propor*100,2),round(comp_portfolio[[1]]*100,2))), Expt_Ret = c(Rf*100,round(Expt_Ret*100,2)))})
        
        
        
        output$assets <- renderTable({
          table()
        })
      })
    }
    
    
   
  })
  
  
  
  
  
})