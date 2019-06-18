
OptimalPortfolio <- function(Expt_Ret_Matr,covar_matrix,Rf){
  
#We use matrix calculus to solve the optimization pb
  
Expt_Ret <- Expt_Ret_Matr
cov_matr <- covar_matrix

Risk_Free <- Rf

inv_cov_matr <- solve(cov_matr)

one_vec <- rep(1,nrow(cov_matr))

top_mat <- inv_cov_matr%*%(Expt_Ret-(Risk_Free*one_vec))
bot_val <- as.numeric(t(one_vec)%*%top_mat)

opt_weights <- top_mat[,1]/bot_val

opt_Ret <- as.numeric(crossprod(opt_weights,Expt_Ret))
opt_Risk <- sqrt(as.numeric(t(opt_weights)%*%cov_matr%*%opt_weights))

Results <- list(opt_weights,opt_Ret,opt_Risk)

return(Results)

}

getData <- function(...){
  arguments <- list(...)
  stocks_data <- list()
  #we retrieve the data from Quandl
  #we retrieve the monthly data for the last 5 years
  for(i in 1:length(arguments[[1]])){
    Quandl.api_key("yJM_dSwza7UMCMYznzWt")
    stocks_data[[i]] <- Quandl(arguments[[1]][i],collapse = "monthly", start_date = "2010-01-01")
    
  }
  return(stocks_data)
}

getMonthlyReturns <- function(list){
  returns <- list()
  for(i in 1:length(list)){
    stock_returns <- list()
    #we calculate the monthly arithmetic return of the stock, index 1 = more redent month
    for(j in 2:nrow(list[[i]])){
      stock_returns[[j-1]]<- (list[[i]][j-1,5]-list[[i]][j,5])/(list[[i]][j,5])
    }
    returns[[i]] <- stock_returns
  }
  return(returns)
}

getYearlyReturns <- function(list){
  returns <- list()
  for(i in 1:length(list)){
    stock_returns <- list()
    #we calculate the yearly arithmetic return of the stock, index 1 = more redent month
    for(j in seq(1,49,12)){
      stock_returns <- append(stock_returns,(list[[i]][j,5]-list[[i]][j+12,5])/(list[[i]][j+12,5]))
    }
    returns[[i]] <- stock_returns
  }
  return(returns)
}

getCovMatr <- function(list){
  sizes <- c()
  covar <- c()
  for (i in 1:length(list)){
    sizes[i] <- length(list[[i]])
  }
  for(j in 1:length(list)){
    for (l in 1:length(list)){
      stock_vector <- c()
      #we calculate the monthly value of the covariance, then we sum all these values and create the covariance matrix
      for(k in 1:min(sizes)){
        stock_vector[[k]]<- (list[[j]][[k]]-aver(list[[j]]))*(list[[l]][[k]]-aver(list[[l]]))/(min(sizes)-1)
      }
      covar <- append(covar,Reduce("+",stock_vector))
    }
    
    
  }
  
  covar_mat <- matrix(covar, nrow = length(list), ncol=length(list))
    
  return(covar_mat)
}
  
aver <- function(list){
  #we calculate the average of the elements of list
  av <- Reduce("+",list)/length(list)
  return(av)
}
  
getMinVarPort <- function(Expt_Ret, covar_matrix){
  
  #we use matrix calculus to solve the minimization problem with the constraint that the sum of all assets' weight is equal to 1 (or 100%)
  top_matrix <- cbind(2*covar_matrix,rep(1,nrow(covar_matrix)))
  vect <- c(rep(1,nrow(covar_matrix)),0)
  mat <- rbind(top_matrix,vect)
  res_vector <- c(rep(0,nrow(covar_matrix)),1)
  final_mat <- solve(mat)%*%res_vector
  Min_weights <- final_mat[1:nrow(covar_matrix),1]
  Min_Exp <- as.numeric(crossprod(Min_weights,Expt_Ret))
  Min_Risk <- sqrt(as.numeric(t(Min_weights)%*%covar_matrix%*%Min_weights))
  
  Results <- list(Min_weights,Min_Exp,Min_Risk)
  return(Results)

}

getEfficientPort <- function(Exp_Ret,cov_matrix){
  #we use matrix calculus to solve the problem
  top_matrix <- cbind(2*cov_matrix,Exp_Ret,rep(1,nrow(cov_matrix)))
  mid_matrix <- c(Exp_Ret,0,0)
  bot_matrix <- c(rep(1,nrow(cov_matrix)),0,0)
  final_matrix <- rbind(top_matrix,mid_matrix,bot_matrix)
  vector <- c(rep(0,nrow(cov_matrix)),max(Exp_Ret),1)
  prod <- solve(final_matrix)%*%vector
  
  Eff_Weights <- prod[1:nrow(cov_matrix),]
  Eff_Expt <- as.numeric(crossprod(Eff_Weights,Exp_Ret))
  Eff_Risk <- sqrt(as.numeric(t(Eff_Weights)%*%cov_matrix%*%Eff_Weights))
  
  Result <- list(Eff_Weights,Eff_Expt,Eff_Risk)
  
  return(Result)
}

getEffFrontier <- function(Min_Weights,Effi_weights,Exp_Ret,cov_matrix){
  #according to the two fund separation theorem every protfolio on the efficient frontier is a combination of two portfolios from the efficient frontier
  a <- seq(-1,1,0.01)
  Exp <- c()
  Risk <- c()
  #we compute a list of efficient portfolios composed with the minimum portfolio and the efficient portfolio
  for(i in a){
    z <- i*Min_Weights+(1-i)*Effi_weights
    E <- as.numeric(crossprod(z,Exp_Ret))
    Exp <- append(Exp,E)
    R <- sqrt(as.numeric(t(z)%*%cov_matrix%*%z))
    Risk <- append(Risk,R)
  }
  
  
  results <-list(Exp,Risk)
  return(results)
}

getCapitalAssetLine<- function(opt_Ret,Rf,opt_Risk){
  #we calculate the CAL's equation using two points : the optimal portfolio and the risk free rate
  a <- (opt_Ret-Rf)/(opt_Risk)
  b <- Rf
  x <- seq(0,1,0.01)
  y <- a*x+b
  
  results <- list(x,y)
  return(results)
}

getCompletePortfolio <- function(propor,Rf,opt_Exp,opt_Risk, opt_weights, Expt_Ret,cov_matix){
  #we calculate the weight of each asset according to the proportion of riskless asset in the portfolio
  comp_weights <- (1-propor)*opt_weights
  Exp_Ret <- propor*Rf+(1-propor)*opt_Exp
  Risk <- (1-propor)*opt_Risk
  
  results <- list(comp_weights,Exp_Ret,Risk)
  return(results)
}

getHistoricalReturn <- function(list){
  results <- c()
  for(i in 1:length(list)){
    results[i]<- mean(as.numeric(list[[i]]))
  }
  return(results)
}

