##  ............................................................................
##  Common Helper Functions  
##  ............................................................................

library(ggplot2)
library(shinyWidgets)
library(data.table)

##  ............................................................................
##  General  
##  ............................................................................


#' Get Beta from Doubling Time
#' 
#' Returns beta from doubling time using formulas here: 
#' https://penn-chime.phl.io/
#'
#' @param doubling.time Numeric. 
#' @param gamma Numeric.
#'
#' @return Numeric. 
getBetaFromDoubling <- function(doubling.time, gamma) {
  
  g <- 2^(1/doubling.time) - 1
  beta <- g + gamma
  return(beta)
  
}


#' Get Beta from Re
#'
#' @param Re Numeric.
#' @param gamma Numeric.
#'
#' @return Numeric. 
getBetaFromRe <- function(Re, gamma) {
  
  beta <- Re * gamma
  return(beta)
  
}


#' Round Non-Date Columns
#'
#' Helper function in the app that rounds any function that does 
#' not have 'date' as the column name. 
#'
#' @param df Dataframe.
#'
#' @return Dataframe. 
roundNonDateCols <- function(df){
  
  df.new <- data.frame(df)
  
  for (col in colnames(df.new)){
    if (col != 'date'){
      df.new[,col] <- round(df.new[,col])
    }
  }
  return(df.new)
  
}

#' Column Bind
#' 
#' Same as rbind.fill in plyr, but with columns. Stolen from here:
#' https://stackoverflow.com/questions/7962267/cbind-a-dataframe-with-an-empty-dataframe-cbind-fill
#'
#' @param ... 
#'
#' @return Dataframe.
cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}



##  ............................................................................
##  Model Helpers  
##  ............................................................................


#' Find 'Current' Information
#' 
#' This function first runs the simulation on Day 1, with one person exposed. 
#' 
#' Based on the metric provided, it finds the day on the curve that corresponds to the 
#' metric. For example, if the user specifies the number of actual hospitalizations is 
#' 100 (num.actual = 100, metric = 'Hospitalizations'), then it finds the day on the
#' curve that corresponds to 100 hospitalizations. 
#' 
#' It then returns a list with that day as well as the number susceptible, exposed, 
#' infected, and recovered on that day. 
#' 
#' @param N Numeric, number of people in the area. 
#' @param beta.vector Vector of numerics, should be the same length as num.days. 
#' @param num.days Numeric. Number of days to simulate.
#' @param num.actual Numeric. Number to match on the curve. 
#' @param metric tring. Metric to use for matching (currently only compatible with 'Cases' 
#' and 'Hospitalizations').
#' @param start.exp Starting number of exposures to simulate. 
#' @param params Parameters to input into the SEIR function. 
#'
#' @return List with the day number match as well as counts for susceptible, exposed, 
#' infected and recovered. 
find.curr.estimates = function(N, beta.vector, num.days, num.actual, 
                               metric, start.exp, params){
  
  # starting number of susceptible people
  start.susc <- N - start.exp
  start.res <- 0 
  start.inf <- 0
  
  SEIR.df = model$SEIR(S0 = start.susc, 
                 E0 = start.exp, 
                 I0 = start.inf, 
                 R0 = start.res, 
                 beta.vector = beta.vector, 
                 num.days = num.days, 
                 influx = list('day' = -1, num.influx = 0), 
                 params = params)
  
  # if hospitalization is the input
  if (metric == 'Hospitalizations'){
    
    # find the difference between hospitalized column and the currently hospitalized number
    SEIR.df$diff_proj <- abs(SEIR.df$hosp - num.actual)
    
    # the # hospitalized will be achieved twice according to model 
    # first as the hospitalizations go up, and second as the hospitalizations go down 
    hosp.numbers <- SEIR.df$hosp
    hosp.change <- hosp.numbers[2:length(hosp.numbers)] - hosp.numbers[1:length(hosp.numbers) - 1]
    hosp.change <- c(0, hosp.change)
    SEIR.df$hosp.change <- hosp.change
    
    curr.day.df <- SEIR.df[which(SEIR.df$hosp.change > 0),]
    curr.day.df <- curr.day.df[curr.day.df$diff_proj == min(curr.day.df$diff_proj, na.rm = TRUE),]
    
    curr.day <- as.integer(curr.day.df$day)
    exposed.estimate <- as.integer(curr.day.df$E)
    susceptible.estimate <- as.integer(curr.day.df$S)
    exposed.estimate <- as.integer(curr.day.df$E)
    infection.estimate <- as.integer(curr.day.df$I)
    recovered.estimate <- as.integer(curr.day.df$R)
    
  }
  
  # if cases are the input
  else{
    curr.day <- 0
    infection.estimate <- num.actual 
    susceptible.estimate <- start.susc - num.actual
    recovered.estimate <- 0
    exposed.estimate <- 0
  }
  
  curr.day.list <- list(
    curr.day = curr.day,
    exposed.estimate = exposed.estimate,
    infection.estimate = infection.estimate, 
    susceptible.estimate = susceptible.estimate, 
    recovered.estimate = recovered.estimate
  )
  
  return(curr.day.list)
}


#' Find Best Fit for Re
#' 
#' This takes in historical hospitalization data (day.vec and num_actual.vec). Given a fixed
#' set of parameters, it finds the Re that gives the least square error with the data.  
#' 
#' The main inputs are day.vec and num_actual vec. Day.vec consists of indices 
#' of the historical days relative to the 'set date.' For example, if the date is set as 
#' April 1, 2020, and the user provides data from March 28, 29, and 31, then the 
#' day.vec = c(-4, -3, -1)
#' 
#' Num_actual.vec is a vector of the values of historical hospitalizations from those dates. 
#' 
#' The function returns a list with 'best.re' (The Re with the best fit) and 'best.vals' 
#' (a vector of numerics with projected values of hospitalizations on the historical dates
#' for which data was provided).
#'
#' @param N Numeric. Number of people in the area. 
#' @param start.exp Numeric. Starting number of exposures. 
#' @param num.days Numeric. Number of days to simulate. 
#' @param day.vec Vector of numerics. 
#' @param num_actual.vec Vector of numerics. 
#' @param params List of paramters for SEIR simulation. 
#'
#' @return List with best Re and the projected number of hospitalizations on the historical 
#' dates for which data was provided. 
findBestRe <- function(N, start.exp, num.days, day.vec, num_actual.vec, params){
  
  # starting number of susceptible people
  start.susc <- N - start.exp
  start.inf <- 0 
  start.res <- 0 
  
  # TODO: implement binary search maybe
  min.sqrd.sum <- Inf
  re_choice <- NA
  vec.choice <- c()
  
  for (re in c(seq(1, 7, 0.1))){
    beta <- getBetaFromRe(re, params$gamma)
    
    SIR.df = SEIR(S0 = start.susc, 
                  E0 = start.exp, 
                  I0 = start.inf, 
                  R0 = start.res, 
                  beta.vector = rep(beta, num.days),
                  num.days = num.days, 
                  influx = list('day' = -1, num.influx = 0), 
                  params = params)
    
    SIR.df$diff_proj <- abs(SIR.df$hosp - num_actual.vec[1])
    hosp.numbers <- SIR.df$hosp
    hosp.change <- hosp.numbers[2:length(hosp.numbers)] - 
      hosp.numbers[1:length(hosp.numbers) - 1]
    
    hosp.change <- c(0, hosp.change)
    SIR.df$hosp.change <- hosp.change
    
    curr.day.df <- SIR.df[SIR.df$hosp.change > 0,]
    curr.day <- curr.day.df[curr.day.df$diff_proj == 
                              min(curr.day.df$diff_proj, na.rm = TRUE),]$day - day.vec[1]
    
    compare.idx <- curr.day + day.vec
    
    compare.vec <- rev(SIR.df[SIR.df$day %in% compare.idx,]$hosp)
    
    sqrd.sum <- sum((num_actual.vec - compare.vec) ** 2)
    
    if (sqrd.sum < min.sqrd.sum){
      re_choice <- re
      min.sqrd.sum <- sqrd.sum
      vec.choice <- compare.vec
    }
  }
  
  list.return <- list(
    'best.re' = re_choice, 
    'best.vals' = vec.choice
  )
  
  return(list.return)
}


#' Creates Vector of Beta Values 
#' 
#' This takes as input an intervention dataframe (int.table). The dataframe should have as 
#' columns 'Day' number, new intervention metric (either New.Re or New.Double.Time) 
#' and the number of days that the intervention is smoothed over (Days.of.Smoothing). 
#' 
#' The function also takes as input a gamma value a flag for whether doubling time or is 
#' used as the metric (usedouble). 
#' 
#' It returns a vector of beta values that correspond with the intervention dataframe. 
#'
#' @param int.table Dataframe with columns: Day, New.Re (or New.Double.Time), and 
#' Days.of.Smoothing
#' @param gamma Numeric. 
#' @param usedouble Boolean. TRUE if doubling time is used as the metric. 
#'
#' @return Vector of beta values to use in the SEIR simulation. 
create.beta.vec <- function(int.table, gamma, usedouble){
  
  # inner helper function
  applygetBeta <- function(x){
    if (usedouble == FALSE){
      return(getBetaFromRe(as.numeric(x['New.Re']), gamma))
    }
    else{
      return(getBetaFromDoubling(as.numeric(x['New.Double.Time']), gamma))
    }
  }
  
  # processing intervention table
  int.table.temp <- int.table
  int.table.temp$beta <- apply(int.table.temp, 1, applygetBeta)
  int.table.temp <- arrange(int.table.temp, Day)
  int.table.temp <- int.table.temp[!duplicated(int.table.temp$Day),]
  
  # rep.vec consists of the number of days the beta value will repeat
  day.vec <- int.table.temp$Day
  rep.vec <- day.vec[2:length(day.vec)] - day.vec[1:length(day.vec) - 1]
  betas <- int.table.temp$beta[1:length(day.vec) - 1]
  smooth.vec <- int.table.temp$Days.of.Smoothing
  
  beta.vec <- c()
  
  for (i in 1:length(rep.vec)){
    beta <- betas[i]
    reps <- rep.vec[i]
    smooth.days <- smooth.vec[i]
    actual.smooth.days <- min(reps, smooth.days)
    
    # dealing with smoothing 
    if (smooth.days > 0){
      beta.last <- betas[i-1]
      beta.diff <- beta - beta.last
      beta.step <- beta.diff / smooth.days
      
      if (beta.step != 0){
        smooth.seq <- seq(beta.last+beta.step, beta, beta.step)
        smooth.seq <- smooth.seq[1:actual.smooth.days]
        
        # deals with case where smoothing doesn't finish in one 
        # intervention before another is applied
        if (smooth.days > reps){
          betas[i] <- smooth.seq[actual.smooth.days]
        }
        
      }
      
      # if the beta value doesn't change 
      else{
        smooth.seq <- rep(beta, actual.smooth.days)
      }
      
      beta.vec <- c(beta.vec, smooth.seq)
    }
    
    beta.vec <- c(beta.vec, rep(beta, reps - actual.smooth.days))
  }
  
  return(beta.vec)
}

##  ............................................................................
##  App Helpers  
##  ............................................................................


#' Add Rows to the Fit Re Table 
#' 
#' Adds dates and hospitalizations to the table with historical data. 
#'
#' @param hist.data Dataframe with Date, Hospitalizations, and Day columns. 
#' @param date.hist Date to add to table. 
#' @param num.hospitalized.hist Hospitalization count to add to table.  
#' @param curr.date The 'current' date set in the app. 
#'
#' @return Dataframe with appended row. 
add.to.hist.table <- function(hist.data, date.hist, num.hospitalized.hist, curr.date){
  new.hist <- rbind(hist.data,
                    list('Date' = as.character(date.hist), 
                         'Hospitalizations' = num.hospitalized.hist, 
                         'Day' = date.hist - curr.date
                    ))
  
  new.hist <- arrange(new.hist, Day)
  new.hist$Date <- as.Date(as.character(new.hist$Date))
  
  return(new.hist)
}


#' Add Rows to Intervention Table 
#' 
#' Bind rows to the intervention table. 
#'
#' @param int.table Dataframe with Day, New.Re (or New.Double.Time), and Days.of.Smoothing 
#' columns.
#' @param params ReactiveValues list of parameters.
#' @param usedouble Boolean. TRUE if doubling time is used in the app. 
#'
#' @return Dataframe with appended row. 
bind.to.intervention <- function(int.table, params, usedouble){
  if (usedouble == TRUE){
    new.table <- rbind(int.table,
                       list('Day' = params$int.new.num.days , 
                            'New.Double.Time'= params$int.new.double, 
                            'Days.of.Smoothing' = params$int.smooth.days
                       ))
  }
  else{
    new.table <- rbind(int.table,
                       list('Day' = params$int.new.num.days, 
                            'New.Re' = params$int.new.r0,
                            'Days.of.Smoothing' = params$int.smooth.days
                       ))
  }
  
  new.table <- arrange(new.table, Day)
  return(new.table)
}


#' Create Cases Dataframe
#' 
#' Returns a processed dataframe of cases for presentation in the app. 
#'
#' @param df Dataframe from the SEIR run. 
#' 
#' @return Dataframe.
create.cases.df <- function(df){
  df_temp <- df
  
  df_temp <- df_temp[df_temp$days.shift >= 0,]
  
  df_temp$Cases <- df_temp$I + df_temp$R + df_temp$E
  df_temp$Active <- df_temp$I + df_temp$E
  df_temp$Resolved <- df_temp$R
  
  df_temp <- df_temp[,c('date', 'days.shift', 'Cases', 'Active', 'Resolved')]
  colnames(df_temp) <- c('date', 'day', 'Cases', 'Active', 'Resolved')
  df_temp <- roundNonDateCols(df_temp)
  
  return(df_temp)
}

#' Create Hospitalization Dataframe
#' 
#' Returns a processed dataframe of hospitalizations for presentation in the app. 
#'
#' @param df Dataframe from the SEIR run. 
#' 
#' @return Dataframe.
create.hosp.df <- function(df){
  df_temp <- df
  
  df_temp <- df_temp[df_temp$days.shift >= 0,]
  
  df_temp <- df_temp[,c('date', 'days.shift', 'hosp', 'icu', 'vent')]
  colnames(df_temp) <- c('date', 'day', 'Hospital', 'ICU', 'Ventilator')
  df_temp <- roundNonDateCols(df_temp)
  
  return(df_temp)
}


#' Create Hospital Resource Dataframe
#' 
#' Returns a processed dataframe of hospital resources for presentation in the app.  
#'
#' @param df Dataframe from the SEIR run. 
#' @param hosp_cap Numeric, hospital capacity. 
#' @param icu_cap Numeric, ICU capacity.
#' @param vent_cap Numeric, ventilator capacity.
#'
#' @return Dataframe.
create.res.df <- function(df, hosp_cap, icu_cap, vent_cap){
  df_temp <- df
  df_temp <- df_temp[df_temp$days.shift >= 0,]
  
  if (!is.null(hosp_cap)){
    df_temp$hosp <- hosp_cap - df_temp$hosp
    df_temp$icu <- icu_cap - df_temp$icu
    df_temp$vent <- vent_cap - df_temp$vent
  }
  
  df_temp <- df_temp[,c('date', 'days.shift', 'hosp', 'icu', 'vent')]
  colnames(df_temp) <- c('date', 'day', 'Hospital', 'ICU', 'Ventilator')
  df_temp <- roundNonDateCols(df_temp)
  
}


#' Graphing Helper
#' 
#' Graphing helper for the app. Returns a ggplot graph. 
#'
#' @param df.to.plot Dataframe with data to plot. 
#' @param selected Vector of strings, representing the columns to plot. 
#' @param plot.day Date selected to show a vertical line. 
#' @param curr.date Date where to start the graph. 
#'
#' @return ggplot graph. 
create.graph <- function(df.to.plot, selected, plot.day, curr.date){
  
  if (length(selected) != 0){
    cols <- c('date', selected)
    
    df.to.plot <- df.to.plot[,cols]
    
    df_melt <- melt(df.to.plot, 'date')
    
    graph <- ggplot(df_melt, aes(x = date, y = value, col = variable)) + geom_point() + 
      geom_line() +  geom_vline(xintercept=curr.date) + theme(text = element_text(size=20)) + 
      geom_vline(xintercept=plot.day, color = 'red') + ylab('') + geom_hline(yintercept = 0)
    
    return(graph)
  }
}
