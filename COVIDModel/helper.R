##  ............................................................................
##  Common Helper Functions  
##  ............................................................................

library(ggplot2)
library(shinyWidgets)
library(data.table)

##  ............................................................................
##  General  
##  ............................................................................


# gets beta based on doubling time, gamma, and N
getBetaFromDoubling <- function(doubling.time, gamma) {
  g <- 2^(1/doubling.time) - 1
  beta <- g + gamma
  return(beta)
}

getBetaFromRe <- function(Re, gamma) {
  beta <- Re * gamma
  return(beta)
}

# rounds non-date columns
roundNonDateCols <- function(df){
  
  df.new <- data.frame(df)
  
  for (col in colnames(df.new)){
    if (col != 'date'){
      df.new[,col] <- round(df.new[,col])
    }
  }
  return(df.new)
}


##  ............................................................................
##  Model Helpers  
##  ............................................................................

# finds current estimates of the number of active infections, 
# number recovered, and number susceptible based on the current # of 
# hospitalizations
find.curr.estimates = function(S0, beta.vector, num.days, num.actual, 
                               metric, start.exp, params){
  
  # starting number of susceptible people
  start.susc <- S0 - start.exp
  start.res <- 0 
  start.inf <- 0
  
  SIR.df = SEIR(S0 = start.susc, 
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
    SIR.df$diff_proj <- abs(SIR.df$hosp - num.actual)
    
    # the # hospitalized will be achieved twice according to model 
    # first as the hospitalizations go up, and second as the hospitalizations go down 
    hosp.numbers <- SIR.df$hosp
    hosp.change <- hosp.numbers[2:length(hosp.numbers)] - hosp.numbers[1:length(hosp.numbers) - 1]
    hosp.change <- c(0, hosp.change)
    SIR.df$hosp.change <- hosp.change
    
    curr.day.df <- SIR.df[which(SIR.df$hosp.change > 0),]
    curr.day.df <- curr.day.df[curr.day.df$diff_proj == min(curr.day.df$diff_proj, na.rm = TRUE),]
    
    curr.day <- as.integer(curr.day.df$day)
    infection.estimate <- as.integer(curr.day.df$I)
    susceptible.estimate <- as.integer(curr.day.df$S)
    recovered.estimate <- as.integer(curr.day.df$R)
    
  }
  
  # if cases are the input
  else{
    curr.day <- 0
    infection.estimate <- num.actual 
    susceptible.estimate <- start.susc - num.actual
    recovered.estimate <- 0
  }
  
  curr.day.list <- list(
    curr.day = curr.day,
    infection.estimate = infection.estimate, 
    susceptible.estimate = susceptible.estimate, 
    recovered.estimate = recovered.estimate
  )
  
  return(curr.day.list)
}


# function to get the best value of Re given historical data 
findBestRe <- function(S0, start.exp, num.days, day.vec, num_actual.vec, params){
  
  # starting number of susceptible people
  start.susc <- S0 - start.exp
  start.inf <- 0 
  start.res <- 0 
  
  # TODO: implement binary search
  
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


# convert intervention table to beta vector
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
  
  # processing df
  int.table.temp <- int.table
  int.table.temp$beta <- apply(int.table.temp, 1, applygetBeta)
  int.table.temp <- arrange(int.table.temp, Day)
  int.table.temp <- int.table.temp[!duplicated(int.table.temp$Day),]

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
    
    if (smooth.days > 0){
      beta.last <- betas[i-1]
      beta.diff <- beta - beta.last
      beta.step <- beta.diff / smooth.days
      
      if (beta.step != 0){
        smooth.seq <- seq(beta.last+beta.step, beta, beta.step)
        smooth.seq <- smooth.seq[1:actual.smooth.days]
        
        if (smooth.days > reps){
          betas[i] <- smooth.seq[actual.smooth.days]
        }
        
      }
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


# add to historical table
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

# bind to intervention table 
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

# create cases dataframe 
create.cases.df <- function(sir.output.df){
  df_temp <- sir.output.df
  
  df_temp <- df_temp[df_temp$days.shift >= 0,]
  
  df_temp$Cases <- df_temp$I + df_temp$R + df_temp$E
  df_temp$Active <- df_temp$I + df_temp$E
  df_temp$Resolved <- df_temp$R
  
  df_temp <- df_temp[,c('date', 'days.shift', 'Cases', 'Active', 'Resolved')]
  colnames(df_temp) <- c('date', 'day', 'Cases', 'Active', 'Resolved')
  df_temp <- roundNonDateCols(df_temp)
  
  return(df_temp)
}

# create hospital dataframe 
create.hosp.df <- function(sir.output.df){
  df_temp <- sir.output.df

  df_temp <- df_temp[df_temp$days.shift >= 0,]
  
  df_temp <- df_temp[,c('date', 'days.shift', 'hosp', 'icu', 'vent')]
  colnames(df_temp) <- c('date', 'day', 'Hospital', 'ICU', 'Ventilator')
  df_temp <- roundNonDateCols(df_temp)
  
  return(df_temp)
}

# create resources dataframe
create.res.df <- function(sir.output.df, hosp_cap, icu_cap, vent_cap){
  df_temp <- sir.output.df
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

# format and create graphs 
create.graph <- function(df.to.plot, selected, plot.day, curr.date){

  if (length(selected) != 0){
    cols <- c('date', selected)
    
    df.to.plot <- df.to.plot[,cols]
    
    df_melt <- melt(df.to.plot, 'date')
  
    graph = ggplot(df_melt, aes(x = date, y = value, col = variable)) + geom_point() + 
      geom_line() +  geom_vline(xintercept=curr.date) + theme(text = element_text(size=20)) + 
      geom_vline(xintercept=plot.day, color = 'red') + ylab('') + geom_hline(yintercept = 0)
    
    return(graph)
  }
}
