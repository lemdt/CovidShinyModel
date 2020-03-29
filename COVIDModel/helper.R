##  ............................................................................
##  Common Helper Functions  
##  ............................................................................

library(ggplot2)
library(shinyWidgets)
library(data.table)

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
                            'New.Double.Time' = params$int.new.double, 
                            'Days of Smoothing' = params$int.smooth.days
                             ))
  }
  else{
    new.table <- rbind(int.table,
                       list('Day' = params$int.new.num.days, 
                            'New.Re' = params$int.new.r0,
                            'Days of Smoothing' = params$int.smooth.days
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
