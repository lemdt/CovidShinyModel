library(ggplot2)
library(shinyWidgets)
library(data.table)

# TODO: figure out how to add function documentation

# runs SIR model simulation, and then calculates hospitalizations, 
# icu numbers, and ventilator numbers 
SIR <- function(S0, I0, R0, beta.vector, gamma, num.days,
                hosp.delay.time = 10, hosp.rate = 0.05, hosp.los = 7,
                icu.delay.time = 2, icu.rate = 0.5, icu.los = 9,
                vent.delay.time = 1, vent.rate = 0.5, vent.los = 10) {
  
  # initialize S, I, R 
  S <- I <- R <- rep(NA_real_, num.days)
  S[1] <- S0
  I[1] <- I0
  R[1] <- R0

  # run SIR model 
  for (tt in 1:(num.days - 1)) {
    beta <- beta.vector[tt]
    S[tt + 1] <-  -beta * S[tt] * I[tt]                  + S[tt]
    I[tt + 1] <-   beta * S[tt] * I[tt] - gamma * I[tt]  + I[tt]
    R[tt + 1] <-                          gamma * I[tt]  + R[tt]
  }

  # create datatable of S, I, R
  dt <- data.table(days = 1:num.days, S, I, R)
  
  # TODO: fix this upstream
  if (length(beta.vector) != 0){
    new.infections <- beta.vector*S*I
  }
  else{
    new.infections <- beta*S*I
  }
  
  # initialize vectors 
  hosp <- icu <- vent <- admit.hosp <- admit.icu <- admit.vent <- 
    discharge.hosp <- discharge.icu <- discharge.vent <- rep(0, num.days)
  
  # iteratively creates hospitalization/icu/ventilation admission numbers based 
  # on hospital delay time and hospitalization rates
  for (tt in 1:num.days) {
    admit.hosp[tt + hosp.delay.time] <- new.infections[tt] * hosp.rate
    
    admit.icu[tt + hosp.delay.time + icu.delay.time
              ] <- new.infections[tt] * hosp.rate * icu.rate
    
    admit.vent[tt + hosp.delay.time + icu.delay.time + vent.delay.time
               ] <- new.infections[tt] * hosp.rate * icu.rate * vent.rate
  }
  
  # iteratively creates hospitalization/icu/ventilation discharge numbers based on 
  # admission numbers and length of stays
  for (tt in 1:num.days) {
    discharge.hosp[tt + hosp.los] <- admit.hosp[tt] 
    discharge.icu[tt + icu.los] <- admit.icu[tt]
    discharge.vent[tt + vent.los] <- admit.vent[tt]
  } 
  
  # iteratively creates hospitalization/icu/ventilation numbers based 
  # previous day numbers plus admits minus discharges 
  for (tt in 2:num.days) {
    hosp[tt] <- hosp[tt - 1] + admit.hosp[tt] - discharge.hosp[tt]
    icu[tt] <- icu[tt - 1] + admit.icu[tt] - discharge.icu[tt]
    vent[tt] <- vent[tt - 1] + admit.vent[tt] - discharge.vent[tt]
  }
  
  # ignore values after num.days, they're not correct
  hosp <- hosp[1:num.days]
  admit.hosp <- admit.hosp[1:num.days]
  discharge.hosp <- discharge.hosp[1:num.days]
  
  icu <- icu[1:num.days] 
  admit.icu <- admit.icu[1:num.days]
  discharge.icu <- discharge.icu[1:num.days]
  
  vent <- vent[1:num.days]
  admit.vent <- admit.vent[1:num.days]
  discharge.vent <- discharge.vent[1:num.days]
  
  # create final data table with all numbers 
  dt2 <- data.table(day = 1:num.days, hosp, admit.hosp, discharge.hosp, 
                    icu, admit.icu, discharge.icu, 
                    vent, admit.vent, discharge.vent, 
                    S, I, R, new.infections)
  return(dt2)
}

# finds current estimates of the number of active infections, 
# number recovered, and number 
find.curr.estimates = function(S0, beta.vector, gamma, num.days, num_actual, 
                               metric, start.inf = 3, 
                               hosp.delay.time = 10, hosp.rate = 0.05, hosp.los = 7,
                               icu.delay.time = 13, icu.rate = 0.02, icu.los = 9,
                               vent.delay.time = 13, vent.rate = 0.01, vent.los = 10){
  
  # starting number of susceptible people
  start.susc <- S0 - start.inf
  start.res <- 0 
  
  SIR.df = SIR(start.susc, start.inf, start.res, beta.vector, gamma, num.days, 
               hosp.delay.time, hosp.rate, hosp.los,
               icu.delay.time, icu.rate, icu.los,
               vent.delay.time, vent.rate, vent.los)
  
  # finds the minimum difference between projection and current
  # NOTE: right now, only uses hospitalization numbers to predict so else statement is never used right now

  if (metric == 'Hospitalization'){
    
    # find the difference between hospitalized column and the currently hospitalized number
    SIR.df$diff_proj <- abs(SIR.df$hosp - num_actual)
    
    # hacky 
    # the # hospitalized will be achieved twice according to model 
    # first as the hospitalizations go up, and second as the hospitalizations go down 
    # we want to find the day
    hosp.numbers <- SIR.df$hosp
    hosp.change <- hosp.numbers[2:length(hosp.numbers)] - hosp.numbers[1:length(hosp.numbers) - 1]
    hosp.change <- c(0, hosp.change)
    SIR.df$hosp.change <- hosp.change

    curr.day.df <- SIR.df[SIR.df$hosp.change > 0,]
    curr.day.df <- curr.day.df[curr.day.df$diff_proj == min(curr.day.df$diff_proj, na.rm = TRUE),] 
  }
  else{
    
    # find the difference between ICU column and the current ICU number
    SIR.df$diff_proj <- abs(SIR.df$icu - num_actual)
    
    icu.numbers <- SIR.df$icu
    icu.change <- icu.numbers[2:length(icu.numbers)] - icu.numbers[1:length(icu.numbers) - 1]
    icu.change <- c(0, icu.change)
    SIR.df$icu.change <- icu.change
    
    curr.day.df <- SIR.df[SIR.df$icu.change > 0,]
    curr.day.df <- curr.day.df[curr.day.df$diff_proj == min(curr.day.df$diff_proj, na.rm = TRUE),] 
  }
  
  curr.day <- as.integer(curr.day.df$day)
  infection.estimate <- as.integer(curr.day.df$I)
  susceptible.estimate <- as.integer(curr.day.df$S)
  recovered.estimate <- as.integer(curr.day.df$R)
  
  curr.day.list <- list(
    curr.day = curr.day,
    infection.estimate = infection.estimate, 
    susceptible.estimate = susceptible.estimate, 
    recovered.estimate = recovered.estimate
  )
  
  return(curr.day.list)
}


# gets doubling time based on R0 and gamma 
doubleTime <- function(R0, gamma) {
  1 / log2(R0 * gamma - gamma + 1)
}

# gets beta based on doubling time, gamma, and S0
getBeta <- function(doubling.time, gamma, S0) {
  g <- 2^(1/doubling.time) - 1
  beta <- (g + gamma) / S0
  return(beta)
}

