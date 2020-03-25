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
  
  N = S[1] + I[1] + R[1]

  # run SIR model 
  for (tt in 1:(num.days - 1)) {
    beta <- beta.vector[tt]
    S[tt + 1] <-  -beta * S[tt] * I[tt] / N                  + S[tt]
    I[tt + 1] <-   beta * S[tt] * I[tt] / N - gamma * I[tt]  + I[tt]
    R[tt + 1] <-                          gamma * I[tt]  + R[tt]
  }

  # create datatable of S, I, R
  dt <- data.table(days = 1:num.days, S, I, R)
  
  # TODO: fix this upstream
  if (length(beta.vector) != 0){
    new.infections <- beta.vector*S*I / N
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
    # discharged from ventilator 
    discharge.vent[tt + vent.los] <- admit.vent[tt]
  } 

  for (tt in 1:num.days) {
    # the number of people discharged from icu includes:
    # 1) Non-Ventilated: all the non-ventilated ICU people admitted at icu.los days earlier 
    # 2) Ventilated: all the people who were discharged from the ventilator on that day
    discharge.icu[tt + icu.los] <- (admit.icu[tt] * (1 - vent.rate)) + discharge.vent[tt + icu.los]
  }

  for (tt in 1:num.days) {
    # the number of people discharged from the hospital includes: 
    # 1) Non-ICU: all the non-ICU people admitted to the hospital at hosp.los days earlier
    # 2) ICU: all the people discharged from the ICU that day 
    discharge.hosp[tt + hosp.los] <- (admit.hosp[tt] * (1 - icu.rate)) + discharge.icu[tt + hosp.los]
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


# function to get the best value of Re given historical data 
findBestRe <- function(S0, gamma, num.days, day.vec, num_actual.vec, 
                       start.inf = 3, hosp.delay.time = 10, 
                       hosp.rate = 0.05, hosp.los = 7, icu.delay.time = 13, 
                       icu.rate = 0.02, icu.los = 9,vent.delay.time = 13, 
                       vent.rate = 0.01, vent.los = 10){
  
  # starting number of susceptible people
  start.susc <- S0 - start.inf
  start.res <- 0 
  
  # TODO: implement binary search
  
  min.sqrd.sum <- Inf
  re_choice <- NA
  vec.choice <- c()
  
  for (re in c(seq(1, 7, 0.1))){
    beta <- getBetaFromRe(re, gamma)
    
    SIR.df = SIR(start.susc, start.inf, start.res, rep(beta, num.days), gamma, num.days, 
                 hosp.delay.time, hosp.rate, hosp.los,
                 icu.delay.time, icu.rate, icu.los,
                 vent.delay.time, vent.rate, vent.los)
    
    SIR.df$diff_proj <- abs(SIR.df$hosp - num_actual.vec[1])
    hosp.numbers <- SIR.df$hosp
    hosp.change <- hosp.numbers[2:length(hosp.numbers)] - hosp.numbers[1:length(hosp.numbers) - 1]
    hosp.change <- c(0, hosp.change)
    SIR.df$hosp.change <- hosp.change
    
    curr.day.df <- SIR.df[SIR.df$hosp.change > 0,]
    curr.day <- curr.day.df[curr.day.df$diff_proj == min(curr.day.df$diff_proj, na.rm = TRUE),]$day - day.vec[1]
    
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