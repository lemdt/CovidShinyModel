##  ............................................................................
##  Model 1 Main Code
##  ............................................................................

#' Run SEIR model 
#' 
#' Runs the SEIR model using the Model 1 specification. 
#' https://docs.google.com/document/d/1CEoKQ1pD1x4yH7GzR3mViWtfVG-4Do8ViZUd4z_t9Ts/edit?usp=sharing
#' 
#' The simulation ignores births and deaths.  
#'
#' @param S0 Numeric. Initial number of susceptible individuals. 
#' @param E0 Numeric. Initial number of exposed individuals. 
#' @param I0 Numeric. Initial number of infected individuals. 
#' @param R0 Numeric. Initial number of recovered individuals.
#' @param beta.vector Vector, of beta values to use in the simulation on each day. 
#' Should have the same length as num.days.  
#' @param num.days Numeric. Number of days to run the simulation. 
#' @param influx List. This is hash consisting of a 'day' and an 'influx' number, 
#' meant to represent an influx of infections into the region 
#' @param params List. This should contain the parameters for running the SEIR, including:
#' - Sigma 
#' - Gamma 
#' - hosp.delay.time: time between infection and hospitalization
#' - hosp.rate: percent hospitalized out of those infected
#' - hosp.los: hospital length of stay if not admitted to ICU 
#' - icu.delay.time: time between hospitalization and ICU admission
#' - icu.rate: percent ICU admitted among those hospitalized
#' - icu.los : average ICU length of stay if not ventilated
#' - vent.delay.time: time between ICU ventilation and being put on a ventilator
#' - vent.rate: percent ventilated among those ICU admitted 
#' - vent.los: average time on a ventilator
#'
#' @return Dataframe, with susceptible counts, exposed counts, infected counts, 
#' hospitalization numbers, and recovery numbers. 
SEIR <- function(S0, E0, I0, R0, beta.vector, 
                 num.days, influx, params) {
  
  # get parameters 
  sigma <- params$sigma
  gamma <- params$gamma
  hosp.delay.time <- params$hosp.delay.time 
  hosp.rate <- params$hosp.rate
  hosp.los <- params$hosp.los
  icu.delay.time <- params$icu.delay.time 
  icu.rate <- params$icu.rate
  icu.los <- params$icu.los
  vent.delay.time <- params$vent.delay.time
  vent.rate <- params$vent.rate
  vent.los <- params$vent.los
  
  # initialize S, I, R 
  S <- E <- I <- R <- rep(NA_real_, num.days)
  S[1] <- S0
  E[1] <- E0
  I[1] <- I0
  R[1] <- R0
  
  N = S[1] + E[1] + I[1] + R[1]
  
  # run SIR model 
  for (tt in 1:(num.days - 1)) {
    beta <- beta.vector[tt]
    S[tt + 1] <-  -beta * S[tt] * I[tt] / N                  + S[tt]
    E[tt + 1] <-  beta * S[tt] * I[tt] / N - sigma * E[tt]   + E[tt]
    I[tt + 1] <-  sigma * E[tt] - gamma * I[tt]              + I[tt]
    R[tt + 1] <-  gamma * I[tt]                              + R[tt]
    
    if (influx[['day']] == tt-1){
      S[tt] <- S[tt] - influx[['num.influx']]
      E[tt] <- E[tt] + influx[['num.influx']]
    }
  }
  
  # create datatable of S, E, I, R
  dt <- data.frame(days = 0:(num.days-1), S, E, I, R)
  
  new.infections <- sigma * E
  
  new.infections <- c(I0, new.infections[1:num.days-1])
  
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
  dt2 <- data.frame(day = 0:(num.days-1), hosp, admit.hosp, discharge.hosp, 
                    icu, admit.icu, discharge.icu, 
                    vent, admit.vent, discharge.vent, 
                    S, E, I, R, new.infections)
  return(dt2)
}


#' Process dataframe to download 
#' 
#' Processes download dataframe with columns with specified names in a
#' specified order. For model 1, no processing is currently done. 
#'
#' @param df Dataframe. 
#'
#' @return Dataframe which has been processed. 
process.df.for.download <- function(df){
  return(df)
}
