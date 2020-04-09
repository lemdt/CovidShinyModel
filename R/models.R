##  ............................................................................
##  Model Caller 
##  ............................................................................

#' Model Caller 
#'
#' @param model String, either M0, M1, or M2, depending on the model chosen 
#' @param ... arguments to the specific model chosen 
#'
#' @return Dataframe, with susceptible counts, exposed counts, infected counts,
#' hospitalization numbers, and recovery numbers numbers.
SEIR <- function(model = 'M0', ...){
  if (model == 'M0'){
    SEIR_M0(...)
  }
  else if (model == 'M1'){
    SEIR_M1(...)
  }
  else{
    SEIR_M2(...)
  }
}

##  ............................................................................
##  Model 0 Main Code
##  ............................................................................


#' Run SEIR model for Model 0
#'
#' Runs the SEIR model using the Model 0 specification.
#' https://drive.google.com/file/d/12Vppztxe6JdNUHEv9hMQZdmsLVs3elsJ/view?usp=sharing
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
#' - gamma.r: The rate at which non-hospitalized people recover.
#' - gamma.h: The rate at which to-be hospitalized people in the general population
#' get admitted to the hospital.
#' - psi: The rate at which those that are hospitalized leave the hospital.
#' - hosp.rate: percent hospitalized out of those infected
#' - icu.rate: percent ICU admitted among those hospitalized
#' - vent.rate: percent ventilated among those ICU admitted
#'
#' @return Dataframe, with susceptible counts, exposed counts, infected counts,
#' hospitalization numbers, and recovery numbers numbers.
SEIR_M0 <- function(S0,
                    E0,
                    I0,
                    R0,
                    beta.vector,
                    num.days,
                    influx,
                    params) {
  # get parameters
  sigma <- params$sigma
  gamma.r <- params$gamma.r
  gamma.h <- params$gamma.h
  psi <- params$psi
  hosp.rate <- params$hosp.rate
  icu.rate <- params$icu.rate
  vent.rate <- params$vent.rate
  
  # non-hospitalized rate
  non.hosp.rate <- 1 - hosp.rate
  
  # initialize S, I, R, IR, IH, HP, DC
  # HP = hospital population
  # DC = discharge from hospital
  S <- E <- IH <- IR <- R <- HP <- DC <- rep(NA_real_, num.days)
  S[1] <- S0
  E[1] <- E0
  IR[1] <- I0 * non.hosp.rate
  IH[1] <- I0 * hosp.rate
  R[1] <- R0
  HP[1] <- 0
  DC[1] <- 0
  
  N = S[1] + E[1] + IR[1] + IH[1] + R[1] + HP[1] + DC[1]
  
  # run SIR model
  for (tt in 1:(num.days - 1)) {
    beta <- beta.vector[tt]
    dS <- (-beta * S[tt] * (IR[tt] + IH[tt])) / N
    dE <- beta * S[tt] * (IR[tt] + IH[tt]) / N - sigma * E[tt]
    dR <- gamma.r * IR[tt]
    dIR <- (sigma * E[tt] * non.hosp.rate) - dR
    dIH <- (sigma * E[tt] * hosp.rate) - (gamma.h * IH[tt])
    dHP <- (gamma.h * IH[tt]) - (psi * HP[tt])
    dDC <- psi * HP[tt]
    
    S[tt + 1] <-  S[tt] + dS
    E[tt + 1] <- E[tt] + dE
    IR[tt + 1] <- IR[tt] + dIR
    IH[tt + 1] <- IH[tt] + dIH
    R[tt + 1] <- R[tt] + dR
    HP[tt + 1] <- HP[tt] + dHP
    DC[tt + 1] <- DC[tt] + dDC
    
    if (influx[['day']] == tt) {
      S[tt + 1] <- S[tt + 1] - influx[['num.influx']]
      E[tt + 1] <- E[tt + 1] + influx[['num.influx']]
    }
  }
  
  I <- IR + IH + HP
  
  df.return <-
    data.frame(day = 0:(num.days - 1), S, E, I, IR, IH, R, HP, DC)
  
  df.return$icu <- df.return$HP * icu.rate
  df.return$vent <- df.return$icu * vent.rate
  df.return$hosp <- df.return$HP
  df.return$R.orig <- df.return$R
  df.return$R <- df.return$R + df.return$DC
  
  return(df.return)
  
}

##  ............................................................................
##  Model 1 Main Code
##  ............................................................................

#' Run SEIR model for Model 1
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
SEIR_M1 <- function(S0,
                    E0,
                    I0,
                    R0,
                    beta.vector,
                    num.days,
                    influx,
                    params) {
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
    
    if (influx[['day']] == tt) {
      S[tt + 1] <- S[tt + 1] - influx[['num.influx']]
      E[tt + 1] <- E[tt + 1] + influx[['num.influx']]
    }
  }
  
  # create datatable of S, E, I, R
  dt <- data.frame(days = 0:(num.days - 1), S, E, I, R)
  
  new.infections <- sigma * E
  
  new.infections <- c(I0, new.infections[1:num.days - 1])
  
  # initialize vectors
  hosp <- icu <- vent <- admit.hosp <- admit.icu <- admit.vent <-
    discharge.hosp <-
    discharge.icu <- discharge.vent <- rep(0, num.days)
  
  # iteratively creates hospitalization/icu/ventilation admission numbers based
  # on hospital delay time and hospitalization rates
  for (tt in 1:num.days) {
    admit.hosp[tt + hosp.delay.time] <- new.infections[tt] * hosp.rate
    
    admit.icu[tt + hosp.delay.time + icu.delay.time] <-
      new.infections[tt] * hosp.rate * icu.rate
    
    admit.vent[tt + hosp.delay.time + icu.delay.time + vent.delay.time] <-
      new.infections[tt] * hosp.rate * icu.rate * vent.rate
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
    discharge.icu[tt + icu.los] <-
      (admit.icu[tt] * (1 - vent.rate)) + discharge.vent[tt + icu.los]
  }
  
  for (tt in 1:num.days) {
    # the number of people discharged from the hospital includes:
    # 1) Non-ICU: all the non-ICU people admitted to the hospital at hosp.los days earlier
    # 2) ICU: all the people discharged from the ICU that day
    discharge.hosp[tt + hosp.los] <-
      (admit.hosp[tt] * (1 - icu.rate)) + discharge.icu[tt + hosp.los]
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
  dt2 <-
    data.frame(
      day = 0:(num.days - 1),
      hosp,
      admit.hosp,
      discharge.hosp,
      icu,
      admit.icu,
      discharge.icu,
      vent,
      admit.vent,
      discharge.vent,
      S,
      E,
      I,
      R,
      new.infections
    )
  return(dt2)
}


##  ............................................................................
##  Model 2 Main Code
##  ............................................................................

#' Generate transition matrix
#'
#' This function creates the Markov transition matrix using the transition probabilties.
#' This transition matrix will be used in the Markov model, as diagramed here:
#' https://drive.google.com/file/d/1_A3Q6C46z4hx02ayqVPhbWuIWFy6Y8ln/view?usp=sharing
#'
#' @param p.g_g Numeric. Transition probability from G -> G state.
#' @param p.g_icu Numeric. Transition probability from G -> ICU state.
#' @param p.g_d Numeric. Transition probability from G -> D state.
#' @param p.icu_g Numeric. Transition probability from ICU -> G state.
#' @param p.icu_icu Numeric. Transition probability from ICU -> ICU state.
#' @param p.icu_v Numeric. Transition probability from ICU -> V state
#' @param p.v_icu Numeric. Transition probability from V -> ICU state.
#' @param p.v_v Numeric. Transition probability from V -> V state.
#' @param p.v_m Numeric. Transition probability from V -> M state.
#' @param p.g_m Numeric. Transition probability from G -> M state.
#' @param p.icu_m Numeric. Transition probability from ICU -> M state.
#'
#' @return Transition matrix, to be used in the update step of the Markov model.
createTransition <- function(p.g_g,
                             p.g_icu,
                             p.g_d,
                             p.icu_g,
                             p.icu_icu,
                             p.icu_v,
                             p.v_icu,
                             p.v_v,
                             p.v_m,
                             p.g_m = 0,
                             p.icu_m = 0) {
  tmat <- matrix(
    c(
      p.g_g,
      p.g_icu,
      0,
      p.g_d,
      p.g_m,
      p.icu_g,
      p.icu_icu,
      p.icu_v,
      0,
      p.icu_m,
      0,
      p.v_icu,
      p.v_v,
      0,
      p.v_m,
      0,
      0,
      0,
      1,
      0,
      0,
      0,
      0,
      0,
      1
    ),
    nrow = 5,
    byrow = TRUE
  )
  
  return(tmat)
}


#' Run Markov Simulation
#'
#' This function takes in as inputs a vector of new hospitalizations per day and
#' runs the Markov simulation using this.
#'
#' It returns a dataframe with the number of people in the general hospital (G.state),
#' non-ventilated ICU (ICU.state), ventilated ICU (V.state), discharged (DCH.state),
#' and dead (M.state).
#'
#' @param new.g.vec Vector of numerics, which describe the number of new hospitalizations
#' per day.
#' @param trans.mat Matrix. For Markov simulation.
#'
#' @return Dataframe, with counts of each state.
runMarkov <- function(new.g.vec, trans.mat) {
  # h, icu, vent, d, m
  df.final <- data.frame()
  
  step.vec <- c(0, 0, 0, 0, 0)
  
  for (new.g in new.g.vec) {
    step.vec <- as.vector(step.vec %*% trans.mat)
    step.vec[1] <- step.vec[1] + new.g
    df.final <- rbind(df.final, step.vec)
  }
  
  colnames(df.final) <-
    c('G.state', 'ICU.state', 'V.state', 'DCH.state', 'M.state')
  return(df.final)
}


#' Run SEIR model for Model 2
#'
#' Runs the SEIR model, incorporating the separate hospitalization Markov component.
#' More about the technique is here:
#' https://drive.google.com/file/d/157yB7O62xyJps2_HUtAelqsQ8e9feTj_/view?usp=sharing
#' https://sites.me.ucsb.edu/~moehlis/APC514/tutorials/tutorial_seasonal/node4.html
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
#' @param params List. This should contain the parameters for running the SEIR,
#' including sigma, gamma.r, gamma.h, hosp.rate, and the transition probabilities
#' (p.g_g, p.g_icu, p.g_d)
#'
#' @return Dataframe, with susceptible counts, exposed counts, infected counts,
#' hospitalization numbers, recovery numbers, and death numbers.
SEIR_M2 <- function(S0,
                    E0,
                    I0,
                    R0,
                    beta.vector,
                    num.days,
                    influx,
                    params) {
  # get parameters
  sigma <- params$sigma
  gamma.r <- params$gamma.r
  gamma.h <- params$gamma.h
  hosp.rate <- params$hosp.rate
  p.g_g <- params$p.g_g
  p.g_icu <- params$p.g_icu
  p.g_d <- params$p.g_d
  p.icu_g <- params$p.icu_g
  p.icu_icu <- params$p.icu_icu
  p.icu_v <- params$p.icu_v
  p.v_icu <- params$p.v_icu
  p.v_v <- params$p.v_v
  p.v_m <- params$p.v_m
  
  # create transition matrix
  trans.mat <- createTransition(
    p.g_g,
    p.g_icu,
    p.g_d,
    p.icu_g,
    p.icu_icu,
    p.icu_v,
    p.v_icu,
    p.v_v,
    p.v_m,
    p.g_m = 0,
    p.icu_m = 0
  )
  
  # non-hospitalized rate
  non.hosp.rate <- 1 - hosp.rate
  
  # initialize S, I, R, newG
  S <- E <- IH <- IR <- R <- newG <- rep(NA_real_, num.days)
  S[1] <- S0
  E[1] <- E0
  IR[1] <- I0 * non.hosp.rate
  IH[1] <- I0 * hosp.rate
  R[1] <- R0
  newG[1] <- 0
  
  # sum of the entire population
  N = S[1] + E[1] + IR[1] + IH[1] + R[1]
  
  # run SIR model
  for (tt in 1:(num.days - 1)) {
    beta <- beta.vector[tt]
    dS <- (-beta * S[tt] * (IR[tt] + IH[tt])) / N
    dE <- beta * S[tt] * (IR[tt] + IH[tt]) / N - sigma * E[tt]
    dR <- gamma.r * IR[tt]
    dIR <- (sigma * E[tt] * non.hosp.rate) - dR
    dIH <- (sigma * E[tt] * hosp.rate) - (gamma.h * IH[tt])
    
    S[tt + 1] <-  S[tt] + dS
    E[tt + 1] <- E[tt] + dE
    IR[tt + 1] <- IR[tt] + dIR
    IH[tt + 1] <- IH[tt] + dIH
    R[tt + 1] <- R[tt] + dR
    newG[tt + 1] <- gamma.h * IH[tt]
    
    if (influx[['day']] == tt) {
      S[tt + 1] <- S[tt + 1] - influx[['num.influx']]
      E[tt + 1] <- E[tt + 1] + influx[['num.influx']]
    }
  }
  
  # create initial dataframe
  df.return <-
    data.frame(day = 0:(num.days - 1), S, E, IR, IH, R, newG)
  
  # run Markov
  markov.df <- runMarkov(new.g.vec = newG,
                         trans.mat = trans.mat)
  
  # add a day column
  markov.df$day <- 0:(num.days - 1)
  
  # merge original and Markov dataframes
  df.return <- merge(df.return, markov.df, by = 'day')
  
  # processing some columns
  df.return$I <- df.return$IR + df.return$IH + df.return$G.state +
    df.return$ICU.state + df.return$V.state
  
  df.return$R.orig <- df.return$R
  df.return$R <- df.return$R + df.return$DCH.state
  df.return$hosp <-
    df.return$G.state + df.return$ICU.state + df.return$V.state
  df.return$icu <- df.return$ICU.state + df.return$V.state
  df.return$vent <- df.return$V.state
  
  return(df.return)
}

