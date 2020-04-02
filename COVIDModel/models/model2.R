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
                             p.icu_m = 0){

  tmat <- matrix(c(p.g_g, p.g_icu, 0, p.g_d, p.g_m,
                   p.icu_g, p.icu_icu, p.icu_v, 0, p.icu_m,
                   0, p.v_icu, p.v_v, 0, p.v_m,
                   0, 0, 0, 1, 0,
                   0, 0, 0, 0, 1), nrow = 5, byrow = TRUE)
  
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
runMarkov <- function(new.g.vec, trans.mat){
  
  # h, icu, vent, d, m 
  df.final <- data.frame()
  
  step.vec <- c(0, 0, 0, 0, 0)
  
  for (new.g in new.g.vec){
    step.vec <- as.vector(step.vec %*% trans.mat)
    step.vec[1] <- step.vec[1] + new.g 
    df.final <- rbind(df.final, step.vec)
  }
  
  colnames(df.final) <- c('G.state', 'ICU.state', 'V.state', 'DCH.state', 'M.state')
  return(df.final)
}


#' Run SEIR model 
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
SEIR <- function(S0, E0, I0, R0, beta.vector, 
                 num.days, influx, params) {
 
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
  trans.mat <- createTransition(p.g_g, 
                                p.g_icu, 
                                p.g_d, 
                                p.icu_g,
                                p.icu_icu, 
                                p.icu_v,
                                p.v_icu, 
                                p.v_v, 
                                p.v_m,
                                p.g_m = 0,
                                p.icu_m = 0)
  
  # non-hospitalized rate
  non.hosp.rate <- 1 - hosp.rate
  
  # initialize S, I, R, newG
  S <- E<- IH <- IR <- R <- newG <- rep(NA_real_, num.days)
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
    dS <- (-beta * S[tt] * (IR[tt] + IH[tt]))/N
    dE <- beta * S[tt] * (IR[tt] + IH[tt])/ N - sigma * E[tt]
    dR <- gamma.r * IR[tt]
    dIR <- (sigma * E[tt] * non.hosp.rate) - dR
    dIH <- (sigma * E[tt] * hosp.rate) - (gamma.h * IH[tt])
    
    S[tt + 1] <-  S[tt] + dS
    E[tt + 1] <- E[tt] + dE
    IR[tt + 1] <- IR[tt] + dIR
    IH[tt + 1] <- IH[tt] + dIH
    R[tt + 1] <- R[tt] + dR
    newG[tt + 1] <- gamma.h * IH[tt]
    
    if (influx[['day']] == tt-1){
      S[tt] <- S[tt] - influx[['num.influx']]
      E[tt] <- E[tt] + influx[['num.influx']]
    }
  }
  
  # create initial dataframe
  df.return <- data.frame(day = 0:(num.days-1), S, E, IR, IH, R, newG)
  
  # run Markov 
  markov.df <- runMarkov(new.g.vec = newG,
                         trans.mat = trans.mat)
  
  # add a day column 
  markov.df$day <- 0:(num.days-1)
  
  # merge original and Markov dataframes
  df.return <- merge(df.return, markov.df, by = 'day')
  
  # processing some columns 
  df.return$I <- df.return$IR + df.return$IH + df.return$G.state + 
    df.return$ICU.state + df.return$V.state

  df.return$R.orig <- df.return$R
  df.return$R <- df.return$R + df.return$DCH.state
  df.return$hosp <- df.return$G.state + df.return$ICU.state + df.return$V.state
  df.return$icu <- df.return$ICU.state + df.return$V.state
  df.return$vent <- df.return$V.state
  
  return(df.return)
}


#' Process dataframe to download 
#' 
#' Processes download dataframe with columns with specified names in a
#' specified order. 
#'
#' @param df Dataframe. 
#'
#' @return Dataframe. 
process.df.for.download <- function(df){
  
  df$R <- df$R.orig
  df$HOSP.report <- df$hosp
  df$ICU.report <- df$icu 
  df$VENT.report <- df$vent
  df$DISCHARGE.report <- df$DCH.state
  df$MORTALITY.report <- df$M.state
  
  df <- df[,c('day', 'days.shift', 'date', 'S', 'E', 'I', 'IR', 'IH', 'R',
              'newG', 'G.state', 'ICU.state', 'V.state', 'DCH.state', 'M.state', 
              'HOSP.report', 'ICU.report', 'VENT.report', 'DISCHARGE.report', 'MORTALITY.report')]
  
  colnames(df) <- c('day', 'days.shift', 'date', 'S', 'E', 'I', 'IR', 'IH', 'R',
                    'G_new', 'G_only', 'ICU_only', 'V_only', 'DCH', 'M', 
                    'M5_hosp', 'M5_icut', 'M5_vent', 'M5_dch', 'M5_M')
  
  return(df)
}
