##  ............................................................................
##  Model 0 Main Code
##  ............................................................................


# Runs SEIR for model 0
SEIR <- function(S0, E0, I0, R0, beta.vector, 
                 num.days, influx, params) {

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
  S <- E<- IH <- IR <- R <- HP <- DC <- rep(NA_real_, num.days)
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
    dS <- (-beta * S[tt] * (IR[tt] + IH[tt]))/N
    dE <- beta * S[tt] * (IR[tt] + IH[tt])/ N - sigma * E[tt]
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

    if (influx[['day']] == tt-1){
      S[tt] <- S[tt] - influx[['num.influx']]
      E[tt] <- E[tt] + influx[['num.influx']]
    }
  }
  
  I <- IR + IH + HP
  
  df.return <- data.frame(day = 0:(num.days-1), S, E, I, IR, IH, R, HP, DC)
  
  df.return$icu <- df.return$HP * icu.rate 
  df.return$vent <- df.return$icu * vent.rate
  df.return$hosp <- df.return$HP
  df.return$R.orig <- df.return$R
  df.return$R <- df.return$R + df.return$DC
  
  return(df.return)
  
}

process.df.for.download <- function(df){
  
  df.return <- df
  df.return$R <- df.return$R.orig

  df.return <- df.return[,c('day', 'days.shift', 'date', 'S', 'E', 'I', 'IR', 
                     'IH', 'R', 'HP', 'icu', 'vent', 'DC')]
  
  return(df.return)
  
}