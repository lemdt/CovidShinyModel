##  ............................................................................
##  Model 2 Main Code
##  ............................................................................

library(data.table)

# Transition Matrix
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

# Run Markov Simulation 
runMarkov <- function(new.g.vec, trans.mat){
  
  # h, icu, vent, d, m 
  df.final <- data.frame()
  
  step.vec <- c(0, 0, 0, 0, 0)
  
  for (new.g in new.g.vec){
    step.vec <- as.vector(step.vec %*% trans.mat)
    step.vec[1] <- step.vec[1] + new.g 
    df.final <- rbind(df.final, step.vec)
  }
  
  colnames(df.final) <- c('g.only', 'icu.only', 'vent.only', 'disc', 'death')
  return(df.final)
}

# Runs SEIR, combines with Markov
SEIR <- function(S0, E0, I0, R0, beta.vector, 
                 num.days, influx, params) {
  
  # get parameters
  sigma <- params$sigma
  gamma.r <- params$gamma
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
  
  I <- IR + IH + newG
  
  df.return <- data.frame(day = 0:(num.days-1), S, E, I, IR, IH, R, newG)
  
  markov.df <- runMarkov(new.g.vec = newG,
                         trans.mat = trans.mat)
  
  markov.df$day <- 0:(num.days-1)
  
  df.return <- merge(df.return, markov.df, by = 'day')
  
  df.return$R <- df.return$R + df.return$disc
  df.return$hosp <- df.return$g.only + df.return$icu.only + df.return$vent.only
  df.return$icu <- df.return$icu.only + df.return$vent.only
  df.return$vent <- df.return$vent.only
  
  return(df.return)
  
}