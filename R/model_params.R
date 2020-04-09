##  ............................................................................
##  Default Parameters 
##  ............................................................................

default_params_M0 <- list(
  incubation.period = 5,
  sigma = 1 / 5,
  illness.length = 7,
  gamma.r = 1 / 7,
  inf.to.hosp = 7,
  gamma.h = 1 / 7,
  gamma = 1 / 7,
  hosp.los = 10,
  psi = 1 / 10,
  hosp.rate = 0.06,
  icu.rate = 0.5,
  vent.rate = 0.85,
  int.new.r0 = 2.8,
  int.new.double = 6,
  int.new.num.days = 0,
  int.smooth.days = 0,
  hosp.avail = 1000,
  icu.avail = 200,
  vent.avail = 100
)

default_params_M1 <- list(
  illness.length = 7,
  gamma = 1 / 7,
  incubation.period = 5,
  sigma = 1 / 5,
  hosp.delay.time = 10,
  hosp.rate = 0.06,
  hosp.los = 7,
  icu.delay.time = 5,
  icu.rate = 0.3,
  icu.los = 1,
  vent.delay.time = 1,
  vent.rate = 0.64,
  vent.los = 10,
  int.new.r0 = 2.8,
  int.new.double = 6,
  int.new.num.days = 0,
  int.smooth.days = 0,
  hosp.avail = 1000,
  icu.avail = 200,
  vent.avail = 100
)

default_params_M2 <- list(
  incubation.period = 5,
  sigma = 1 / 5,
  illness.length = 7,
  gamma.r = 1 / 7,
  inf.to.hosp = 7,
  gamma.h = 1 / 7,
  gamma = 1 / 7,
  hosp.rate = 0.06,
  p.g_g = 0.5,
  p.g_icu = 0.25,
  p.g_d = 0.25,
  p.icu_g = 0.2,
  p.icu_icu = 0.3,
  p.icu_v = 0.5,
  p.v_icu = 0.2,
  p.v_v = 0.792,
  p.v_m = 0.008,
  int.new.r0 = 2.8,
  int.new.double = 6,
  int.new.num.days = 0,
  int.smooth.days = 0,
  hosp.avail = 1000,
  icu.avail = 200,
  vent.avail = 100
)

#' Returns to Parameter UI elements for the Shiny App 
#'
#' @param model String, either M0, M1, or M2, depending on the model chosen
#'
#' @return the default parameters for the model used
default_params <- function(model){
  if (model == 'M0'){
    default_params_M0
  }
  else if (model == 'M1'){
    default_params_M1
  }
  else{
    default_params_M2
  }
}

##  ............................................................................
##  Parameters Pages 
##  ............................................................................


#' Returns to Parameter UI elements for the Shiny App 
#'
#' @param model String, either M0, M1, or M2, depending on the model chosen 
#'
#' @return UI elements for parameters in Shiny App 
parameters_page <- function(model){
  if (model == 'M0'){
    div(
      incubation.period.input(default_params_M0$incubation.period),
      illness.length.input(default_params_M0$illness.length),
      hosp.after.inf.input(default_params_M0$inf.to.hosp),
      hosp.rate.input(default_params_M0$hosp.rate),
      hosp.los.input.model0(default_params_M0$hosp.los),
      icu.rate.input(default_params_M0$icu.rate),
      vent.rate.input(default_params_M0$vent.rate)
    )
  }
  else if (model == 'M1'){
    div(
      incubation.period.input(default_params_M1$incubation.period),
      illness.length.input(default_params_M1$illness.length),
      hosp.rate.input(default_params_M1$hosp.rate),
      icu.rate.input(default_params_M1$icu.rate),
      vent.rate.input(default_params_M1$vent.rate),
      hosp.after.inf.input(default_params_M1$hosp.delay.time),
      icu.after.hosp.input(default_params_M1$icu.delay.time),
      vent.after.icu.input(default_params_M1$vent.delay.time),
      hosp.los.input(default_params_M1$hosp.los),
      icu.los.input(default_params_M1$icu.los),
      vent.los.input(default_params_M1$vent.los)
    )
  }
  else{
    div(
      incubation.period.input(default_params_M2$incubation.period),
      hosp.rate.input(default_params_M2$hosp.rate),
      illness.length.input(default_params_M2$illness.length),
      hosp.after.inf.input(default_params_M2$inf.to.hosp),
      
      HTML(g.trans.prob.head),
      trans.prob.slider(
        inputId = 'p.g_icu',
        label = g.to.icu.wording,
        value = default_params_M2$p.g_icu
      ),
      trans.prob.slider(
        inputId = 'p.g_d',
        label = g.to.disc.wording,
        value = default_params_M2$p.g_d
      ),
      
      HTML(icu.trans.prob.head),
      trans.prob.slider(
        inputId = 'p.icu_g',
        label = icu.to.g.wording,
        value = default_params_M2$p.icu_g
      ),
      trans.prob.slider(
        inputId = 'p.icu_v',
        label = icu.to.vent.wording,
        value = default_params_M2$p.icu_v
      ),
      
      HTML(vent.trans.prob.head),
      trans.prob.slider(
        inputId = 'p.v_icu',
        label = vent.to.icu.wording,
        value = default_params_M2$p.v_icu
      ),
      trans.prob.slider(
        inputId = 'p.v_m',
        label = vent.to.m.wording,
        value = default_params_M2$p.v_m,
        min = 0,
        max = 0.2,
        step = 0.001
      )
    )
  }
}

