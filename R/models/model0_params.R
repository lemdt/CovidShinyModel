##  ............................................................................
##  Model 0 Helper Code
##  ............................................................................

# default parameters for Model 0
default.params <- list(
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


# modal to enter parameters that pops up after clicking on "Customize Other Parameters"
parameters.page <- function(){
  fluidPage(
    incubation.period.input(default.params$incubation.period),
    illness.length.input(default.params$illness.length),
    hosp.after.inf.input(default.params$inf.to.hosp),
    hosp.rate.input(default.params$hosp.rate),
    hosp.los.input.model0(default.params$hosp.los),
    icu.rate.input(default.params$icu.rate),
    vent.rate.input(default.params$vent.rate)
  )
}



#' Process parameters for export
#'
#' @param params ReactiveValues list.
#'
#' @return List of processed parameters.
process.params.for.download <- function(params) {
  param.list <- list(
    'Incubation Period' = params$incubation.period,
    'Length of Infectiousness (days)' = params$illness.length,
    'Symptomatic to Hospitalization (days)' = params$inf.to.hosp,
    'Percent Hospitalized of all Infections' = params$hosp.rate,
    'Percent ICU Admitted of Those Hospitalized' = params$icu.rate,
    'Average Percent Ventilated of Those ICU Admitted' = params$vent.rate,
    'Hospital Length of Stay (days)' = params$hosp.los
  )

  return(param.list)
}
