##  ............................................................................
##  Model 1 Helper Code
##  ............................................................................

# default parameters for Model 1
default.params <- reactiveValues(
  illness.length = 7,
  gamma = 1/7,
  incubation.period = 5,
  sigma = 1/5,
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

# modal to enter parameters that pops up after clicking on "Customize Other Parameters"
parameters.modal <- function(params){
  params.page <- modalDialog(
    fluidPage(
      incubation.period.input(params$incubation.period),
      illness.length.input(params$illness.length),
      hosp.rate.input(params$hosp.rate),
      icu.rate.input(params$icu.rate),
      vent.rate.input(params$vent.rate),
      hosp.after.inf.input(params$hosp.delay.time),
      icu.after.hosp.input(params$icu.delay.time),
      vent.after.icu.input(params$vent.delay.time),
      hosp.los.input(params$hosp.los),
      icu.los.input(params$icu.los),
      vent.los.input(params$vent.los)),
    footer = tagList(
      actionButton("save", "Save and Close")
    )
  )
  
  return(params.page)
}

#' Save Parameters
#' 
#' Saves parameters in the 'params' ReactiveVals. 
#'
#' @param params ReactiveVals list. 
#' @param input List (from shiny's inputs). 
#'
#' @return ReactiveVals list, with new set of saved parameters. 
save.params <- function(params,input){
  params$illness.length = input$illness.length
  params$gamma = 1/input$illness.length
  params$hosp.delay.time = input$hosp.after.inf
  params$hosp.rate = input$hosp.rate
  params$hosp.los = input$hosp.los
  params$icu.delay.time = input$icu.after.hosp
  params$icu.rate = input$icu.rate
  params$icu.los = input$icu.los
  params$vent.delay.time = input$vent.after.icu
  params$vent.rate = input$vent.rate
  params$vent.los = input$vent.los
  params$incubation.period = input$incubation.period
  params$sigma = 1/input$incubation.period
  
  return(params)
}