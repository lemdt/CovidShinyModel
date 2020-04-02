##  ............................................................................
##  Model 2 Helper Code
##  ............................................................................

# default parameters for Model 2
default.params <- reactiveValues(
  incubation.period = 5,
  sigma = 1/5,
  illness.length = 7,
  gamma.r = 1/7,
  inf.to.hosp = 7, 
  gamma.h = 1/7,
  gamma = 1/7,
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

# modal to enter parameters that pops up after clicking on "Customize Other Parameters"
parameters.modal <- function(params){
  params.page <- modalDialog(
    fluidPage(
      incubation.period.input(params$incubation.period),
      hosp.rate.input(params$hosp.rate),
      illness.length.input(params$illness.length),
      hosp.after.inf.input(params$inf.to.hosp),
      
      HTML(g.trans.prob.head),
      trans.prob.slider(inputId = 'p.g_icu', 
                        label = g.to.icu.wording,
                        value = params$p.g_icu), 
      trans.prob.slider(inputId = 'p.g_d', 
                        label = g.to.disc.wording,
                        value = params$p.g_d),
      
      HTML(icu.trans.prob.head),
      trans.prob.slider(inputId = 'p.icu_g', 
                        label = icu.to.g.wording,
                        value = params$p.icu_g),
      trans.prob.slider(inputId = 'p.icu_v', 
                        label = icu.to.vent.wording,
                        value = params$p.icu_v),
      
      HTML(vent.trans.prob.head),
      trans.prob.slider(inputId = 'p.v_icu', 
                        label = vent.to.icu.wording,
                        value = params$p.v_icu),
      trans.prob.slider(inputId = 'p.v_m', 
                        label = vent.to.m.wording,
                        value = params$p.v_m, 
                        min = 0, 
                        max = 0.2,
                        step = 0.001)
      ),
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
  params$gamma.r = 1/input$illness.length
  params$inf.to.hosp = input$hosp.after.inf
  params$gamma.h = 1/input$hosp.after.inf
  params$gamma = ((1 - input$hosp.rate) * 1/input$illness.length) + 
    (input$hosp.rate * 1/input$hosp.after.inf)
  params$hosp.rate = input$hosp.rate
  params$incubation.period = input$incubation.period
  params$sigma = 1/input$incubation.period
  params$p.g_icu = input$p.g_icu
  params$p.g_d = input$p.g_d
  params$p.g_g = 1 - input$p.g_d - input$p.g_icu
  params$p.icu_g = input$p.icu_g
  params$p.icu_v = input$p.icu_v
  params$p.icu_icu = 1 - input$p.icu_g - input$p.icu_v
  params$p.v_icu = input$p.v_icu
  params$p.v_m = input$p.v_m
  params$p.v_v = 1 - input$p.v_icu - input$p.v_m
  
  return(params)
}


#' Process parameters for export
#'
#' @param params ReactiveValues list.
#'
#' @return List of processed parameters. 
process.params.for.download <- function(params){
  param.list <- list(
    'Incubation Period' = params$incubation.period,
    'Length of Infectiousness (days)' = params$illness.length, 
    'Symptomatic to Hospitalization (days)' = params$inf.to.hosp, 
    'percent hospitalized of all infections' = params$hosp.rate, 
    'Transition Probability from G->G' = params$p.g_g,
    'Transition Probability from G->icu' = params$p.g_icu,
    'Transition Probability from G->discharge' = params$p.g_d, 
    'Transition Probability from icu->G' = params$p.icu_g,
    'Transition Probabiltiy from icu->icu' = params$p.icu_icu, 
    'Transition Probability from icu->vent' = params$p.icu_v, 
    'Transition Probability from vent->icu' = params$p.v_icu,
    'Transition Probability from vent->vent' = params$p.v_v, 
    'Transition Proabbility from vent->death' = params$p.v_m
  )
  
  return(param.list)
}
