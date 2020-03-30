##  ............................................................................
##  Model 2 Helper Code
##  ............................................................................

# libraries 
library(shiny)

default.params <- reactiveValues(
  incubation.period = 5,
  sigma = 1/5,
  illness.length = 7,
  gamma = 1/7,
  inf.to.hosp = 10, 
  gamma.h = 1/10,
  hosp.rate = 0.06, 
  p.g_g = 0.85,
  p.g_icu = 0.1,
  p.g_d = 0.05,
  p.icu_g = 0.15, 
  p.icu_icu = 0.35, 
  p.icu_v = 0.5, 
  p.v_icu = 0.1,
  p.v_v = 0.83,
  p.v_m = 0.07,
  int.new.r0 = 2.8, 
  int.new.double = 6,
  int.new.num.days = 0, 
  int.smooth.days = 0,
  hosp.avail = 1000, 
  icu.avail = 200, 
  vent.avail = 100
)

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
                        value = params$p.v_m)
      ),
    footer = tagList(
      save.parameter.action
    )
  )
  
  return(params.page)
}

save.params <- function(params,input){
  params$illness.length = input$illness.length
  params$gamma = 1/input$illness.length
  params$inf.to.hosp = input$hosp.after.inf
  params$gamma.h = 1/input$hosp.after.inf
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
