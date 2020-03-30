##  ............................................................................
## Compilation of all of the inputs that are used in the app
##  ............................................................................

library(shiny)
library(shinyWidgets)
source('wording.R')

##  ............................................................................
##  General
##  ............................................................................

how.to.use.link <- actionLink('howtouse', about.link.wording)

##  ............................................................................
##  Location inputs
##  ............................................................................

num.people.input <- numericInput(inputId = 'num_people', 
                                 label = num.people.wording, 
                                 value = 883305)

curr.date.input <- dateInput(inputId = 'curr_date', 
                             label = curr.date.wording)

input.metric.input <- radioGroupButtons(inputId = 'input.metric', 
                                        label = input.metric.wording, 
                                        choices = c('Hospitalizations', 'Cases'),
                                        justified = TRUE, 
                                        status = "primary")

num.hosp.input <- numericInput(inputId = 'num_hospitalized', 
                               label = hosp.input.wording, 
                               value = 50)

num.cases.input <- numericInput(inputId = 'num_cases', 
                                label = cases.input.wording, 
                                value = 50)

double.time.input <- function(date.select){
  input.return <- sliderInput(inputId = 'doubling_time', 
                              label = sprintf(prior.double.wording, date.select), 
                              min = 1, 
                              max = 12, 
                              step = 1, 
                              value = 6)
  return(input.return)
}

r0.prior.input <- function(date.select){
  input.return <- sliderInput(inputId = 'r0_prior', 
                              label = sprintf(prior.re.wording, date.select), 
                              min = 0.1, 
                              max = 7, 
                              step = 0.1, 
                              value = 2.8)
  return(input.return)
}

pred.re.action <- function(date.select){
  input.return <- actionLink('predict_re', sprintf(estimate.re.action.wording, date.select))
  
  return(input.return)
}

##  ............................................................................
##  Intervention inputs
##  ............................................................................

show.int.input <- checkboxInput(inputId = 'showint', 
                                label = add.int.cb.wording)

int.double.input <- sliderInput(inputId = 'new_double', 
                                label = int.double.wording, 
                                min = 0, 
                                max = 50, 
                                step = 1, 
                                value = 6)

int.re.input <- sliderInput(inputId = 'r0_new', 
                            label = int.re.wording, 
                            min = 0.1, 
                            max = 6, 
                            step = 0.1,
                            value = 2.8)

int.date.input <- function(curr.date, hosp.delay.time){
  input.return <- dateInput(inputId = 'int_date', 
                            label = int.date.wording, 
                            min = curr.date - hosp.delay.time, 
                            value = curr.date)
  
  return(input.return)
}

smooth.int.input <- sliderInput('smooth.int', 
                                label = int.smooth.wording, 
                                value = 0, 
                                min = 0, 
                                max = 30)

add.int.action <- actionButton(inputId = 'add_intervention', 
                               label = save.int.wording)

##  ............................................................................
##  Influx inputs
##  ............................................................................

show.influx.input <- checkboxInput(inputId = 'showinflux', 
                                   label = influx.cb.wording)

influx.date.input <- function(curr.date, hosp.delay.time){
  input.return <- dateInput(inputId = 'influx_date', 
                            label = influx.date.wording, 
                            min = curr.date - hosp.delay.time, 
                            value = curr.date)
  
  return(input.return)
}

num.influx.input <- numericInput('num.influx', 
                                 label = influx.num.wording, 
                                 value = 0)


##  ............................................................................
##  Settings Inputs
##  ............................................................................

proj.num.days.input <- sliderInput(inputId = 'proj_num_days', 
                                   label = proj.days.wording, 
                                   min = 10, 
                                   max = 730, 
                                   step = 5, 
                                   value = 365)

use.double.input <-  materialSwitch(inputId = "usedouble", 
                                    label = use.double.wording, 
                                    status = 'primary')

other.params.button <- actionButton(inputId = 'parameters_modal',
                                   label = cust.params.wording)


##  ............................................................................
##  Re Estimation Inputs
##  ............................................................................

date.hist.input <- function(curr.date){
  input.return <- dateInput(inputId = 'date.hist', 
                            label = 'Date',
                            value = curr.date,
                            max = curr.date,
                            min = curr.date - 14)
  
  return(input.return)
}

hist.hosp.input <- numericInput(inputId = 'num.hospitalized.hist', 
                                label = 'Number Hospitalized', 
                                value = NA)

add.hist.action <- actionButton('add.hist', 'Add Data')

run.fit.action <- actionButton(inputId = 'run.fit', 
                               label = 'Estimate Re')

##  ............................................................................
##  Model 1 Parameters
##  ............................................................................

incubation.period.input <- function(incubation.period){
  input.return <- sliderInput('incubation.period', incubation.input.wording, min = 0, max = 20, step = 1, 
                              value = incubation.period, width = '100%')
  
  return(input.return)
}

illness.length.input <- function(illness.length){
  input.return <- sliderInput('illness.length', infectious.input.wording, min = 0, max = 20, step = 1, 
                              value = illness.length, width = '100%')
  
  return(input.return)
}


hosp.rate.input <- function(hosp.rate){
  input.return <- sliderInput('hosp.rate', per.hosp.wording, min = 0, max = 1, step = 0.01, 
                              value = hosp.rate, width = '100%')

  return(input.return)
}


icu.rate.input <- function(icu.rate){
  input.return <- sliderInput('icu.rate', per.icu.wording, min = 0, max = 1, step = 0.01, 
                              value = icu.rate, width = '100%')
  
  return(input.return)
  
}

vent.rate.input <- function(vent.rate){
  input.return <- sliderInput('vent.rate', per.vent.wording, min = 0, max = 1, step = 0.01, 
                              value = vent.rate, width = '100%')
  
  return(input.return)
}


hosp.after.inf.input <- function(hosp.delay.time){
  input.return <- sliderInput('hosp.after.inf', inf.to.hosp.wording, min = 0, max = 30, step = 1, 
                              value = hosp.delay.time, width = '100%')
  
  return(input.return)
  
}

icu.after.hosp.input <- function(icu.delay.time){
  input.return <- sliderInput('icu.after.hosp', hosp.to.icu.wording, min = 0, max = 30, step = 1, 
                              value = icu.delay.time, width = '100%')
  
  return(input.return)
}

vent.after.icu.input <- function(vent.delay.time){
  input.return <- sliderInput('vent.after.icu', icu.to.vent.wording, min = 0, max = 30, step = 1, 
                              value = vent.delay.time, width = '100%')
  
  return(input.return)
}

hosp.los.input <- function(hosp.los){
  input.return <- sliderInput('hosp.los', hosp.los.wording, min = 1, max = 15, step = 1, 
                              value = hosp.los, width = '100%')
  
  return(input.return)
}

icu.los.input <- function(icu.los){
  input.return <- sliderInput('icu.los', icu.los.wording, min = 1, max = 15, step = 1,
                              value = icu.los, width = '100%')
  
  return(input.return)
}

vent.los.input <- function(vent.los){
  input.return <-sliderInput('vent.los', vent.los.wording, min = 1, max = 15, step = 1, 
                             value = vent.los, width = '100%')
  
  return(input.return)
  
}

save.parameter.action <- actionButton("save", params.save.msg)

##  ............................................................................
##  Model 2 Parameters
##  ............................................................................

trans.prob.slider <- function(inputId, label, value){
  input.return <- sliderInput(inputId, label, min = 0, max = 1, 
                                 value = value, step = 0.01, width = '100%')
  return(input.return)
}

##  ............................................................................
##  Graph selection inputs
##  ............................................................................

selected.graph.input <- radioGroupButtons(inputId = 'selected_graph', 
                                          label = '', 
                                          choices = c('Cases', 'Hospitalization', 'Hospital Resources'),
                                          justified = TRUE, 
                                          status = "primary")

selected.cases.input <- checkboxGroupInput(inputId = 'selected_cases', 
                                           label = 'Selected', 
                                           choices = c('Active', 'Resolved', 'Cases'), 
                                           selected = c('Active', 'Resolved', 'Cases'), 
                                           inline = TRUE)

selected.hosp.input <- checkboxGroupInput(inputId = 'selected_hosp', 
                                          label = 'Selected', 
                                          choices = c('Hospital', 'ICU', 'Ventilator'), 
                                          selected =  c('Hospital', 'ICU', 'Ventilator'), 
                                          inline = TRUE)

selected.res.input <- checkboxGroupInput(inputId = 'selected_res', 
                                         label = 'Selected', 
                                         choices = c('Hospital', 'ICU', 'Ventilator'), 
                                         selected =  c('Hospital', 'ICU', 'Ventilator'), 
                                         inline = TRUE)

go.left.button <- actionButton("goleft", "", icon = icon("arrow-left"), width = '100%')

go.right.button <- actionButton("goright", "", icon = icon("arrow-right"), width = '100%')

##  ............................................................................
##  Resource availability 
##  ............................................................................

hosp.cap.input <- function(hosp.avail){
  input.return <- numericInput(inputId = 'hosp_cap', 
                               label = avail.hosp.wording, 
                               value = hosp.avail)
  return(input.return)
}

icu.cap.input <- function(icu.avail){
  input.return <- numericInput(inputId = 'icu_cap', 
                               label = avail.icu.wording, 
                               value = icu.avail)
  return(input.return)
}

vent.cap.input <- function(vent.avail){
  input.return <- numericInput(inputId = 'vent_cap', 
                               label = avail.vent.wording , 
                               value = vent.avail)
  return(input.return)
}

  