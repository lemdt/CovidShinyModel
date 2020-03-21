source('helper.R')

library(shiny)
library(ggplot2)
library(shinyWidgets)
library(data.table)
library(DT)
library(dplyr)

# start simulation from this number of infections
# TODO: should do a test that this works...if we start with a different start.inf
# are the results different? 
start.inf <- 1
r0.default <- 2.8


shinyServer(function(input, output) {
    
    ##  ............................................................................
    ##  Selection of R0 or Doubling Time 
    ##  ............................................................................
    
    output$prediction_fld <- renderUI({
        
        numericInput(inputId = 'num_hospitalized', 
                     label = 'Estimate of current inpatients with COVID19 (diagnosed or not) on Day 0', 
                     value = 50)
    })
    
    ##  ............................................................................
    ##  Selection of R0 or Doubling Time 
    ##  ............................................................................
    
    output$prior_val <- renderUI({
        if (input$usedouble == TRUE){
            sliderInput(inputId = 'doubling_time', 
                        label = 'Doubling Time (days) Before Day 0', 
                        min = 1, 
                        max = 12, 
                        step = 1, 
                        value = 6)
        }
        else{
            sliderInput(inputId = 'r0_prior', 
                        label = 'Re Before Day 0', 
                        min = 0.1, 
                        max = 6, 
                        step = 0.1, 
                        value = 2.8)
        }
    })
    
    output$int_val <- renderUI({
        if (input$usedouble == TRUE){
            sliderInput(inputId = 'new_double', 
                        label = 'New Doubling Time (days) After Interventions', 
                        min = 0, 
                        max = 50, 
                        step = 1, 
                        value = 6)
        }
        else{
            sliderInput(inputId = 'r0_new', 
                        label = 'New Re After Intervention', 
                        min = 0.1, 
                        max = 6, 
                        step = 0.1,
                        value = 2.8)
        }
    })
    
    ##  ............................................................................
    ##  Parameter selection 
    ##  ............................................................................
    
    # initializing a set of parameters  
    params <- reactiveValues(
        illness.length = 14,
        gamma = 1/14,
        hosp.delay.time = 10, 
        hosp.rate = 0.06, 
        hosp.los = 11,
        icu.delay.time = 5, 
        icu.rate = 0.3, 
        icu.los = 8, 
        vent.delay.time = 1, 
        vent.rate = 0.64, 
        vent.los = 10
    )
    
    # modal pop-up to update parameters
    observeEvent(input$parameters_modal,{
        showModal(modalDialog(
            fluidPage(
                sliderInput('illness.length', 'Average Length of Illness', min = 0, max = 20, step = 1, 
                            value = params$illness.length, width = '100%'),
                
                sliderInput('hosp.rate', 'Percent Hospitalized Among Infections', min = 0, max = 1, step = 0.01, 
                            value = params$hosp.rate, width = '100%'),
                
                sliderInput('icu.rate', 'Percent ICU Admitted Among Hospitalized', min = 0, max = 1, step = 0.01, 
                            value = params$icu.rate, width = '100%'),
                
                sliderInput('vent.rate', 'Percent Ventilated Among ICU Admissions', min = 0, max = 1, step = 0.01, 
                            value = params$vent.rate, width = '100%'),
                
                sliderInput('hosp.after.inf', 'Infection to hospitalization (days)', min = 0, max = 30, step = 1, 
                            value = params$hosp.delay.time, width = '100%'),
                
                sliderInput('icu.after.hosp', 'Hospitalization to ICU Admission (days)', min = 0, max = 30, step = 1, 
                            value = params$icu.delay.time, width = '100%'),
                
                sliderInput('vent.after.icu', 'ICU Admission to Ventilation (days)', min = 0, max = 30, step = 1, 
                            value = params$vent.delay.time, width = '100%'),
                
                sliderInput('hosp.los', 'Hospital Length of Stay (days)', min = 5, max = 15, step = 1, 
                            value = params$hosp.los, width = '100%'),
                
                sliderInput('icu.los', 'ICU Length of Stay (days)', min = 5, max = 15, step = 1,
                            value = params$icu.los, width = '100%'),
                
                sliderInput('vent.los', 'Ventilation Course (days)', min = 5, max = 15, step = 1, 
                            value = params$vent.los, width = '100%')),
            footer = tagList(
                actionButton("save", "Save and Close")
            )
        )
        )
    })
    
    observeEvent(input$save, {
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
        removeModal()
    })
    
    
    ##  ............................................................................
    ##  Initialization 
    ##  ............................................................................
    
    
    initial_beta_vector <- reactive({
        
        if (input$usedouble == FALSE){
            doubling_time <- doubleTime(input$r0_prior, params$gamma)
        }
        else{
            doubling_time <- input$doubling_time
        }
        
        # calculates beta
        beta <- getBeta(doubling_time, params$gamma, input$num_people)
        
        initial.beta.vector <- rep(beta, input$proj_num_days)
        
        initial.beta.vector
    })
    
    
    curr.day.list <- reactive({
        
        predict.metric <- 'Hospitalization'
        num.actual <- input$num_hospitalized
        
        find.curr.estimates(S0 = input$num_people,
                            beta.vector = initial_beta_vector(), 
                            gamma = params$gamma, 
                            num.days = input$proj_num_days, 
                            num_actual = num.actual,
                            metric = predict.metric,
                            start.inf = start.inf,
                            hosp.delay.time = params$hosp.delay.time, 
                            hosp.rate = params$hosp.rate, 
                            hosp.los = params$hosp.los,
                            icu.delay.time = params$icu.delay.time, 
                            icu.rate = params$icu.rate, 
                            icu.los = params$icu.los,
                            vent.delay.time = params$vent.delay.time, 
                            vent.rate = params$vent.rate, 
                            vent.los = params$vent.los)
    })
    
    
    ##  ............................................................................
    ##  Interventions 
    ##  ............................................................................
    
    intervention.table <- reactiveVal(
        data.frame('Day' = numeric(0),
                   'New R0' = numeric(0))
    )
    
    observeEvent(input$usedouble, {
        if (input$usedouble == TRUE){
            intervention.table(
                data.frame('Day' = numeric(0),
                           'New Double Time' = numeric(0))
            )
        }
        else{
            intervention.table(
                data.frame('Day' = numeric(0),
                           'New R0' = numeric(0))
            )
        }
    })
    
    observeEvent(input$add_intervention,{
        
        if (input$usedouble == TRUE){
            intervention.table(rbind(intervention.table(),
                                     list('Day' = input$int_day, 
                                          'New.Double.Time' = input$new_double
                                     )))
        }
        else{
            intervention.table(rbind(intervention.table(),
                                         list('Day' = input$int_day, 
                                              'New.R0' = input$r0_new
                                         )))
        }
        intervention.table(arrange(intervention.table(), Day))
        removeModal()
    })
    
    observeEvent(input$cancel_int,{
        removeModal()
    })
    
    output$int_table <- renderDataTable({
        
        int.df <- intervention.table()
        
        if (nrow(int.df) > 0){
            int.df[["Delete"]] <-
                paste0('
               <div class="btn-group" role="group" aria-label="Basic example">
               <button type="button" class="btn btn-secondary delete" id=delete', '_', int.df$Day, '>Delete</button>
               </div>
               ')
            
        }
        
        datatable(int.df,
                   escape=F, selection = 'none',
                   options = list(pageLength = 5, language = list(
                       zeroRecords = "No interventions added.",
                       search = 'Find in table:'), dom = 't'), rownames = FALSE)
        
    })
    
    observeEvent(input$lastClick, {
        if (grepl('delete', input$lastClickId)){
            delete_day <- as.numeric(strsplit(input$lastClickId, '_')[[1]][2])
            intervention.table(intervention.table()[intervention.table()$Day != delete_day,])
        }
    })
    ##  ............................................................................
    ##  Projection 
    ##  ............................................................................
    
    beta.vector <- reactive({
        
        int.table.temp <- intervention.table()

        # determines what 'day' we are on using the initialization
        curr.day  <- as.numeric(curr.day.list()['curr.day'])
        
        if (is.na(curr.day)){
            # TODO: replace hacky fix to bug with non-hacky fix
            curr.day = 365
            new.num.days = 1000
        }
        
        # setting doubling time
        if (input$usedouble == FALSE){
            
            if (!is.null(input$r0_prior)){
                int.table.temp <- rbind(int.table.temp, 
                                        list(Day = c(-curr.day, input$proj_num_days, input$int_day),
                                             New.R0 = c(input$r0_prior, NA, input$r0_new)))
            }
            else{
                int.table.temp <- rbind(int.table.temp, 
                                        list(Day = c(-curr.day, input$proj_num_days),
                                             New.R0 = c(r0.default, NA)))
            }
            
            applyDoubleTime <- function(x){
                return(doubleTime(as.numeric(x['New.R0']), 
                                  params$gamma))
            }
            
            int.table.temp$New.Double.Time <- apply(int.table.temp, 1, applyDoubleTime)
        }
        else{
            int.table.temp <- rbind(int.table.temp, 
                                    list(Day = c(-curr.day, input$proj_num_days, input$int_day),
                                         New.Double.Time = c(input$doubling_time, NA, input$new_double)))
        }
        
        applygetBeta <- function(x){
            return(getBeta(as.numeric(x['New.Double.Time']), 
                           params$gamma, 
                           input$num_people))
        }
        
        int.table.temp$beta <- apply(int.table.temp, 1, applygetBeta)
        int.table.temp <- arrange(int.table.temp, Day)
        int.table.temp <- int.table.temp[!duplicated(int.table.temp$Day),]
        
        day.vec <- int.table.temp$Day
        rep.vec <- day.vec[2:length(day.vec)] - day.vec[1:length(day.vec) - 1]
        betas <- int.table.temp$beta[1:length(day.vec) - 1]

        beta.vec <- c()
        for (i in 1:length(rep.vec)){
            beta <- betas[i]
            reps <- rep.vec[i]
            beta.vec <- c(beta.vec, rep(beta, reps))
        }
        
        beta.vec
    })
    
    
    sir.output.df <- reactive({
        
        # run the same model as initialization model but run extra days
        curr.day  <- as.numeric(curr.day.list()['curr.day'])
        new.num.days <- input$proj_num_days + curr.day
        new.num.days <- ifelse(is.na(new.num.days), 365, new.num.days)
        
        # starting conditions
        start.susc <- input$num_people - start.inf
        start.res <- 0
        
        SIR.df = SIR(S0 = start.susc, 
                     I0 = start.inf, 
                     R0 = start.res,
                     beta.vector = beta.vector(),
                     gamma = params$gamma,
                     num.days = new.num.days, 
                     hosp.delay.time = params$hosp.delay.time,
                     hosp.rate = params$hosp.rate, 
                     hosp.los = params$hosp.los,
                     icu.delay.time = params$icu.delay.time, 
                     icu.rate = params$icu.rate, 
                     icu.los = params$icu.los,
                     vent.delay.time = params$vent.delay.time, 
                     vent.rate = params$vent.rate, 
                     vent.los = params$vent.los)
        
        # shift the number of days to account for day 0 in the model 
        SIR.df$days.shift <- SIR.df$day - curr.day
        
        SIR.df$date <- SIR.df$days.shift + as.Date(input$curr_date)
        
        SIR.df
    })
    
    
    ##  ............................................................................
    ##  Plot Outputs 
    ##  ............................................................................
    
    # UI depends on what graph is selected
    output$plot_output <- renderUI({
        
        if (!is.null(input$num_hospitalized)){
            if (!is.na(input$num_hospitalized)){
                if (input$selected_graph == 'Cases'){
                    fluidPage(
                        checkboxGroupInput(inputId = 'selected_cases', 
                                           label = 'Selected', 
                                           choices = c('Active', 'Resolved', 'Cases'), 
                                           selected = c('Active', 'Resolved', 'Cases'), 
                                           inline = TRUE),
                        
                        plotOutput(outputId = 'cases.plot', 
                                   click = "plot_click")
                    )
                }
                else if (input$selected_graph == 'Hospitalization'){
                    fluidPage(
                        checkboxGroupInput(inputId = 'selected_hosp', 
                                           label = 'Selected', 
                                           choices = c('Hospital', 'ICU', 'Ventilator'), 
                                           selected =  c('Hospital', 'ICU', 'Ventilator'), 
                                           inline = TRUE),
                        
                        plotOutput(outputId = 'hospitalization.plot', 
                                   click = "plot_click")
                    )
                }
                else{
                    fluidPage(
                        column(4,
                               numericInput(inputId = 'hosp_cap', 
                                            label = 'Hospital Bed Availability', 
                                            value = 1000)),
                        
                        column(4,
                               numericInput(inputId = 'icu_cap', 
                                            label = 'ICU Space Availability', 
                                            value = 200)),
                        
                        column(4,
                               numericInput(inputId = 'vent_cap', 
                                            label = 'Ventilator Availability', 
                                            value = 100)),
                        
                        fluidPage(checkboxGroupInput(inputId = 'selected_res', 
                                                     label = 'Selected', 
                                                     choices = c('Hospital', 'ICU', 'Ventilator'), 
                                                     selected =  c('Hospital', 'ICU', 'Ventilator'), 
                                                     inline = TRUE)),
                        
                        plotOutput(outputId = 'resource.plot', 
                                   click = "plot_click")
                    )
                }
            }
        }
    })
    
    
    ##  ............................................................................
    ##  Dataframes for Visualization and Downloading  
    ##  ............................................................................
    
    cases.df <- reactive({
        df_temp <- sir.output.df()
        df_temp$Cases <- df_temp$I + df_temp$R
        df_temp$Active <- df_temp$I 
        df_temp$Resolved <- df_temp$R
        
        df_temp <- df_temp[,c('date', 'Cases', 'Active', 'Resolved')]
        colnames(df_temp) <- c('date', 'Cases', 'Active', 'Resolved')
        df_temp
    })
    
    hospitalization.df <- reactive({
        df_temp <- sir.output.df()
        df_temp <- df_temp[,c('date', 'hosp', 'icu', 'vent')]
        colnames(df_temp) <- c('date', 'Hospital', 'ICU', 'Ventilator')
        df_temp
    })
    
    resource.df = reactive({
        df_temp <- sir.output.df()
        
        df_temp$hosp <- input$hosp_cap - df_temp$hosp
        df_temp$icu <- input$icu_cap - df_temp$icu
        df_temp$vent <- input$vent_cap - df_temp$vent
        
        df_temp <- df_temp[,c('date', 'hosp', 'icu', 'vent')]
        colnames(df_temp) <- c('date', 'Hospital', 'ICU', 'Ventilator')
        df_temp
    })
    
    ##  ............................................................................
    ##  Graphs   
    ##  ............................................................................
    
    plot_day <- reactiveVal(NULL)
    
    observeEvent(input$curr_date, {
        plot_day(input$curr_date)
    })
    
    observeEvent(input$plot_click, {
        plot_day(as.Date(round(input$plot_click$x), origin = "1970-01-01"))
    })
    
    output$hospitalization.plot <- renderPlot({
        df.to.plot <- hospitalization.df()
        
        if (length(input$selected_hosp) != 0){
            cols <- c('date', input$selected_hosp)
            
            df.to.plot <- df.to.plot[,..cols]
            
            df_melt <- melt(df.to.plot, 'date')
            
            ggplot(df_melt, aes(x = date, y = value, col = variable)) + geom_point() + geom_line(
            ) +  geom_vline(xintercept=input$curr_date) + theme(text = element_text(size=20)
            ) +  geom_vline(xintercept=plot_day(), color = 'red') 
        }
    })
    
    
    output$resource.plot <- renderPlot({
        df.to.plot <- resource.df()
        
        if (length(input$selected_res) != 0){
            cols <- c('date', input$selected_res)
            
            df.to.plot <- df.to.plot[,..cols]
            df_melt <- melt(df.to.plot, 'date')
            
            ggplot(df_melt, aes(x = date, y = value, col = variable)) + geom_point() + geom_line(
            ) +  geom_vline(xintercept=input$curr_date) + theme(text = element_text(size=20)
            ) +  geom_vline(xintercept=plot_day(), color = 'red') + geom_hline(yintercept = 0) 
        }
    })
    
    output$cases.plot <- renderPlot({
        df.to.plot <- cases.df()
        
        if (length(input$selected_cases) != 0){
            cols <- c('date', input$selected_cases)
            df.to.plot <- df.to.plot[,..cols]
            df_melt <- melt(df.to.plot, 'date')
            
            ggplot(df_melt, aes(x = date, y = value, col = variable)) + geom_point(
            ) + geom_line() +  geom_vline(xintercept=input$curr_date) + theme(text = element_text(size=20)
            ) +  geom_vline(xintercept=plot_day(), color = 'red')
        }
    })
    
    
    ##  ............................................................................
    ##  Natural Language Outputs   
    ##  ............................................................................
    
    # Estimated number of infections
    output$infected_ct <- renderUI({
        infected <- curr.day.list()['infection.estimate']
        
        curr_date <- format(input$curr_date, format="%B %d, %Y")
        
        HTML(sprintf('<h3><b>Estimated <u>%s</u> Active Infections on %s</b></h3>', infected, curr_date))
    })
    
    # Word description 
    output$description <- renderUI({
        
        df_temp <- sir.output.df()
        select.row <- df_temp[df_temp$date == plot_day(),]
        select.date <- format(select.row$date, format="%B %d, %Y")
        select.day <- select.row$days.shift
        
        if (input$selected_graph == 'Cases'){
            cases <- round(select.row$I + select.row$R)
            active <- floor(select.row$I)
            
            HTML(sprintf('<h4>On %s (in <b>%s</b> days), there will be <b>%s COVID-19 cases</b> in the region, 
                             with <b>%s actively infected</b>.</h4>', 
                         select.date, select.day, cases, active))
            
            
        }
        else if (input$selected_graph == 'Hospitalization'){
            hosp <- round(select.row$hosp)
            icu <- round(select.row$icu)
            vent <- round(select.row$vent)
            
            HTML(sprintf('<h4>On %s (in <b>%s</b> days), there will be <b>%s hospitalized from COVID-19</b> in the region, 
                             with <b>%s in ICU care</b> and <b>%s on ventilators</b>.</h4>', 
                         select.date, select.day, hosp, icu, vent))
            
            
        }
        else{
            hosp_res <- input$hosp_cap - round(select.row$hosp)
            icu_res <- input$icu_cap - round(select.row$icu)
            vent_res <- input$vent_cap - round(select.row$vent)
            
            HTML(sprintf('<h4>On %s (in <b>%s</b> days), there will be <b>%s hospital beds available</b> in the region, 
                             with <b>%s available ICU beds</b> and <b>%s available ventilators</b>.</h4>', 
                         select.date, select.day, hosp_res, icu_res, vent_res))
            
            
        }
    })
    
    ##  ............................................................................
    ##  Download Data   
    ##  ............................................................................
    output$downloadData <- downloadHandler(
        filename <- function() {
            paste(input$selected_graph, '-', Sys.Date(), '.csv', sep='')
        },
        content <- function(file) {
            if (input$selected_graph == 'Cases'){
                data <- cases.df()
            }
            else if (input$selected_graph == 'Hospitalization'){
                data <- hospitalization.df()
            }
            else{
                data <- resource.df()
            }
            write.csv(data.frame(data), file, row.names = FALSE)
        }
    )
}
)