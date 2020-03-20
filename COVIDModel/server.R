source('helper.R')

library(shiny)
library(ggplot2)
library(shinyWidgets)
library(data.table)

# start simulation from this number of infections
# TODO: should do a test that this works...if we start with a different start.inf
# are the results different? 
start.inf <- 1


shinyServer(function(input, output) {
    
    ##  ............................................................................
    ##  Selection of R0 or Doubling Time 
    ##  ............................................................................
    
    output$prediction_fld = renderUI({
        
        if (input$predict_metric == 'Hospitalization'){
            numericInput(inputId = 'num_hospitalized', 
                         label = 'Number Hospitalized from COVID-19 at Day 0', 
                         value = 10)
        }
        else if (input$predict_metric == 'ICU Patients'){
            numericInput(inputId = 'num_icu', 
                         label = 'Number in ICU for COVID-19 at Day 0', 
                         value = 2)
        }
        else{
            numericInput(inputId = 'num_hospitalized', 
                         label = 'Number Currently Hospitalized from COVID-19', 
                         value = 2)
        }
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
                        label = 'R0 Before Day 0', 
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
                        label = 'New R0 After Interventions', 
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
        hosp.rate = 0.15, 
        hosp.los = 7,
        icu.delay.time = 2, 
        icu.rate = 0.5, 
        icu.los = 9, 
        vent.delay.time = 1, 
        vent.rate = 0.5, 
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
        if (input$predict_metric == 'Hospitalization'){
            num.actual = input$num_hospitalized
        }
        else if (input$predict_metric == 'ICU Patients'){
            num.actual = input$num_icu
        }
        
        find.curr.estimates(S0 = input$num_people,
                            beta.vector = initial_beta_vector(), 
                            gamma = params$gamma, 
                            num.days = input$proj_num_days, 
                            num_actual = num.actual,
                            metric = input$predict_metric,
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
    ##  Projection 
    ##  ............................................................................
    
    beta.vector <- reactive({

        # determines what 'day' we are on using the initialization
        curr.day  <- as.numeric(curr.day.list()['curr.day'])
        
        # in the new projection, we want to project to a new number of days
        new.num.days <- input$proj_num_days + curr.day
        
        if (is.na(curr.day)){
            
            # TODO: hacky fix to bug
            curr.day = 365
            new.num.days = 1000
        }
        
        # setting doubling time
        if (input$usedouble == FALSE){
            doubling_time <- doubleTime(input$r0_prior, params$gamma)
            new_double <- doubleTime(input$r0_new, params$gamma)
        }
        else{
            doubling_time <- input$doubling_time
            new_double <- input$new_double
        }
        
        # creating a beta vector 
        beta <- getBeta(doubling_time, params$gamma, input$num_people)
        new_beta <- getBeta(new_double, params$gamma, input$num_people)
        beta_vec <- c(rep(beta, curr.day + input$int_day), 
                     rep(new_beta, new.num.days - curr.day - input$int_day))
        
        beta_vec
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
        
        SIR.df
    })
    
    
    ##  ............................................................................
    ##  Plot Outputs 
    ##  ............................................................................
    
    # UI depends on what graph is selected
    output$plot_output <- renderUI({
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
    })
    
    
    ##  ............................................................................
    ##  Dataframes for Visualization and Downloading  
    ##  ............................................................................
    
    cases.df <- reactive({
        df_temp <- sir.output.df()
        df_temp$Cases <- df_temp$I + df_temp$R
        df_temp$Active <- df_temp$I 
        df_temp$Resolved <- df_temp$R
        
        df_temp <- df_temp[,c('days.shift', 'Cases', 'Active', 'Resolved')]
        colnames(df_temp) <- c('days', 'Cases', 'Active', 'Resolved')
        df_temp
    })
    
    hospitalization.df <- reactive({
        df_temp <- sir.output.df()
        df_temp <- df_temp[,c('days.shift', 'hosp', 'icu', 'vent')]
        colnames(df_temp) <- c('days', 'Hospital', 'ICU', 'Ventilator')
        df_temp
    })
    
    resource.df = reactive({
        df_temp <- sir.output.df()
        
        df_temp$hosp <- input$hosp_cap - df_temp$hosp
        df_temp$icu <- input$icu_cap - df_temp$icu
        df_temp$vent <- input$vent_cap - df_temp$vent
        
        df_temp <- df_temp[,c('days.shift', 'hosp', 'icu', 'vent')]
        colnames(df_temp) <- c('days', 'Hospital', 'ICU', 'Ventilator')
        df_temp
    })
    
    ##  ............................................................................
    ##  Graphs   
    ##  ............................................................................
    
    plot_day <- reactiveVal(0)
    
    observeEvent(input$plot_click, {
        plot_day(round(input$plot_click$x))
    })
    
    output$hospitalization.plot <- renderPlot({
        df.to.plot <- hospitalization.df()
        
        if (length(input$selected_hosp) != 0){
            cols <- c('days', input$selected_hosp)
            
            df.to.plot <- df.to.plot[,..cols]
            
            df_melt <- melt(df.to.plot, 'days')
            
            ggplot(df_melt, aes(x = days, y = value, col = variable)) + geom_point() + geom_line(
            ) +  geom_vline(xintercept=0) + theme(text = element_text(size=20)
            ) +  geom_vline(xintercept=plot_day(), color = 'red')  + xlab('days')
        }
    })
    

    output$resource.plot <- renderPlot({
        df.to.plot <- resource.df()
        
        if (length(input$selected_res) != 0){
            cols <- c('days', input$selected_res)
            
            df.to.plot <- df.to.plot[,..cols]
            df_melt <- melt(df.to.plot, 'days')
            
            ggplot(df_melt, aes(x = days, y = value, col = variable)) + geom_point() + geom_line(
            ) +  geom_vline(xintercept=0) + theme(text = element_text(size=20)
            ) +  geom_vline(xintercept=plot_day(), color = 'red')  + xlab('days') + geom_hline(yintercept = 0) 
        }
    })
    
    output$cases.plot <- renderPlot({
        df.to.plot <- cases.df()
        
        if (length(input$selected_cases) != 0){
            cols <- c('days', input$selected_cases)
            df.to.plot <- df.to.plot[,..cols]
            df_melt <- melt(df.to.plot, 'days')
            
            ggplot(df_melt, aes(x = days, y = value, col = variable)) + geom_point(
            ) + geom_line() +  geom_vline(xintercept=0) + theme(text = element_text(size=20)
            ) +  geom_vline(xintercept=plot_day(), color = 'red') + xlab('days')
        }
    })
    
    
    ##  ............................................................................
    ##  Natural Language Outputs   
    ##  ............................................................................
    
    # Estimated number of infections
    output$infected_ct <- renderUI({
        infected <- curr.day.list()['infection.estimate']
        
        HTML(sprintf('<h3><b>Estimated Current Number of Active Infections</b>: %s</h3>', infected))
    })
    
    # Word description 
    output$description <- renderUI({
        
        df_temp <- sir.output.df()
        select.row <- df_temp[df_temp$days.shift == plot_day(),]
        
        if (input$selected_graph == 'Cases'){
            cases <- round(select.row$I + select.row$R)
            active <- floor(select.row$I)
            
            if (plot_day() == 0){
                HTML(sprintf('<h4>There are currently <b>%s COVID-19 cases</b> in the region, 
                             with <b>%s actively infected</b>.</h4>', 
                             cases, active))
            }
            else{
                HTML(sprintf('<h4>In <b>%s</b> days, there will be <b>%s COVID-19 cases</b> in the region, 
                             with <b>%s actively infected</b>.</h4>', 
                             plot_day(), cases, active))
            }
            
        }
        else if (input$selected_graph == 'Hospitalization'){
            hosp <- round(select.row$hosp)
            icu <- round(select.row$icu)
            vent <- round(select.row$vent)
            
            if (plot_day() == 0){
                HTML(sprintf('<h4>There are currently <b>%s hospitalized from COVID-19</b> in the region, 
                             with <b>%s in ICU care</b> and <b>%s on ventilators</b>.</h4>', 
                             hosp, icu, vent))
            }
            else{
                HTML(sprintf('<h4>In <b>%s</b> days, there will be <b>%s hospitalized from COVID-19</b> in the region, 
                             with <b>%s in ICU care</b> and <b>%s on ventilators</b>.</h4>', 
                             plot_day(), hosp, icu, vent))
            }
            
        }
        else{
            hosp_res <- input$hosp_cap - round(select.row$hosp)
            icu_res <- input$icu_cap - round(select.row$icu)
            vent_res <- input$vent_cap - round(select.row$vent)
            
            if (plot_day() == 0){
                HTML(sprintf('<h4>There are currently <b>%s hospital beds available</b> in the region, with 
                             <b>%s available ICU beds</b> and <b>%s available ventilators</b>.</h4>', 
                             hosp_res, icu_res, vent_res))
            }
            else{
                HTML(sprintf('<h4>In <b>%s</b> days, there will be <b>%s hospital beds available</b> in the region, 
                             with <b>%s available ICU beds</b> and <b>%s available ventilators</b>.</h4>', 
                             plot_day(), hosp_res, icu_res, vent_res))
            }
            
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