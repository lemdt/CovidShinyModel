# loading helper functions
source('helper.R')

# loading model functions

# loading language strings
source('wording.R')

# loading inputs
source('inputs.R')

# libraries
library(shiny)
library(ggplot2)
library(shinyWidgets)
library(data.table)
library(DT)
library(dplyr)
library(shinyjs)

# start simulation from this number of infections
# TODO: should do a test that this works...if we start with a different start.inf
# are the results different? 
start.exp.default <- 1
r0.default <- 2.8
est.days <- 365


shinyServer(function(input, output, session) {
    
    ##  ............................................................................
    ##  Helper Modal 
    ##  ............................................................................
    
    # modal pop-up helper screen
    observeEvent(input$howtouse,{
        showModal(modalDialog(
            fluidPage(
                HTML(about.wording)
                
            ),
            size = 'l'
        )
        )
    })
    
    ##  ............................................................................
    ##  Prediction Field (Currently Only Hospitalizations) 
    ##  ............................................................................
    
    output$prediction_fld <- renderUI({
        
        if (input$input.metric == 'Hospitalizations'){
            num.hosp.input
        }
        else{
            num.cases.input
        }
    })
    
    ##  ............................................................................
    ##  Selection of R0 or Doubling Time 
    ##  ............................................................................
    
    output$prior_val <- renderUI({
        
        date.select <- format(input$curr_date, format="%B %d")

        if (input$usedouble == TRUE){
            double.time.input(date.select)
        }
        else{
            fluidPage(
                fluidRow(
                    r0.prior.input(date.select),
                    pred.re.action(date.select)
                )
            )
            
        }
    })
    
    output$int_val <- renderUI({
        if (input$usedouble == TRUE){
            int.double.input
        }
        else{
            int.re.input
        }
    })
    
    ##  ............................................................................
    ##  Estimation of Re 
    ##  ............................................................................
    
    re.estimates <- reactiveValues(
        graph = NULL,
        best.estimate = NULL
    )
    
    observeEvent(input$predict_re, {
        
        showModal(
            modalDialog(

                useShinyjs(),

                HTML(estimate.re.header),

                splitLayout(

                    date.hist.input(input$curr_date),

                    hist.hosp.input

                ),

                add.hist.action,

                dataTableOutput(
                    outputId = 'input_hosp_dt'
                ), 

                tags$script("$(document).on('click', '#input_hosp_dt button', function () {
                  Shiny.onInputChange('lastClickId',this.id);
                                             Shiny.onInputChange('lastClick', Math.random())
                                             });"),

                HTML('<br>'),

                run.fit.action,

                div(id = "predict.ui.toggle",
                    fluidPage(
                        uiOutput('best.re'),
                        plotOutput('fit.plot')
                    )

                ) %>% hidden()

            )
        )
        
    })
    
    hist.data <- reactiveVal(
        data.frame('Date' = character(0),
                   'Hospitalizations' = numeric(0),
                   'Day' = numeric(0))
    )
    

    observeEvent(input$add.hist,{
        if (!as.character(input$date.hist) %in% as.character(hist.data()$Date) & 
            !is.na(input$num.hospitalized.hist)){
            new.hist <- rbind(hist.data(),
                            list('Date' = as.character(input$date.hist), 
                                 'Hospitalizations' = input$num.hospitalized.hist, 
                                 'Day' = input$date.hist - input$curr_date
                                 ))
            
            new.hist <- arrange(new.hist, Day)
            new.hist$Date <- as.Date(as.character(new.hist$Date))
            hist.data(new.hist)
            updateDateInput(session, 
                            inputId = 'date.hist', 
                            value = input$date.hist - 1)
        }
        else if (as.character(input$date.hist) %in% as.character(hist.data()$Date))(
            showNotification(re.warning.date.repeat,
                             type = "error")
        )
        else{
            showNotification(re.warning.blank.num,
                             type = "error")
        }
    })
    
    output$input_hosp_dt <- renderDataTable({
        
        hist.dt <- hist.data()

        if (nrow(hist.dt) > 0){
            hist.dt[["Delete"]] <-
                paste0('
               <div class="btn-group" role="group" aria-label="">
               <button type="button" class="btn btn-secondary delete" id=delhist', '_', hist.dt$Day, '>Delete</button>
               </div>
               ')
            
        }
        hist.dt$Day <- NULL
        
        datatable(hist.dt,
                  escape=F, selection = 'none',
                  options = list(pageLength = 10, language = list(
                      zeroRecords = re.blank.table,
                      search = search.msg), dom = 't'), rownames = FALSE)
        
    })
    
    observeEvent(input$lastClick, {
        if (grepl('delhist', input$lastClickId)){
            delete_day <- as.numeric(strsplit(input$lastClickId, '_')[[1]][2])
            hist.data(hist.data()[hist.data()$Day != delete_day,])
        }
    })
    
    observeEvent(input$run.fit, {
        
        hist.temp <- hist.data()
        hist.temp <- arrange(hist.temp, desc(Date))
        
        if (nrow(hist.temp) >= 2){
            best.fit <- findBestRe(S0 = input$num_people, 
                                   gamma = params$gamma, 
                                   num.days = est.days, 
                                   day.vec = hist.temp$Day, 
                                   num_actual.vec = hist.temp$Hospitalizations,
                                   start.exp = start.exp.default,
                                   hosp.delay.time = params$hosp.delay.time, 
                                   hosp.rate = params$hosp.rate, 
                                   hosp.los = params$hosp.los,
                                   icu.delay.time = params$icu.delay.time, 
                                   icu.rate = params$icu.rate, 
                                   icu.los = params$icu.los,
                                   vent.delay.time = params$vent.delay.time, 
                                   vent.rate = params$vent.rate, 
                                   vent.los = params$vent.los)
            
            best.vals <- best.fit$best.vals
            
            df.graph <- data.frame(Date = hist.temp$Date, 
                                   Predicted = best.vals,
                                   Actual = hist.temp$Hospitalizations)
            
            df.melt <- melt(df.graph, id.vars = 'Date')
            
            re.estimates$graph <- ggplot(df.melt, aes(x = Date, y = value, col = variable)
            ) + geom_point() + geom_line() + theme(text = element_text(size=20)) +
                theme(legend.title=element_blank())
            
            re.estimates$best.estimate <- sprintf(best.re.msg, 
                                                  best.fit$best.re)
            
            show("predict.ui.toggle")
            
        }
        else{
            showNotification(re.warning.more.data,
                             type = "error")
        }
        
    })
    output$best.re <- renderUI({
       HTML(re.estimates$best.estimate)
    })
    
    output$fit.plot <- renderPlot({
        re.estimates$graph
    })
    
    observeEvent(input$curr_date, {
        hist.data(data.frame('Date' = character(0),
                             'Hospitalizations' = numeric(0),
                             'Day' = numeric(0)))
    })
    
    ##  ............................................................................
    ##  Parameter selection 
    ##  ............................................................................
    
    # initializing a set of parameters  
    params <- reactiveValues(
        illness.length = 14,
        gamma = 1/14,
        incubation.period = 5,
        sigma = 1/7,
        hosp.delay.time = 7, 
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
    
    # modal pop-up to update parameters
    observeEvent(input$parameters_modal,{
        showModal(modalDialog(
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
                save.parameter.action
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
        params$incubation.period = input$incubation.period
        params$sigma = 1/input$incubation.period
        removeModal()
    })
    
    
    ##  ............................................................................
    ##  Initialization 
    ##  ............................................................................
    
    
    initial_beta_vector <- reactive({
        
        if (input$usedouble == FALSE){
            beta <- getBetaFromRe(input$r0_prior, params$gamma)
        }
        else{
            beta <- getBetaFromDoubling(input$doubling_time, params$gamma)
        }

        initial.beta.vector <- rep(beta, est.days)
        
        initial.beta.vector
    })
    
    
    curr.day.list <- reactive({
        
        if (input$input.metric == 'Hospitalizations'){
            predict.metric <- 'Hospitalization'
            num.actual <- input$num_hospitalized
            
            find.curr.estimates(S0 = input$num_people,
                                beta.vector = initial_beta_vector(), 
                                sigma = params$sigma,
                                gamma = params$gamma, 
                                num.days = est.days, 
                                num_actual = num.actual,
                                metric = predict.metric,
                                start.exp = start.exp.default,
                                hosp.delay.time = params$hosp.delay.time, 
                                hosp.rate = params$hosp.rate, 
                                hosp.los = params$hosp.los,
                                icu.delay.time = params$icu.delay.time, 
                                icu.rate = params$icu.rate, 
                                icu.los = params$icu.los,
                                vent.delay.time = params$vent.delay.time, 
                                vent.rate = params$vent.rate, 
                                vent.los = params$vent.los)
        }
        else{
            curr.values <- list(
                curr.day = 0,
                infection.estimate = input$num_cases, 
                susceptible.estimate = input$num_people - input$num_cases, 
                recovered.estimate = 0
            )
            
            curr.values
        }
    })
    
    
    ##  ............................................................................
    ##  Interventions 
    ##  ............................................................................
    
    
    output$intervention_ui <- renderUI({ 
        
        if (input$showint){
            fluidPage(
                fluidRow(    
                    int.date.input(input$curr_date, params$hosp.delay.time),
                    uiOutput(outputId = 'int_val'),
                    smooth.int.input,
                    add.int.action
                    )
            )
        }
        
    })
    
    
    observeEvent(input$showint, {
        params$int.new.double <- input$doubling_time
        params$int.new.r0 <- input$r0_prior
        params$int.new.num.days <- 0
        params$int.smooth.days <- 0
        
    })

    observeEvent(input$smooth.int, {
        params$int.smooth.days <- input$smooth.int
    })
    
    observeEvent(input$doubling_time,{
        if(input$showint == FALSE){
            params$int.new.double <- input$doubling_time
        }
    })

    
    observeEvent(input$r0_prior,{
        if(input$showint == FALSE){
            params$int.new.r0 <- input$r0_prior
        }
    })

    
    observeEvent(input$new_double, {
        
        params$int.new.double <- input$new_double
        
    })
    
    
    observeEvent(input$r0_new, {
        
        params$int.new.r0 <- input$r0_new
        
    })
    
    observeEvent(input$int_date, {

        params$int.new.num.days <- input$int_date - input$curr_date

    })

    
    intervention.table <- reactiveVal(
        data.frame('Day' = numeric(0),
                   'New R0' = numeric(0), 
                   'Days of Smoothing' =  numeric(0))
    )
    
    observeEvent(input$usedouble, {
        if (input$usedouble == TRUE){
            intervention.table(
                data.frame('Day' = numeric(0),
                           'New Double Time' = numeric(0), 
                           'Days of Smoothing' =  numeric(0))
            )
        }
        else{
            intervention.table(
                data.frame('Day' = numeric(0),
                           'New Re' = numeric(0), 
                           'Days of Smoothing' =  numeric(0))
            )
        }
    })
    
    observeEvent(input$add_intervention,{
        
        if (!params$int.new.num.days %in% intervention.table()$Day){
            if (input$usedouble == TRUE){
                intervention.table(rbind(intervention.table(),
                                         list('Day' = params$int.new.num.days , 
                                              'New.Double.Time' = params$int.new.double, 
                                              'Days of Smoothing' = params$int.smooth.days
                                         )))
            }
            else{
                intervention.table(rbind(intervention.table(),
                                         list('Day' = params$int.new.num.days, 
                                              'New.Re' = params$int.new.r0,
                                              'Days of Smoothing' = params$int.smooth.days
                                         )))
            }
        
        intervention.table(arrange(intervention.table(), Day))
        }
        
        else{
            showNotification(double.int.warning, type = 'error')
        }
    })
    
    output$int_table <- renderDataTable({
        
        int.df <- intervention.table()
        
        int.df$Date <- int.df$Day + input$curr_date
        int.df <- int.df[,c('Date', 'New.Re', 'Days.of.Smoothing')]
        colnames(int.df) <- c('Date', 'New Re', 'Days of Smoothing')
        
        if (nrow(int.df) > 0){
            int.df[["Delete"]] <-
                paste0('
               <div class="btn-group" role="group" aria-label="Basic example">
               <button type="button" class="btn btn-secondary delete" id=delete', '_', int.df$Date, '>Delete</button>
               </div>
               ')
            
        }
        
        datatable(int.df,
                  escape=F, selection = 'none',
                  options = list(pageLength = 10, language = list(
                      zeroRecords = "No interventions added.",
                      search = 'Find in table:'), dom = 't'), rownames = FALSE)
        
    })
    
    observeEvent(input$lastClick, {
        if (grepl('delete', input$lastClickId)){
            delete_day <- as.numeric(strsplit(input$lastClickId, '_')[[1]][2])
            intervention.table(intervention.table()[intervention.table()$Date != delete_day,])
        }
    })
    
    ##  ............................................................................
    ##  Influx of Infections 
    ##  ............................................................................
    
    output$influx_ui <- renderUI({ 
        
        if (input$showinflux){
            fluidPage(
                fluidRow(    
                    influx.date.input(input$curr_date, params$hosp.delay.time),
                    num.influx.input
            )
            )
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
            curr.day <- 365
            new.num.days <- 1000
        }

        # setting doubling time
        if (input$usedouble == FALSE){
            
            if (!is.null(input$r0_prior) & !is.null(params$int.new.r0)){
                int.table.temp <- rbind(int.table.temp, 
                                        list(Day = c(params$int.new.num.days, -curr.day, input$proj_num_days ),
                                             New.Re = c(params$int.new.r0, input$r0_prior, NA ), 
                                             Days.of.Smoothing = c(params$int.smooth.days, 0, 0)))
            }
            else{
                int.table.temp <- rbind(int.table.temp, 
                                        list(Day = c(-curr.day, input$proj_num_days),
                                             New.Re = c(r0.default, NA),
                                             Days.of.Smoothing = c(0, 0)))
            }
            
        }
        else{
            int.table.temp <- rbind(int.table.temp, 
                                    list(Day = c(params$int.new.num.days, -curr.day, input$proj_num_days),
                                         New.Double.Time = c(params$int.new.double, input$doubling_time, NA ),
                                         Days.of.Smoothing = c(params$int.smooth.days, 0, 0)))
        }

        applygetBeta <- function(x){
            if (input$usedouble == FALSE){
                return(getBetaFromRe(as.numeric(x['New.Re']), 
                                     params$gamma))
            }
            else{
                return(getBetaFromDoubling(as.numeric(x['New.Double.Time']), 
                                           params$gamma))
            }
        }
        
        int.table.temp$beta <- apply(int.table.temp, 1, applygetBeta)
        int.table.temp <- arrange(int.table.temp, Day)
        int.table.temp <- int.table.temp[!duplicated(int.table.temp$Day),]
        
        day.vec <- int.table.temp$Day
        rep.vec <- day.vec[2:length(day.vec)] - day.vec[1:length(day.vec) - 1]
        betas <- int.table.temp$beta[1:length(day.vec) - 1]
        smooth.vec <- int.table.temp$Days.of.Smoothing
        
        beta.vec <- c()

        for (i in 1:length(rep.vec)){
            beta <- betas[i]
            reps <- rep.vec[i]
            smooth.days <- smooth.vec[i]
            actual.smooth.days <- min(reps, smooth.days)

            if (smooth.days > 0){
                beta.last <- betas[i-1]
                beta.diff <- beta - beta.last
                beta.step <- beta.diff / smooth.days
                
                if (beta.step != 0){
                    smooth.seq <- seq(beta.last+beta.step, beta, beta.step)
                    smooth.seq <- smooth.seq[1:actual.smooth.days]

                    if (smooth.days > reps){
                        betas[i] <- smooth.seq[actual.smooth.days]
                    }

                }
                else{
                    smooth.seq <- rep(beta, actual.smooth.days)
                }
                
                beta.vec <- c(beta.vec, smooth.seq)
            }
            
            beta.vec <- c(beta.vec, rep(beta, reps - actual.smooth.days))
        }

        beta.vec
        
    })
    
    
    sir.output.df <- reactive({
        
        if (input$input.metric == 'Hospitalizations'){
            # run the same model as initialization model but run extra days
            curr.day  <- as.numeric(curr.day.list()['curr.day'])
            new.num.days <- input$proj_num_days + curr.day
            new.num.days <- ifelse(is.na(new.num.days), 365, new.num.days)
            
            # starting conditions
            start.susc <- input$num_people - start.exp.default
            start.inf <- 0
            start.res <- 0
            
            # influx of infections
            
            influx = list('day' = -1, num.influx = 0)

            if (input$showinflux == TRUE){
                if (length(input$influx_date > 0)){
                    influx.day <- input$influx_date - input$curr_date + curr.day
                    influx <- list(
                        'day' = influx.day, 
                        'num.influx' = input$num.influx
                    )
                }
            }
            
            SIR.df = SIR(S0 = start.susc,
                         E0 = start.exp.default,
                         I0 = start.inf, 
                         R0 = start.res,
                         beta.vector = beta.vector(),
                         sigma = params$sigma, 
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
                         vent.los = params$vent.los, 
                         influx = influx)
            
            # shift the number of days to account for day 0 in the model 
            SIR.df$days.shift <- SIR.df$day - curr.day
            SIR.df[SIR.df$days.shift == 0,]$hosp <- input$num_hospitalized
        }
        else {
            
            num.cases <- ifelse(length(input$num_cases) != 0, input$num_cases, 0)
            start.susc <- input$num_people - num.cases
            start.exp <- 0
            start.inf <- num.cases 
            start.res <- 0 
            num.days <- input$proj_num_days

            SIR.df = SIR(S0 = start.susc, 
                         E0 = start.exp,
                         I0 = start.inf, 
                         R0 = start.res,
                         beta.vector = beta.vector(),
                         sigma = params$sigma, 
                         gamma = params$gamma,
                         num.days = num.days, 
                         hosp.delay.time = params$hosp.delay.time,
                         hosp.rate = params$hosp.rate, 
                         hosp.los = params$hosp.los,
                         icu.delay.time = params$icu.delay.time, 
                         icu.rate = params$icu.rate, 
                         icu.los = params$icu.los,
                         vent.delay.time = params$vent.delay.time, 
                         vent.rate = params$vent.rate, 
                         vent.los = params$vent.los)
            
            SIR.df$days.shift <- SIR.df$day
        }
        
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
                        selected.cases.input,
                        plotOutput(outputId = 'cases.plot', 
                                   click = "plot_click")
                    )
                }
                else if (input$selected_graph == 'Hospitalization'){
                    fluidPage(
                        selected.hosp.input,
                        plotOutput(outputId = 'hospitalization.plot', 
                                   click = "plot_click")
                    )
                }
                else{
                    fluidPage(
                        column(4, hosp.cap.input(params$hosp.avail)),
                        column(4,icu.cap.input(params$icu.avail)),
                        column(4,vent.cap.input(params$vent.avail)),
                        fluidPage(selected.res.input),
                        plotOutput(outputId = 'resource.plot', 
                                   click = "plot_click")
                    )
                }
            }
        }
    })
    
    observeEvent(input$hosp_cap, {
        params$hosp.avail <- input$hosp_cap
    })
    
    observeEvent(input$icu_cap, {
        params$icu.avail <- input$icu_cap
    })
    
    observeEvent(input$vent_cap, {
        params$vent.avail <- input$vent_cap
    })
    
    
    ##  ............................................................................
    ##  Dataframes for Visualization and Downloading  
    ##  ............................................................................
    
    roundNonDateCols <- function(df){
        
        df.new <- data.frame(df)
        
        for (col in colnames(df.new)){
            if (col != 'date'){
                df.new[,col] <- round(df.new[,col])
            }
        }
        return(df.new)
    }
    
    cases.df <- reactive({
        df_temp <- sir.output.df()
        df_temp <- df_temp[df_temp$days.shift >= 0,]
        
        df_temp$Cases <- df_temp$I + df_temp$R + df_temp$E
        df_temp$Active <- df_temp$I + df_temp$E
        df_temp$Resolved <- df_temp$R
        
        df_temp <- df_temp[,c('date', 'days.shift', 'Cases', 'Active', 'Resolved')]
        colnames(df_temp) <- c('date', 'day', 'Cases', 'Active', 'Resolved')
        df_temp <- roundNonDateCols(df_temp)
        df_temp
    })
    
    hospitalization.df <- reactive({
        df_temp <- sir.output.df()
        df_temp <- df_temp[df_temp$days.shift >= 0,]
        
        df_temp <- df_temp[,c('date', 'days.shift', 'hosp', 'icu', 'vent')]
        colnames(df_temp) <- c('date', 'day', 'Hospital', 'ICU', 'Ventilator')
        df_temp <- roundNonDateCols(df_temp)
        df_temp
    })
    
    resource.df <- reactive({
        df_temp <- sir.output.df()
        df_temp <- df_temp[df_temp$days.shift >= 0,]
        
        if (!is.null(input$hosp_cap)){
            df_temp$hosp <- input$hosp_cap - df_temp$hosp
            df_temp$icu <- input$icu_cap - df_temp$icu
            df_temp$vent <- input$vent_cap - df_temp$vent
        }
        
        df_temp <- df_temp[,c('date', 'days.shift', 'hosp', 'icu', 'vent')]
        colnames(df_temp) <- c('date', 'day', 'Hospital', 'ICU', 'Ventilator')
        df_temp <- roundNonDateCols(df_temp)
        df_temp
    })
    
    ##  ............................................................................
    ##  Table output   
    ##  ............................................................................
    
    output$rendered.table <- renderDataTable({
        if (input$selected_graph == 'Cases'){
            df.render <- cases.df()
        }
        else if (input$selected_graph == 'Hospitalization'){
            df.render <- hospitalization.df()
        }
        else{
            df.render <- resource.df()
        }
        
        df.render$date <- format(df.render$date, format="%B %d, %Y")
        
        datatable(data=df.render, 
                  escape=F, selection = 'single',
                  options = list(pageLength = 10, 
                                 lengthChange = FALSE,
                                 searching = FALSE), rownames = FALSE)
        
    })
    
    observeEvent(input$rendered.table_row_last_clicked,{
        row.id <- input$rendered.table_row_last_clicked
        
        if (input$selected_graph == 'Cases'){
            df.table = cases.df()
        }
        else if (input$selected_graph == 'Hospitalization'){
            df.table = hospitalization.df()
        }
        else{
            df.table = resource.df()
        }
        
        select.date <- df.table[row.id,'date']
        plot_day(select.date)
        
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
        
        proxy <- dataTableProxy(
            'rendered.table',
            session = shiny::getDefaultReactiveDomain(),
            deferUntilFlush = TRUE
        )
        
        selectRows(proxy, plot_day() - input$curr_date + 1)
        selectPage(proxy, ceiling((plot_day() - input$curr_date + 1) / 10))
        
    })
    
    observeEvent(input$goright, {
        if (plot_day() != input$curr_date + input$proj_num_days){
            plot_day(plot_day() + 1)
            
            proxy <- dataTableProxy(
                'rendered.table',
                session = shiny::getDefaultReactiveDomain(),
                deferUntilFlush = TRUE
            )
            
            selectRows(proxy, plot_day() - input$curr_date + 1)
            selectPage(proxy, ceiling((plot_day() - input$curr_date + 1) / 10))
        }
        
    })
    
    observeEvent(input$goleft, {
        if (plot_day() != input$curr_date){
            plot_day(plot_day() - 1)
            
            proxy <- dataTableProxy(
                'rendered.table',
                session = shiny::getDefaultReactiveDomain(),
                deferUntilFlush = TRUE
            )
            
            selectRows(proxy, plot_day() - input$curr_date + 1)
            selectPage(proxy, ceiling((plot_day() - input$curr_date + 1) / 10))
        }
        
    })
    
    
    output$hospitalization.plot <- renderPlot({
        df.to.plot <- hospitalization.df()
        df.to.plot$day <- NULL
        
        if (length(input$selected_hosp) != 0){
            cols <- c('date', input$selected_hosp)
            
            df.to.plot <- df.to.plot[,cols]
            
            df_melt <- melt(df.to.plot, 'date')
            
            ggplot(df_melt, aes(x = date, y = value, col = variable)) + geom_point() + geom_line(
            ) +  geom_vline(xintercept=input$curr_date) + theme(text = element_text(size=20)
            ) +  geom_vline(xintercept=plot_day(), color = 'red') + ylab('')
        }
    })
    
    
    output$resource.plot <- renderPlot({
        df.to.plot <- resource.df()
        df.to.plot$day <- NULL
        
        if (length(input$selected_res) != 0){
            cols <- c('date', input$selected_res)
            
            df.to.plot <- df.to.plot[,cols]
            df_melt <- melt(df.to.plot, 'date')
            
            ggplot(df_melt, aes(x = date, y = value, col = variable)) + geom_point() + geom_line(
            ) +  geom_vline(xintercept=input$curr_date) + theme(text = element_text(size=20)
            ) +  geom_vline(xintercept=plot_day(), color = 'red') + geom_hline(yintercept = 0) + ylab('') 
        }
    })
    
    output$cases.plot <- renderPlot({
        df.to.plot <- cases.df()
        df.to.plot$day <- NULL
        
        if (length(input$selected_cases) != 0){
            cols <- c('date', input$selected_cases)
            df.to.plot <- df.to.plot[,cols]
            df_melt <- melt(df.to.plot, 'date')
            
            ggplot(df_melt, aes(x = date, y = value, col = variable)) + geom_point(
            ) + geom_line() +  geom_vline(xintercept=input$curr_date) + theme(text = element_text(size=20)
            ) +  geom_vline(xintercept=plot_day(), color = 'red') + ylab('')
        }
    })
    
    
    ##  ............................................................................
    ##  Natural Language Outputs   
    ##  ............................................................................
    
    # Estimated number of infections
    output$infected_ct <- renderUI({
        
        curr_date <- format(input$curr_date, format="%B %d, %Y")
        curr.day <- curr.day.list()['curr.day']
        
        if (input$input.metric == 'Hospitalizations'){
            infected <- round(sir.output.df()[sir.output.df()$day == curr.day,]$I + 
                                  sir.output.df()[sir.output.df()$day == curr.day,]$E)
            cases <- round(sir.output.df()[sir.output.df()$day == curr.day,]$I + 
                sir.output.df()[sir.output.df()$day == curr.day,]$R + 
                    sir.output.df()[sir.output.df()$day == curr.day,]$E)
            
            HTML(sprintf(curr.inf.est.wording, curr_date, cases, infected))
        }
        else{
            infected <- input$num_cases
            cases <- input$num_cases
            HTML(sprintf(curr.inf.known.wording, curr_date, infected, cases))
        }
    })
    
    # Word description 
    output$description <- renderUI({
        
        df_temp <- sir.output.df()
        select.row <- df_temp[df_temp$date == plot_day(),]
        select.date <- format(select.row$date, format="%B %d, %Y")
        select.day <- select.row$days.shift
        
        if (input$selected_graph == 'Cases'){
            cases <- round(select.row$I + select.row$R + select.row$E)
            active <- floor(select.row$I + select.row$E)
            
            if (length(select.day) != 0){
                if (select.day == 0){
                    HTML(sprintf(cases.curr.wording, select.date, cases, active))
                }
                else{
                    HTML(sprintf(cases.fut.wording, select.date, cases, active))
                }
            }
            
            
        }
        else if (input$selected_graph == 'Hospitalization'){
            hosp <- round(select.row$hosp)
            icu <- round(select.row$icu)
            vent <- round(select.row$vent)
            
            if (select.day == 0){
                HTML(sprintf(hosp.curr.wording, select.date, hosp, icu, vent))
            }
            else{
                HTML(sprintf(hosp.fut.wording, select.date, hosp, icu, vent))
            }
            
            
        }
        else{
            hosp_res <- input$hosp_cap - round(select.row$hosp)
            icu_res <- input$icu_cap - round(select.row$icu)
            vent_res <- input$vent_cap - round(select.row$vent)
            
            if (select.day == 0){
                HTML(sprintf(res.curr.wording, select.date, hosp_res, icu_res, vent_res))
            }
            else{
                HTML(sprintf(res.fut.wording, select.date, hosp_res, icu_res, vent_res))
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
            # write.csv(data.frame(data), file, row.names = FALSE)
            df.output <- sir.output.df()[,c('day', 'days.shift', 'date',
                                            'S', 'E', 'I', 'R', 'new.infections',
                                            'admit.hosp', 'discharge.hosp', 'hosp', 
                                            'admit.icu', 'discharge.icu', 'icu',
                                            'admit.vent', 'discharge.vent', 'vent')]
            write.csv(data.frame(df.output), file, row.names = FALSE)
        }
    )
}
)