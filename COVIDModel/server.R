# loading model functions
source('models/model0.R')
source('models/model0_params.R')

# loading common helper functions
source('helper.R')

# loading language strings
source('wording.R')

# loading inputs
source('inputsAndPages.R')

# libraries
library(shiny)
library(ggplot2)
library(shinyWidgets)
library(data.table)
library(DT)
library(plyr)
library(dplyr)
library(shinyjs)

# start simulation from this number of exposures
start.exp.default <- 1
r0.default <- 2.8
est.days <- 365

shinyServer(function(input, output, session) {
    ##  ............................................................................
    ##  Bookmarking logic
    ##  ............................................................................
    
    setBookmarkExclude(c(
        # DT sends this, no reason to save it in bookmarks
        "int_table_rows_all", "int_table_rows_current", "int_table_search", "int_table_state", "int_table_cell_clicked",
        "rendered.table_rows_all", "rendered.table_rows_current", "rendered.table_rows_selected", "rendered.table_search", "rendered.table_state", "rendered.table_cell_clicked",
        # Buttons shouldn't be saved; they can cause problems on restore if their
        # stored values cause observeEvents to be triggered
        "parameters_modal", "add_intervention", "save", "goleft", "goright",
        "showint", "predict_re", "howtouse",
        "lastClick", "lastClickId"
    ))
    
    # Shiny takes care of restoring most inputs automatically, but state whose
    # "single source of truth" is on the server (in the form of reactiveVal and
    # reactiveValues, usually) need to be manually saved and restored.
    onBookmark(function(state) {
        state$values$params <- reactiveValuesToList(params)
        state$values$intervention.table <- intervention.table()
        state$values$plot_day <- plot_day()
    })
    onRestore(function(state) {
        mapply(function(name, value) {
            params[[name]] <- value
        }, names(state$values$params), state$values$params)
        plot_day(state$values$plot_day)
        # There are two complicating factors with restoring intervention.table
        # from bookmarked state.
        #
        # First, the bookmarked state doesn't contain type information, so Shiny
        # gives this value back to us as a list, not a data frame. Use
        # as.data.frame to turn it back into a data frame.
        #
        # Second, each individual column doesn't contain type information
        # either, so IF there weren't any rows to the intervention table, then
        # the columns are given back to us as NULL, which as.data.frame will
        # simply throw out; the resulting data frame as 0 columns. Forcing each
        # column to be numeric preserves the columns.
        intervention.table(as.data.frame(
            lapply(state$values$intervention.table, as.numeric)
        ))
    })
    onBookmarked(function(url) {
        # url <- shorten_url_somehow(url)
        showBookmarkUrlModal(url)
    })
    
    
    ##  ............................................................................
    ##  Helper Modal 
    ##  ............................................................................
    
    # modal pop-up helper screen
    observeEvent(input$howtouse,{
        showModal(about.page)
    })
    
    ##  ............................................................................
    ##  Prediction Field (Currently Only Hospitalizations and Cases) 
    ##  ............................................................................
    
    output$prediction_fld <- renderUI({
        if (input$input.metric == 'Hospitalizations'){
            numericInput(inputId = 'num_hospitalized', 
                         label = "Estimate of current inpatients with COVID-19:", 
                         value = 50)
        }
        else{
            numericInput(inputId = 'num_cases', 
                         label = "Estimate of number of cases of COVID-19:", 
                         value = 50)
        }
    })
    
    ##  ............................................................................
    ##  Selection of R0 or Doubling Time 
    ##  ............................................................................
    
    output$prior_val <- renderUI({
        
        date.select <- format(input$curr_date, format="%B %d")
        
        if (input$usedouble == TRUE){
            sliderInput(inputId = 'doubling_time', 
                        label = sprintf("Doubling Time (days) Before %s", date.select), 
                        min = 1, 
                        max = 12, 
                        step = 1, 
                        value = 6)
        }
        else{
            fluidPage(
                fluidRow(
                    sliderInput(inputId = 'r0_prior', 
                                label = sprintf("Re Before %s", date.select), 
                                min = 0.1, 
                                max = 7, 
                                step = 0.1, 
                                value = 2.8),
                    actionLink('predict_re', sprintf("Estimate Re prior to %s based on data.", date.select))
                )
            )
        }
    })
    
    output$int_val <- renderUI({
        if (input$usedouble == TRUE){
            sliderInput(inputId = 'new_double', 
                        label = "New Doubling Time (days) After Interventions", 
                        min = 0, 
                        max = 50, 
                        step = 1, 
                        value = 6)
        }
        else{
            sliderInput(inputId = 'r0_new', 
                        label = "New Re After Intervention", 
                        min = 0.1, 
                        max = 6, 
                        step = 0.1,
                        value = 2.8)
        }
    })
    
    ##  ............................................................................
    ##  Estimation of Re 
    ##  ............................................................................
    
    historical.df.blank <- data.frame('Date' = character(0),
                                      'Hospitalizations' = numeric(0),
                                      'Day' = numeric(0))
    
    re.estimates <- reactiveValues(
        graph = NULL,
        best.estimate = NULL
    )
    
    observeEvent(input$predict_re, {
        showModal(predict.re.page(input$curr_date))
    })
    
    hist.data <- reactiveVal(historical.df.blank)
    
    observeEvent(input$add.hist,{
        if (!as.character(input$date.hist) %in% as.character(hist.data()$Date) & 
            !is.na(input$num.hospitalized.hist)){
            
            new.hist <- add.to.hist.table(hist.data = hist.data(), 
                                          date.hist = input$date.hist, 
                                          num.hospitalized.hist = input$num.hospitalized.hist, 
                                          curr.date = input$curr_date)
            
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
                      zeroRecords = "No historical data added.",
                      search = "Find in table:"), dom = 't'), rownames = FALSE)
        
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
            best.fit <- findBestRe(N = input$num_people, 
                                   start.exp = start.exp.default,
                                   num.days = est.days, 
                                   day.vec = hist.temp$Day, 
                                   num_actual.vec = hist.temp$Hospitalizations,
                                   params = params)
            
            best.vals <- best.fit$best.vals
            
            df.graph <- data.frame(Date = hist.temp$Date, 
                                   Predicted = best.vals,
                                   Actual = hist.temp$Hospitalizations)
            
            df.melt <- melt(df.graph, id.vars = 'Date')
            
            re.estimates$graph <- ggplot(df.melt, aes(x = Date, y = value, col = variable)) + 
                geom_point() + geom_line() + theme(text = element_text(size=20)) + 
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
        hist.data(historical.df.blank)
    })
    
    ##  ............................................................................
    ##  Parameter selection 
    ##  ............................................................................
    
    # Markov Model selection
    observeEvent(input$model_select, {
        if (input$model_select == TRUE){
            source('models/model2.R')
            source('models/model2_params.R')

        }
        else{
            source('models/model0.R')
            source('models/model0_params.R')
        }

        default.params.list <- reactiveValuesToList(default.params)
        for (param in names(default.params.list)){
            params[[param]] = as.numeric(default.params.list[param])
        }
        
        # incredibly hacky way to deal with forcing the reactive graphs/tables to update
        num_people <- input$num_people
        updateNumericInput(session, "num_people", value = num_people + 1)
        updateNumericInput(session, "num_people", value = num_people)
    })
    
    # initializing a set of parameters  
    params <- default.params
    
    # modal pop-up to update parameters
    observeEvent(input$parameters_modal,{
        showModal(parameters.modal(params))
    })
    
    observeEvent(input$save, {
        params <- save.params(params, input)
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
        
        num.actual <- ifelse(is.null(input$num_cases), 
                             ifelse(is.null(input$num_hospitalized), 50, input$num_hospitalized),
                             input$num_cases)
        
        find.curr.estimates(N = input$num_people,
                            beta.vector = initial_beta_vector(), 
                            num.days = est.days, 
                            num.actual = num.actual,
                            metric = input$input.metric,
                            start.exp = start.exp.default,
                            params = params)
    })
    
    
    ##  ............................................................................
    ##  Interventions 
    ##  ............................................................................
    
    # blank intervention dataframes
    int.df.with.re <- data.frame('Day' = numeric(0),
                                 'New Re' = numeric(0), 
                                 'Days of Smoothing' =  numeric(0))
    
    int.df.with.double <- data.frame('Day' = numeric(0),
                                     'New Double Time' = numeric(0), 
                                     'Days of Smoothing' =  numeric(0))
    
    intervention.table <- reactiveVal(int.df.with.re)
    
    output$intervention_ui <- renderUI({ 
        if (input$showint){
            
            # date input differs based on whether Hospitalization or Cases are the input 
            if (input$input.metric == 'Hospitalizations'){
                int.date.input <- dateInput(inputId = 'int_date', 
                                            label = "Date Intervention is Implemented", 
                                            min = input$curr_date - params$hosp.delay.time, 
                                            value = input$curr_date)
            }
            else{
                int.date.input <- dateInput(inputId = 'int_date', 
                                            label = "Date Intervention is Implemented", 
                                            min = input$curr_date + 1, 
                                            value = input$curr_date + 1)
            }
            
            fluidPage(
                fluidRow(    
                    int.date.input,
                    uiOutput(outputId = 'int_val'),
                    sliderInput('smooth.int', 
                                label = "Smoothed over how many days?", 
                                value = 0, 
                                min = 0, 
                                max = 30),
                    actionButton(inputId = 'add_intervention', 
                                 label =  "Save Intervention")
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
    
    observeEvent(input$smooth.int, {
        params$int.smooth.days <- input$smooth.int
    })
    
    observeEvent(input$int_date, {
        params$int.new.num.days <- input$int_date - input$curr_date
    })
    
    observeEvent(input$usedouble, ignoreInit = TRUE, {
        if (input$usedouble == TRUE){
            intervention.table(int.df.with.double)
        }
        else{
            intervention.table(
                data.frame(int.df.with.re)
            )
        }
    })
    
    observeEvent(input$add_intervention,{
        
        if (!params$int.new.num.days %in% intervention.table()$Day){
            new.table <- bind.to.intervention(int.table = intervention.table(),
                                              params = params,
                                              usedouble = input$usedouble)
            
            intervention.table(new.table)
        }
        
        else{
            showNotification(double.int.warning, type = 'error')
        }
    })
    
    output$int_table <- renderDataTable({
        
        int.df <- intervention.table()
        
        int.df$Date <- int.df$Day + input$curr_date
        
        if (input$usedouble){
            int.df <- int.df[,c('Date', 'New.Double.Time', 'Days.of.Smoothing', 'Day')]
            colnames(int.df) <- c('Date', 'New Double Time', 'Days of Smoothing', 'Day')
        }
        else{
            int.df <- int.df[,c('Date', 'New.Re', 'Days.of.Smoothing', 'Day')]
            colnames(int.df) <- c('Date', 'New Re', 'Days of Smoothing', 'Day')
        }
        
        if (nrow(int.df) > 0){
            int.df[["Delete"]] <-
                paste0('
               <div class="btn-group" role="group" aria-label="">
               <button type="button" class="btn btn-secondary delete" id=delete', '_', int.df$Day, '>Delete</button>
               </div>
               ')
        }
        
        int.df$Day <- NULL
        
        datatable(int.df,
                  escape=F, selection = 'none',
                  options = list(pageLength = 10, language = list(
                      zeroRecords = "No interventions added.",
                      search = 'Find in table:'), dom = 't'), rownames = FALSE)
        
    })
    
    observeEvent(input$lastClick, {
        if (grepl('delete', input$lastClickId)){
            delete_day <- strsplit(input$lastClickId, '_')[[1]][2]
            intervention.table(intervention.table()[intervention.table()$Day != delete_day,])
        }
    })
    
    ##  ............................................................................
    ##  Influx of Infections 
    ##  ............................................................................
    
    output$influx_ui <- renderUI({ 
        
        if (input$showinflux){
            fluidPage(
                fluidRow(    
                    dateInput(inputId = 'influx_date', 
                              label = "Date of Influx", 
                              min = input$curr_date - params$hosp.delay.time, 
                              value = input$curr_date),
                    numericInput('num.influx', 
                                 label =  "Number of Infected Entering Region", 
                                 value = 0)
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
        
        # hacky way to deal with error at the startup 
        if (is.na(curr.day)){
            curr.day <- 365
            new.num.days <- 1000
        }
        
        # creating intervention table to create a beta vector
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
        
        create.beta.vec(int.table = int.table.temp, 
                        usedouble = input$usedouble,
                        gamma = params$gamma)
    })
    
    
    seir.output.df <- reactive({
        
        # get current day
        curr.day  <- as.numeric(curr.day.list()['curr.day'])
        
        # make influx list
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
        
        if (input$input.metric == 'Hospitalizations'){
            
            # run the same model as initialization model but run extra days
            new.num.days <- input$proj_num_days + curr.day
            new.num.days <- ifelse(is.na(new.num.days), 365, new.num.days)
            
            # starting conditions
            start.susc <- input$num_people - start.exp.default
            start.inf <- 0
            start.res <- 0
            
            seir.df = SEIR(S0 = start.susc,
                           E0 = start.exp.default,
                           I0 = start.inf, 
                           R0 = start.res,
                           beta.vector = beta.vector(),
                           num.days = new.num.days, 
                           influx = influx,
                           params = params)
            
            # shift the number of days to account for day 0 in the model 
            seir.df$days.shift <- seir.df$day - curr.day
            seir.df[seir.df$days.shift == 0,]$hosp <- input$num_hospitalized
        }
        else {
            
            num.cases <- ifelse(length(input$num_cases) != 0, input$num_cases, 0)
            start.susc <- input$num_people - num.cases
            start.exp <- 0
            start.inf <- num.cases 
            start.res <- 0 
            num.days <- input$proj_num_days
            
            seir.df <- SEIR(S0 = start.susc, 
                            E0 = start.exp,
                            I0 = start.inf, 
                            R0 = start.res,
                            beta.vector = beta.vector(),
                            num.days = num.days, 
                            influx = influx,
                            params = params)
            
            seir.df$days.shift <- seir.df$day
        }
        
        seir.df$date <- seir.df$days.shift + as.Date(input$curr_date)
        
        seir.df
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
                column(4, numericInput(inputId = 'hosp_cap', 
                                       label = "Hospital Bed Availability", 
                                       value = params$hosp.avail)),
                column(4,numericInput(inputId = 'icu_cap', 
                                      label = "ICU Space Availability", 
                                      value = params$icu.avail)),
                column(4,numericInput(inputId = 'vent_cap', 
                                      label = avail.vent.wording , 
                                      value = params$vent.avail)),
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
    
    cases.df <- reactive({
        create.cases.df(df = seir.output.df())
    })
    
    hospitalization.df <- reactive({
        create.hosp.df(df = seir.output.df())
    })
    
    resource.df <- reactive({
        create.res.df(df = seir.output.df(), 
                      hosp_cap = input$hosp_cap, 
                      icu_cap = input$icu_cap, 
                      vent_cap = input$vent_cap)
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
        create.graph(df.to.plot = hospitalization.df(), 
                     selected = input$selected_hosp, 
                     plot.day = plot_day(),
                     curr.date = input$curr_date)
    })
    
    
    output$resource.plot <- renderPlot({
        create.graph(df.to.plot = resource.df(),
                     selected = input$selected_res, 
                     plot.day = plot_day(),
                     curr.date = input$curr_date)
    })
    
    output$cases.plot <- renderPlot({
        create.graph(df.to.plot = cases.df(),
                     selected = input$selected_cases, 
                     plot.day = plot_day(),
                     curr.date = input$curr_date)
    })
    
    
    ##  ............................................................................
    ##  Natural Language Outputs   
    ##  ............................................................................
    
    # estimatated number of infections of "day 0"
    output$infected_ct <- renderUI({
        curr.date <- format(input$curr_date, format="%B %d, %Y")
        
        if (input$input.metric == 'Hospitalizations'){
            curr.day <- curr.day.list()['curr.day']
            curr.row <- seir.output.df()[seir.output.df()$day == curr.day,]
            
            infected <- round(curr.row$I + curr.row$E)
            cases <- round(curr.row$I + curr.row$R + curr.row$E)
            
            HTML(sprintf(curr.inf.est.wording, curr.date, cases, infected))
        }
        else{
            infected <- input$num_cases
            cases <- input$num_cases
            HTML(sprintf(curr.inf.known.wording, curr.date, infected, cases))
        }
    })
    
    # describing each timestep in words 
    output$description <- renderUI({
        
        df_temp <- seir.output.df()
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
            paste('Projections', '-', Sys.Date(), '.csv', sep='')
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
            
            # TODO: this is for testing only, uncomment and 
            # remove bottom lines after testing is done
            # write.csv(data.frame(data), file, row.names = FALSE)
            df.output <- seir.output.df()
            
            # model specific dataframe downloads 
            # process.df.for.download function is in model1.R or model2.R
            df.output <- process.df.for.download(df.output)

            # process parameters 
            gen.param.names <- c('Number of people in Area', 'Date') 
            gen.param.vals <- c(input$num_people, input$curr_date)
            
            # doubling time or Re 
            if (input$usedouble == TRUE){
                gen.param.names <- c(gen.param.names, 'Prior Doubling Time')
                gen.param.vals <- c(gen.param.vals, input$doubling_time)
            }
            else{
                gen.param.names <- c(gen.param.names, 'Prior Re')
                gen.param.vals <- c(gen.param.vals, input$r0_prior)
            }
            
            # hospitalizations or cases 
            if (input$input.metric == 'Hospitalizations'){
                gen.param.names <- c(gen.param.names, 'Initial Hospitalizations')
                gen.param.vals <- c(gen.param.vals, input$num_hospitalized)
            }
            else{
                gen.param.names <- c(gen.param.names, 'Initial Cases')
                gen.param.vals <- c(gen.param.vals, input$num_cases)
            }
            
            if (input$showinflux){
                gen.param.names <- c(gen.param.names, 'Influx Date', 'Influx Number')
                gen.param.vals <- c(gen.param.vals, input$influx.date, input$num_influx)
            }
            
            params.list <- process.params.for.download(params)
            
            params.df <- data.frame(PARAMETERS = rep(NA, length(c(gen.param.names, names(params.list)))),
                                    params = c(gen.param.names, names(params.list)),
                                  values = c(gen.param.vals, unlist(params.list)),
                                  INTERVENTIONS = rep(NA, length(c(gen.param.names, names(params.list)))))
            

            df.binded <- cbind.fill(df.output, params.df)
            
            if (nrow(intervention.table()) > 0){
                df.binded <- cbind.fill(df.output, params.df, intervention.table())
            }
            else{
                df.binded <- cbind.fill(df.output, params.df)
            }
            
            write.csv(data.frame(df.binded), file, row.names = FALSE, na = '')
        }
    )
}
)