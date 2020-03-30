# loading model functions
source('model2.R')
source('model2_helper.R')

# loading common helper functions
source('helper.R')

# loading language strings
source('wording.R')

# loading inputs
source('inputs.R')

# loading side UIs
source('sideuis.R')


# libraries
library(shiny)
library(ggplot2)
library(shinyWidgets)
library(data.table)
library(DT)
library(dplyr)
library(shinyjs)

# start simulation from this number of exposures
start.exp.default <- 1
r0.default <- 2.8
est.days <- 365


shinyServer(function(input, output, session) {
    
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
            prior.re.page(date.select)
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

        find.curr.estimates(S0 = input$num_people,
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
    
    output$intervention_ui <- renderUI({ 
        if (input$showint){
            intervention.ui(input$curr_date, params$hosp.delay.time)
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
    
    intervention.table <- reactiveVal(int.df.with.re)
    
    observeEvent(input$usedouble, {
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
        int.df <- int.df[,c('Date', 'New.Re', 'Days.of.Smoothing', 'Day')]
        colnames(int.df) <- c('Date', 'New Re', 'Days of Smoothing', 'Day')
        
        
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
            influx.ui(input$curr_date, params$hosp.delay.time)
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
        
        influx = list('day' = -1, num.influx = 0)

        if (input$input.metric == 'Hospitalizations'){
            # run the same model as initialization model but run extra days
            
            curr.day  <- as.numeric(curr.day.list()['curr.day'])
            new.num.days <- input$proj_num_days + curr.day
            
            new.num.days <- ifelse(is.na(new.num.days), 365, new.num.days)
            
            # starting conditions
            start.susc <- input$num_people - start.exp.default
            start.inf <- 0
            start.res <- 0
            
            if (input$showinflux == TRUE){
                if (length(input$influx_date > 0)){
                    influx.day <- input$influx_date - input$curr_date + curr.day
                    influx <- list(
                        'day' = influx.day, 
                        'num.influx' = input$num.influx
                    )
                }
            }
            
            SIR.df = SEIR(S0 = start.susc,
                          E0 = start.exp.default,
                          I0 = start.inf, 
                          R0 = start.res,
                          beta.vector = beta.vector(),
                          num.days = new.num.days, 
                          influx = influx,
                          params = params)
            
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
            influx <- list('day' = -1, num.influx = 0)

            SIR.df <- SEIR(S0 = start.susc, 
                           E0 = start.exp,
                           I0 = start.inf, 
                           R0 = start.res,
                           beta.vector = beta.vector(),
                           num.days = num.days, 
                           influx = influx,
                           params = params)
            
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
                    cases.graph.ui
                }
                else if (input$selected_graph == 'Hospitalization'){
                    hosp.graph.ui
                }
                else{
                    res.graph.ui(hosp.avail = params$hosp.avail, 
                                 icu.avail = params$icu.avail, 
                                 vent.avail = params$vent.avail)
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
    
    cases.df <- reactive({
        create.cases.df(sir.output.df = sir.output.df())
    })
    
    hospitalization.df <- reactive({
        create.hosp.df(sir.output.df = sir.output.df())
    })
    
    resource.df <- reactive({
        create.res.df(sir.output.df = sir.output.df(), 
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
        
        curr_date <- format(input$curr_date, format="%B %d, %Y")
        curr.day <- curr.day.list()['curr.day']
        curr.row <- sir.output.df()[sir.output.df()$day == curr.day,]
        
        if (input$input.metric == 'Hospitalizations'){
            infected <- round(curr.row$I + curr.row$E)

            cases <- round(curr.row$I + curr.row$R + curr.row$E)
            
            HTML(sprintf(curr.inf.est.wording, curr_date, cases, infected))
        }
        else{
            infected <- input$num_cases
            cases <- input$num_cases
            HTML(sprintf(curr.inf.known.wording, curr_date, infected, cases))
        }
    })
    
    # describing each timestep in words 
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
            df.output <- sir.output.df()
            write.csv(data.frame(df.output), file, row.names = FALSE)
        }
    )
}
)