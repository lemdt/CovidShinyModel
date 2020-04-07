utils::globalVariables("SEIR")


#' Server part
#' @import shiny
M0 <- new.env()
source('R/models/model0.R', local = M0)
source('R/models/model0_params.R', local = M0)

M2 <- new.env()
source('R/models/model2.R', local = M2)
source('R/models/model2_params.R', local = M2)

# start simulation from this number of exposures
start.exp.default <- 1
r0.default <- 2.8
est.days <- 365

server <- function(input, output, session) {
    model <- reactiveVal(M0)

    # initializing a set of parameters
    params <- reactiveValues()
    
    for (model_param in names(M0$default.params)){
        params[[model_param]] = M0$default.params[[model_param]]
    }

    frozen_lines <- reactiveVal(NULL)

    ##  ............................................................................
    ##  Bookmarking logic
    ##  ............................................................................

    setBookmarkExclude(
        c(
            # DT sends this, no reason to save it in bookmarks
            "int_table_rows_all",
            "int_table_rows_current",
            "int_table_search",
            "int_table_state",
            "int_table_cell_clicked",
            "rendered.table_rows_all",
            "rendered.table_rows_current",
            "rendered.table_rows_selected",
            "rendered.table_search",
            "rendered.table_state",
            "rendered.table_cell_clicked",
            # Buttons shouldn't be saved; they can cause problems on restore if their
            # stored values cause observeEvents to be triggered
            "parameters_modal",
            "add_intervention",
            "save",
            "goleft",
            "goright",
            "showint",
            "howtouse",
            "lastClick",
            "lastClickId"
        )
    )

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
        },
        names(state$values$params),
        state$values$params)
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
        intervention.table(as.data.frame(lapply(
            state$values$intervention.table, as.numeric
        )))
    })
    onBookmarked(function(url) {
        url <- urlshorteneR::isgd_LinksShorten(url)
        showBookmarkUrlModal(url)
    })


    ##  ............................................................................
    ##  Helper Modal
    ##  ............................................................................

    # modal pop-up helper screen
    observeEvent(input$howtouse, {
        showModal(modalDialog(HTML(about.wording), size = "l", easyClose = TRUE))
    })

    ##  ............................................................................
    ##  Selection of R0 or Doubling Time
    ##  ............................................................................

    observeEvent(input$curr_date, {
        date.select <- format(input$curr_date, format = "%B %d")
        updateSliderInput(session, 'doubling_time',
                          label = sprintf("Doubling Time (days) Before %s", date.select))
        updateSliderInput(session, 'r0_prior',
                          label = sprintf("Basic reproductive number R0 before %s", date.select))
    })

    ##  ............................................................................
    ##  Estimation of Re
    ##  ............................................................................

    re.estimates <- reactiveValues(graph = NULL,
                                   best.estimate = NULL)


    historical.df.blank <- data.frame(
        'Date' = character(0),
        'Hospitalizations' = numeric(0),
        'Day' = numeric(0)
    )

    hist.data <- reactiveVal(historical.df.blank)

    observeEvent(input$add.hist, {
        if (!as.character(input$date.hist) %in% as.character(hist.data()$Date) &
            !is.na(input$num.hospitalized.hist)) {
            new.hist <- add.to.hist.table(
                hist.data = hist.data(),
                date.hist = input$date.hist,
                num.hospitalized.hist = input$num.hospitalized.hist,
                curr.date = input$curr_date
            )

            hist.data(new.hist)

            updateDateInput(session,
                            inputId = 'date.hist',
                            value = input$date.hist - 1)

        }
        else if (as.character(input$date.hist) %in% as.character(hist.data()$Date))
            (
                showNotification(re.warning.date.repeat,
                                 type = "error")
            )
        else{
            showNotification(re.warning.blank.num,
                             type = "error")
        }
    })

    output$input_hosp_dt <- DT::renderDataTable({
        hist.dt <- hist.data()

        if (nrow(hist.dt) > 0) {
            hist.dt[["Delete"]] <-
                paste0(
                    '
               <div class="btn-group" role="group" aria-label="">
               <button type="button" class="btn btn-secondary delete" id=delhist',
                    '_',
                    hist.dt$Day,
                    '>Delete</button>
               </div>
               '
                )

        }
        hist.dt$Day <- NULL

        DT::datatable(
            hist.dt,
            escape = FALSE,
            selection = 'none',
            options = list(
                pageLength = 10,
                language = list(zeroRecords = "No historical data added.",
                                search = "Find in table:"),
                dom = 't'
            ),
            rownames = FALSE
        )

    })

    observeEvent(input$lastClick, {
        if (grepl('delhist', input$lastClickId)) {
            delete_day <- as.numeric(strsplit(input$lastClickId, '_')[[1]][2])
            hist.data(hist.data()[hist.data()$Day != delete_day, ])
        }
    })

    observeEvent(input$run.fit, {
        hist.temp <- hist.data()
        hist.temp <- dplyr::arrange(hist.temp, dplyr::desc(Date))

        if (nrow(hist.temp) >= 2) {
            best.fit <- findBestRe(
                seir_func = model()$SEIR,
                N = input$num_people,
                start.exp = start.exp.default,
                num.days = est.days,
                day.vec = hist.temp$Day,
                num_actual.vec = hist.temp$Hospitalizations,
                params = params
            )

            best.vals <- best.fit$best.vals

            df.graph <- data.frame(
                Date = hist.temp$Date,
                Predicted = best.vals,
                Actual = hist.temp$Hospitalizations
            )

            df.melt <- tidyr::pivot_longer(
              df.graph,
              -Date,
              names_to = "variable"
            )

            re.estimates$graph <- re_estimate_plot(df.melt)

            re.estimates$best.estimate <- sprintf(best.re.msg,
                                                  best.fit$best.re)

            shinyjs::show("predict.ui.toggle")

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

    # For the release, we are removing the Markov Model switch for now. 
    # This is currently a beta feature only for internal use. 
    # Markov Model selection
    # observeEvent(input$model_select, ignoreInit = TRUE, {
    #     if (input$model_select == TRUE) {
    #         model(M2)
    #     } else {
    #         model(M0)
    #     }
    # 
    #     default.params.list <- reactiveValuesToList(model()$default.params)
    #     for (param in names(default.params.list)) {
    #         params[[param]] = as.numeric(default.params.list[param])
    #     }
    # 
    #     # incredibly hacky way to deal with forcing the reactive graphs/tables to update
    #     num_people <- input$num_people
    #     updateNumericInput(session, "num_people", value = num_people + 1)
    #     updateNumericInput(session, "num_people", value = num_people)
    # })

    output$params_ui <- renderUI({

        div(
            HTML("<br><h4><b>Parameters</b></h4><br>"),
            isolate(model())$parameters.page(params)
        )
    })

    observeEvent(input$illness.length,{
        params$illness.length <- input$illness.length
        params$gamma.r <- 1 / input$illness.length
        params$gamma <- ((1 - input$hosp.rate) * 1 / input$illness.length) +
            (input$hosp.rate * 1 / input$hosp.after.inf)
    })

    observeEvent(input$hosp.after.inf,{
        params$inf.to.hosp <- input$hosp.after.inf
        params$gamma.h <- 1 / input$hosp.after.inf
        params$gamma <- ((1 - input$hosp.rate) * 1 / input$illness.length) +
            (input$hosp.rate * 1 / input$hosp.after.inf)

    })

    observeEvent(input$incubation.period,{
        params$incubation.period <- input$incubation.period
        params$sigma <- 1 / input$incubation.period
    })

    observeEvent(input$hosp.los,{
        params$hosp.los <- input$hosp.los
        params$psi <- 1 / input$hosp.los
    })

    observeEvent(input$hosp.rate, {
        params$hosp.rate <- input$hosp.rate
        params$gamma <- ((1 - input$hosp.rate) * 1 / input$illness.length) +
            (input$hosp.rate * 1 / input$hosp.after.inf)
    })

    observeEvent(input$icu.rate, {
        params$icu.rate <- input$icu.rate
    })

    observeEvent(input$vent.rate, {
        params$vent.rate <- input$vent.rate
    })

    observeEvent(input$p.g_icu, {
        params$p.g_icu = input$p.g_icu
        params$p.g_g = 1 - input$p.g_d - input$p.g_icu
    })

    observeEvent(input$p.g_d, {
        params$p.g_d = input$p.g_d
        params$p.g_g = 1 - input$p.g_d - input$p.g_icu
    })

    observeEvent(input$p.icu_g, {
        params$p.icu_g = input$p.icu_g
        params$p.icu_icu = 1 - input$p.icu_g - input$p.icu_v
    })

    observeEvent(input$p.icu_v, {
        params$p.icu_v = input$p.icu_v
        params$p.icu_icu = 1 - input$p.icu_g - input$p.icu_v
    })

    observeEvent(input$p.v_icu, {
        params$p.v_icu = input$p.v_icu
        params$p.v_v = 1 - input$p.v_icu - input$p.v_m
    })

    observeEvent(input$p.v_m, {
        params$p.v_m = input$p.v_m
        params$p.v_v = 1 - input$p.v_icu - input$p.v_m
    })


    ##  ............................................................................
    ##  Initialization
    ##  ............................................................................


    initial_beta_vector <- reactive({
        if (input$usedouble == FALSE) {
            beta <- getBetaFromRe(input$r0_prior, params$gamma)
        }
        else{
            beta <- getBetaFromDoubling(input$doubling_time, params$gamma)
        }

        initial.beta.vector <- rep(beta, est.days)
        initial.beta.vector
    })

    curr.day.list <- reactive({
        find.curr.estimates(
            seir_func = isolate(model())$SEIR,
            N = input$num_people,
            beta.vector = initial_beta_vector(),
            num.days = est.days,
            num.actual = input$num_hospitalized,
            metric = "Hospitalizations",
            start.exp = start.exp.default,
            params = params
        )
    })


    ##  ............................................................................
    ##  Interventions
    ##  ............................................................................

    # blank intervention dataframes
    int.df.with.re <- data.frame(
        'Day' = numeric(0),
        'New Re' = numeric(0),
        'Days of Smoothing' =  numeric(0)
    )

    int.df.with.double <- data.frame(
        'Day' = numeric(0),
        'New Double Time' = numeric(0),
        'Days of Smoothing' =  numeric(0)
    )

    intervention.table <- reactiveVal(int.df.with.re)

    observe({
        min <- input$curr_date
        val <- input$curr_date
        updateDateInput(session, "int_date", min = min, value = val)
    })

    observeEvent(input$showint, {
        params$int.new.double <- input$doubling_time
        params$int.new.r0 <- input$r0_prior
        params$int.new.num.days <- 0
        params$int.smooth.days <- 0

    })

    observeEvent(input$doubling_time, {
        if (input$showint == FALSE) {
            params$int.new.double <- input$doubling_time
        }
    })

    observeEvent(input$r0_prior, {
        if (input$showint == FALSE) {
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
        if (input$usedouble == TRUE) {
            intervention.table(int.df.with.double)
        }
        else{
            intervention.table(data.frame(int.df.with.re))
        }
    })

    observeEvent(input$add_intervention, {
        if (!params$int.new.num.days %in% intervention.table()$Day) {
            new.table <- bind.to.intervention(
                int.table = intervention.table(),
                params = reactiveValuesToList(params),
                usedouble = input$usedouble
            )

            intervention.table(new.table)
        }

        else{
            showNotification(double.int.warning, type = 'error')
        }
    })

    output$int_table <- DT::renderDataTable({
        int.df <- intervention.table()

        int.df$Date <- int.df$Day + input$curr_date

        if (input$usedouble) {
            int.df <-
                int.df[, c('Date',
                           'New.Double.Time',
                           'Days.of.Smoothing',
                           'Day')]
            colnames(int.df) <-
                c('Date', 'New Double Time', 'Days of Smoothing', 'Day')
        }
        else{
            int.df <- int.df[, c('Date', 'New.Re', 'Days.of.Smoothing', 'Day')]
            colnames(int.df) <-
                c('Date', 'New Re', 'Days of Smoothing', 'Day')
        }

        if (nrow(int.df) > 0) {
            int.df[["Delete"]] <-
                paste0(
                    '
               <div class="btn-group" role="group" aria-label="">
               <button type="button" class="btn btn-secondary delete" id=delete',
                    '_',
                    int.df$Day,
                    '>Delete</button>
               </div>
               '
                )
        }

        int.df$Day <- NULL

        DT::datatable(
            int.df,
            escape = FALSE,
            selection = 'none',
            options = list(
                pageLength = 10,
                language = list(zeroRecords = "No interventions added.",
                                search = 'Find in table:'),
                dom = 't'
            ),
            rownames = FALSE
        )

    })

    observeEvent(input$lastClick, {
        if (grepl('delete', input$lastClickId)) {
            delete_day <- strsplit(input$lastClickId, '_')[[1]][2]
            intervention.table(intervention.table()[intervention.table()$Day != delete_day, ])
        }
    })

    ##  ............................................................................
    ##  Influx of Infections
    ##  ............................................................................

    output$influx_ui <- renderUI({
        if (input$showinflux) {
            div(
                dateInput(
                    inputId = 'influx_date',
                    label = "Date of Influx",
                    min = input$curr_date,
                    value = input$curr_date
                ),
                numericInput('num.influx',
                             label =  "Number of Infected Entering Region",
                             value = 0)
                
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
        if (is.na(curr.day)) {
            curr.day <- 365
            new.num.days <- 1000
        }

        # creating intervention table to create a beta vector
        if (input$usedouble == FALSE) {
            if (!is.null(input$r0_prior) && !is.null(params$int.new.r0)) {
                int.table.temp <- rbind(list(
                                            Day = c(
                                                params$int.new.num.days,
                                                -curr.day,
                                                input$proj_num_days
                                            ),
                                            New.Re = c(params$int.new.r0, input$r0_prior, NA),
                                            Days.of.Smoothing = c(params$int.smooth.days, 0, 0)
                                        ),
                                        int.table.temp)
            }
            else{
                int.table.temp <- rbind(list(
                                            Day = c(-curr.day, input$proj_num_days),
                                            New.Re = c(r0.default, NA),
                                            Days.of.Smoothing = c(0, 0)
                                        ),
                                        int.table.temp)
            }

        }
        else{
            int.table.temp <- rbind(
                list(
                    Day = c(
                        params$int.new.num.days,
                        -curr.day,
                        input$proj_num_days
                    ),
                    New.Double.Time = c(params$int.new.double, input$doubling_time, NA),
                    Days.of.Smoothing = c(params$int.smooth.days, 0, 0)
                ),
                int.table.temp
            )
        }

        create.beta.vec(
            int.table = int.table.temp,
            usedouble = input$usedouble,
            gamma = params$gamma
        )
    })


    seir.output.df <- reactive({
        # get current day
        curr.day  <- as.numeric(curr.day.list()['curr.day'])

        # make influx list
        influx = list('day' = -1, num.influx = 0)

        if (input$showinflux == TRUE) {
            if (length(input$influx_date > 0)) {
                influx.day <- input$influx_date - input$curr_date + curr.day
                influx <- list('day' = influx.day,
                               'num.influx' = input$num.influx)
            }
        }

        # run the same model as initialization model but run extra days
        new.num.days <- input$proj_num_days + curr.day
        new.num.days <-
            ifelse(is.na(new.num.days), 365, new.num.days)
        
        # starting conditions
        start.susc <- input$num_people - start.exp.default
        start.inf <- 0
        start.res <- 0
        
        seir.df = isolate(model())$SEIR(
            S0 = start.susc,
            E0 = start.exp.default,
            I0 = start.inf,
            R0 = start.res,
            beta.vector = beta.vector(),
            num.days = new.num.days,
            influx = influx,
            params = params
        )
        
        # shift the number of days to account for day 0 in the model
        seir.df$days.shift <- seir.df$day - curr.day
        
        # Note: this will cause an error if Model 2 is run because icu.rate and vent.rate 
        # are not available
        #
        # TODO: this is very hack-y.... And may not give a good alignment for projections 
        # at days.shift>0
        num.hosp.input <- input$num_hospitalized
        num.icu.orig <- num.hosp.input * params$icu.rate
        num.vent.orig <- num.icu.orig * params$vent.rate 
        
        seir.df[seir.df$days.shift == 0,]$hosp <- num.hosp.input
        seir.df[seir.df$days.shift == 0,]$icu <- num.icu.orig
        seir.df[seir.df$days.shift == 0,]$vent <- num.vent.orig
        

        seir.df$date <-
            seir.df$days.shift + as.Date(input$curr_date)

        seir.df
    })


    ##  ............................................................................
    ##  Plot Outputs
    ##  ............................................................................

    observeEvent(input$hosp_cap, {
        params$hosp.avail <- input$hosp_cap
    })

    observeEvent(input$icu_cap, {
        params$icu.avail <- input$icu_cap
    })

    observeEvent(input$vent_cap, {
        params$vent.avail <- input$vent_cap
    })
    
    observeEvent(input$selected_graph, ignoreInit = TRUE,{
        if (input$selected_graph == "Hospital Resources"){

            updateNumericInput(session,
                               inputId = "hosp_cap", 
                               value = params$hosp_cap)
            
            updateNumericInput(session,
                               inputId = "icu_cap", 
                               value = params$icu_cap)
            
            updateNumericInput(session,
                               inputId = "vent_cap", 
                               value = params$vent_cap)
            updateCheckboxGroupInput(session, 
                                     inputId = "selected_lines",
                                     choices = c('Hospital', 'ICU', 'Ventilator'),
                                     selected =  c('Hospital', 'ICU', 'Ventilator'),
                                     inline = TRUE)
        }
        else if (input$selected_graph == "Hospitalization"){

            updateCheckboxGroupInput(session, 
                                     inputId = "selected_lines",
                                     choices = c('Hospital', 'ICU', 'Ventilator'),
                                     selected =  c('Hospital', 'ICU', 'Ventilator'),
                                     inline = TRUE)
        }
        else{
            updateCheckboxGroupInput(session, 
                                     inputId = "selected_lines",
                                     choices = c('Total Cases', 'Active Cases', 'Resolved Cases'),
                                     selected =  c('Total Cases', 'Active Cases', 'Resolved Cases'),
                                     inline = TRUE)
        }
        
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
        create.res.df(
            df = seir.output.df(),
            hosp_cap = input$hosp_cap,
            icu_cap = input$icu_cap,
            vent_cap = input$vent_cap
        )
    })

    ##  ............................................................................
    ##  Table output
    ##  ............................................................................

    selected_graph_data <- reactive({
        if (input$selected_graph == 'Cases') {
            cases.df()
        }
        else if (input$selected_graph == 'Hospitalization') {
            hospitalization.df()
        }
        else {
            resource.df()
        }
    })

    output$rendered.table <- DT::renderDataTable({
        df.render <- selected_graph_data()
        df.render$date <- format(df.render$date, format = "%B %d, %Y")

        DT::datatable(
            data = df.render,
            escape = FALSE,
            selection = 'single',
            options = list(
                pageLength = 10,
                lengthChange = FALSE,
                searching = FALSE
            ),
            rownames = FALSE
        )

    })

    observeEvent(input$rendered.table_row_last_clicked, {
        row.id <- input$rendered.table_row_last_clicked
        df.table <- selected_graph_data()
        select.date <- df.table[row.id, 'date']
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

        proxy <- DT::dataTableProxy(
            'rendered.table',
            session = shiny::getDefaultReactiveDomain(),
            deferUntilFlush = TRUE
        )

        DT::selectRows(proxy, plot_day() - input$curr_date + 1)
        DT::selectPage(proxy, ceiling((plot_day() - input$curr_date + 1) / 10))

    })

    observeEvent(input$goright, {
        if (plot_day() != input$curr_date + input$proj_num_days) {
            plot_day(plot_day() + 1)

            proxy <- DT::dataTableProxy(
                'rendered.table',
                session = shiny::getDefaultReactiveDomain(),
                deferUntilFlush = TRUE
            )

            DT::selectRows(proxy, plot_day() - input$curr_date + 1)
            DT::selectPage(proxy, ceiling((
                plot_day() - input$curr_date + 1
            ) / 10))
        }

    })

    observeEvent(input$goleft, {
        if (plot_day() != input$curr_date) {
            plot_day(plot_day() - 1)

            proxy <- DT::dataTableProxy(
                'rendered.table',
                session = shiny::getDefaultReactiveDomain(),
                deferUntilFlush = TRUE
            )

            DT::selectRows(proxy, plot_day() - input$curr_date + 1)
            DT::selectPage(proxy, ceiling((
                plot_day() - input$curr_date + 1
            ) / 10))
        }

    })


    output$rendered_plot <- renderPlot({
        if (input$selected_graph == 'Hospitalization'){
            create.graph(
                df.to.plot = hospitalization.df(),
                selected = input$selected_lines,
                plot.day = plot_day(),
                curr.date = input$curr_date,
                frozen_data = frozen_lines()
            )
        }
        else if (input$selected_graph == 'Hospital Resources'){
            create.graph(
                df.to.plot = resource.df(),
                selected = input$selected_lines,
                plot.day = plot_day(),
                curr.date = input$curr_date,
                frozen_data = frozen_lines()
            )
        }
        else{
            create.graph(
                df.to.plot = cases.df(),
                selected = input$selected_lines,
                plot.day = plot_day(),
                curr.date = input$curr_date,
                frozen_data = frozen_lines()
            )
        }
    })

    observe({
        shinyjs::toggleState("freeze_reset", condition = !is.null(frozen_lines()))
    })

    observe({
        shinyjs::toggleState("freeze_btn", condition = nzchar(trimws(input$freeze_name)))
    })

    observe({
        input$selected_graph
        input$selected_lines
        input$freeze_reset
        frozen_lines(NULL)
    })

    observe({
        show_freeze <-
            length(input$selected_lines) == 1
        shinyjs::toggle("freeze-section", condition = show_freeze)
    })

    observeEvent(input$freeze_btn, {
        name <- trimws(input$freeze_name)
        selected <- input$selected_lines
        if (name == selected) {
            shinyalert::shinyalert("Can't use the same name as the selected variable.", type = "error")
            return()
        }
        if (name %in% unique(frozen_lines()$variable)) {
            shinyalert::shinyalert("Can't use the same name twice.", type = "error")
            return()
        }
        cols <- c("date", selected)
        new_data <- selected_graph_data()[, cols]
        names(new_data)[2] <- name
        new_data <- tidyr::pivot_longer(new_data, -date, names_to = "variable")
        if (is.null(frozen_lines())) {
            frozen_lines(new_data)
        } else {
            frozen_lines(dplyr::full_join(frozen_lines(), new_data))
        }
    })

    ##  ............................................................................
    ##  Natural Language Outputs
    ##  ............................................................................

    # estimatated number of infections of "day 0"
    output$infected_ct <- renderUI({
        curr.date <- format(input$curr_date, format = "%B %d, %Y")

        curr.day <- curr.day.list()['curr.day']
        curr.row <-
            seir.output.df()[seir.output.df()$day == curr.day, ]
        
        infected <- round(curr.row$I + curr.row$E)
        cases <- round(curr.row$I + curr.row$R + curr.row$E)
        
        HTML(sprintf(curr.inf.est.wording, curr.date, cases, infected))
    })

    # describing each timestep in words
    output$description <- renderUI({
        df_temp <- seir.output.df()
        select.row <- df_temp[df_temp$date == plot_day(), ]
        select.date <- format(select.row$date, format = "%B %d, %Y")
        select.day <- select.row$days.shift

        if (input$selected_graph == 'Cases') {
            cases <- round(select.row$I + select.row$R + select.row$E)
            active <- floor(select.row$I + select.row$E)

            if (length(select.day) != 0) {
                if (select.day == 0) {
                    HTML(sprintf(
                        cases.curr.wording,
                        select.date,
                        cases,
                        active
                    ))
                }
                else{
                    HTML(sprintf(
                        cases.fut.wording,
                        select.date,
                        cases,
                        active
                    ))
                }
            }
        }
        else if (input$selected_graph == 'Hospitalization') {
            hosp <- round(select.row$hosp)
            icu <- round(select.row$icu)
            vent <- round(select.row$vent)

            if (select.day == 0) {
                HTML(sprintf(
                    hosp.curr.wording,
                    select.date,
                    hosp,
                    icu,
                    vent
                ))
            }
            else{
                HTML(sprintf(hosp.fut.wording, select.date, hosp, icu, vent))
            }


        }
        else{
            hosp_res <- input$hosp_cap - round(select.row$hosp)
            icu_res <- input$icu_cap - round(select.row$icu)
            vent_res <- input$vent_cap - round(select.row$vent)

            if (select.day == 0) {
                HTML(
                    sprintf(
                        res.curr.wording,
                        select.date,
                        hosp_res,
                        icu_res,
                        vent_res
                    )
                )
            }
            else{
                HTML(sprintf(
                    res.fut.wording,
                    select.date,
                    hosp_res,
                    icu_res,
                    vent_res
                ))
            }

        }
    })

    ##  ............................................................................
    ##  Download Data
    ##  ............................................................................
    output$downloadData <- downloadHandler(filename <- function() {
        paste('Projections', '-', Sys.Date(), '.csv', sep = '')
    },
    content <- function(file) {
        data <- selected_graph_data()

        # TODO: this is for testing only, uncomment and
        # remove bottom lines after testing is done
        # utils::write.csv(data.frame(data), file, row.names = FALSE)
        df.output <- seir.output.df()

        # model specific dataframe downloads
        # process.df.for.download function is in model1.R or model2.R
        df.output <- model()$process.df.for.download(df.output)

        # TODO: this is dirty. Should perhaps be parsed out into a function.
        # process parameters
        gen.param.names <- c('Number of people in Area', 'Date')
        gen.param.vals <- c(input$num_people, input$curr_date)

        # doubling time or Re
        if (input$usedouble == TRUE) {
            gen.param.names <- c(gen.param.names, 'Prior Doubling Time')
            gen.param.vals <-
                c(gen.param.vals, input$doubling_time)
        }
        else{
            gen.param.names <- c(gen.param.names, 'Prior Re')
            gen.param.vals <- c(gen.param.vals, input$r0_prior)
        }

        # hospitalizations or cases
        gen.param.names <- c(gen.param.names, paste('Initial', input$metric))
        gen.param.vals <- c(gen.param.vals, input$num_hospitalized)

        if (input$showinflux) {
            gen.param.names <-
                c(gen.param.names, 'Influx Date', 'Influx Number')
            gen.param.vals <-
                c(gen.param.vals,
                  input$influx.date,
                  input$num_influx)
        }

        params.list <- model()$process.params.for.download(params)

        params.df <-
            data.frame(
                PARAMETERS = rep(NA, length(c(
                    gen.param.names, names(params.list)
                ))),
                params = c(gen.param.names, names(params.list)),
                values = c(gen.param.vals, unlist(params.list)),
                INTERVENTIONS = rep(NA, length(c(
                    gen.param.names, names(params.list)
                )))
            )


        df.binded <- cbind.fill(df.output, params.df)

        if (nrow(intervention.table()) > 0) {
            df.binded <- cbind.fill(df.output, params.df, intervention.table())
        }
        else{
            df.binded <- cbind.fill(df.output, params.df)
        }

        utils::write.csv(data.frame(df.binded),
                         file,
                         row.names = FALSE,
                         na = '')
    })
}
