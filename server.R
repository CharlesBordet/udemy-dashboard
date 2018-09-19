
function(input, output, session) {
    
    output$test <- renderUI({
        # udemy_report_data()
    })
    
    # ------ Reactive ---------------------------------------------------------
    
    values <- reactiveValues(
        sales = sales,
        redemptions = redemptions,
        refunds = refunds,
        months = unique(sales$month),
        # This flag checks whether the Udemy report entered is
        # in the correct format
        # When it becomes TRUE, the button to validate the import appears
        check_udemy_report_filepath = NULL
    )
    
    # SALES DATA
    dat_sales <- reactive({
        course <- req(input$select_courses)
        if (course == "All") {
            sales0 <- copy(values$sales)
        } else {
            sales0 <- values$sales[`Course Name` == course]
        }
        sales0[, month := paste(substr(`Formatted Date`, 1, 3),
                                substr(`Formatted Date`, 9, 10))]
        values$months <- unique(sales0$month)
        sales0[, month := factor(month, levels = values$months)]
        sales0[, day := as.Date(`Formatted Date`, format = "%b %d '%y")]
        sales0[, week := as.Date(paste0(format(day, "%y-W%W"), "-Mon"), format = "%y-W%W-%a")]
        sales0[, `Course Name` := factor(`Course Name`, levels = unique(`Course Name`))]
        sales0
    })
    
    # REFUNDS DATA
    dat_refunds <- reactive({
        course <- req(input$select_courses)
        if (course == "All") {
            refunds0 <- copy(values$refunds)
        } else {
            refunds0 <- values$refunds[`Course Name` == course]
        }
        refunds0[, day := as.Date(`Refund Date`)]
        refunds0[, month := format(day, format = "%b %y")]
        refunds0[, month := factor(month, levels = values$months)]
        refunds0[, week := as.Date(paste0(format(day, "%y-W%W"), "-Mon"), format = "%y-W%W-%a")]
        refunds0[, `Course Name` := factor(`Course Name`, levels = unique(`Course Name`))]
        refunds1 <- refunds0[
            setDT(list(month = factor(values$months, levels = values$months))),
            on = "month"]
        refunds1
    })
    
    total_number_of_students <- reactive({
        nrow(req(dat_sales()))
    })
    
    total_revenue <- reactive({
        revenue <- sum(req(dat_sales())$`Instructor Share`)
        revenue <- round(revenue, 2)
        revenue <- paste("$", revenue)
        revenue
    })
    
    total_refunds <- reactive({
        nrow(req(dat_refunds()))
    })
    
    total_amount_of_refunds <- reactive({
        refund_amount <- sum(req(dat_refunds())$`Instructor Refund Amount`, na.rm = T)
        refund_amount <- paste("$", refund_amount)
        refund_amount
    })
    
    # ------ UI ---------------------------------------------------------------
    
    # SELECT COURSES
    output$ui_select_courses <- renderUI({
        selectInput("select_courses", "Course:",
                    choices = c("All", unique(values$sales$`Course Name`)))
    })
    
    # COMPARE COURSES
    output$ui_select_courses_compare <- renderUI({
        if (req(input$select_courses == "All")) {
            checkboxInput("select_courses_compare", "Compare courses")
        } else {
            NULL
        }
    })
    
    output$ui_select_timeframe <- renderUI({
        radioButtons("select_timeframe", "Timeframe:", inline = TRUE,
                     choices = c("Month" = "month", "Week" = "week", "Day" = "day"))
    })
    
    # ------ OVERVIEW ---------------------------------------------------------
    
    output$kpi_total_number_of_students <- renderValueBox({
        valueBox(total_number_of_students(), "Total number of students",
                 icon = icon("user"),
                 color = "purple")
    })
    
    output$kpi_total_revenue <- renderValueBox({
        valueBox(total_revenue(), "Total revenue",
                 icon = icon("usd"),
                 color = "green")
    })
    
    output$kpi_total_refunds <- renderValueBox({
        valueBox(total_refunds(), "Total of refunds",
                 icon = icon("undo"),
                 color = "yellow")
    })
    
    output$kpi_total_amount_of_refunds <- renderValueBox({
        valueBox(total_amount_of_refunds(), "Total amount refunded",
                 icon = icon("usd"),
                 color = "red")
    })
    
    output$kpis_per_month <- renderPlot({
        req(input$select_courses_compare * 1)
        compare <- input$select_courses_compare * (input$select_courses == "All")
        timeframe <- input$select_timeframe
        by <- c("Course Name", timeframe)
        
        sales0 <- dat_sales()
        sales1 <- sales0[, .(students = .N,
                             revenue = sum(`Instructor Share`)), 
                         by = by]
        
        refunds1 <- dat_refunds()
        refunds1 <- refunds1[, .(refunds = sum(!is.na(`Refund Date`)),
                                 amount = sum(`Instructor Refund Amount`)), 
                             by = by]
        
        # PLOT
        theme <- theme_minimal(base_size = 17) +
            theme(panel.grid.major.x = element_blank()) +
            theme(axis.text.x = element_text(angle = 90)) +
            theme(legend.position = "none")
        
        sales_students <- 
            ggplot(sales1, aes_string(timeframe, "students", fill = "`Course Name`")) + 
            theme +
            labs(title = paste0(stringr::str_to_title(timeframe), 
                                "ly enrollment in Udemy courses"), 
                 x = "Month", 
                 y = "Number of students")
        sales_revenue <- 
            ggplot(sales1, aes_string(timeframe, "revenue", fill = "`Course Name`")) +
            theme +
            labs(title = paste0(stringr::str_to_title(timeframe), 
                                "ly revenue of Udemy courses"), 
                 x = "Month", 
                 y = "Revenue ($)")
        refunds_number <-
            ggplot(refunds1, aes_string(timeframe, "refunds", fill = "`Course Name`")) +
            theme +
            labs(title = paste0("Number of refunds per ", timeframe), 
                 x = "Month", 
                 y = "Refunds")
        refunds_amount <-
            ggplot(refunds1, aes_string(timeframe, "amount", fill = "`Course Name`")) +
            theme +
            labs(title = paste0("Amount of refunds per ", timeframe), 
                 x = "Month", 
                 y = "Amount ($)")
        if (compare) {
            sales_students <- sales_students +
                geom_bar(stat = "identity", position = "dodge") +
                theme(legend.position = "top", legend.direction = "horizontal")
            sales_revenue <- sales_revenue + geom_bar(stat = "identity", position = "dodge")
            refunds_number <- refunds_number + geom_bar(stat = "identity", position = "dodge")
            refunds_amount <- refunds_amount + geom_bar(stat = "identity", position = "dodge")
            tmp <- ggplot_gtable(ggplot_build(sales_students))
            leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
            legend <- tmp$grobs[[leg]]
        } else {
            sales_students <- sales_students + geom_bar(stat = "identity", fill = "#605ca8")
            sales_revenue <- sales_revenue + geom_bar(stat = "identity", fill = "#00a65a")
            refunds_number <- refunds_number + geom_bar(stat = "identity", fill = "#f39c12")
            refunds_amount <- refunds_amount + geom_bar(stat = "identity", fill = "#dd4b39")
        }
        if (timeframe %in% c("day", "week")) {
            sales_students <- sales_students + 
                scale_x_date(breaks = "1 month", labels = scales::date_format("%b %y"))
            sales_revenue <- sales_revenue + 
                scale_x_date(breaks = "1 month", labels = scales::date_format("%b %y"))
            refunds_number <- refunds_number + 
                scale_x_date(breaks = "1 month", labels = scales::date_format("%b %y"))
            refunds_amount <- refunds_amount + 
                scale_x_date(breaks = "1 month", labels = scales::date_format("%b %y"))
        }
        
        if (input$dimension[1] < 1000) {
            ncol <- 1
        } else {
            ncol <- 2
        }
        
        if (compare) {
            grid.arrange(legend,
                         arrangeGrob(sales_students + theme(legend.position = "none"), 
                                     sales_revenue, refunds_number,
                                     refunds_amount, ncol = ncol),
                         nrow = 2, heights = c(1, 10))
        } else {
            grid.arrange(sales_students, sales_revenue, refunds_number,
                         refunds_amount, ncol = ncol)
        }
    })
    outputOptions(output, "kpis_per_month", priority = 0)
    
    output$students_per_month <- renderPlot({
        sales0 <- copy(values$sales)
        sales0[, month := paste(substr(`Formatted Date`, 1, 3),
                                substr(`Formatted Date`, 9, 10))]
        sales0[, month := factor(month, levels = unique(sales0$month))]
        sales1 <- sales0[, .(students = .N), by = month]
        
        ggplot(sales1, aes(x = month, y = students)) +
            geom_bar(stat = "identity", fill = "#605ca8") +
            labs(title = "Monthly enrollment in Udemy courses", x = "Month", 
                 y = "Number of students") +
            scale_y_continuous(breaks = seq(0, 2000, by = 100)) +
            theme_minimal(base_size = 17) +
            theme(panel.grid.major.x = element_blank()) 
    })
    
    output$revenue_per_month <- renderPlot({
        sales0 <- copy(values$sales)
        sales0[, month := paste(substr(`Formatted Date`, 1, 3),
                                substr(`Formatted Date`, 9, 10))]
        sales0[, month := factor(month, levels = unique(sales0$month))]
        sales1 <- sales0[, .(revenue = sum(`Instructor Share`)), by = month]
        
        ggplot(sales1, aes(x = month, y = revenue)) +
            geom_bar(stat = "identity", fill = "#00a65a") +
            labs(title = "Monthly revenue of Udemy courses", x = "Month", 
                 y = "Revenue ($)") +
            scale_y_continuous(breaks = seq(0, 2000, by = 250)) +
            theme_minimal(base_size = 17) +
            theme(panel.grid.major.x = element_blank()) 
    })
    
    output$refunds_per_month <- renderPlot({
        refunds0 <- copy(values$refunds)
        refunds0[, month := format(as.Date(`Refund Date`), format = "%b %y")]
        refunds0[, month := factor(month, levels = unique(refunds0$month))]
        refunds1 <- refunds0[, .(refunds = .N), by = month]
        
        ggplot(refunds1, aes(x = month, y = refunds)) +
            geom_bar(stat = "identity", fill = "#f39c12") +
            labs(title = "Number of refunds per month", x = "Month", 
                 y = "Refunds") +
            scale_y_continuous(breaks = seq(0, 20, by = 5)) +
            theme_minimal(base_size = 17) +
            theme(panel.grid.major.x = element_blank()) 
    })
    
    output$amount_of_refunds_per_month <- renderPlot({
        refunds0 <- copy(values$refunds)
        refunds0[, month := format(as.Date(`Refund Date`), format = "%b %y")]
        refunds0[, month := factor(month, levels = unique(refunds0$month))]
        refunds1 <- refunds0[, .(amount = sum(`Instructor Refund Amount`)), by = month]
        
        ggplot(refunds1, aes(x = month, y = amount)) +
            geom_bar(stat = "identity", fill = "#dd4b39") +
            labs(title = "Amount of refunds per month", x = "Month", 
                 y = "Amount ($)") +
            scale_y_continuous(breaks = seq(0, 2000, by = 10)) +
            theme_minimal(base_size = 17) +
            theme(panel.grid.major.x = element_blank()) 
    })
    
    # ------ LOAD DATA --------------------------------------------------------
    
    # Import data from Udemy report
    udemy_report_data <- reactive({
        if (is.null(input$udemy_report_filepath)) {
            return(NULL)
        }
        name <- input$udemy_report_filepath$name
        path <- input$udemy_report_filepath$datapath
        # Check data is correct
        name <- strsplit(name, "-")[[1]]
        if (length(name) != 3) {
            return(FALSE)
        }
        if (name[1] != "statement") {
            return(FALSE)
        }
        if (name[2] != "Charles Bordet") {
            return(FALSE)
        }
        if (grepl("\\.csv$", name[3]) != TRUE) {
            return(FALSE)
        }
        raw_report <- readLines(path)
        sales_row <- grep("^Sales", raw_report)
        if (length(sales_row) == 0) {
            return(FALSE)
        }
        redemptions_row <- grep("^Redemptions", raw_report)
        refunds_row <- grep("^Refunds", raw_report)
        nrows <- min(redemptions_row, refunds_row, length(raw_report) + 1) - 
            sales_row - 2
        sales <- fread(path, skip = sales_row, fill = TRUE, nrows = nrows,
                       sep = ",")
        if (length(redemptions_row) > 0) {
            nrows <- min(refunds_row, length(raw_report) + 1) - 
                redemptions_row - 2
            redemptions <- fread(path, skip = redemptions_row, fill = TRUE,
                                 nrows = nrows, sep = ",")
        } else {
            redemptions <- NULL
        }
        if (length(refunds_row) > 0) {
            nrows <- length(raw_report) - refunds_row - 1
            refunds <- fread(path, skip = refunds_row, fill = TRUE,
                             nrows = nrows,
                             sep = ",")
        } else {
            refunds <- NULL
        }
        report <- list(sales = sales,
                       redemptions = redemptions,
                       refunds = refunds)
        return(report)
    })
    
    # Check if data is correct
    observe({
        if (is.null(udemy_report_data())) {
            values$check_udemy_report_filepath <- NULL
        } else if (is.logical(udemy_report_data()) && udemy_report_data() == FALSE) {
            values$check_udemy_report_filepath <- FALSE
        } else {
            values$check_udemy_report_filepath <- TRUE
        }
    })
    
    # Reset data check
    observeEvent(input$load_data, {
        values$check_udemy_report_filepath <- NULL
    })
    
    # Display modal Dialog to import new data
    observeEvent(input$load_data, {
        showModal(modalDialog(
            title = "Load data",
            footer = NULL,
            size = "s",
            easyClose = TRUE,
            
            fileInput("udemy_report_filepath",
                      label = "Udemy report file",
                      accept = ".csv"),
            
            uiOutput("ui_udemy_report_data_submit")
        ))
    })
    
    # Hide/Display validation button to import data
    output$ui_udemy_report_data_submit <- renderUI({
        if (is.null(values$check_udemy_report_filepath)) {
            NULL
        } else if (values$check_udemy_report_filepath == FALSE) {
            p("The file you have uploaded is incorrect.",
              class = "alert alert-danger")
        } else {
            tagList(
                actionButton("udemy_report_data_submit",
                             label = "Upload the data")
            )
        }
    })
    
    # Save the new data
    observeEvent(input$udemy_report_data_submit, {
        report <- udemy_report_data()
        
        sales <- report$sales
        if (!is.null(sales)) {
            values$sales <- rbind(values$sales, sales) %>% unique
            setkey(values$sales, "Transaction Id")
            sales <<- values$sales
            saveRDS(values$sales, "data/sales.rds")
        }
        redemptions <- report$redemptions
        if (!is.null(redemptions)) {
            values$redemptions <- rbind(values$redemptions, redemptions) %>% unique
            setkey(values$redemptions, "Split Id")
            redemptions <<- values$redemptions
            saveRDS(values$redemptions, "data/redemptions.rds")
        }
        refunds <- report$refunds
        if (!is.null(refunds)) {
            values$refunds <- rbind(values$refunds, refunds) %>% unique
            setkey(values$refunds, "Refund Date")
            refunds <<- values$refunds
            saveRDS(values$refunds, "data/refunds.rds")
        }
        
        removeModal()
        
        showModal(modalDialog(
            title = "Data successfully loaded",
            size = "s",
            easyClose = TRUE,
            
            tagList(
                img(src = "check.png", width = 100, height = 100)
            ),
            style = "text-align: center"
        )) 
    })
    
}