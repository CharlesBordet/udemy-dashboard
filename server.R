
function(input, output, session) {
    
    output$test <- renderUI({
        # udemy_report_data()
    })
    
    values <- reactiveValues(
        sales = sales,
        redemptions = redemptions,
        refunds = refunds,
        # This flag checks whether the Udemy report entered is
        # in the correct format
        # When it becomes TRUE, the button to validate the import appears
        check_udemy_report_filepath = NULL
    )
    
    # ------ OVERVIEW ---------------------------------------------------------
    
    output$kpi_total_number_of_students <- renderValueBox({
        valueBox(nrow(values$sales), "Total number of students",
                 icon = icon("user"),
                 color = "purple")
    })
    
    output$kpi_total_revenue <- renderValueBox({
        revenue <- sum(values$sales$`Instructor Share`)
        revenue <- round(revenue, 2)
        revenue <- paste("$", revenue)
        valueBox(revenue, "Total revenue",
                 icon = icon("usd"),
                 color = "green")
    })
    
    output$kpi_total_refunds <- renderValueBox({
        valueBox(nrow(values$refunds), "Total of refunds",
                 icon = icon("undo"),
                 color = "yellow")
    })
    
    output$kpi_total_amount_of_refunds <- renderValueBox({
        refund_amount <- sum(refunds$`Instructor Refund Amount`)
        refund_amount <- paste("$", refund_amount)
        valueBox(refund_amount, "Total amount refunded",
                 icon = icon("usd"),
                 color = "red")
    })
    
    output$students_per_month <- renderPlot({
        sales0 <- copy(sales)
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
        sales0 <- copy(sales)
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