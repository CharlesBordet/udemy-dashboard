
dashboardPage(
    
    dashboardHeader(title = "Udemy Dashboard", titleWidth = "250px"),
    
    dashboardSidebar(width = "250px",
        
        sidebarMenu(
            menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
            menuItem("Apprendre la Data Science avec R",
                     tabName = "datascience-r", icon = icon("angle-double-right")),
            menuItem("Le Deep Learning de A à Z", 
                     tabName = "le-deep-learning-de-a-a-z",
                     icon = icon("angle-double-right")),
            menuItem("Intelligence Artificielle de A à Z",
                     tabName = "intelligence-artificielle-az",
                     icon = icon("angle-double-right")),
            actionButton("load_data", " Load data", icon = icon("upload")),
            uiOutput("test")
        )
        
    ),
    
    dashboardBody(
        
        useShinyjs(),
        
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
        ),
        
        tabItem(tabName = "overview",
            fluidRow(
                valueBoxOutput("kpi_total_number_of_students", width = 3),
                valueBoxOutput("kpi_total_revenue", width = 3),
                valueBoxOutput("kpi_total_refunds", width = 3),
                valueBoxOutput("kpi_total_amount_of_refunds", width = 3)
            ),
            fluidRow(
                box(width = 6,
                    plotOutput("students_per_month")),
                box(width = 6,
                    plotOutput("revenue_per_month"))
            )
                # Graph of revenue/number of students per month
                # per course
                
                # Value per student
        )
    )
)