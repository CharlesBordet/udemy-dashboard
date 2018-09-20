
dashboardPage(
    
    skin = "black",
    
    dashboardHeader(title = "Udemy Dashboard", titleWidth = "250px",
                    disable = TRUE),
    
    dashboardSidebar(width = "250px",
        
        sidebarMenu(
            menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
            uiOutput("test")
        ),
        collapsed = TRUE
        
    ),
    
    dashboardBody(
        
        useShinyjs(),
        
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
        ),
        
        tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
        
        tabItem(
            tabName = "overview",
            fluidRow(
                valueBoxOutput("kpi_total_number_of_students", width = 3),
                valueBoxOutput("kpi_total_revenue", width = 3),
                valueBoxOutput("kpi_total_refunds", width = 3),
                valueBoxOutput("kpi_total_amount_of_refunds", width = 3)
            ),
            fluidRow(
                column(width = 3,
                       box(width = NULL,
                           uiOutput("ui_select_courses"),
                           uiOutput("ui_select_courses_compare"))),
                column(width = 3,
                       box(width = NULL,
                           uiOutput("ui_select_timeframe")))
            ),
            fluidRow(
                box(width = 12,
                    plotOutput("kpis_per_month", height = "1300px"))
            ),
            fluidRow(
                actionButton("load_data", " Load data", icon = icon("upload"),
                             class = "center-block")
            )
                
                # Graph of revenue/number of students per month
                # per course
                
                # Value per student
        )
    )
)