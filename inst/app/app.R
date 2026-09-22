library(shiny)
library(AreasOfOpportunity)

level_labels <- c(
  output_area = "Output Area",
  lsoa = "LSOA",
  ward = "Ward",
  lad = "Local Authority District",
  county = "County",
  csp = "Community Safety Partnership",
  police_force_area = "Police Force Area"
)

ui <- fluidPage(
  titlePanel("Areas of Opportunity"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "level", "Area type",
        choices = setNames(names(level_labels), level_labels)
      ),
      selectizeInput(
        "area_code", "Area",
        choices = NULL,
        options = list(placeholder = "Search for an area...")
      ),
      hr(),
      dateRangeInput(
        "date_range", "Date range",
        start = Sys.Date() - 90, end = Sys.Date() - 30
      ),
      actionButton("fetch_crime", "Get crime data"),
      actionButton("fetch_stops", "Get stop-and-search data")
    ),
    mainPanel(
      plotOutput("area_map", height = "400px"),
      h4("Crime outcomes"),
      DT::dataTableOutput("crime_table"),
      h4("Stop-and-search outcomes"),
      DT::dataTableOutput("stop_table")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$level,
    {
      areas <- ao_areas(input$level)
      labels <- if (all(is.na(areas$name))) areas$code else ifelse(is.na(areas$name), areas$code, areas$name)
      updateSelectizeInput(
        session, "area_code",
        choices = setNames(areas$code, labels),
        selected = character(0),
        server = TRUE
      )
    },
    ignoreNULL = FALSE
  )

  area_plot <- reactive({
    req(input$area_code)
    ao_plot_area_in_context(input$level, input$area_code)
  })

  output$area_map <- renderPlot(area_plot())

  date_range_ym <- reactive({
    req(input$date_range)
    format(input$date_range, "%Y-%m")
  })

  crime_summary <- eventReactive(input$fetch_crime, {
    req(input$area_code)
    withProgress(message = "Fetching crime data...", {
      crimes <- ao_data("crime", input$level, input$area_code, date = date_range_ym())
    })
    if (nrow(crimes) == 0) {
      return(tibble::tibble(`Outcome` = character(), `Count` = integer()))
    }
    crimes |>
      dplyr::mutate(outcome_category = dplyr::coalesce(outcome_category, "No outcome recorded yet")) |>
      dplyr::count(outcome_category, name = "Count") |>
      dplyr::arrange(dplyr::desc(.data$Count)) |>
      dplyr::rename(Outcome = "outcome_category")
  })

  output$crime_table <- DT::renderDataTable(crime_summary(), options = list(pageLength = 10))

  stop_summary <- eventReactive(input$fetch_stops, {
    req(input$area_code)
    withProgress(message = "Fetching stop-and-search data...", {
      stops <- ao_data("stop_search", input$level, input$area_code, date = date_range_ym())
    })
    if (nrow(stops) == 0) {
      return(tibble::tibble(`Outcome` = character(), `Count` = integer()))
    }
    stops |>
      dplyr::mutate(outcome = dplyr::if_else(is.na(outcome) | outcome == "", "No outcome recorded", outcome)) |>
      dplyr::count(outcome, name = "Count") |>
      dplyr::arrange(dplyr::desc(.data$Count)) |>
      dplyr::rename(Outcome = "outcome")
  })

  output$stop_table <- DT::renderDataTable(stop_summary(), options = list(pageLength = 10))
}

shinyApp(ui, server)
