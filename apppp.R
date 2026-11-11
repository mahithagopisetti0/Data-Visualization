0

library(shiny)
library(ggplot2)
library(dplyr)

# 1. DATASET: Synthetic hotel booking records
set.seed(123)
n <- 500

hotels <- data.frame(
  Hotel = sample(c("Grand Palace", "Royal Comfort",
                   "Sunshine Residency"), n, TRUE),
  Room = sample(c("Standard", "Deluxe", "Suite"), n, TRUE),
  Segment = sample(c("Business", "Leisure", "Family"), n, TRUE),
  Date = as.Date("2025-01-01") + sample(0:364, n, TRUE),
  Nights = sample(1:7, n, TRUE),
  Rate = round(runif(n, 2000, 7000)),
  Status = sample(c("Completed", "Cancelled"), n, TRUE,
                  prob = c(0.85, 0.15))
) |>
  mutate(
    Revenue = ifelse(Status == "Completed", Rate * Nights, 0),
    Month = as.Date(format(Date, "%Y-%m-01"))
  )

# 2. DASHBOARD INTERFACE
ui <- fluidPage(
  titlePanel("StayWise: Hotel Revenue Analytics"),
  sidebarLayout(
    sidebarPanel(
      selectInput("hotel", "Select Hotel",
                  c("All Hotels", unique(hotels$Hotel))),
      selectInput("room", "Room Type",
                  c("All Rooms", unique(hotels$Room))),
      dateRangeInput("dates", "Date Range",
                     start = min(hotels$Date),
                     end = max(hotels$Date)),
      selectInput("graph", "Revenue Comparison",
                  c("Hotel", "Room", "Segment"))
    ),
    mainPanel(
      h3("Performance Summary"),
      verbatimTextOutput("kpi"),
      plotOutput("trend", height = 280),
      plotOutput("compare", height = 280),
      plotOutput("distribution", height = 250),
      plotOutput("relationship", height = 280),
      tableOutput("summary")
    )
  )
)

# 3. SERVER: FILTER DATA AND CALCULATE METRICS
server <- function(input, output) {
  data <- reactive({
    d <- hotels |>
      filter(Date >= input$dates[1], Date <= input$dates[2])
    if (input$hotel != "All Hotels")
      d <- d |> filter(Hotel == input$hotel)
    if (input$room != "All Rooms")
      d <- d |> filter(Room == input$room)
    d
  })
  
  output$kpi <- renderText({
    d <- data()
    completed <- d |> filter(Status == "Completed")
    paste(
      "Total Revenue: Rs.", format(sum(d$Revenue), big.mark = ","),
      "\nBookings:", nrow(d),
      "\nCancellation Rate:",
      round(mean(d$Status == "Cancelled") * 100, 1), "%",
      "\nMean Room Rate: Rs.", round(mean(d$Rate)),
      "\nMedian Room Rate: Rs.", median(d$Rate),
      "\nCompleted Room Nights:", sum(completed$Nights)
    )
  })
  
  # 4. LINE CHART: MONTHLY REVENUE TREND
  output$trend <- renderPlot({
    d <- data() |> group_by(Month) |>
      summarise(Revenue = sum(Revenue), .groups = "drop")
    ggplot(d, aes(Month, Revenue)) +
      geom_line(color = "#00A896", linewidth = 1.2) +
      geom_point(color = "#F4A261", size = 2) +
      labs(title = "Monthly Revenue Trend",
           x = "Month", y = "Revenue (Rs.)") +
      theme_minimal()
  })
  
  # 5. BAR CHART: SELECTABLE COMPARISON
  output$compare <- renderPlot({
    d <- data() |> group_by(.data[[input$graph]]) |>
      summarise(Revenue = sum(Revenue), .groups = "drop")
    ggplot(d, aes(x = .data[[input$graph]], y = Revenue,
                  fill = .data[[input$graph]])) +
      geom_col(show.legend = FALSE) +
      labs(title = paste("Revenue by", input$graph),
           x = input$graph, y = "Revenue (Rs.)") +
      theme_minimal()
  })
  
  # 6. HISTOGRAM: ROOM RATE DISTRIBUTION
  output$distribution <- renderPlot({
    ggplot(data(), aes(Rate)) +
      geom_histogram(bins = 20, fill = "#457B9D",
                     color = "white") +
      labs(title = "Room Rate Distribution",
           x = "Room Rate (Rs.)", y = "Bookings") +
      theme_minimal()
  })
  
  # 7. SCATTER PLOT AND REGRESSION LINE
  output$relationship <- renderPlot({
    d <- data() |> filter(Status == "Completed")
    ggplot(d, aes(Nights, Revenue, color = Room)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Length of Stay vs Revenue",
           x = "Nights", y = "Revenue (Rs.)",
           color = "Room Type") +
      theme_minimal()
  })
  
  # 8. SUMMARY TABLE
  output$summary <- renderTable({
    data() |> group_by(Hotel) |>
      summarise(
        Bookings = n(),
        Revenue = sum(Revenue),
        Average_Rate = round(mean(Rate)),
        .groups = "drop"
      )
  })
}

# 9. RUN APPLICATION
shinyApp(ui = ui, server = server)