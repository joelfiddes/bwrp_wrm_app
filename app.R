library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(RColorBrewer)

# === Load Data ===
catchments <- st_read("./inputs/modelcatchments.shp") %>%
  mutate(index = row_number() - 1)

anomaly_df <- read_csv("./input/anomaly_df.csv")

# === UI ===
ui <- fluidPage(
  titlePanel("Water Resources Atlas Baluchistan"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = NULL),
      selectInput("variable", "Select Variable:", choices = NULL),
      radioButtons("map_view", "Map View:", choices = c("Anomaly" = "anomaly", "Annual Mean" = "annual_mean")),
      helpText("Click a catchment to view its daily time series below.")
    ),
    mainPanel(
      leafletOutput("map", height = "600px"),
      plotOutput("timeseries_plot", height = "300px")
    )
  )
)

# === Server ===
server <- function(input, output, session) {

  # Update dropdowns
  observe({
    updateSelectInput(session, "year", choices = sort(unique(anomaly_df$year)))
    updateSelectInput(session, "variable", choices = sort(unique(anomaly_df$variable)))
  })

  # Reactive: Filtered anomaly data
  filtered_data <- reactive({
    req(input$year, input$variable)
    anomaly_df %>%
      filter(year == input$year, variable == input$variable)
  })

  # Join with spatial data
  joined_sf <- reactive({
    left_join(catchments, filtered_data(), by = "index")
  })

  # Store clicked index
  clicked_index <- reactiveVal(NULL)

  observeEvent(input$map_shape_click, {
    print(input$map_shape_click)
    clicked_index(as.numeric(input$map_shape_click$id))
  })

  # Render map
  output$map <- renderLeaflet({
    req(joined_sf(), input$map_view)
    data <- joined_sf()

    # Choose column to map
    column_to_plot <- input$map_view
    values <- data[[column_to_plot]]

    # Set color scale
    if (column_to_plot == "anomaly") {
      max_abs <- max(abs(values), na.rm = TRUE)
      color_range <- c(-max_abs, max_abs)
      pal <- colorNumeric(palette = brewer.pal(11, "RdBu"), domain = color_range, na.color = "transparent")
    } else {
      color_range <- range(values, na.rm = TRUE)
      pal <- colorNumeric(palette = rev("viridis"), domain = color_range, na.color = "transparent")
    }

    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(values),
        fillOpacity = 0.8,
        color = "black",
        weight = 0.5,
        layerId = ~index,
        label = ~paste0("Index: ", index, "<br>", column_to_plot, ": ", round(values, 3)),
        highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = color_range,
        title = paste(column_to_plot, "<br>", input$variable, input$year),
        labFormat = labelFormat(digits = 2)
      )
  })

  # Time series plot
  output$timeseries_plot <- renderPlot({
    req(clicked_index(), input$variable)
    idx <- clicked_index()
    var <- input$variable

    # Build file path for this index
    file_path <- paste0("./results/result_", idx, ".csv")
    if (!file.exists(file_path)) {
      plot.new()
      title(main = paste("File not found:", file_path))
      return()
    }

    # Read time series
    ts_data <- read_csv(file_path)

    if (!("Date" %in% names(ts_data))) {
      plot.new()
      title(main = "No 'Date' column found in file.")
      return()
    }

    # Convert date to Date class
    ts_data <- ts_data %>% mutate(date = as.Date(Date))

    if (!(var %in% names(ts_data))) {
      plot.new()
      title(main = paste("Variable", var, "not found in file."))
      return()
    }

    # Plot daily variable
    ggplot(ts_data, aes(x = date, y = .data[[var]])) +
      geom_line(color = "blue") +
      labs(
        title = paste("Daily", var, "for Catchment", idx),
        x = "Date", y = var
      ) +
      theme_minimal()
  })

}

# === Run the app ===
shinyApp(ui, server)
