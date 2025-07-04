# R Shiny App for Verbier and Bagnes Water Quality Data
# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

# Function to load barometric pressure data
load_barometric_pressure <- function() {
  bp_file <- "bpverbier.txt"
  if (file.exists(bp_file)) {
    # Read barometric pressure data
    bp_data <- read_delim(bp_file, delim = ";", skip = 2, col_names = c("stn", "time", "prestas0"))
    
    # Convert time from unix format to datetime
    bp_data <- bp_data %>%
      mutate(
        datetime = ymd_hm(time),
        pressure_hPa = as.numeric(prestas0)
      ) %>%
      select(datetime, pressure_hPa) %>%
      filter(!is.na(datetime), !is.na(pressure_hPa))
    
    return(bp_data)
  } else {
    # Return empty data frame if file doesn't exist
    return(data.frame(datetime = as.POSIXct(character()), pressure_hPa = numeric()))
  }
}

# Oxygen solubility at standard pressure (Weiss 1970)
O2_sat_std <- function(temp_C) {
  Tk <- temp_C + 273.15
  lnC <- -139.34411 + (1.575701e5 / Tk) - (6.642308e7 / Tk^2) + (1.243800e10 / Tk^3) - (8.621949e11 / Tk^4)
  exp(lnC)
}

# DO correction for barometric pressure
correct_DO <- function(DO_mgL, Temp_C, Pressure_hPa) {
  DO_sat_standard <- O2_sat_std(Temp_C)
  DO_percent <- (DO_mgL / DO_sat_standard) * 100
  DO_sat_actual <- DO_sat_standard * (Pressure_hPa / 1013.25)
  DO_corrected <- (DO_percent / 100) * DO_sat_actual
  return(DO_corrected)
}

# Function to interpolate barometric pressure for given timestamps
interpolate_pressure <- function(datetime_vector, bp_data) {
  if (nrow(bp_data) == 0) {
    # Return standard pressure if no barometric data available
    return(rep(1013.25, length(datetime_vector)))
  }
  
  # Interpolate pressure values
  pressure_interp <- approx(
    x = as.numeric(bp_data$datetime),
    y = bp_data$pressure_hPa,
    xout = as.numeric(datetime_vector),
    method = "linear",
    rule = 2  # Use nearest value for extrapolation
  )$y
  
  # Replace NA values with standard pressure
  pressure_interp[is.na(pressure_interp)] <- 1013.25
  
  return(pressure_interp)
}

# Function to load and concatenate CSV files from a folder
load_and_concatenate_data <- function(folder_path, bp_data) {
  csv_files <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)
  
  data_list <- list()
  
  for (file in csv_files) {
    # Read CSV file, skip first row (title), use second row as headers
    temp_data <- read_csv(file, skip = 1, col_types = cols())
    
    # Clean column names
    colnames(temp_data) <- c("Index", "DateTime", "DO_conc_mg_L", "Temp_C")
    
    # Convert DateTime
    temp_data$DateTime <- mdy_hms(temp_data$DateTime)
    
    # Add source file info
    temp_data$Source_File <- basename(file)
    
    data_list[[length(data_list) + 1]] <- temp_data
  }
  
  # Concatenate all data
  combined_data <- bind_rows(data_list)
  
  # Sort by DateTime
  combined_data <- combined_data %>% arrange(DateTime)
  
  # Interpolate barometric pressure for each timestamp
  combined_data$Pressure_hPa <- interpolate_pressure(combined_data$DateTime, bp_data)
  
  # Apply dissolved oxygen correction using proper method
  combined_data <- combined_data %>%
    mutate(
      DO_corrected = correct_DO(DO_conc_mg_L, Temp_C, Pressure_hPa)
    )
  
  return(combined_data)
}

# Function to aggregate data by temporal resolution
aggregate_by_resolution <- function(data, resolution) {
  if (resolution == "10min") {
    data$time_group <- floor_date(data$DateTime, "10 minutes")
  } else if (resolution == "1hour") {
    data$time_group <- floor_date(data$DateTime, "hour")
  } else if (resolution == "6hour") {
    data$time_group <- floor_date(data$DateTime, "6 hours")
  } else if (resolution == "12hour") {
    data$time_group <- floor_date(data$DateTime, "12 hours")
  } else if (resolution == "daily") {
    data$time_group <- floor_date(data$DateTime, "day")
  } else {
    data$time_group <- data$DateTime  # No aggregation
  }
  
  # Aggregate by time group
  aggregated <- data %>%
    group_by(time_group) %>%
    summarise(
      DateTime = first(time_group),
      DO_conc_mg_L = mean(DO_conc_mg_L, na.rm = TRUE),
      Temp_C = mean(Temp_C, na.rm = TRUE),
      DO_corrected = mean(DO_corrected, na.rm = TRUE),
      Pressure_hPa = mean(Pressure_hPa, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    select(-time_group)
  
  return(aggregated)
}

# Load barometric pressure data
bp_data <- load_barometric_pressure()

# Load data for both locations
verbier_data <- load_and_concatenate_data("verbier/", bp_data)
bagnes_data <- load_and_concatenate_data("bagnes/", bp_data)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Mount Resilience - Water Quality Monitoring"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Raw Data - Verbier", tabName = "raw_verbier", icon = icon("chart-line")),
      menuItem("Raw Data - Bagnes", tabName = "raw_bagnes", icon = icon("chart-line")),
      menuItem("Verbier Correction", tabName = "verbier", icon = icon("mountain")),
      menuItem("Bagnes Correction", tabName = "bagnes", icon = icon("water"))
    )
  ),
  
  dashboardBody(
    # Add JavaScript for Ctrl+D detection
    tags$script(HTML("
      $(document).keydown(function(e) {
        if (e.ctrlKey && e.which == 68) { // Ctrl+D
          e.preventDefault();
          Shiny.setInputValue('ctrl_d_pressed', Math.random());
        }
      });
    ")),
    
    tabItems(
      # Raw Data - Verbier Tab
      tabItem(tabName = "raw_verbier",
        fluidRow(
          box(
            title = "Raw Data Controls - Verbier", status = "warning", solidHeader = TRUE,
            width = 12,
            h4("Instructions: Rectangle selection to zoom, Double-click to reset zoom, Ctrl+D to delete selected points"),
            p("Note: Ctrl+D will remove BOTH parameters for selected data points"),
            fluidRow(
              column(6,
                h5("Controls:"),
                p("• Drag rectangle: Zoom to selection"),
                p("• Double-click: Reset zoom to full view"),
                p("• Ctrl+D: Delete points in last selection")
              ),
              column(6,
                actionButton("reset_verbier", "Reset All Data", class = "btn-danger"),
                downloadButton("save_verbier", "Save Cleaned Data", class = "btn-success")
              )
            ),
            br(),
            h5("Selection Info:"),
            verbatimTextOutput("verbier_selection_info")
          )
        ),
        
        fluidRow(
          box(
            title = "Raw Dissolved Oxygen - Interactive Selection", status = "warning", solidHeader = TRUE,
            width = 6,
            plotOutput("raw_verbier_do_plot", brush = "raw_verbier_do_plot_brush", dblclick = "raw_verbier_do_plot_dblclick")
          ),
          box(
            title = "Raw Temperature - Interactive Selection", status = "warning", solidHeader = TRUE,
            width = 6,
            plotOutput("raw_verbier_temp_plot", brush = "raw_verbier_temp_plot_brush", dblclick = "raw_verbier_temp_plot_dblclick")
          )
        ),
        
        fluidRow(
          box(
            title = "Raw Data Table - Verbier", status = "warning", solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("raw_verbier_table")
          )
        )
      ),
      
      # Raw Data - Bagnes Tab
      tabItem(tabName = "raw_bagnes",
        fluidRow(
          box(
            title = "Raw Data Controls - Bagnes", status = "warning", solidHeader = TRUE,
            width = 12,
            h4("Instructions: Rectangle selection to zoom, Double-click to reset zoom, Ctrl+D to delete selected points"),
            p("Note: Ctrl+D will remove BOTH parameters for selected data points"),
            fluidRow(
              column(6,
                h5("Controls:"),
                p("• Drag rectangle: Zoom to selection"),
                p("• Double-click: Reset zoom to full view"),
                p("• Ctrl+D: Delete points in last selection")
              ),
              column(6,
                actionButton("reset_bagnes", "Reset All Data", class = "btn-danger"),
                downloadButton("save_bagnes", "Save Cleaned Data", class = "btn-success")
              )
            ),
            br(),
            h5("Selection Info:"),
            verbatimTextOutput("bagnes_selection_info")
          )
        ),
        
        fluidRow(
          box(
            title = "Raw Dissolved Oxygen - Interactive Selection", status = "warning", solidHeader = TRUE,
            width = 6,
            plotOutput("raw_bagnes_do_plot", brush = "raw_bagnes_do_plot_brush", dblclick = "raw_bagnes_do_plot_dblclick")
          ),
          box(
            title = "Raw Temperature - Interactive Selection", status = "warning", solidHeader = TRUE,
            width = 6,
            plotOutput("raw_bagnes_temp_plot", brush = "raw_bagnes_temp_plot_brush", dblclick = "raw_bagnes_temp_plot_dblclick")
          )
        ),
        
        fluidRow(
          box(
            title = "Raw Data Table - Bagnes", status = "warning", solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("raw_bagnes_table")
          )
        )
      ),
      
      # Verbier Tab
      tabItem(tabName = "verbier",
        fluidRow(
          box(
            title = "Verbier - Data Summary & Controls", status = "primary", solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(6,
                h4(paste("Total Records:", nrow(verbier_data))),
                h4(paste("Date Range:", min(verbier_data$DateTime, na.rm = TRUE), "to", max(verbier_data$DateTime, na.rm = TRUE)))
              ),
              column(6,
                h5("Temporal Resolution:"),
                selectInput("verbier_resolution", "Choose Resolution:", 
                           choices = list("10 minutes" = "10min", 
                                        "1 hour" = "1hour", 
                                        "6 hours" = "6hour", 
                                        "12 hours" = "12hour", 
                                        "Daily" = "daily"),
                           selected = "1hour"),
                br(),
                downloadButton("save_verbier_corrected", "Save Corrected Data", class = "btn-success")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Dissolved Oxygen Time Series", status = "primary", solidHeader = TRUE,
            width = 6,
            plotlyOutput("verbier_do_plot")
          ),
          box(
            title = "Temperature Time Series", status = "primary", solidHeader = TRUE,
            width = 6,
            plotlyOutput("verbier_temp_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "DO vs Temperature Correlation", status = "primary", solidHeader = TRUE,
            width = 6,
            plotlyOutput("verbier_correlation_plot")
          ),
          box(
            title = "Corrected vs Original DO", status = "primary", solidHeader = TRUE,
            width = 6,
            plotlyOutput("verbier_correction_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Data Table", status = "primary", solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("verbier_table")
          )
        )
      ),
      
      # Bagnes Tab
      tabItem(tabName = "bagnes",
        fluidRow(
          box(
            title = "Bagnes - Data Summary & Controls", status = "info", solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(6,
                h4(paste("Total Records:", nrow(bagnes_data))),
                h4(paste("Date Range:", min(bagnes_data$DateTime, na.rm = TRUE), "to", max(bagnes_data$DateTime, na.rm = TRUE)))
              ),
              column(6,
                h5("Temporal Resolution:"),
                selectInput("bagnes_resolution", "Choose Resolution:", 
                           choices = list("10 minutes" = "10min", 
                                        "1 hour" = "1hour", 
                                        "6 hours" = "6hour", 
                                        "12 hours" = "12hour", 
                                        "Daily" = "daily"),
                           selected = "1hour"),
                br(),
                downloadButton("save_bagnes_corrected", "Save Corrected Data", class = "btn-success")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Dissolved Oxygen Time Series", status = "info", solidHeader = TRUE,
            width = 6,
            plotlyOutput("bagnes_do_plot")
          ),
          box(
            title = "Temperature Time Series", status = "info", solidHeader = TRUE,
            width = 6,
            plotlyOutput("bagnes_temp_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "DO vs Temperature Correlation", status = "info", solidHeader = TRUE,
            width = 6,
            plotlyOutput("bagnes_correlation_plot")
          ),
          box(
            title = "Corrected vs Original DO", status = "info", solidHeader = TRUE,
            width = 6,
            plotlyOutput("bagnes_correction_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Data Table", status = "info", solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("bagnes_table")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive values for raw data manipulation
  raw_verbier_data <- reactiveVal(verbier_data)
  raw_bagnes_data <- reactiveVal(bagnes_data)
  
  # Reactive values for zoom ranges
  verbier_do_zoom <- reactiveVal(NULL)
  verbier_temp_zoom <- reactiveVal(NULL)
  bagnes_do_zoom <- reactiveVal(NULL)
  bagnes_temp_zoom <- reactiveVal(NULL)
  
  # Reactive values for last selection (for Ctrl+D deletion)
  verbier_do_last_selection <- reactiveVal(NULL)
  verbier_temp_last_selection <- reactiveVal(NULL)
  bagnes_do_last_selection <- reactiveVal(NULL)
  bagnes_temp_last_selection <- reactiveVal(NULL)
  
  # Reset functions
  observeEvent(input$reset_verbier, {
    raw_verbier_data(verbier_data)
    # Reset zoom ranges
    verbier_do_zoom(NULL)
    verbier_temp_zoom(NULL)
    showNotification("Verbier data reset to original values", type = "message")
  })
  
  observeEvent(input$reset_bagnes, {
    raw_bagnes_data(bagnes_data)
    # Reset zoom ranges
    bagnes_do_zoom(NULL)
    bagnes_temp_zoom(NULL)
    showNotification("Bagnes data reset to original values", type = "message")
  })
  
  # Raw Data Plots with Selection
  output$raw_verbier_do_plot <- renderPlot({
    data <- raw_verbier_data()
    # Create a column to track selected/deleted points
    data$is_deleted <- is.na(data$DO_conc_mg_L) | is.na(data$Temp_C)
    
    # Apply zoom if available
    zoom_range <- verbier_do_zoom()
    if (!is.null(zoom_range)) {
      xlim <- c(as.POSIXct(zoom_range$xmin, origin = "1970-01-01"), 
                as.POSIXct(zoom_range$xmax, origin = "1970-01-01"))
      ylim <- c(zoom_range$ymin, zoom_range$ymax)
    } else {
      xlim <- range(data$DateTime, na.rm = TRUE)
      ylim <- range(data$DO_conc_mg_L, na.rm = TRUE)
    }
    
    plot(data$DateTime, data$DO_conc_mg_L, type = "l", col = "blue", 
         main = "Raw dissolved oxygen - Verbier (rectangle selection)", 
         xlab = "Date time", ylab = "DO concentration (mg/L)",
         xlim = xlim, ylim = ylim)
    points(data$DateTime[data$is_deleted], data$DO_conc_mg_L[data$is_deleted], col = "red", pch = 16)
  })
  
  output$raw_verbier_temp_plot <- renderPlot({
    data <- raw_verbier_data()
    # Create a column to track selected/deleted points
    data$is_deleted <- is.na(data$DO_conc_mg_L) | is.na(data$Temp_C)
    
    # Apply zoom if available
    zoom_range <- verbier_temp_zoom()
    if (!is.null(zoom_range)) {
      xlim <- c(as.POSIXct(zoom_range$xmin, origin = "1970-01-01"), 
                as.POSIXct(zoom_range$xmax, origin = "1970-01-01"))
      ylim <- c(zoom_range$ymin, zoom_range$ymax)
    } else {
      xlim <- range(data$DateTime, na.rm = TRUE)
      ylim <- range(data$Temp_C, na.rm = TRUE)
    }
    
    plot(data$DateTime, data$Temp_C, type = "l", col = "orange", 
         main = "Raw temperature - Verbier (rectangle selection)", 
         xlab = "Date time", ylab = "Temperature (°C)",
         xlim = xlim, ylim = ylim)
    points(data$DateTime[data$is_deleted], data$Temp_C[data$is_deleted], col = "red", pch = 16)
  })
  
  output$raw_bagnes_do_plot <- renderPlot({
    data <- raw_bagnes_data()
    # Create a column to track selected/deleted points
    data$is_deleted <- is.na(data$DO_conc_mg_L) | is.na(data$Temp_C)
    
    # Apply zoom if available
    zoom_range <- bagnes_do_zoom()
    if (!is.null(zoom_range)) {
      xlim <- c(as.POSIXct(zoom_range$xmin, origin = "1970-01-01"), 
                as.POSIXct(zoom_range$xmax, origin = "1970-01-01"))
      ylim <- c(zoom_range$ymin, zoom_range$ymax)
    } else {
      xlim <- range(data$DateTime, na.rm = TRUE)
      ylim <- range(data$DO_conc_mg_L, na.rm = TRUE)
    }
    
    plot(data$DateTime, data$DO_conc_mg_L, type = "l", col = "darkgreen", 
         main = "Raw dissolved oxygen - Bagnes (rectangle selection)", 
         xlab = "Date time", ylab = "DO concentration (mg/L)",
         xlim = xlim, ylim = ylim)
    points(data$DateTime[data$is_deleted], data$DO_conc_mg_L[data$is_deleted], col = "red", pch = 16)
  })
  
  output$raw_bagnes_temp_plot <- renderPlot({
    data <- raw_bagnes_data()
    # Create a column to track selected/deleted points
    data$is_deleted <- is.na(data$DO_conc_mg_L) | is.na(data$Temp_C)
    
    # Apply zoom if available
    zoom_range <- bagnes_temp_zoom()
    if (!is.null(zoom_range)) {
      xlim <- c(as.POSIXct(zoom_range$xmin, origin = "1970-01-01"), 
                as.POSIXct(zoom_range$xmax, origin = "1970-01-01"))
      ylim <- c(zoom_range$ymin, zoom_range$ymax)
    } else {
      xlim <- range(data$DateTime, na.rm = TRUE)
      ylim <- range(data$Temp_C, na.rm = TRUE)
    }
    
    plot(data$DateTime, data$Temp_C, type = "l", col = "orange", 
         main = "Raw temperature - Bagnes (rectangle selection)", 
         xlab = "Date time", ylab = "Temperature (°C)",
         xlim = xlim, ylim = ylim)
    points(data$DateTime[data$is_deleted], data$Temp_C[data$is_deleted], col = "red", pch = 16)
  })
  
  # Handle brush selections - Updated for zoom and specific point selection
  observe({
    brush <- input$raw_verbier_do_plot_brush
    if (!is.null(brush)) {
      # Store zoom range
      verbier_do_zoom(brush)
      verbier_do_last_selection(brush)
    }
  })
  
  observe({
    brush <- input$raw_verbier_temp_plot_brush
    if (!is.null(brush)) {
      # Store zoom range
      verbier_temp_zoom(brush)
      verbier_temp_last_selection(brush)
    }
  })
  
  observe({
    brush <- input$raw_bagnes_do_plot_brush
    if (!is.null(brush)) {
      # Store zoom range
      bagnes_do_zoom(brush)
      bagnes_do_last_selection(brush)
    }
  })
  
  observe({
    brush <- input$raw_bagnes_temp_plot_brush
    if (!is.null(brush)) {
      # Store zoom range
      bagnes_temp_zoom(brush)
      bagnes_temp_last_selection(brush)
    }
  })
  
  # Handle double-click events to reset zoom
  observeEvent(input$raw_verbier_do_plot_dblclick, {
    verbier_do_zoom(NULL)
    showNotification("Verbier DO zoom reset to full data range", type = "message")
  })
  
  observeEvent(input$raw_verbier_temp_plot_dblclick, {
    verbier_temp_zoom(NULL)
    showNotification("Verbier Temperature zoom reset to full data range", type = "message")
  })
  
  observeEvent(input$raw_bagnes_do_plot_dblclick, {
    bagnes_do_zoom(NULL)
    showNotification("Bagnes DO zoom reset to full data range", type = "message")
  })
  
  observeEvent(input$raw_bagnes_temp_plot_dblclick, {
    bagnes_temp_zoom(NULL)
    showNotification("Bagnes Temperature zoom reset to full data range", type = "message")
  })
  
  # Selection info outputs
  output$verbier_selection_info <- renderText({
    valid_points <- sum(!is.na(raw_verbier_data()$DO_conc_mg_L) & !is.na(raw_verbier_data()$Temp_C))
    paste("Valid data points:", valid_points, "out of", nrow(raw_verbier_data()), "total points")
  })
  
  output$bagnes_selection_info <- renderText({
    valid_points <- sum(!is.na(raw_bagnes_data()$DO_conc_mg_L) & !is.na(raw_bagnes_data()$Temp_C))
    paste("Valid data points:", valid_points, "out of", nrow(raw_bagnes_data()), "total points")
  })
  
  # Raw data tables
  output$raw_verbier_table <- DT::renderDataTable({
    raw_verbier_data() %>%
      select(DateTime, DO_conc_mg_L, Temp_C, DO_corrected, Pressure_hPa, Source_File) %>%
      mutate(
        DateTime = as.character(DateTime),
        DO_conc_mg_L = round(DO_conc_mg_L, 2),
        Temp_C = round(Temp_C, 2),
        DO_corrected = round(DO_corrected, 2),
        Pressure_hPa = round(Pressure_hPa, 1)
      ) %>%
      DT::datatable(options = list(pageLength = 15, scrollX = TRUE))
  })
  
  output$raw_bagnes_table <- DT::renderDataTable({
    raw_bagnes_data() %>%
      select(DateTime, DO_conc_mg_L, Temp_C, DO_corrected, Pressure_hPa, Source_File) %>%
      mutate(
        DateTime = as.character(DateTime),
        DO_conc_mg_L = round(DO_conc_mg_L, 2),
        Temp_C = round(Temp_C, 2),
        DO_corrected = round(DO_corrected, 2),
        Pressure_hPa = round(Pressure_hPa, 1)
      ) %>%
      DT::datatable(options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # Verbier plots (now using reactive data with temporal resolution)
  output$verbier_do_plot <- renderPlotly({
    aggregated_data <- aggregate_by_resolution(raw_verbier_data(), input$verbier_resolution)
    p <- ggplot(aggregated_data, aes(x = DateTime)) +
      geom_line(aes(y = DO_conc_mg_L, color = "Original"), alpha = 0.7) +
      geom_line(aes(y = DO_corrected, color = "Corrected"), alpha = 0.7) +
      labs(title = "Dissolved oxygen concentration over time (Verbier)",
           x = "Date time", y = "DO concentration (mg/L)",
           color = "Type") +
      theme_minimal() +
      scale_color_manual(values = c("Original" = "blue", "Corrected" = "red"))
    
    ggplotly(p)
  })
  
  output$verbier_temp_plot <- renderPlotly({
    aggregated_data <- aggregate_by_resolution(raw_verbier_data(), input$verbier_resolution)
    p <- ggplot(aggregated_data, aes(x = DateTime, y = Temp_C)) +
      geom_line(color = "orange", alpha = 0.7) +
      labs(title = "Temperature over time (Verbier)",
           x = "Date time", y = "Temperature (°C)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$verbier_correlation_plot <- renderPlotly({
    aggregated_data <- aggregate_by_resolution(raw_verbier_data(), input$verbier_resolution)
    p <- ggplot(aggregated_data, aes(x = Temp_C, y = DO_corrected)) +
      geom_point(alpha = 0.6, color = "darkblue") +
      geom_smooth(method = "lm", se = TRUE, color = "red") +
      labs(title = "Corrected DO vs temperature (Verbier)",
           x = "Temperature (°C)", y = "Corrected DO (mg/L)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$verbier_correction_plot <- renderPlotly({
    aggregated_data <- aggregate_by_resolution(raw_verbier_data(), input$verbier_resolution)
    p <- ggplot(aggregated_data, aes(x = DO_conc_mg_L, y = DO_corrected)) +
      geom_point(alpha = 0.6, color = "purple") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
      labs(title = "Original vs corrected DO (Verbier)",
           x = "Original DO (mg/L)", y = "Corrected DO (mg/L)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Bagnes plots (now using reactive data with temporal resolution)
  output$bagnes_do_plot <- renderPlotly({
    aggregated_data <- aggregate_by_resolution(raw_bagnes_data(), input$bagnes_resolution)
    p <- ggplot(aggregated_data, aes(x = DateTime)) +
      geom_line(aes(y = DO_conc_mg_L, color = "Original"), alpha = 0.7) +
      geom_line(aes(y = DO_corrected, color = "Corrected"), alpha = 0.7) +
      labs(title = "Dissolved oxygen concentration over time (Bagnes)",
           x = "Date time", y = "DO concentration (mg/L)",
           color = "Type") +
      theme_minimal() +
      scale_color_manual(values = c("Original" = "blue", "Corrected" = "red"))
    
    ggplotly(p)
  })
  
  output$bagnes_temp_plot <- renderPlotly({
    aggregated_data <- aggregate_by_resolution(raw_bagnes_data(), input$bagnes_resolution)
    p <- ggplot(aggregated_data, aes(x = DateTime, y = Temp_C)) +
      geom_line(color = "orange", alpha = 0.7) +
      labs(title = "Temperature over time (Bagnes)",
           x = "Date time", y = "Temperature (°C)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$bagnes_correlation_plot <- renderPlotly({
    aggregated_data <- aggregate_by_resolution(raw_bagnes_data(), input$bagnes_resolution)
    p <- ggplot(aggregated_data, aes(x = Temp_C, y = DO_corrected)) +
      geom_point(alpha = 0.6, color = "darkgreen") +
      geom_smooth(method = "lm", se = TRUE, color = "red") +
      labs(title = "Corrected DO vs temperature (Bagnes)",
           x = "Temperature (°C)", y = "Corrected DO (mg/L)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$bagnes_correction_plot <- renderPlotly({
    aggregated_data <- aggregate_by_resolution(raw_bagnes_data(), input$bagnes_resolution)
    p <- ggplot(aggregated_data, aes(x = DO_conc_mg_L, y = DO_corrected)) +
      geom_point(alpha = 0.6, color = "darkred") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
      labs(title = "Original vs corrected DO (Bagnes)",
           x = "Original DO (mg/L)", y = "Corrected DO (mg/L)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Data Tables (now using reactive data)
  output$verbier_table <- DT::renderDataTable({
    aggregated_data <- aggregate_by_resolution(raw_verbier_data(), input$verbier_resolution)
    aggregated_data %>%
      select(DateTime, DO_conc_mg_L, DO_corrected, Temp_C, Pressure_hPa, Source_File) %>%
      mutate(
        DateTime = as.character(DateTime),
        DO_conc_mg_L = round(DO_conc_mg_L, 2),
        DO_corrected = round(DO_corrected, 2),
        Temp_C = round(Temp_C, 2),
        Pressure_hPa = round(Pressure_hPa, 1)
      ) %>%
      DT::datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$bagnes_table <- DT::renderDataTable({
    aggregated_data <- aggregate_by_resolution(raw_bagnes_data(), input$bagnes_resolution)
    aggregated_data %>%
      select(DateTime, DO_conc_mg_L, DO_corrected, Temp_C, Pressure_hPa, Source_File) %>%
      mutate(
        DateTime = as.character(DateTime),
        DO_conc_mg_L = round(DO_conc_mg_L, 2),
        DO_corrected = round(DO_corrected, 2),
        Temp_C = round(Temp_C, 2),
        Pressure_hPa = round(Pressure_hPa, 1)
      ) %>%
      DT::datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Save functionality
  output$save_verbier <- downloadHandler(
    filename = function() {
      paste("verbier_cleaned_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(raw_verbier_data(), file, row.names = FALSE)
    }
  )
  
  output$save_bagnes <- downloadHandler(
    filename = function() {
      paste("bagnes_cleaned_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(raw_bagnes_data(), file, row.names = FALSE)
    }
  )
  
  # Save functionality for corrected data with BP
  output$save_verbier_corrected <- downloadHandler(
    filename = function() {
      resolution <- input$verbier_resolution
      paste("verbier_corrected_", resolution, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      aggregated_data <- aggregate_by_resolution(raw_verbier_data(), input$verbier_resolution)
      # Include BP in the export
      export_data <- aggregated_data %>%
        select(DateTime, DO_conc_mg_L, DO_corrected, Temp_C, Pressure_hPa) %>%
        rename(
          "Date_Time" = DateTime,
          "DO_Original_mg_L" = DO_conc_mg_L,
          "DO_Corrected_mg_L" = DO_corrected,
          "Temperature_C" = Temp_C,
          "Barometric_Pressure_hPa" = Pressure_hPa
        )
      write.csv(export_data, file, row.names = FALSE)
    }
  )
  
  output$save_bagnes_corrected <- downloadHandler(
    filename = function() {
      resolution <- input$bagnes_resolution
      paste("bagnes_corrected_", resolution, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      aggregated_data <- aggregate_by_resolution(raw_bagnes_data(), input$bagnes_resolution)
      # Include BP in the export
      export_data <- aggregated_data %>%
        select(DateTime, DO_conc_mg_L, DO_corrected, Temp_C, Pressure_hPa) %>%
        rename(
          "Date_Time" = DateTime,
          "DO_Original_mg_L" = DO_conc_mg_L,
          "DO_Corrected_mg_L" = DO_corrected,
          "Temperature_C" = Temp_C,
          "Barometric_Pressure_hPa" = Pressure_hPa
        )
      write.csv(export_data, file, row.names = FALSE)
    }
  )
  
  # Handle Ctrl+D for deletion of last selected area
  observeEvent(input$ctrl_d_pressed, {
    # Check which tab is currently active and delete from the appropriate dataset
    # We'll check if there are recent selections and delete from all that have them
    
    # Verbier DO deletion
    selection <- verbier_do_last_selection()
    if (!is.null(selection)) {
      data <- raw_verbier_data()
      
      # Get the selection bounds
      time_min <- as.POSIXct(selection$xmin, origin = "1970-01-01")
      time_max <- as.POSIXct(selection$xmax, origin = "1970-01-01")
      value_min <- selection$ymin
      value_max <- selection$ymax
      
      # Find points within the rectangular selection
      selected_indices <- which(
        data$DateTime >= time_min & 
        data$DateTime <= time_max &
        data$DO_conc_mg_L >= value_min & 
        data$DO_conc_mg_L <= value_max &
        !is.na(data$DO_conc_mg_L)
      )
      
      if (length(selected_indices) > 0) {
        # Delete both DO and temperature for selected points
        data$DO_conc_mg_L[selected_indices] <- NA
        data$Temp_C[selected_indices] <- NA
        # Recalculate corrected DO
        data$DO_corrected <- correct_DO(data$DO_conc_mg_L, data$Temp_C, data$Pressure_hPa)
        raw_verbier_data(data)
        showNotification(paste("Deleted", length(selected_indices), "Verbier DO data points with Ctrl+D"), type = "warning")
        # Clear the selection after deletion
        verbier_do_last_selection(NULL)
      }
    }
    
    # Verbier Temperature deletion
    selection <- verbier_temp_last_selection()
    if (!is.null(selection)) {
      data <- raw_verbier_data()
      
      # Get the selection bounds
      time_min <- as.POSIXct(selection$xmin, origin = "1970-01-01")
      time_max <- as.POSIXct(selection$xmax, origin = "1970-01-01")
      value_min <- selection$ymin
      value_max <- selection$ymax
      
      # Find points within the rectangular selection
      selected_indices <- which(
        data$DateTime >= time_min & 
        data$DateTime <= time_max &
        data$Temp_C >= value_min & 
        data$Temp_C <= value_max &
        !is.na(data$Temp_C)
      )
      
      if (length(selected_indices) > 0) {
        # Delete both DO and temperature for selected points
        data$DO_conc_mg_L[selected_indices] <- NA
        data$Temp_C[selected_indices] <- NA
        # Recalculate corrected DO
        data$DO_corrected <- correct_DO(data$DO_conc_mg_L, data$Temp_C, data$Pressure_hPa)
        raw_verbier_data(data)
        showNotification(paste("Deleted", length(selected_indices), "Verbier Temperature data points with Ctrl+D"), type = "warning")
        # Clear the selection after deletion
        verbier_temp_last_selection(NULL)
      }
    }
    
    # Bagnes DO deletion
    selection <- bagnes_do_last_selection()
    if (!is.null(selection)) {
      data <- raw_bagnes_data()
      
      # Get the selection bounds
      time_min <- as.POSIXct(selection$xmin, origin = "1970-01-01")
      time_max <- as.POSIXct(selection$xmax, origin = "1970-01-01")
      value_min <- selection$ymin
      value_max <- selection$ymax
      
      # Find points within the rectangular selection
      selected_indices <- which(
        data$DateTime >= time_min & 
        data$DateTime <= time_max &
        data$DO_conc_mg_L >= value_min & 
        data$DO_conc_mg_L <= value_max &
        !is.na(data$DO_conc_mg_L)
      )
      
      if (length(selected_indices) > 0) {
        # Delete both DO and temperature for selected points
        data$DO_conc_mg_L[selected_indices] <- NA
        data$Temp_C[selected_indices] <- NA
        # Recalculate corrected DO
        data$DO_corrected <- correct_DO(data$DO_conc_mg_L, data$Temp_C, data$Pressure_hPa)
        raw_bagnes_data(data)
        showNotification(paste("Deleted", length(selected_indices), "Bagnes DO data points with Ctrl+D"), type = "warning")
        # Clear the selection after deletion
        bagnes_do_last_selection(NULL)
      }
    }
    
    # Bagnes Temperature deletion
    selection <- bagnes_temp_last_selection()
    if (!is.null(selection)) {
      data <- raw_bagnes_data()
      
      # Get the selection bounds
      time_min <- as.POSIXct(selection$xmin, origin = "1970-01-01")
      time_max <- as.POSIXct(selection$xmax, origin = "1970-01-01")
      value_min <- selection$ymin
      value_max <- selection$ymax
      
      # Find points within the rectangular selection
      selected_indices <- which(
        data$DateTime >= time_min & 
        data$DateTime <= time_max &
        data$Temp_C >= value_min & 
        data$Temp_C <= value_max &
        !is.na(data$Temp_C)
      )
      
      if (length(selected_indices) > 0) {
        # Delete both DO and temperature for selected points
        data$DO_conc_mg_L[selected_indices] <- NA
        data$Temp_C[selected_indices] <- NA
        # Recalculate corrected DO
        data$DO_corrected <- correct_DO(data$DO_conc_mg_L, data$Temp_C, data$Pressure_hPa)
        raw_bagnes_data(data)
        showNotification(paste("Deleted", length(selected_indices), "Bagnes Temperature data points with Ctrl+D"), type = "warning")
        # Clear the selection after deletion
        bagnes_temp_last_selection(NULL)
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
