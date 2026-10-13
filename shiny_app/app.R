# ============================================================
# SMART TRAFFIC CONGESTION PREDICTION
# R Shiny Application
# ============================================================


# ============================================================
# 1. LOAD REQUIRED PACKAGES
# ============================================================

library(shiny)
library(randomForest)


# ============================================================
# 2. LOAD TRAINED RANDOM FOREST MODEL
# ============================================================

rf_model <- readRDS("../models/random_forest_model.rds")


# ============================================================
# 3. USER INTERFACE
# ============================================================

ui <- fluidPage(
  
  # ----------------------------------------------------------
  # Custom CSS styling
  # ----------------------------------------------------------
  
  tags$head(
    
    tags$style(HTML("

      body {
        background-color: #f5f7fa;
        font-family: Arial, sans-serif;
      }

      .title-panel {
        background-color: #1f4e79;
        color: white;
        padding: 25px;
        margin-bottom: 20px;
        border-radius: 8px;
      }

      .title-panel h1 {
        margin: 0;
        font-weight: bold;
      }

      .subtitle {
        margin-top: 8px;
        font-size: 16px;
      }

      .well {
        background-color: white;
        border: 1px solid #d9dee5;
        border-radius: 8px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.08);
      }

      .section-title {
        font-weight: bold;
        color: #1f4e79;
        margin-bottom: 20px;
      }

      .prediction-box {
        padding: 35px;
        margin-top: 20px;
        margin-bottom: 25px;
        border-radius: 12px;
        text-align: center;
        font-size: 36px;
        font-weight: bold;
      }

      .prediction-low {
        background-color: #dff0d8;
        color: #3c763d;
        border: 2px solid #5cb85c;
      }

      .prediction-medium {
        background-color: #fcf8e3;
        color: #8a6d3b;
        border: 2px solid #f0ad4e;
      }

      .prediction-high {
        background-color: #f2dede;
        color: #a94442;
        border: 2px solid #d9534f;
      }

      .info-box {
        background-color: white;
        border-left: 5px solid #1f4e79;
        padding: 18px;
        margin-top: 20px;
        border-radius: 5px;
      }

      .info-title {
        font-weight: bold;
        color: #1f4e79;
        font-size: 18px;
        margin-bottom: 10px;
      }

      .status-box {
        background-color: #ffffff;
        padding: 15px;
        border-radius: 8px;
        border: 1px solid #d9dee5;
        margin-bottom: 10px;
      }

      .btn-primary {
        width: 100%;
        font-size: 16px;
        font-weight: bold;
        padding: 12px;
      }

      .note {
        color: #666666;
        font-size: 13px;
      }

    "))
    
  ),
  
  
  # ==========================================================
  # PAGE TITLE
  # ==========================================================
  
  div(
    class = "title-panel",
    
    h1("Smart Traffic Congestion Prediction"),
    
    div(
      class = "subtitle",
      "Machine Learning based traffic congestion classification using Random Forest"
    )
    
  ),
  
  
  # ==========================================================
  # MAIN LAYOUT
  # ==========================================================
  
  sidebarLayout(
    
    
    # ========================================================
    # LEFT SIDE - INPUTS
    # ========================================================
    
    sidebarPanel(
      
      h3(
        class = "section-title",
        "Traffic and Weather Information"
      ),
      
      
      # ------------------------------------------------------
      # Holiday
      # ------------------------------------------------------
      
      selectInput(
        "holiday",
        "Holiday:",
        choices = rf_model$forest$xlevels$holiday,
        selected = "None"
      ),
      
      
      # ------------------------------------------------------
      # Temperature
      # ------------------------------------------------------
      
      numericInput(
        "temp",
        "Temperature (Kelvin):",
        value = 288,
        min = 243,
        max = 310,
        step = 0.1
      ),
      
      div(
        class = "note",
        "Temperature range is based on the project dataset."
      ),
      
      br(),
      
      
      # ------------------------------------------------------
      # Rainfall
      # ------------------------------------------------------
      
      numericInput(
        "rain_1h",
        "Rainfall (mm):",
        value = 0,
        min = 0,
        max = 55.63,
        step = 0.1
      ),
      
      
      # ------------------------------------------------------
      # Snowfall
      # ------------------------------------------------------
      
      numericInput(
        "snow_1h",
        "Snowfall (mm):",
        value = 0,
        min = 0,
        max = 0.51,
        step = 0.01
      ),
      
      
      # ------------------------------------------------------
      # Cloud coverage
      # ------------------------------------------------------
      
      numericInput(
        "clouds_all",
        "Cloud Coverage (%):",
        value = 50,
        min = 0,
        max = 100,
        step = 1
      ),
      
      
      # ------------------------------------------------------
      # Weather
      # ------------------------------------------------------
      
      selectInput(
        "weather_main",
        "Weather:",
        choices = rf_model$forest$xlevels$weather_main,
        selected = "Clear"
      ),
      
      
      # ------------------------------------------------------
      # Hour
      # ------------------------------------------------------
      
      selectInput(
        "hour",
        "Hour of Day:",
        choices = 0:23,
        selected = 12
      ),
      
      
      # ------------------------------------------------------
      # Day of week
      # ------------------------------------------------------
      
      selectInput(
        "day_of_week",
        "Day of Week:",
        choices = rf_model$forest$xlevels$day_of_week,
        selected = "Monday"
      ),
      
      
      # ------------------------------------------------------
      # Month
      # ------------------------------------------------------
      
      selectInput(
        "month",
        "Month:",
        choices = rf_model$forest$xlevels$month,
        selected = "January"
      ),
      
      
      # ------------------------------------------------------
      # Year
      # ------------------------------------------------------
      
      numericInput(
        "year",
        "Year:",
        value = 2018,
        min = 2012,
        max = 2018,
        step = 1
      ),
      
      div(
        class = "note",
        "The model was developed using traffic data from 2012-2018."
      ),
      
      br(),
      
      
      # ------------------------------------------------------
      # Prediction button
      # ------------------------------------------------------
      
      actionButton(
        "predict",
        "Predict Congestion",
        class = "btn-primary"
      )
      
    ),
    
    
    # ========================================================
    # RIGHT SIDE - OUTPUT
    # ========================================================
    
    mainPanel(
      
      h3(
        class = "section-title",
        "Prediction Result"
      ),
      
      
      # ------------------------------------------------------
      # Prediction result
      # ------------------------------------------------------
      
      uiOutput("prediction_box"),
      
      
      # ------------------------------------------------------
      # Derived information
      # ------------------------------------------------------
      
      h3(
        class = "section-title",
        "Derived Information"
      ),
      
      uiOutput("prediction_details"),
      
      
      # ------------------------------------------------------
      # Model information
      # ------------------------------------------------------
      
      div(
        class = "info-box",
        
        div(
          class = "info-title",
          "About the Prediction Model"
        ),
        
        p(
          "The application uses a Random Forest classification model "
          ,"trained to classify traffic congestion into three levels:"
        ),
        
        tags$ul(
          tags$li("Low Congestion"),
          tags$li("Medium Congestion"),
          tags$li("High Congestion")
        ),
        
        p(
          "The model uses traffic-related time, weather, and calendar "
          ,"features to generate the prediction."
        )
        
      ),
      
      
      # ------------------------------------------------------
      # Project information
      # ------------------------------------------------------
      
      div(
        class = "info-box",
        
        div(
          class = "info-title",
          "Project Information"
        ),
        
        p(
          strong("Project: "),
          "Smart Traffic Congestion Prediction"
        ),
        
        p(
          strong("Dataset: "),
          "Metro Interstate Traffic Volume Dataset"
        ),
        
        p(
          strong("Machine Learning Algorithm: "),
          "Random Forest"
        ),
        
        p(
          strong("Target: "),
          "Low / Medium / High Congestion"
        ),
        
        p(
          strong("Application Framework: "),
          "R Shiny"
        )
        
      )
      
    )
    
  )
  
)


# ============================================================
# 4. SERVER LOGIC
# ============================================================

server <- function(input, output, session) {
  
  
  # ==========================================================
  # PREDICTION FUNCTION
  # ==========================================================
  
  prediction <- eventReactive(input$predict, {
    
    
    # --------------------------------------------------------
    # Input validation
    # --------------------------------------------------------
    
    validate(
      
      need(
        !is.null(input$temp) &&
          input$temp >= 243 &&
          input$temp <= 310,
        "Temperature must be between 243 K and 310 K."
      ),
      
      need(
        !is.null(input$rain_1h) &&
          input$rain_1h >= 0 &&
          input$rain_1h <= 55.63,
        "Rainfall must be between 0 and 55.63 mm."
      ),
      
      need(
        !is.null(input$snow_1h) &&
          input$snow_1h >= 0 &&
          input$snow_1h <= 0.51,
        "Snowfall must be between 0 and 0.51 mm."
      ),
      
      need(
        !is.null(input$clouds_all) &&
          input$clouds_all >= 0 &&
          input$clouds_all <= 100,
        "Cloud coverage must be between 0% and 100%."
      ),
      
      need(
        !is.null(input$year) &&
          input$year >= 2012 &&
          input$year <= 2018,
        "Year must be between 2012 and 2018."
      )
      
    )
    
    
    # --------------------------------------------------------
    # Convert hour to numeric
    # --------------------------------------------------------
    
    hour_value <- as.numeric(input$hour)
    
    
    # --------------------------------------------------------
    # Determine weekend
    # --------------------------------------------------------
    
    is_weekend_value <- ifelse(
      
      input$day_of_week %in%
        c("Saturday", "Sunday"),
      
      "TRUE",
      
      "FALSE"
      
    )
    
    
    # --------------------------------------------------------
    # Determine rush hour
    # --------------------------------------------------------
    
    is_rush_hour_value <- ifelse(
      
      hour_value %in%
        c(6, 7, 8, 9, 15, 16, 17, 18),
      
      "Yes",
      
      "No"
      
    )
    
    
    # --------------------------------------------------------
    # Create prediction data frame
    # --------------------------------------------------------
    
    new_data <- data.frame(
      
      holiday = factor(
        input$holiday,
        levels = rf_model$forest$xlevels$holiday
      ),
      
      temp = input$temp,
      
      rain_1h = input$rain_1h,
      
      snow_1h = input$snow_1h,
      
      clouds_all = input$clouds_all,
      
      weather_main = factor(
        input$weather_main,
        levels = rf_model$forest$xlevels$weather_main
      ),
      
      hour = hour_value,
      
      day_of_week = factor(
        input$day_of_week,
        levels = rf_model$forest$xlevels$day_of_week
      ),
      
      month = factor(
        input$month,
        levels = rf_model$forest$xlevels$month
      ),
      
      year = input$year,
      
      is_weekend = factor(
        is_weekend_value,
        levels = rf_model$forest$xlevels$is_weekend
      ),
      
      is_rush_hour = factor(
        is_rush_hour_value,
        levels = rf_model$forest$xlevels$is_rush_hour
      )
      
    )
    
    
    # --------------------------------------------------------
    # Generate Random Forest prediction
    # --------------------------------------------------------
    
    predicted_class <- predict(
      
      rf_model,
      
      newdata = new_data,
      
      type = "response"
      
    )
    
    
    # --------------------------------------------------------
    # Return prediction
    # --------------------------------------------------------
    
    as.character(predicted_class)
    
  })
  
  
  # ==========================================================
  # DISPLAY PREDICTION
  # ==========================================================
  
  output$prediction_box <- renderUI({
    
    req(prediction())
    
    
    predicted_value <- prediction()
    
    
    # --------------------------------------------------------
    # Select CSS class based on prediction
    # --------------------------------------------------------
    
    prediction_class <- switch(
      
      predicted_value,
      
      "Low" = "prediction-low",
      
      "Medium" = "prediction-medium",
      
      "High" = "prediction-high"
      
    )
    
    
    # --------------------------------------------------------
    # Display prediction
    # --------------------------------------------------------
    
    div(
      
      class = paste(
        "prediction-box",
        prediction_class
      ),
      
      tags$div(
        style = "font-size: 18px; margin-bottom: 10px;",
        "Predicted Congestion Level"
      ),
      
      tags$div(
        predicted_value
      )
      
    )
    
  })
  
  
  # ==========================================================
  # DISPLAY PREDICTION DETAILS
  # ==========================================================
  
  output$prediction_details <- renderUI({
    
    req(prediction())
    
    hour_value <- as.numeric(input$hour)
    
    day_type <- if (
      input$day_of_week %in% c("Saturday", "Sunday")
    ) {
      "Weekend"
    } else {
      "Weekday"
    }
    
    traffic_period <- if (
      hour_value %in% c(6, 7, 8, 9, 15, 16, 17, 18)
    ) {
      "Rush Hour"
    } else {
      "Non-Rush Hour"
    }
    
    div(
      class = "status-box",
      
      p(
        strong("Study Area: "),
        "Westbound I-94, Minneapolis–Saint Paul"
      ),
      
      p(
        strong("Day: "),
        input$day_of_week
      ),
      
      p(
        strong("Time: "),
        sprintf("%02d:00", hour_value)
      ),
      
      p(
        strong("Traffic Period: "),
        traffic_period
      ),
      
      p(
        strong("Day Type: "),
        day_type
      ),
      
      p(
        strong("Weather: "),
        input$weather_main
      ),
      
      p(
        strong("Temperature: "),
        input$temp,
        " K"
      )
    )
    
  })
  
  
  # ==========================================================
  # DISPLAY WEEKEND / WEEKDAY
  # ==========================================================
  
  output$weekend_status <- renderText({
    
    req(input$day_of_week)
    
    
    if (
      input$day_of_week %in%
      c("Saturday", "Sunday")
    ) {
      
      "Day Type: Weekend"
      
    } else {
      
      "Day Type: Weekday"
      
    }
    
  })
  
  
  # ==========================================================
  # DISPLAY RUSH HOUR STATUS
  # ==========================================================
  
  output$rush_hour_status <- renderText({
    
    req(input$hour)
    
    
    hour_value <- as.numeric(input$hour)
    
    
    if (
      hour_value %in%
      c(6, 7, 8, 9, 15, 16, 17, 18)
    ) {
      
      "Traffic Period: Rush Hour"
      
    } else {
      
      "Traffic Period: Non-Rush Hour"
      
    }
    
  })
  
}


# ============================================================
# 5. RUN SHINY APPLICATION
# ============================================================

shinyApp(
  
  ui = ui,
  
  server = server
  
)
