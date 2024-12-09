
library(shiny)
library(ggplot2)
library(wesanderson)


data <- read.csv("parkinsons_updrs.data.csv")

data <- data %>%
  select(-index, -Jitter..., -Jitter.Abs., -Jitter.RAP, -Jitter.PPQ5, -Jitter.DDP, -Shimmer,
         -Shimmer.dB., -Shimmer.APQ3, -Shimmer.APQ5, -Shimmer.APQ11, -Shimmer.DDA, - NHR, - HNR, -RPDE,
         -DFA, -PPE)

data <- data %>%
  arrange(subject., test_time)

data <- data %>%
  group_by(subject.) %>%
  mutate(
    delta_updrs = motor_UPDRS - lag(motor_UPDRS),  # Change in motor UPDRS
    delta_time = test_time - lag(test_time)        # Change in test time
  )

data <- data %>%
  mutate(progression_rate = delta_updrs / delta_time)

data <- data %>%
  filter(!is.na(progression_rate))

data <- data %>%
  select(-delta_updrs, -delta_time)

colors <- wes_palette("Royal2")
box <- colors[c(5,4)]
box2 <- colors[c(5,1)]


ui <- navbarPage(
  "Parkinson's Disease Analysis",
  tabPanel(
    "EDA",
    sidebarLayout(
      sidebarPanel(
        selectInput("plot_choice", 
                    "Choose a Visualization:", 
                    choices = c("Age Distribution", 
                                "UPDRS Progression Over Time", 
                                "UPDRS Scores by Sex", 
                                "Progression Rate vs Age"),
                    selected = "Age Distribution"),
        conditionalPanel(
          condition = "input.plot_choice == 'UPDRS Scores by Sex' || input.plot_choice == 'Progression Rate vs Age'",
          checkboxInput("show_legend", "Show Legend", TRUE)
        )
      ),
      mainPanel(
        plotOutput("eda_plot")
      )
    )
  ),
  tabPanel(
    "Predictive Modeling",
    sidebarLayout(
      sidebarPanel(
        selectInput("model_type", 
                    "Select Model Type:", 
                    choices = c("Linear Regression", "Random Forest"),
                    selected = "Linear Regression"),
        sliderInput("test_split", 
                    "Training-Test Split (%):", 
                    min = 50, max = 90, value = 70),
        actionButton("train_model", "Train Model")
      ),
      mainPanel(
        verbatimTextOutput("model_summary"),
        plotOutput("model_plot")
      )
    )
  )
)

server <- function(input, output) {
  # --- EDA Logic ---
  output$eda_plot <- renderPlot({
    plot_choice <- input$plot_choice
    
    if (plot_choice == "Age Distribution") {
      ggplot(data, aes(x = age)) +
        geom_histogram(binwidth = 5, fill = box[1], color = "black") +
        labs(title = "Age Distribution", x = "Age", y = "Count")
      
    } else if (plot_choice == "UPDRS Progression Over Time") {
      ggplot(data, aes(x = test_time, y = total_UPDRS)) +
        geom_point(alpha = 0.6, color = box[1]) +
        geom_smooth(method = "loess", color = box[2]) +
        labs(title = "UPDRS Progression Over Time", 
             x = "Time Since Recruitment", 
             y = "Total UPDRS Score")
      
    } else if (plot_choice == "UPDRS Scores by Sex") {
      ggplot(data, aes(x = as.factor(sex), y = total_UPDRS, fill = as.factor(sex))) +
        geom_boxplot(color = "black") +
        scale_fill_manual(values = box) +
        labs(title = "UPDRS Scores by Sex", 
             x = "Sex", 
             y = "Total UPDRS Score",
             fill = "Sex") +
        theme_minimal() +
        theme(legend.position = ifelse(input$show_legend, "right", "none"))
      
    } else if (plot_choice == "Progression Rate vs Age") {
      ggplot(data, aes(x = age, y = progression_rate, color = as.factor(sex))) +
        geom_point(alpha = 0.7) +
        geom_smooth(method = "lm", se = FALSE) +
        scale_color_manual(values = box) +
        labs(title = "Progression Rate vs Age", 
             x = "Age", 
             y = "Progression Rate",
             color = "Sex") +
        theme_minimal() +
        theme(legend.position = ifelse(input$show_legend, "right", "none"))
    }
  })
  

  observeEvent(input$train_model, {
    # Split data into training and testing sets
    set.seed(123)
    train_index <- createDataPartition(data$total_UPDRS, p = input$test_split / 100, list = FALSE)
    train_data <- data[train_index, ]
    test_data <- data[-train_index, ]
    
    # Train the selected model
    if (input$model_type == "Linear Regression") {
      model <- lm(total_UPDRS ~ age + sex + test_time, data = train_data)
      predictions <- predict(model, test_data)
      output$model_summary <- renderPrint({
        summary(model)
      })
      
      output$model_plot <- renderPlot({
        ggplot(test_data, aes(x = total_UPDRS, y = predictions)) +
          geom_point(color = box[1], alpha = 0.7) +
          geom_abline(slope = 1, intercept = 0, color = "black", size = 1) + # Black regression line
          labs(title = "Linear Regression Predictions vs Actuals", 
               x = "Actual UPDRS Score", 
               y = "Predicted UPDRS Score")
      })
      
    } else if (input$model_type == "Random Forest") {
      library(randomForest)
      model <- randomForest(total_UPDRS ~ age + sex + test_time, data = train_data, ntree = 100)
      predictions <- predict(model, test_data)
      output$model_summary <- renderPrint({
        model
      })
      
      output$model_plot <- renderPlot({
        ggplot(test_data, aes(x = total_UPDRS, y = predictions)) +
          geom_point(color = box[1], alpha = 0.7) +
          geom_abline(slope = 1, intercept = 0, color = "black", size = 1) + # Black regression line
          labs(title = "Random Forest Predictions vs Actuals", 
               x = "Actual UPDRS Score", 
               y = "Predicted UPDRS Score")
      })
    }
  })
}


shinyApp(ui = ui, server = server)

library(shiny)
runApp("~/Desktop/ms/4 Fall/Data Visualization/Final/final_app.R")

