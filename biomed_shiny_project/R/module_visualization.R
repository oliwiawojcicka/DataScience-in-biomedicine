visualization_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      box(
        width = 6,
        title = "Age distribution",
        status = "primary",
        solidHeader = TRUE,
        plotOutput(ns("age_plot"))
      ),
      
      box(
        width = 6,
        title = "Sex distribution",
        status = "primary",
        solidHeader = TRUE,
        plotOutput(ns("sex_plot"))
      )
    ),
    
    fluidRow(
      box(
        width = 6,
        title = "Blood type distribution",
        status = "primary",
        solidHeader = TRUE,
        plotOutput(ns("blood_plot"))
      ),
      
      box(
        width = 6,
        title = "Dosage by age",
        status = "primary",
        solidHeader = TRUE,
        plotOutput(ns("dosage_age_plot"))
      )
    )
  )
}


visualization_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    
    output$age_plot <- renderPlot({
      ggplot(data_reactive(), aes(x = age)) +
        geom_histogram(
          bins = 30,
          fill = "#2C7FB8",
          color = "white"
        ) +
        labs(
          x = "Age",
          y = "Count",
          title = "Distribution of patient age"
        ) +
        theme_minimal()
    })
    
    output$sex_plot <- renderPlot({
      ggplot(data_reactive(), aes(x = sex)) +
        geom_bar(
          fill = "#2C7FB8",
          color = "white"
        ) +
        labs(
          x = "Sex",
          y = "Count",
          title = "Distribution of patient sex"
        ) +
        theme_minimal()
    })
    
    output$blood_plot <- renderPlot({
      ggplot(data_reactive(), aes(x = blood_type)) +
        geom_bar(
          fill = "#2C7FB8",
          color = "white"
        ) +
        labs(
          x = "Blood type",
          y = "Count",
          title = "Distribution of blood types"
        ) +
        theme_minimal()
    })
    
    output$dosage_age_plot <- renderPlot({
      ggplot(data_reactive(), aes(x = age, y = dosage_mg)) +
        geom_point(
          color = "#2C7FB8",
          alpha = 0.7
        ) +
        labs(
          x = "Age",
          y = "Dosage [mg]",
          title = "Dosage by patient age"
        ) +
        theme_minimal()
    })
  })
}
