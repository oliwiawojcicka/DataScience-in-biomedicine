library(shiny)
library(ggplot2)
library(palmerpenguins)
?penguins

data <- as.data.frame(penguins)
numeric_cols <- c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")
data_clean <- data[complete.cases(data[, numeric_cols, drop = FALSE]), ]

data_clean$species <- as.factor(data_clean$species) 
data_clean$island  <- as.factor(data_clean$island)

data_clean$sex <- as.character(data_clean$sex)
data_clean$sex[is.na(data_clean$sex) | data_clean$sex == ""] <- "unknown"
data_clean$sex <- as.factor(data_clean$sex)

data_clean$year <- as.integer(as.character(data_clean$year))
year_min <- min(data_clean$year, na.rm = TRUE)
year_max <- max(data_clean$year, na.rm = TRUE)

ui <- fluidPage(
  titlePanel("Penguin Measurements Dashboard"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearRange", "Year range:",
                  min = year_min, max = year_max, 
                  value = c(year_min, year_max), 
                  step = 1, sep = ""),
      checkboxGroupInput("species", "Species (select one or more):",
                         choices = levels(data_clean$species),
                         selected = levels(data_clean$species),
                         inline = TRUE),
      checkboxGroupInput("island", "Island (select one or more):",
                         choices = levels(data_clean$island),
                         selected = levels(data_clean$island),
                         inline = TRUE),
      checkboxGroupInput("sex", "Sex (select one or more):",
                         choices = levels(data_clean$sex),
                         selected = levels(data_clean$sex),
                         inline = TRUE),
      selectInput("xvar", "X variable (used in BOTH plots):",
                  choices = numeric_cols, selected = "bill_length_mm"),
      selectInput("yvar", "Y variable (SCATTER only):",
                  choices = numeric_cols, selected = "body_mass_g"),
      sliderInput("bins", "Histogram bins:", 
                  min = 5, max = 60, value = 20)
    ),
    
    mainPanel(
      tabsetPanel(tabPanel("Histogram", plotOutput("histPlot")),
                  tabPanel("Scatter", plotOutput("scatterPlot")))
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    df <- data_clean
    df <- df[df$year >= input$yearRange[1] & df$year <= input$yearRange[2], ,drop = FALSE]
    if (!is.null(input$species)) df <- df[df$species %in% input$species, ,drop = FALSE] else df <- df[0, ]
    if (!is.null(input$island)) df <- df[df$island %in% input$island,  ,drop = FALSE] else df <- df[0, ]
    if (!is.null(input$sex)) df <- df[df$sex %in% input$sex, ,drop = FALSE] else df <- df[0, ]
    df})
  output$histPlot <- renderPlot({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "No data left after filtering. Please change filters."))
    ggplot(df, aes(x = .data[[input$xvar]])) +
      geom_histogram(bins = input$bins, fill = "steelblue", alpha = 0.8) +
      labs(title = paste("Distribution of", input$xvar),
           subtitle = paste("Filtered by year:", input$yearRange[1], "-", input$yearRange[2]),
           x = input$xvar, y = "Count") + theme_minimal()})
  output$scatterPlot <- renderPlot({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "No data left after filtering. Please change filters."))
    ggplot(df, aes(x = .data[[input$xvar]], y = .data[[input$yvar]], color = species)) +
      geom_point(size = 2, alpha = 0.85) +
      labs(title = paste("Relationship:", input$xvar, "vs", input$yvar),
           subtitle = paste("Colored by species | year:", input$yearRange[1], "-", input$yearRange[2]),
           x = input$xvar, y = input$yvar) + theme_minimal()})
}

shinyApp(ui, server)
