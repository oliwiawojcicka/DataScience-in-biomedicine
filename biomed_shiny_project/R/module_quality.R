quality_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      valueBoxOutput(ns("total_records"), width = 3),
      valueBoxOutput(ns("total_missing"), width = 3),
      valueBoxOutput(ns("problem_records"), width = 3),
      valueBoxOutput(ns("avg_completeness"), width = 3)
    ),
    
    fluidRow(
      box(
        width = 6,
        title = "Completeness by variable",
        status = "primary",
        solidHeader = TRUE,
        plotOutput(ns("missing_plot"), height = "420px")
      ),
      
      box(
        width = 6,
        title = "Quality rule violations",
        status = "primary",
        solidHeader = TRUE,
        plotOutput(ns("rules_plot"), height = "420px")
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Completeness metrics",
        status = "primary",
        solidHeader = TRUE,
        DTOutput(ns("completeness_table"))
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Problematic patient records",
        status = "danger",
        solidHeader = TRUE,
        p("This table shows records that failed at least one quality control rule."),
        DTOutput(ns("problems_table"))
      )
    )
  )
}


quality_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    
    quality_data <- reactive({
      quality_flags(data_reactive())
    })
    
    completeness_data <- reactive({
      completeness_metrics(data_reactive())
    })
    
    output$total_records <- renderValueBox({
      valueBox(
        nrow(data_reactive()),
        "Total records",
        icon = icon("database"),
        color = "blue"
      )
    })
    
    output$total_missing <- renderValueBox({
      valueBox(
        sum(is.na(data_reactive())),
        "Missing values",
        icon = icon("triangle-exclamation"),
        color = "yellow"
      )
    })
    
    output$problem_records <- renderValueBox({
      q <- quality_data()
      
      valueBox(
        sum(q$has_problem, na.rm = TRUE),
        "Problematic records",
        icon = icon("bug"),
        color = "red"
      )
    })
    
    output$avg_completeness <- renderValueBox({
      c <- completeness_data()
      avg <- round(100 - mean(c$missing_percent), 1)
      
      valueBox(
        paste0(avg, "%"),
        "Average completeness",
        icon = icon("check-circle"),
        color = "green"
      )
    })
    
    output$missing_plot <- renderPlot({
      metrics <- completeness_data()
      
      ggplot(
        metrics,
        aes(
          x = reorder(variable, missing_percent),
          y = missing_percent
        )
      ) +
        geom_col(fill = "#2C7FB8") +
        coord_flip() +
        labs(
          x = "Variable",
          y = "Missing values [%]",
          title = "Percentage of missing values by variable"
        ) +
        theme_minimal(base_size = 13)
    })
    
    output$rules_plot <- renderPlot({
      q <- quality_data()
      
      rule_summary <- data.frame(
        rule = c(
          "Invalid patient ID",
          "Invalid age",
          "Invalid date of birth",
          "Invalid sex",
          "Invalid weight",
          "Invalid height",
          "Invalid blood type",
          "Missing diagnosis",
          "Invalid dosage",
          "Age/date mismatch"
        ),
        count = c(
          sum(q$invalid_patient_id, na.rm = TRUE),
          sum(q$invalid_age, na.rm = TRUE),
          sum(q$invalid_dob, na.rm = TRUE),
          sum(q$invalid_sex, na.rm = TRUE),
          sum(q$invalid_weight, na.rm = TRUE),
          sum(q$invalid_height, na.rm = TRUE),
          sum(q$invalid_blood_type, na.rm = TRUE),
          sum(q$missing_diagnosis, na.rm = TRUE),
          sum(q$invalid_dosage, na.rm = TRUE),
          sum(q$age_dob_mismatch, na.rm = TRUE)
        )
      )
      
      ggplot(
        rule_summary,
        aes(
          x = reorder(rule, count),
          y = count
        )
      ) +
        geom_col(fill = "#2C7FB8") +
        coord_flip() +
        labs(
          x = "Quality rule",
          y = "Number of violations",
          title = "Detected quality rule violations"
        ) +
        theme_minimal(base_size = 13)
    })
    
    output$completeness_table <- renderDT({
      metrics <- completeness_data()
      
      datatable(
        metrics,
        rownames = FALSE,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          ordering = TRUE
        )
      )
    })
    
    output$problems_table <- renderDT({
      q <- quality_data()
      
      problematic <- q %>%
        filter(has_problem == TRUE) %>%
        select(
          patient_id,
          age,
          date_of_birth,
          sex,
          weight,
          height,
          blood_type,
          diagnosis_code,
          dosage_mg,
          invalid_patient_id,
          invalid_age,
          invalid_dob,
          invalid_sex,
          invalid_weight,
          invalid_height,
          invalid_blood_type,
          missing_diagnosis,
          invalid_dosage,
          age_dob_mismatch
        )
      
      datatable(
        problematic,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 25, 50, 100),
          scrollX = TRUE,
          ordering = TRUE
        )
      )
    })
  })
}
