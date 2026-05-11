form_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      box(
        width = 6,
        title = "Add new patient",
        
        textInput(ns("patient_id"), "Patient ID", placeholder = "P-2001"),
        dateInput(ns("date_of_birth"), "Date of birth"),
        numericInput(ns("age"), "Age", value = NA, min = 0, max = 120),
        selectInput(ns("sex"), "Sex", choices = c("", "Male", "Female")),
        numericInput(ns("weight"), "Weight [kg]", value = NA),
        numericInput(ns("height"), "Height [m]", value = NA),
        selectInput(
          ns("blood_type"),
          "Blood type",
          choices = c("", "A+", "A-", "B+", "B-", "AB+", "AB-", "O+", "O-")
        ),
        textInput(ns("diagnosis_code"), "Diagnosis code"),
        numericInput(ns("dosage_mg"), "Dosage [mg]", value = NA),
        selectInput(ns("smoker"), "Smoker", choices = c("", "Yes", "No")),
        textInput(ns("doctor_name"), "Doctor name"),
        
        actionButton(ns("save_btn"), "Save patient")
      ),
      
      box(
        width = 6,
        title = "Validation details",
        uiOutput(ns("validation_output"))
      )
    )
  )
}


form_server <- function(id, data_reactive, pool, logged_user, user_role, refresh_trigger) {
  moduleServer(id, function(input, output, session) {
    
    new_patient <- reactive({
      data.frame(
        patient_id = input$patient_id,
        date_of_birth = as.Date(input$date_of_birth),
        age = as.integer(input$age),
        sex = input$sex,
        weight = as.numeric(input$weight),
        height = as.numeric(input$height),
        blood_type = input$blood_type,
        diagnosis_code = input$diagnosis_code,
        dosage_mg = as.integer(input$dosage_mg),
        smoker = ifelse(
          input$smoker == "Yes",
          TRUE,
          ifelse(input$smoker == "No", FALSE, NA)
        ),
        doctor_name = input$doctor_name,
        stringsAsFactors = FALSE
      )
    })
    
    validation_errors <- reactive({
      validate_new_patient(new_patient(), data_reactive()$patient_id)
    })
    
    output$validation_output <- renderUI({
      errors <- validation_errors()
      
      if (length(errors) == 0) {
        tags$div(
          style = "color: green; font-weight: bold;",
          "All validation checks passed. The patient record can be saved."
        )
      } else {
        tags$div(
          tags$p(
            style = "color: red; font-weight: bold;",
            "The following validation errors were detected:"
          ),
          tags$ul(
            lapply(errors, function(error) {
              tags$li(error)
            })
          )
        )
      }
    })
    
    observeEvent(input$save_btn, {
      req(logged_user())
      
      if (!(user_role() %in% c("admin", "researcher"))) {
        showModal(
          modalDialog(
            title = "Permission denied",
            "You do not have permission to add patient records.",
            easyClose = TRUE
          )
        )
        return()
      }
      
      errors <- validation_errors()
      
      if (length(errors) > 0) {
        showModal(
          modalDialog(
            title = "Validation failed",
            tags$p("The patient record was not saved. Please correct the following errors:"),
            tags$ul(
              lapply(errors, function(error) {
                tags$li(error)
              })
            ),
            easyClose = TRUE
          )
        )
        return()
      }
      
      p <- new_patient()
      p$created_by <- logged_user()
      p$created_at <- Sys.time()
      p$modified_by <- NA_character_
      p$modified_at <- as.POSIXct(NA)
      
      DBI::dbWriteTable(pool, "patients", p, append = TRUE, row.names = FALSE)
      
      write_audit_log(
        pool,
        p$patient_id,
        "INSERT",
        logged_user(),
        new_value = paste(capture.output(print(p)), collapse = "\n")
      )
      
      refresh_trigger(refresh_trigger() + 1)
      
      showModal(
        modalDialog(
          title = "Patient saved",
          paste("Patient", p$patient_id, "was successfully saved to the database."),
          easyClose = TRUE
        )
      )
    })
  })
}
