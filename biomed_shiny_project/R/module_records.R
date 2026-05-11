records_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      box(
        width = 12,
        title = "Record filters",
        status = "primary",
        solidHeader = TRUE,
        
        fluidRow(
          column(
            width = 3,
            textInput(ns("filter_patient_id"), "Patient ID", placeholder = "e.g. P-1001")
          ),
          column(
            width = 2,
            selectInput(
              ns("filter_sex"),
              "Sex",
              choices = c("All", "Male", "Female")
            )
          ),
          column(
            width = 2,
            selectInput(
              ns("filter_blood_type"),
              "Blood type",
              choices = c("All", "A+", "A-", "B+", "B-", "AB+", "AB-", "O+", "O-")
            )
          ),
          column(
            width = 3,
            textInput(ns("filter_diagnosis"), "Diagnosis code", placeholder = "e.g. D123")
          ),
          column(
            width = 2,
            actionButton(ns("clear_filters"), "Clear filters", class = "btn-primary")
          )
        ),
        
        fluidRow(
          column(
            width = 4,
            textInput(ns("filter_doctor"), "Doctor name", placeholder = "e.g. Dr. Smith")
          )
        )
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Patient records",
        status = "primary",
        solidHeader = TRUE,
        DTOutput(ns("patients_table"))
      )
    ),
    
    fluidRow(
      box(
        width = 6,
        title = "Edit selected record",
        status = "primary",
        solidHeader = TRUE,
        helpText("Editing is available only for admin and researcher users."),
        numericInput(ns("edit_dosage"), "New dosage [mg]", value = NA),
        textInput(ns("edit_doctor"), "New doctor name"),
        actionButton(ns("update_btn"), "Update selected row", class = "btn-primary")
      ),
      
      box(
        width = 6,
        title = "Delete selected record",
        status = "danger",
        solidHeader = TRUE,
        helpText("Deleting is available only for admin and researcher users."),
        actionButton(ns("delete_btn"), "Delete selected row", class = "btn-danger")
      )
    )
  )
}


records_server <- function(id, data_reactive, pool, logged_user, user_role, refresh_trigger) {
  moduleServer(id, function(input, output, session) {
    
    filtered_data <- reactive({
      df <- data_reactive()
      
      if (!is.null(input$filter_patient_id) && input$filter_patient_id != "") {
        df <- df %>%
          filter(str_detect(
            str_to_lower(patient_id),
            fixed(str_to_lower(input$filter_patient_id))
          ))
      }
      
      if (!is.null(input$filter_sex) && input$filter_sex != "All") {
        df <- df %>%
          filter(sex == input$filter_sex)
      }
      
      if (!is.null(input$filter_blood_type) && input$filter_blood_type != "All") {
        df <- df %>%
          filter(blood_type == input$filter_blood_type)
      }
      
      if (!is.null(input$filter_diagnosis) && input$filter_diagnosis != "") {
        df <- df %>%
          filter(str_detect(
            str_to_lower(diagnosis_code),
            fixed(str_to_lower(input$filter_diagnosis))
          ))
      }
      
      if (!is.null(input$filter_doctor) && input$filter_doctor != "") {
        df <- df %>%
          filter(str_detect(
            str_to_lower(doctor_name),
            fixed(str_to_lower(input$filter_doctor))
          ))
      }
      
      df
    })
    
    observeEvent(input$clear_filters, {
      updateTextInput(session, "filter_patient_id", value = "")
      updateSelectInput(session, "filter_sex", selected = "All")
      updateSelectInput(session, "filter_blood_type", selected = "All")
      updateTextInput(session, "filter_diagnosis", value = "")
      updateTextInput(session, "filter_doctor", value = "")
    })
    
    output$patients_table <- renderDT({
      df <- filtered_data()
      
      datatable(
        df,
        selection = list(mode = "single", target = "row"),
        rownames = FALSE,
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 25, 50, 100),
          scrollX = TRUE,
          ordering = TRUE,
          order = list(list(0, "asc")),
          autoWidth = TRUE,
          dom = "lfrtip",
          columnDefs = list(
            list(width = "130px", targets = "_all")
          )
        )
      )
    }, server = FALSE)
    
    observeEvent(input$patients_table_rows_selected, {
      row <- input$patients_table_rows_selected
      
      if (length(row) == 0) {
        return()
      }
      
      df <- filtered_data()
      
      updateNumericInput(
        session,
        "edit_dosage",
        value = df$dosage_mg[row]
      )
      
      updateTextInput(
        session,
        "edit_doctor",
        value = df$doctor_name[row]
      )
    })
    
    observeEvent(input$update_btn, {
      req(logged_user())
      
      if (!(user_role() %in% c("admin", "researcher"))) {
        showModal(
          modalDialog(
            title = "Permission denied",
            "You do not have permission to edit patient records.",
            easyClose = TRUE,
            footer = modalButton("Close")
          )
        )
        return()
      }
      
      row <- input$patients_table_rows_selected
      
      if (length(row) == 0) {
        showModal(
          modalDialog(
            title = "No record selected",
            "Please select one patient record before updating.",
            easyClose = TRUE,
            footer = modalButton("Close")
          )
        )
        return()
      }
      
      if (is.na(input$edit_dosage) || input$edit_dosage <= 0 || input$edit_dosage > 5000) {
        showModal(
          modalDialog(
            title = "Invalid dosage",
            "Dosage must be greater than 0 and not greater than 5000 mg.",
            easyClose = TRUE,
            footer = modalButton("Close")
          )
        )
        return()
      }
      
      df <- filtered_data()
      patient_id <- df$patient_id[row]
      
      old_value <- paste(
        capture.output(print(df[row, ])),
        collapse = "\n"
      )
      
      DBI::dbExecute(
        pool,
        "UPDATE patients
         SET dosage_mg = $1,
             doctor_name = $2,
             modified_by = $3,
             modified_at = CURRENT_TIMESTAMP
         WHERE patient_id = $4",
        params = list(
          as.integer(input$edit_dosage),
          input$edit_doctor,
          logged_user(),
          patient_id
        )
      )
      
      new_value <- paste0(
        "dosage_mg=", input$edit_dosage,
        "; doctor_name=", input$edit_doctor
      )
      
      write_audit_log(
        pool,
        patient_id,
        "UPDATE",
        logged_user(),
        old_value = old_value,
        new_value = new_value
      )
      
      refresh_trigger(refresh_trigger() + 1)
      
      showModal(
        modalDialog(
          title = "Record updated",
          paste("Patient", patient_id, "was successfully updated."),
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    })
    
    observeEvent(input$delete_btn, {
      req(logged_user())
      
      if (!(user_role() %in% c("admin", "researcher"))) {
        showModal(
          modalDialog(
            title = "Permission denied",
            "You do not have permission to delete patient records.",
            easyClose = TRUE,
            footer = modalButton("Close")
          )
        )
        return()
      }
      
      row <- input$patients_table_rows_selected
      
      if (length(row) == 0) {
        showModal(
          modalDialog(
            title = "No record selected",
            "Please select one patient record before deleting.",
            easyClose = TRUE,
            footer = modalButton("Close")
          )
        )
        return()
      }
      
      df <- filtered_data()
      patient_id <- df$patient_id[row]
      
      old_value <- paste(
        capture.output(print(df[row, ])),
        collapse = "\n"
      )
      
      DBI::dbExecute(
        pool,
        "DELETE FROM patients WHERE patient_id = $1",
        params = list(patient_id)
      )
      
      write_audit_log(
        pool,
        patient_id,
        "DELETE",
        logged_user(),
        old_value = old_value,
        new_value = NA
      )
      
      refresh_trigger(refresh_trigger() + 1)
      
      showModal(
        modalDialog(
          title = "Record deleted",
          paste("Patient", patient_id, "was successfully deleted."),
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    })
  })
}
