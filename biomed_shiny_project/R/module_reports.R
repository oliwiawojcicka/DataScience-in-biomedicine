reports_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      box(
        width = 12,
        title = "Generate quality report",
        status = "primary",
        solidHeader = TRUE,
        
        p("Click the button below to generate an HTML report describing the current state of the biomedical database."),
        p("The report includes completeness metrics, missing value analysis, problematic records, and data quality checks."),
        
        actionButton(
          ns("generate_report"),
          "Generate HTML report",
          class = "btn-primary"
        ),
        
        br(),
        br(),
        
        uiOutput(ns("report_status"))
      )
    )
  )
}


reports_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    
    report_link <- reactiveVal(NULL)
    
    observeEvent(input$generate_report, {
      
      project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
      
      report_input <- file.path(project_dir, "reports", "quality_report.Rmd")
      functions_path <- file.path(project_dir, "R", "quality_functions.R")
      output_dir <- file.path(project_dir, "www", "generated_reports")
      
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
      
      output_filename <- paste0("quality_report_", Sys.Date(), ".html")
      
      temp_data <- tempfile(fileext = ".rds")
      saveRDS(data_reactive(), temp_data)
      
      tryCatch(
        {
          rmarkdown::render(
            input = report_input,
            output_file = output_filename,
            output_dir = output_dir,
            params = list(
              data_path = temp_data,
              functions_path = functions_path
            ),
            envir = new.env(parent = globalenv()),
            knit_root_dir = project_dir,
            quiet = TRUE
          )
          
          link <- paste0("generated_reports/", output_filename)
          report_link(link)
          
          showModal(
            modalDialog(
              title = "Report generated successfully",
              tags$p("The HTML quality report was generated successfully."),
              tags$p("You can open it using the link below:"),
              tags$a(
                href = link,
                target = "_blank",
                "Open HTML report"
              ),
              easyClose = TRUE,
              footer = modalButton("Close")
            )
          )
        },
        error = function(e) {
          showModal(
            modalDialog(
              title = "Report generation failed",
              tags$p("The report could not be generated."),
              tags$p("Please check that the following files exist:"),
              tags$ul(
                tags$li("reports/quality_report.Rmd"),
                tags$li("R/quality_functions.R"),
                tags$li("www/generated_reports")
              ),
              tags$p("Technical details are available in the R console."),
              easyClose = TRUE,
              footer = modalButton("Close")
            )
          )
          
          message("Report generation failed:")
          message(conditionMessage(e))
        }
      )
    })
    
    output$report_status <- renderUI({
      link <- report_link()
      
      if (is.null(link)) {
        tags$div(
          style = "color: #555;",
          "No report has been generated yet."
        )
      } else {
        tags$div(
          style = "background-color: #eef5fb; border-left: 4px solid #2C7FB8; padding: 12px;",
          tags$strong("Latest generated report: "),
          tags$a(
            href = link,
            target = "_blank",
            "Open HTML report"
          )
        )
      }
    })
  })
}