library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(jsonlite)
library(mongolite)

ui <- dashboardPage(
  dashboardHeader(title = "Genomic Provenance Monitor"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("System Health", tabName = "health", icon = icon("heartbeat")),
      menuItem("Efficiency & Nodes", tabName = "efficiency", icon = icon("server")),
      menuItem("Throughput", tabName = "throughput", icon = icon("database")),
      menuItem("Provenance Records", tabName = "records", icon = icon("table")),
      menuItem("Full JSON-LD Schema", tabName = "schema", icon = icon("file-code"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ---------------- SYSTEM HEALTH ----------------
      tabItem(
        tabName = "health",
        h2("System Health - Integrity Checks"),
        
        fluidRow(
          valueBoxOutput("total_records"),
          valueBoxOutput("failed_checks"),
          valueBoxOutput("successful_checks")
        ),
        
        fluidRow(
          box(
            title = "Failed SHA256 or Seqfu checks by execution node",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("failed_by_node_plot")
          )
        )
      ),
      
      # ---------------- EFFICIENCY ----------------
      tabItem(
        tabName = "efficiency",
        h2("Efficiency Analysis - Execution Nodes"),
        
        fluidRow(
          box(
            title = "Average processing time by execution node",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("duration_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Top 10 slowest processing records",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            DTOutput("slowest_table")
          )
        )
      ),
      
      # ---------------- THROUGHPUT ----------------
      tabItem(
        tabName = "throughput",
        h2("Throughput - Data Volume Moved to Long-Term Storage"),
        
        fluidRow(
          valueBoxOutput("total_gb"),
          valueBoxOutput("average_gb"),
          valueBoxOutput("total_files")
        ),
        
        fluidRow(
          box(
            title = "Total data volume processed by execution node",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("throughput_plot")
          )
        )
      ),
      
      # ---------------- RECORDS ----------------
      tabItem(
        tabName = "records",
        h2("Provenance Records - Search, Filter and Select"),
        
        fluidRow(
          box(
            title = "Reactive Filters",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            uiOutput("node_filter"),
            selectInput(
              inputId = "status_filter",
              label = "Integrity check status:",
              choices = c("All", "Only failed", "Only OK"),
              selected = "All"
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Processed genomic provenance records imported from MongoDB",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            DTOutput("records_table")
          )
        )
      ),
      
      # ---------------- FULL SCHEMA ----------------
      tabItem(
        tabName = "schema",
        h2("Full JSON-LD Provenance Schema"),
        
        box(
          title = "Selected record full Provenance schema from MongoDB",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          verbatimTextOutput("record_details")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # MongoDB connection
  mongo_conn <- mongo(
    collection = "provenance_logs",
    db = "genomics",
    url = "mongodb://localhost:27017"
  )
  
  # Read summary fields from MongoDB and convert list columns into normal R values
  read_provenance_from_mongodb <- function() {
    
    data <- mongo_conn$find(
      '{}',
      fields = '{
        "_id": 0,
        "record_id": 1,
        "label": 1,
        "executionNode": 1,
        "startTime": 1,
        "endTime": 1,
        "durationMinutes": 1,
        "sha256_status": 1,
        "seqfu_status": 1,
        "totalSizeGB": 1,
        "fileCount": 1,
        "category": 1
      }'
    )
    
    if (nrow(data) == 0) {
      return(data.frame(
        record_id = character(),
        label = character(),
        executionNode = character(),
        startTime = character(),
        endTime = character(),
        durationMinutes = numeric(),
        sha256_status = character(),
        seqfu_status = character(),
        totalSizeGB = numeric(),
        fileCount = numeric(),
        category = character(),
        stringsAsFactors = FALSE
      ))
    }
    
    get_value <- function(x) {
      if (is.null(x)) {
        return(NA)
      }
      
      if (is.list(x)) {
        return(as.character(unlist(x))[1])
      }
      
      return(as.character(x)[1])
    }
    
    clean_data <- data.frame(
      record_id = sapply(data$record_id, get_value),
      label = sapply(data$label, get_value),
      executionNode = sapply(data$executionNode, get_value),
      startTime = sapply(data$startTime, get_value),
      endTime = sapply(data$endTime, get_value),
      durationMinutes = as.numeric(sapply(data$durationMinutes, get_value)),
      sha256_status = sapply(data$sha256_status, get_value),
      seqfu_status = sapply(data$seqfu_status, get_value),
      totalSizeGB = as.numeric(sapply(data$totalSizeGB, get_value)),
      fileCount = as.numeric(sapply(data$fileCount, get_value)),
      category = sapply(data$category, get_value),
      stringsAsFactors = FALSE
    )
    
    clean_data
  }
  
  # Main reactive dataset
  provenance_data <- reactive({
    read_provenance_from_mongodb()
  })
  
  # Node filter
  output$node_filter <- renderUI({
    data <- provenance_data()
    
    selectInput(
      inputId = "node_selected",
      label = "Execution node:",
      choices = c("All", sort(unique(data$executionNode))),
      selected = "All"
    )
  })
  
  # Filtered data
  filtered_data <- reactive({
    data <- provenance_data()
    
    if (!is.null(input$node_selected) && input$node_selected != "All") {
      data <- data %>%
        filter(executionNode == input$node_selected)
    }
    
    if (!is.null(input$status_filter) && input$status_filter == "Only failed") {
      data <- data %>%
        filter(sha256_status == "FAILED" | seqfu_status == "FAILED")
    }
    
    if (!is.null(input$status_filter) && input$status_filter == "Only OK") {
      data <- data %>%
        filter(sha256_status == "OK" & seqfu_status == "OK")
    }
    
    data
  })
  
  # ---------------- VALUE BOXES ----------------
  
  output$total_records <- renderValueBox({
    valueBox(
      value = nrow(provenance_data()),
      subtitle = "Total provenance records in MongoDB",
      icon = icon("database"),
      color = "blue"
    )
  })
  
  output$failed_checks <- renderValueBox({
    data <- provenance_data()
    
    failed <- sum(data$sha256_status == "FAILED" | data$seqfu_status == "FAILED", na.rm = TRUE)
    
    valueBox(
      value = failed,
      subtitle = "Records with failed SHA256 or Seqfu checks",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$successful_checks <- renderValueBox({
    data <- provenance_data()
    
    successful <- sum(data$sha256_status == "OK" & data$seqfu_status == "OK", na.rm = TRUE)
    
    valueBox(
      value = successful,
      subtitle = "Records with successful integrity checks",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$total_gb <- renderValueBox({
    data <- provenance_data()
    total <- sum(data$totalSizeGB, na.rm = TRUE)
    
    valueBox(
      value = round(total, 2),
      subtitle = "Total GB processed",
      icon = icon("hdd"),
      color = "green"
    )
  })
  
  output$average_gb <- renderValueBox({
    data <- provenance_data()
    avg <- mean(data$totalSizeGB, na.rm = TRUE)
    
    valueBox(
      value = round(avg, 2),
      subtitle = "Average GB per provenance record",
      icon = icon("chart-bar"),
      color = "blue"
    )
  })
  
  output$total_files <- renderValueBox({
    data <- provenance_data()
    total <- sum(data$fileCount, na.rm = TRUE)
    
    valueBox(
      value = total,
      subtitle = "Total FASTQ files",
      icon = icon("file"),
      color = "purple"
    )
  })
  
  # ---------------- TABLES ----------------
  
  output$records_table <- renderDT({
    datatable(
      filtered_data() %>%
        select(
          record_id,
          label,
          executionNode,
          startTime,
          endTime,
          durationMinutes,
          sha256_status,
          seqfu_status,
          totalSizeGB,
          fileCount,
          category
        ),
      selection = "single",
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  output$slowest_table <- renderDT({
    datatable(
      provenance_data() %>%
        arrange(desc(durationMinutes)) %>%
        select(
          record_id,
          label,
          executionNode,
          durationMinutes,
          sha256_status,
          seqfu_status,
          totalSizeGB
        ) %>%
        head(10),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  # ---------------- FULL JSON-LD SCHEMA ----------------
  
  output$record_details <- renderPrint({
    selected <- input$records_table_rows_selected
    
    if (length(selected) == 0) {
      cat("Go to 'Provenance Records', click one row in the table, then return here to see the full JSON-LD Provenance schema.")
    } else {
      data <- filtered_data()
      selected_id <- data$record_id[selected]
      
      query <- sprintf('{"record_id": "%s"}', selected_id)
      
      full_record <- mongo_conn$find(
        query,
        fields = '{"_id": 0}'
      )
      
      cat(toJSON(full_record, pretty = TRUE, auto_unbox = TRUE))
    }
  })
  
  # ---------------- PLOTS ----------------
  
  output$failed_by_node_plot <- renderPlot({
    data <- provenance_data() %>%
      mutate(failed = sha256_status == "FAILED" | seqfu_status == "FAILED") %>%
      group_by(executionNode) %>%
      summarise(failedChecks = sum(failed, na.rm = TRUE), .groups = "drop")
    
    barplot(
      height = data$failedChecks,
      names.arg = data$executionNode,
      main = "Failed integrity checks by node",
      xlab = "Execution node",
      ylab = "Number of failed records"
    )
  })
  
  output$duration_plot <- renderPlot({
    data <- provenance_data() %>%
      group_by(executionNode) %>%
      summarise(avgDuration = mean(durationMinutes, na.rm = TRUE), .groups = "drop")
    
    barplot(
      height = data$avgDuration,
      names.arg = data$executionNode,
      main = "Average processing duration by node",
      xlab = "Execution node",
      ylab = "Average duration in minutes"
    )
  })
  
  output$throughput_plot <- renderPlot({
    data <- provenance_data() %>%
      group_by(executionNode) %>%
      summarise(totalGB = sum(totalSizeGB, na.rm = TRUE), .groups = "drop")
    
    barplot(
      height = data$totalGB,
      names.arg = data$executionNode,
      main = "Total data volume processed by node",
      xlab = "Execution node",
      ylab = "Total GB"
    )
  })
}

shinyApp(ui, server)