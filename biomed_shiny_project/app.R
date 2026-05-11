source("global.R")

users <- data.frame(
  username = c("admin", "researcher", "viewer"),
  password = c("admin123", "research123", "viewer123"),
  role = c("admin", "researcher", "viewer"),
  stringsAsFactors = FALSE
)

ui <- dashboardPage(
  dashboardHeader(title = "Biomedical Admissions"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Login", tabName = "login", icon = icon("user")),
      menuItem("Visualization", tabName = "visualization", icon = icon("chart-column")),
      menuItem("Records", tabName = "records", icon = icon("table")),
      menuItem("Quality", tabName = "quality", icon = icon("check-circle")),
      menuItem("New patient", tabName = "form", icon = icon("plus")),
      menuItem("Reports", tabName = "reports", icon = icon("file"))
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    tabItems(
      tabItem("login", login_ui("login")),
      tabItem("visualization", visualization_ui("visualization")),
      tabItem("records", records_ui("records")),
      tabItem("quality", quality_ui("quality")),
      tabItem("form", form_ui("form")),
      tabItem("reports", reports_ui("reports"))
    )
  )
)

server <- function(input, output, session) {
  pool <- create_db_pool()
  onStop(function() poolClose(pool))

  logged_user <- reactiveVal(NULL)
  user_role <- reactiveVal(NULL)
  refresh_trigger <- reactiveVal(0)

  data_reactive <- reactive({
    refresh_trigger()
    get_patients(pool)
  })

  login_server("login", users, logged_user, user_role)
  visualization_server("visualization", data_reactive)
  records_server("records", data_reactive, pool, logged_user, user_role, refresh_trigger)
  quality_server("quality", data_reactive)
  form_server("form", data_reactive, pool, logged_user, user_role, refresh_trigger)
  reports_server("reports", data_reactive)
}

shinyApp(ui, server)
