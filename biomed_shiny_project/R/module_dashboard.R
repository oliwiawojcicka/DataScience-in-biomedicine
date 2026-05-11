login_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(
        width = 6,
        offset = 3,
        
        box(
          width = 12,
          title = "User login",
          status = "primary",
          solidHeader = TRUE,
          
          div(
            class = "login-box-custom",
            
            textInput(
              ns("username"),
              "Username",
              placeholder = "Enter your username"
            ),
            
            passwordInput(
              ns("password"),
              "Password",
              placeholder = "Enter your password"
            ),
            
            actionButton(
              ns("login_btn"),
              "Log in",
              class = "btn-primary login-button"
            ),
            
            br(),
            br(),
            
            div(
              class = "login-help",
              tags$p("Demo accounts:"),
              tags$ul(
                tags$li("admin / admin123"),
                tags$li("researcher / research123"),
                tags$li("viewer / viewer123")
              )
            ),
            
            verbatimTextOutput(ns("login_status"))
          )
        )
      )
    )
  )
}


login_server <- function(id, users, logged_user, user_role) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$login_btn, {
      row <- users[
        users$username == input$username &
          users$password == input$password,
      ]
      
      if (nrow(row) == 1) {
        logged_user(row$username)
        user_role(row$role)
        
        showModal(
          modalDialog(
            title = "Login successful",
            paste("You are logged in as", row$username),
            easyClose = TRUE,
            footer = modalButton("Close")
          )
        )
      } else {
        showModal(
          modalDialog(
            title = "Login failed",
            "Invalid username or password. Please try again.",
            easyClose = TRUE,
            footer = modalButton("Close")
          )
        )
      }
    })
    
    output$login_status <- renderText({
      if (is.null(logged_user())) {
        "You are not logged in."
      } else {
        paste0(
          "Logged in user: ", logged_user(),
          "\nRole: ", user_role()
        )
      }
    })
  })
}
