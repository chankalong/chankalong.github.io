library(shiny)
library(shinythemes)
library(highcharter)
library(forcats)
library(googlesheets4)

# user database for logins
user_base <- tibble::tibble(
  user = "refresh",
  password = "boc12345",
  permissions = "admin",
  name = "Re:Fresh BOKSS"
)

# login tab ui to be rendered on launch
login_tab <- tabPanel(
  title = icon("lock"), 
  value = "login", 
  shinyauthr::loginUI("login")
)

# additional tabs to be added after login
home_tab <- tabPanel(
  title = icon("user"),
  value = "home",
  column(
    width = 12, 
    highchartOutput("chart1")
    #tags$h2("User Information"),
    #verbatimTextOutput("user_data")
  )
)

# initial app UI with only login tab
ui <- navbarPage(
  title = "Re:Fresh e-platform Website Usage",
  id = "tabs", # must give id here to add/remove tabs in server
  collapsible = TRUE,
  login_tab
)

server <- function(input, output, session) {
  # call the shinyauthr login and logout server modules
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = "user",
    pwd_col = "password",
    reload_on_logout = TRUE,
    log_out = reactive(logout_init())
  )
  
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  observeEvent(credentials()$user_auth, {
    # if user logs in successfully
    if (credentials()$user_auth) { 
      # remove the login tab
      removeTab("tabs", "login")
      # add home tab 
      appendTab("tabs", home_tab, select = TRUE)
      # render user data output
      output$chart1 <- renderHighchart({
        gs4_deauth()
        participant_frequency_long <- read_sheet("1QaGF2rGGusVQj0FkP9Q63HZ35564qDOjnGgVSxYYZ-I")
        
        hchart(participant_frequency_long, "line", hcaes(x = fct_reorder(factor(year.month), rowid), y = eventCount , group = eventName)) %>%
          hc_tooltip(
            crosshairs = TRUE,
            backgroundColor = "#F0F0F0",
            shared = TRUE, 
            borderWidth = 5
          ) %>% hc_xAxis(title = "") %>% hc_yAxis(title = "")
      })
    }
  })
}

shinyApp(ui, server)