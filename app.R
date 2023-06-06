# Design CT Application

library(shiny)
library(glue)
library(rclipboard)
library(bslib)


source("R/fct_main.R")
source("R/fct_child.R")

study_type_choices <- list(
  "CT Chest" = "chest" , 
  "CTWA" = "whole_abd",
  "CT Chest + WA" = "chest_whole_abd", 
  "CTA liver" = "cta_liver"
)

age_group_choices <- list(
  "Younger Child" = "younger_child", 
  "Older Child" = "older_child"
)

rate_formula_choices <- list(
 "(Contrast [ml] + 15) / 45"  = "no_delay",
 "(Contrast [ml] + 15) / (delay [s] - 15)" = "delay"
)

# UI
ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "minty"),

    # Application title
    titlePanel("Design Pediatric CT"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            selectInput("study_type", "Study:", choices = study_type_choices, selected = NULL),
            selectInput("age_group", "Age group:", choices = age_group_choices, selected = NULL),
            numericInput("weight_kg", "Weight (kg):", value = NULL, min = 0),
            selectInput("rate_formula", "Rate formular:", choices = rate_formula_choices, selected = NULL),
            selectInput("iv_location", "IV line location:", choices = c("arm", "leg"), selected = NULL),
            uiOutput("UI_delay_sec")
        ),

        # Main
        mainPanel(
          verbatimTextOutput("design_text"),
          # UI ouputs for the copy-to-clipboard buttons
          uiOutput("clip")
        )
    )
)

# Server
server <- function(input, output) {
  # Design Text
  design_text <- reactive({
    req(input$study_type, input$age_group, input$weight_kg)
    if(input$rate_formula == "delay") { req(input$delay_sec) }
    
    capture.output(
      print_design_ct(
        study_type = input$study_type,
        age_group = input$age_group,
        weight_kg = input$weight_kg,
        iv_location = input$iv_location,
        rate_formula = input$rate_formula,
        delay_sec = input$delay_sec
      )
    )
  })
  
  output$UI_delay_sec <- renderUI({
    if(input$rate_formula == "delay") {
      numericInput("delay_sec", "Choose Delay (sec):", value = NULL, min = 0)
    }
  })
  
  # Design Text Output
  output$design_text <- renderPrint({
    
    cat(design_text(), sep = "\n")
    
  })
  
  # Add clipboard buttons
  output$clip <- renderUI({
    output$clip <- renderUI({
      rclipButton(
        inputId = "clipbtn",
        label = "Copy to clipboard",
        clipText = design_text(), 
        icon = icon("clipboard")
      )
    })
  })
  
  # Workaround for execution within RStudio version < 1.2
  if (interactive()){
    observeEvent(input$clipbtn, clipr::write_clip(design_text()))
  }

  
}

# Run the application 
shinyApp(ui = ui, server = server)
