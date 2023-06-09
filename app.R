# Design CT Application

library(shiny)
library(glue)
library(rclipboard)
library(bslib)


# Choices -----------------------------------------------------------------


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


# UI ----------------------------------------------------------------------


ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "minty"),
    # Copy botton
    rclipboardSetup(),

    # Application title
    titlePanel("Design Pediatric CT"),
    tags$hr(),
    

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            h5(helpText("Select Parameters")),
            selectInput("study_type", "Study:", choices = study_type_choices, selected = "CT Chest"),
            selectInput("age_group", "Age group:", choices = age_group_choices, selected = NULL),
            numericInput("weight_kg", "Weight (kg):", value = NULL, min = 0),
            uiOutput("UI_rate_formula"),
            uiOutput("UI_delay_sec"),
            #selectInput("iv_location", "IV line location:", choices = c("arm", "leg"), selected = NULL),
        ),

        # Main
        mainPanel(
          verbatimTextOutput("design_text"),
          # UI ouputs for the copy-to-clipboard buttons
          uiOutput("clip")
        )
    )
)


# Server ------------------------------------------------------------------


server <- function(input, output) {
  
  # Design Text
  design_text_str <- reactive({
    req(input$study_type, input$age_group, input$weight_kg)
    if((input$study_type %in% c("whole_abd", "chest_whole_abd"))) {
      if(input$rate_formula == "delay") { req(input$delay_sec) }
    }
    
    capture.output(
      design_ct(
        study_type = input$study_type,
        age_group = input$age_group,
        weight_kg = input$weight_kg,
        rate_formula = input$rate_formula,
        delay_sec = input$delay_sec
      )
    ) |> 
      paste(collapse = "\n")
    
  })
  
  output$UI_rate_formula <- renderUI({
    req(input$study_type)
    if(input$study_type %in% c("whole_abd", "chest_whole_abd") ) {
      selectInput("rate_formula", "Rate formular:", choices = rate_formula_choices, selected = NULL)
      }
  })
  
  output$UI_delay_sec <- renderUI({
    req(input$rate_formula)
    if((input$study_type %in% c("whole_abd", "chest_whole_abd")) & input$rate_formula == "delay") {
      numericInput("delay_sec", "Choose Delay (sec):", value = NULL, min = 0)
    }
  })
  
  # Design Text Output
  output$design_text <- renderPrint({
    
    cat(design_text_str())
    
  })
  
  # Add clipboard buttons
  output$clip <- renderUI({
    output$clip <- renderUI({
      rclipButton(
        inputId = "clipbtn",
        label = "Copy",
        clipText = design_text_str(), 
        icon = icon("clipboard")
      )
    })
  })
  
  # Workaround for execution within RStudio version < 1.2
  if (interactive()){
    observeEvent(input$clipbtn, clipr::write_clip(design_text_str()))
  }

  
}

# Run the application 
shinyApp(ui = ui, server = server)
