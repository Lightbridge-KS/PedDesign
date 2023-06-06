library(rclipboard)
library(shiny)

# The UI
ui <- bootstrapPage(
  
  rclipboardSetup(),
  
  # Add a text input
  textInput("copytext", "Copy this:", "Zlika!"),
  
  # UI ouputs for the copy-to-clipboard buttons
  uiOutput("clip"),
  
  # A text input for testing the clipboard content.
  textInput("paste", "Paste here:")
  
)

# The server
server <- function(input, output) {
  
  more_text <- reactive({ paste("Hi ", input$copytext)})
  
  # Add clipboard buttons
  output$clip <- renderUI({
    output$clip <- renderUI({
      rclipButton(
        inputId = "clipbtn",
        label = "rclipButton Copy",
        clipText = more_text(), 
        icon = icon("clipboard")
      )
    })
  })
  
  # Workaround for execution within RStudio version < 1.2
  if (interactive()){
    observeEvent(input$clipbtn, clipr::write_clip(more_text()))
  }
  
}

shinyApp(ui = ui, server = server)
