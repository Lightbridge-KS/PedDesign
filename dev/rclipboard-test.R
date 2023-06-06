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
  
  # Add clipboard buttons
  output$clip <- renderUI({
    output$clip <- renderUI({
      rclipButton(
        inputId = "clipbtn",
        label = "rclipButton Copy",
        clipText = input$copytext, 
        icon = icon("clipboard")
      )
    })
  })
  
  # Workaround for execution within RStudio version < 1.2
  if (interactive()){
    observeEvent(input$clipbtn, clipr::write_clip(input$copytext))
  }
  
}

shinyApp(ui = ui, server = server)
