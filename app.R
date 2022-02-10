library(shiny)
library(shinyWidgets)
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "litera"),
  h2("Resolution Calculator"),
  numericInput(
    "orig_size",
    label = "Length of longest size of original",
    value = NA,
    min = 0
  ),
  prettyRadioButtons(
    "unit",
    label = 'Unit',
    choices = c("mm", "cm", "inches", "feet"),
    inline = T
  ),
  numericInput(
    "pixels",
    label = "Target pixels",
    value = NA,
    min = 0
  ),
  actionButton("calc", label = "Calculate resolution", class = "btn-primary"),
  actionButton("reset", label = "Reset"),
  h3(textOutput("Resolution"))
)

server <- function(input, output, session) {
  scan_res <- function() {
    if (!is.na(input$orig_size) & !is.na(input$pixels)) {
      size_inches <- switch(
        input$unit,
        mm = 0.0394 * input$orig_size,
        cm = 0.394 * input$orig_size,
        inches = input$orig_size,
        feet = 12 * input$orig_size
      )
      resolution <- input$pixels / size_inches
      return(paste0(round(resolution), " dpi"))
    } else
      return(" ")
  }
  observeEvent(input$calc, {
    output$Resolution <- renderText({
      scan_res()
    })
  })
  observeEvent(input$reset, {
    updateNumericInput(session, "orig_size", value = NA)
    updateNumericInput(session, "pixels", value = NA)
  })
  
  
}

shinyApp(ui, server)