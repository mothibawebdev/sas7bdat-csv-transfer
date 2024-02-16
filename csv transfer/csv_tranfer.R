library(shiny)
library(haven)
library(DT)

#incrse file upload size to 2GB
options(shiny.maxRequestSize = 2147483648)
# Define UI ----
ui <- fluidPage(
  titlePanel(span(" IeDEA", style= "color: darkred", span(" .sasdbat transfer to CSV", ))),
  sidebarLayout(
    sidebarPanel(
      img(src = "logo2.png", height = 100, width = 500),
      h3("CSV Transfer is used as an alternative tool to transfer .sas7bdat datasets to a .csv file."),
      h4("IeDEA Tables"),
      h5("tblART"),
      h5("tblBAS"),
      h5("And more...")
    ),
    mainPanel(
      fluidRow(
        column(4,
               fileInput("file", h3("Select file to transfer")),
               textInput("text", h3("Provide file name"), placeholder = "Enter file name..."),
               actionButton("export_csv", "Export to CSV", style = "color: white; background-color: blue;"),
               br()
        )
      ),
      
      fluidRow(
        br(),
        column(12,
               DTOutput("table")  # Moved DTOutput inside the mainPanel
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  data <- reactive({
    if (!is.null(input$file)) {
      haven::read_sas(input$file$datapath)
    }
  })
  
  output$table <- renderDT({
    datatable(
      data(),
      options = list(pageLength = input$rows_per_page)
    )
  })
  
  observeEvent(input$export_csv, {
    if (!is.null(input$file)) {
      withProgress(message = 'Exporting CSV...', value = 0, {
        # Simulating a long process
        for (i in 1:15) {
          incProgress(1/15)
          Sys.sleep(0.2)
        }
        write.csv(data(), file = paste0(input$text, ".csv"), row.names = FALSE)
        # Clear the text input field after export
        updateTextInput(session, "text", value = "")
      })
    }
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
