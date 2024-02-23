library(rsconnect)
library(shiny)
library(haven)
library(DT)
library(foreign)
library(RODBC)
library(htmltools)
library(shinyWidgets)

rsconnect::setAccountInfo(name='iedea-dt', token='B64519710E2C403E20FCDB812C58F9EF', secret='i/XRXO6PnJghPX4jicVKjfqtdB3P5kEyAXrlDdsV')

#install.packages(update=TRUE)

#incrse file upload size to 2GB
options(shiny.maxRequestSize = 5 * 1024^3)
# Define UI ----
ui <- fluidPage(
  tags$head(
    tags$link(rel="IeDEA-DT", href="favicon.ico"),
    tags$title("IeDEA Data Transfer Toolkit")
  ),
  #titlePanel("IeDEA -DT"),
  br(),
  sidebarLayout(
    sidebarPanel(
      img(src = "logo2.png", height = 150, width = "100%"),
      h3("IeDEA Dataset Transfer Toolkit is a tool used for transfering datasets to a csv format. It accepts
         files like sas, dta and txt files"),
      br(),
      
    ),
    mainPanel(
      fluidRow(
        column(6,
               wellPanel(
                 #div(class="box-header box box-solid box-success", h3("File Transer")),
                 fileInput("file", HTML("Upload file to transfer: Max 5GB<br>
                    Allowed file formats: <span class='badge badge-fix' style='background-color: blue'>sas</span>
                                        <span class='badge badge-fix' style='background-color: maroon'>dta</span>
                                        <span class='badge badge-fix' style='background-color: white; color: black'>txt</span>"), multiple=TRUE),
                 textInput("text", h4("Provide file name")),
                 actionButton("export_csv", "Export to CSV", class="btn-success"),
                 br()
               )
        ),
        column(6,
               wellPanel(
                 h4("Summary of Uploaded IeDEA Table"),
                 tableOutput("summary_table")
               )
        )
      )
    )
  ), #end of sidelayout
  fluidRow(
    column(12,
           h3("Table output summary"),
           DTOutput("table")
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    if (endsWith(input$file$name, ".dta")) {
      haven::read_dta(input$file$datapath)
    } else if (endsWith(input$file$name, ".sas7bdat")) {
      haven::read_sas(input$file$datapath)
    } else if(endsWith(input$file$datapath, ".txt")) {
      read.table(input$file$datapath, header=TRUE, sep=",")
    }
      else{NULL}
  })
  
  output$summary_table <- renderTable({
    
    if (!is.null(data())) {
      filename <- input$file$name
      tablename <- sub("\\.\\w+$", "", filename) # Extracting table name from filename
      total_records <- nrow(data()) # Total number of records
      column_names <- colnames(data()) # Column names
      
      #table_html <- paste(HTML("<span class='badge badge-fix'>", tablename, "</span>", sep = ""))
      summary_data <- data.frame("Table" = tablename,
                                 "Records" = total_records,
                                 "Columns" = paste(column_names, collapse = ", "))
      summary_data
    }
  })
  
  output$table <- renderDT({
    datatable(
      data(),
      options = list(
        pageLength = input$rows_per_page,
        dom = 't<"dt-custom-search"i>p'
      )
    )
  })
  
  observeEvent(input$export_csv, {
    req(input$file)
    withProgress(message = 'Exporting Data...', value = 0, {
      # Simulating a long process
      for (i in 1:15) {
        incProgress(1/15)
        Sys.sleep(0.2)
      }
      
      write.csv(data(), file = paste0(input$text, ".csv"), row.names = FALSE)
      showModal(
        modalDialog(
          title="Success!",
          "File exported Successfully",
          easyClose = TRUE
        )
      )
      # Clear the text input field after export
      updateTextInput(session, "text", value = "")
    })
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
