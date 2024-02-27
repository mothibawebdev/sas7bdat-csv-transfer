library(shiny)
library(haven)
library(DT)
library(foreign)
#library(RODBC)
library(htmltools)
library(shinyWidgets)
library(shinythemes)

#install.packages(update=TRUE)

#increase file upload size to 5GB
options(shiny.maxRequestSize = 5 * 1024^3)
# Define UI ----
ui <- fluidPage(theme = shinytheme("readable"),
  tags$head(
    tags$link(rel="IeDEA-DT", href="favicon.ico"),
    tags$title("IeDEA CSV Converter Toolkit"),
    tags$style(
      HTML(
        "
        .footer {
          background-color: #f5f5f5;
          color: black;
          text-align: center;
          padding: 10px;
          position: fixed;
          left: 0;
          bottom: 0;
          width: 100%;
        } "
      )
    )
  ),
  #titlePanel("IeDEA -DT"),
  br(),
  sidebarLayout(
    sidebarPanel(
      img(src = "logo2.png", height = 150, width = "100%"),
      h4("CSV Converter Toolkit is a tool used for transfering datasets to a csv format. It accepts
         files like sas, dta and txt files."),
      p(HTML("Step 1: Select data files to upload<br>
        Step 2: Provide table name<br>
        Step 3; click Export button<br>
        <strong>When uploading multiple files, provide a generic naming like tbl. It will then name your files tbl_1, tbl_2, etc. 
             Do not put them in a zip folder</strong>")),
      br(),
      
    ),
    mainPanel(
      fluidRow(
        column(6,
               wellPanel(
                 tags$fieldset(
                   tags$legend("Select Data Files"),
                   fileInput("file", HTML("Allowed file formats include: <span class='badge badge-fix' style='background-color: blue'>sas</span>
                                  <span class='badge badge-fix' style='background-color: maroon'>dta</span>
                                  <span class='badge badge-fix' style='background-color: white; color: black'>txt</span>
                                        <br><br>Data file(s): Max 5GB"), multiple=TRUE),
                 textInput("text", h4("Table Name"), placeholder = "e.g tblBAS"),
                 actionButton("export_csv", "Export", class="btn-success btn-sm btn-block"),
                 br()
                 )
                 
               )
        ),
        column(6,
               wellPanel(
                 tags$fieldset(
                   tags$legend("Summary of Uploaded Table(s)"),
                 tableOutput("summary_table")
                 )
                 
               )
        )
      )
    )
  ), #end of sidelayout
  fluidRow(
    column(12,
           #h3("Table output summary"),
           DTOutput("table")
    )
  ),
  tags$footer(class="footer", h5(strong(a("IeDEA-SA" ,href = "https://www.iedea-sa.org/", target="_blank")," CSV Converter Toolkit 2024.")))
  
)

# Define server logic ----
server <- function(input, output, session) {
  
  # Function to read each file based on its extension
  read_file <- function(file_path) {
    if (endsWith(file_path, ".dta")) {
      haven::read_dta(file_path)
    } else if (endsWith(file_path, ".sas7bdat")) {
      haven::read_sas(file_path)
    } else if (endsWith(file_path, ".txt")) {
      read.table(file_path, header=TRUE, sep=",")
    } else {
      NULL
    }
  }
  
  # Reactive expression to read all uploaded files
  all_data <- reactive({
    req(input$file)
    lapply(input$file$datapath, read_file)
  })
  
  # Render summary table for each file
  output$summary_table <- renderTable({
    if (!is.null(all_data())) {
      data_summary <- lapply(seq_along(all_data()), function(i) {
        filename <- input$file$name[i]
        tablename <- sub("\\.\\w+$", "", filename)
        total_records <- nrow(all_data()[[i]])
        column_names <- colnames(all_data()[[i]])
        data.frame("Table" = tablename,
                   "Records" = total_records,
                   "Columns" = paste(column_names, collapse = ", "))
      })
      do.call(rbind, data_summary)
    }
  })
  
  # Render datatable for each file
  output$table <- renderDT({
    if (!is.null(all_data()) && length(all_data()) > 0) {
      datatable(
        all_data()[[1]], # Display the first uploaded file initially
        options = list(
          pageLength = input$rows_per_page,
          dom = 't<"dt-custom-search"i>p'
        )
      )
    }
  })
  
  # Export selected data to CSV
  # Export all data to CSV
  observeEvent(input$export_csv, {
    req(input$file)
    withProgress(message = 'Exporting Data...', value = 0, {
      # Simulating a long process
      for (i in 1:length(all_data())) {
        incProgress(1/length(all_data()))
        Sys.sleep(0.2)
        write.csv(all_data()[[i]], file = paste0(input$text, "_", i, ".csv"), row.names = FALSE)
      }
      
      showModal(
        modalDialog(
          title="Success!",
          "Files exported Successfully",
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
