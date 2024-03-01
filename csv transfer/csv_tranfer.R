library(shiny)
library(haven)
library(DT)
library(foreign)
#library(RODBC)
library(htmltools)
library(shinyWidgets)
library(shinythemes)
library(shinyalert)

#install.packages("shinyalert")

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
      h5("CSV Converter Toolkit is a tool used for transfering datasets to a csv format. It accepts
         files like sas, dta and txt files."),
      p(HTML("Step 1: Select data files to upload<br>
        Step 2: Provide table name<br>
        Step 3; click Export button<br>
        <strong>When uploading multiple files, provide a generic naming like tbl. It will then name your files tbl_1, tbl_2, etc. 
             </strong>"))
    ),
    mainPanel(
      fluidRow(
        column(6,
               wellPanel(
                 tags$fieldset(
                   tags$legend("Select Data Files"),
                   fileInput("file", HTML("Allowed file formats include: <span class='badge badge-fix' style='background-color: blue; font-weight: bold;'>sas</span>
                                  <span class='badge badge-fix' style='background-color: maroon; font-weight: bold;'>dta</span>
                                  <span class='badge badge-fix' style='background-color: white; color: black; font-weight: bold;'>txt</span>
                                       <br> Data file(s): Max 5GB"), multiple=TRUE),
                 textInput("text", h4("Table Name"), placeholder = "e.g tblBAS"),
                 actionButton("export_csv", "Export", class="btn-success btn-sm btn-block")
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
      shinyalert(
        title = "Failed!",
        text = "Unsupported file format. Only .dta, .sas7bdat, and .txt files are allowed.",
        type = "error",
        closeOnEsc = TRUE
      )
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
  
  observeEvent(input$export_csv, {
    req(input$file)
    withProgress(message = 'Exporting Data...', value = 0, {
      # Iterate over each uploaded file
      for (i in 1:length(all_data())) {
        incProgress(1/length(all_data()))
        
        # Get the data and calculate the number of splits needed
        data <- all_data()[[i]]
        total_records <- nrow(data)
        num_splits <- ceiling(total_records / 1000000)
        
        # Split the data into parts and export each part
        for (j in 1:num_splits) {
          start_index <- (j - 1) * 1000000 + 1
          end_index <- min(j * 1000000, total_records)
          part_data <- data[start_index:end_index, ]
          
          # Export the part data to a CSV file
          write.csv(part_data, file = paste0(input$text, "_", i, "_part", j, ".csv"), row.names = FALSE)
        }
      }
      
      shinyalert(
        title = "File export complete!",
        text = "File(s) exported completed successfully",
        type = "success",
        closeOnEsc = TRUE
      )
      # Clear the text input field after export
      updateTextInput(session, "text", value = "")
    })
  })
  
  
}


# Run the app ----
shinyApp(ui = ui, server = server)
