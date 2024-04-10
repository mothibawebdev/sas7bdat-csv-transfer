library(shiny)
library(haven)
library(DT)
library(foreign)
library(htmltools)
library(shinyWidgets)
library(shinythemes)
library(shinyalert)
library(shinyFiles)
library(RODBC)
library(shinyMobile)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
library(readxl)
library(hrbrthemes)
library(scales)
library(gridGraphics)
library(gripp)
library(rmarkdown)
library(patchwork)


#con <- odbcConnect("IeDEA_MR_1")

#sf_UI <- shinyFileSave(fileInputId = "saveLocation", label = "Choose where to save files", title = "Save files")

#increase file upload size to 5GB
options(shiny.maxRequestSize = 5 * 1024^3)
# Define UI ----
ui <- navbarPage(theme = shinytheme("cerulean"), title = "IeDEA Converter Toolkit", id = "navt",
                 tags$head(
                   tags$link(rel="IeDEA-DT", href="favicon.ico"),
                   tags$title("IeDEA Converter Toolkit"),
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
        } 
        "
                     )
                   )
                 ),
                 
                 tabPanel(
                   "Data Upload",
                   sidebarLayout(
                     sidebarPanel(
                       img(src = "logo2.png", height = 150, width = "100%"),
                       h5("Converter Toolkit is a tool used for transferring datasets to other formats. It accepts
         files like sas, dta, and txt files."),
                       p(HTML("<ul>
                    <li><span class='badge badge-fix' style='background-color: maroon; font-weight: bold;'>Step 1:</span> Select data files to upload</li>
                    <li><span class='badge badge-fix' style='background-color: blue; font-weight: bold;'>Step 2:</span> Provide table name</li>
                    <li><span class='badge badge-fix' style='background-color: white; color: black; font-weight: bold;'>Step 3:</span> Click Export button</li>
                </ul>
            <strong>When uploading multiple files, provide a generic naming like tbl.</strong>"))
                       #sf_UI
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
                                    actionButton("export_csv", "Export", class="btn-success btn-sm"),
                                    actionButton("analyze", "Analyze", class="btn-info btn-sm")
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
                   )
                 ),#end of data upload panel
                 tabPanel("Analyze",
                          # cat('<div class="panel-body">This page will fix you data and rewrite to csv</div>'),
                          #      
                          # p( "This page will fix you data and rewrite to csv."),
                          # p("Using pre-defined data IDs, ART_ID, MED_ID, etc"),
                          # DTOutput("table_analyze"),
                          fluidRow(
                            column(12,
                                   tabsetPanel(
                                     tabPanel("ART",
                                              fluidRow(
                                                column(12,
                                                       plotlyOutput("gauge_plot")
                                                       
                                                )
                                              )
                                     ),
                                     tabPanel("Enrolment",
                                              fluidRow(
                                                column(6,br(),
                                                  shinycssloaders::withSpinner(tableOutput("total_programs_table"), type = 1)
                                                  
                                                ),
                                                column(6,br(),
                                                  shinycssloaders::withSpinner(plotlyOutput("program_enrolment_plot"), type=1)
                                                       
                                                )
                                              ),
                                              fluidRow(
                                                column(6,br(),
                                                  shinycssloaders::withSpinner(plotlyOutput("age_group_plot"), type=1)
                                                   
                                                ),
                                                column(6,br(),
                                                  shinycssloaders::withSpinner(plotlyOutput('enrolment_plot'), type = 1)
                                                )
                                              )
                                              
                                     ),
                                     tabPanel("Medications",
                                              fluidRow(
                                                column(12, 
                                                       p("Total medication distributed")        
                                                )
                                              )
                                     ),
                                     tabPanel("Visitations",
                                              fluidRow(
                                                column(12,br(),
                                                       shinycssloaders::withSpinner(plotlyOutput("center_total"), type = 1)      
                                                )
                                              )
                                     ),
                                     tabPanel("Dates Comparisions",
                                              fluidRow(
                                                column(12,br(),
                                                  p(HTML('<mark>Connencted to IeDEA_MR_1</span>')),
                                                  p('ART_ED < ART_SD'),
                                                  p('ART_SD/DIS_D/MED_SD/L_ALIVE_D/VIS_D,LAB_D/CD4_D,RNA_D > DEATH_D')
                                                  
                                                  
                                                )
                                              )
                                     )
                                   )
                            )
                          )
                 ),#end of Analyze panel
                 
                 tags$footer(class="footer", h5(strong(a("IeDEA-SA" ,href = "https://www.iedea-sa.org/", target="_blank")," CSV Converter Toolkit 2024."))
                 )
                 
)

# Define server logic ----
server <- function(input, output, session) {
  
  # shinyFileSaveObserver(input, "saveLocation", session, {
  #   cat("You selected file: ", sprintf("%s\n", paste(input$saveLocation, collapse=", ")))
  # })
  observeEvent(input$analyze, {
    updateNavbarPage(session, "navt", selected = "Analyze")
  })
  
  # Function to read each file based on its extension
  read_file <- function(file_path) {
    if (endsWith(file_path, ".dta")) {
      haven::read_dta(file_path)
    } else if (endsWith(file_path, ".sas7bdat")) {
      haven::read_sas(file_path)
    } else if (endsWith(file_path, ".txt")) {
      read.table(file_path, header=TRUE, sep=",")
    } else if (endsWith(file_path, "csv")) {
      read.table(file_path, header=TRUE, sep=",")
    }
    else {
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
  
  # Render pie chart based on 'ART_ID' column
  output$gauge_plot <- renderPlotly({
    req(input$file)
    data <- read_file(input$file$datapath[1]) # Assuming you want to plot gauge for the first uploaded file
    
    # Convert column names to lowercase
    colnames_lower <- tolower(colnames(data))
    
    if ("art_id" %in% colnames_lower) {
      # Calculate counts for all ART_ID values
      art_id_counts <- table(data$art_id)
      
      # Create a data frame for ggplot
      df <- data.frame(ART_ID = names(art_id_counts), Count = as.numeric(art_id_counts))
      
      # Plot bar graph using ggplot
      gg <- ggplot(df, aes(x = ART_ID, y = Count, fill = ART_ID)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(x = "ART ID", y = "Total Count") +
        theme(legend.position = "none")+
        theme(axis.text.x = element_text(angle = 90))
      
      # Convert ggplot object to plotly
      ggplotly(gg)
    } else {
      plot_ly(x = 1, y = 1, type = "scatter", mode = "markers", marker = list(size = 1)) %>%
        layout(title = "tblART not uploaded", xaxis = list(title = ""), yaxis = list(title = ""))
    }
  })
  
  
  #Enrolment per year if tblBAS is uploaded 'enrol_d' 
  output$enrolment_plot <- renderPlotly({
    req(input$file)
    data <- read_file(input$file$datapath[1]) # Assuming you want to plot enrollment for the first uploaded file
    colnames_lower <- tolower(colnames(data)) # Convert column names to lowercase
    
    # Check if 'enrol_d' or 'ENROL_D' column exists (case-insensitive)
    if (any(grepl("enrol_d", colnames_lower, ignore.case = TRUE))) {
      # Convert 'enrol_d' to Date format
      data$enrol_d <- as.Date(data$enrol_d)
      # Extract year from 'enrol_d'
      data$year <- format(data$enrol_d, "%Y")
      data <- filter(data, enrol_d >= as.Date("1998-01-01"))
      
      # Calculate total enrollment per year
      enrolment_per_year <- data %>%
        mutate(enrolment_year = year(enrol_d)) %>%
        group_by(enrolment_year) %>%
        summarize(enrolment_every_year = n()) # Change 'enrolment' to the appropriate column name representing enrolment count
      
      if (nrow(enrolment_per_year) == 0) {
        # If no rows in enrolment_per_year, return a message plot
        plot_ly(x = 1, y = 1, type = "scatter", mode = "markers", marker = list(size = 1)) %>%
          layout(title = "No enrolment data found for the specified years", xaxis = list(title = ""), yaxis = list(title = ""))
      } else {
        # Plot line graph
        p <- ggplot(enrolment_per_year, aes(x = enrolment_year, y = enrolment_every_year)) +
          geom_point()+
          geom_line() +
          labs(title="Enrolment per Year", x = "", y = "") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90))
        
        # Convert ggplot to plotly
        ggplotly(p)
      }
    } else {
      # If 'enrol_d' or 'ENROL_D' column does not exist, return an empty plot
      plot_ly(x = 1, y = 1, type = "scatter", mode = "markers", marker = list(size = 1)) %>%
        layout(title = "tblBAS or Enrol_D not found", xaxis = list(title = ""), yaxis = list(title = ""))
    }
  })
  
  #plot horizontal graph total enrolment per program over the years
  output$program_enrolment_plot <- renderPlotly({
    req(input$file)
    data <- read_file(input$file$datapath[1]) # Assuming you want to plot enrollment for the first uploaded file
    data$enrol_d <- as.Date(data$enrol_d)
    # Extract year from 'enrol_d'
    data$year <- format(data$enrol_d, "%Y")
    # Filter data for years from 1998 onwards
    data <- filter(data, enrol_d >= as.Date("1998-01-01"))
    # Check if 'program' and 'year' columns exist in the data
    if ("program" %in% colnames(data) && "year" %in% colnames(data)) {
      # Group by program and year, then calculate total enrollment for each program
      enrolment_per_program_year <- data %>%
        group_by(program, year) %>%
        summarize(total_enrollment = n())
      
      # Plotting
      plot_per_program <- enrolment_per_program_year %>%
        plot_ly(x = ~total_enrollment, y = ~program, color = ~year, colors = 'Set1', type = 'bar', orientation = 'h') %>%
        layout(title = "Total Enrollment per Program Over the Years",
               xaxis = list(title = ""),
               yaxis = list(title = ""),
               barmode = 'stack')
      
      ggplotly(plot_per_program)
    } else {
      # If 'program' or 'year' columns are missing, return an empty plot with a message
      plot_ly(x = 1, y = 1, type = "scatter", mode = "markers", marker = list(size = 1)) %>%
        layout(title = "Program or Year column not found", xaxis = list(title = ""), yaxis = list(title = ""))
    }
  })
  
  #age-group [0-10] [11-20] [21-30] [31-40] [41+] birth_d at enrolment by gender
  output$age_group_plot <- renderPlotly({
    req(input$file)
    data <- read_file(input$file$datapath[1])
    data$birth_d <- as.Date(data$birth_d)
    data$enrol_d <- as.Date(data$enrol_d) #age age enrolment
    
    age_breaks <- c(0,10,20,30,40,Inf)
    age_labels <- c("[0-10]","[11-20]","[21-30]","[31-40]","[41+]")
    
    data$age_group <- cut(as.numeric(difftime(data$enrol_d,data$birth_d, units = "days"))/365.25,
                          breaks = age_breaks, labels = age_labels, right=FALSE)
    
    # Map numerical sex values to labels
    data$sex_label <- ifelse(data$sex == 1, "Male", 
                             ifelse(data$sex == 2, "Female",
                                    ifelse(data$sex == 9, "Other", "Unknown")))
    
    plot_age_group <- data %>%
      group_by(age_group, sex_label) %>%
      summarise(count = n()) %>%
      plot_ly(x = ~age_group, y = ~count, color = ~sex_label, type = "bar") %>%
      layout(title = "Age at Enrolment by Sex",
             xaxis = list(title = "Age Group"),
             yaxis = list(title = "Count"),
             colorway = c("#1f77b4", "#ff7f0e", "#2ca02c")) # Colors for male, female, and other respectively
    
  })
  
  #tabulate total programs
  output$total_programs_table <- renderTable({
    req(input$file)
    data <- read_file(input$file$datapath[1])  # Assuming you want to use the first uploaded file
    
    # Filter out enrol_d dates before 1998
    data <- data[data$enrol_d >= as.Date("1998-01-01"), ]
    
    # Check if 'program' column exists
    if ("program" %in% colnames(data) && "enrol_d" %in% colnames(data)) {
      # Calculate total count of programs
      program_counts <- table(data$program)
      
      # Get the open date (first enrol_d date) for each program
      open_dates <- tapply(data$enrol_d, data$program, FUN = min)
      last_dates <- tapply(data$enrol_d, data$program, FUN = max)
      
      # Convert the table to a data frame for better rendering in the table
      program_table <- as.data.frame(program_counts)
      names(program_table) <- c("Program", "Total")
      
      # Add open date column to the data frame
      program_table$Open_Date <- sapply(program_table$Program, function(prog) open_dates[[as.character(prog)]])
      program_table$Last_Date <- sapply(program_table$Program, function(prog) last_dates[[as.character(prog)]])
      
      # Reorder the columns, moving "Total" column to the last position
      program_table <- program_table[, c("Program", "Open_Date", "Last_Date", "Total")]
      
      #datatable(program_table, options = list(columnDefs = list(list(width = '500px', targets = 1))))
      program_table
    } else {
      # If 'program' column does not exist, return an empty table with a message
      data.frame(Program = NA, Total = NA, Open_Date = NA, Last_Date)
    }
  })
  
  #overall total visits per center
  output$center_total <- renderPlotly({
    req(input$file)
    data <- read_file(input$file$datapath[1]) # Assuming you want to plot gauge for the first uploaded file
    
    # Convert column names to lowercase
    colnames_lower <- tolower(colnames(data))
    
    if ("center" %in% colnames_lower) {
      # Calculate counts for all ART_ID values
      center_count <- table(data$center)
      
      # Create a data frame for ggplot
      df <- data.frame(CENTER = names(center_count), Count = as.numeric(center_count))
      
      # Plot bar graph using ggplot
      center_visits <- ggplot(df, aes(x = Count, y = CENTER, fill = CENTER)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(y = "", x = "") +  # Empty x-axis label
        theme(legend.position = "none", axis.text.x = element_blank()) +  # Hide x-axis text
        theme(axis.text.y = element_text(angle = 0))  # Rotated y-axis labels
      
      # Convert ggplot object to plotly
      p <- ggplotly(center_visits)
      
      # Modify the labels to display as integers
      p$x$data[[1]]$text <- as.character(df$Count)
      
      return(p)
    } else {
      plot_ly(x = 1, y = 1, type = "scatter", mode = "markers", marker = list(size = 1)) %>%
        layout(title = "tblCenter not uploaded", xaxis = list(title = ""), yaxis = list(title = ""))
    }
  })
  
##check where l_alive_d > death_d(may need to connect to DB)
#--------------------------------
  
#--------------------------------
##check where vis_d > death_d
##check where birth_d < vis_d/enrol_d/death_d
##check invalid date formats
##check where art_ed < art_sd / med_ed < med_sd / dis_ed < dis_sd
  
  
  
  
  
  
  
  
  
  
  
  
  
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
