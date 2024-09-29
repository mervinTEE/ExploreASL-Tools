library(shiny)
library(shinydashboard)
library(DT)
library(base64enc)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "QC Two Panel"),
  dashboardSidebar(
    sidebarMenu(
      textInput("dir_path", "Enter Directory Path:"),
      actionButton("loadDir", "Load Directory"),
      br(),
      textInput("second_dir_path", "Enter Second Directory Path:"),
      actionButton("loadSecondDir", "Load Second Directory"),
      br(),
      selectInput("entries", "Show entries:", 
                  choices = c(5, 10, 20, 50, 100), 
                  selected = 10),
      textInput("csv_filename", "CSV File Name:", value = "grading_results.csv"),
      actionButton("loadCSV", "Load CSV"),
      actionButton("saveCSV", "Save to CSV"),
      br(),
      div(style = "display: flex; height: 38px;margin-left: 15px;",
          strong("Images left:"),
          span(style = "margin-left: 5px;", textOutput("ungraded_count", inline = TRUE))
      )
    )
  ),
  dashboardBody(
    fluidRow(
      id = "main-row",
      column(width = 3,
             box(width = NULL, solidHeader = TRUE,
                 title = "Data Frame",
                 div(class = "table-container", DTOutput("grading_table"))
             )
      ),
      column(width = 9, id = "image_and_buttons_column",
             fluidRow(
               column(width = 6,
                      box(width = NULL, id = "image_preview_box",
                          title = "Image Preview",
                          div(id = "image_preview",
                              uiOutput("image_preview")
                          )
                      )
               ),
               column(width = 6,
                      box(width = NULL, id = "second_image_preview_box",
                          title = "Second Image Preview",
                          div(id = "second_image_preview",
                              uiOutput("second_image_preview")
                          )
                      )
               )
             ),
             box(width = NULL, id = "button-row",
                 actionButton("cbf", "CBF (1)"),
                 actionButton("vascular", "Vascular (2)"),
                 actionButton("artifact", "Artifact (3)"),
                 actionButton("unknown", "Unknown (4)"),
                 actionButton("previous", "Previous (←)"),
                 actionButton("next_image", "Next (→)")
             )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  values <- reactiveValues(
    dir_path = NULL,
    second_dir_path = NULL,
    image_files = NULL,
    second_image_files = NULL,
    current_index = 1,
    grading_data = data.frame(filename = character(), grading = character(), stringsAsFactors = FALSE)
  )
  
  observeEvent(input$loadDir, {
    showModal(modalDialog(
      title = "Load Directory",
      "Are you sure you want to load the directory?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmLoad", "Load")
      )
    ))
  })
  
  observeEvent(input$confirmLoad, {
    removeModal()
    values$dir_path <- input$dir_path
    if (dir.exists(values$dir_path)) {
      values$image_files <- list.files(values$dir_path, pattern = "\\.(jpg|jpeg|png)$", full.names = TRUE)
      values$current_index <- 1
      values$grading_data <- data.frame(
        filename = basename(values$image_files),
        grading = rep("", length(values$image_files)),
        stringsAsFactors = FALSE
      )
      updateUngradedCount()  # Update ungraded count after loading directory
    } else {
      showNotification("Invalid directory path", type = "error")
    }
  })
  
  observeEvent(input$loadSecondDir, {
    values$second_dir_path <- input$second_dir_path
    if (dir.exists(values$second_dir_path)) {
      values$second_image_files <- list.files(values$second_dir_path, pattern = "\\.(jpg|jpeg|png)$", full.names = TRUE)
    } else {
      showNotification("Invalid second directory path", type = "error")
    }
  })
  
  observeEvent(input$loadCSV, {
    req(values$dir_path)  # Ensure the directory path is set
    file_path <- file.path(values$dir_path, input$csv_filename)
    
    if (file.exists(file_path)) {
      loaded_data <- tryCatch({
        read.csv(file_path, stringsAsFactors = FALSE)
      }, error = function(e) {
        showNotification("Error reading CSV file", type = "error")
        return(NULL)
      })
      
      if (!is.null(loaded_data) && all(c("filename", "grading") %in% colnames(loaded_data))) {
        values$grading_data <- loaded_data
        values$current_index <- which(values$grading_data$grading == "")[1]
        if (is.na(values$current_index)) {
          values$current_index <- 1
        }
        updateUngradedCount()  # Update ungraded count after loading CSV
        showNotification("CSV loaded successfully", type = "message")
      } else {
        showNotification("CSV format is incorrect", type = "error")
      }
    } else {
      showNotification("CSV file does not exist", type = "error")
    }
  })
  
  output$image_preview <- renderUI({
    req(values$image_files)
    if (length(values$image_files) > 0) {
      current_file <- values$image_files[values$current_index]
      ext <- tools::file_ext(current_file)
      mime_type <- switch(ext,
                          jpg = "image/jpeg",
                          jpeg = "image/jpeg",
                          png = "image/png",
                          "application/octet-stream")
      
      img_data <- base64enc::base64encode(readBin(current_file, "raw", file.info(current_file)$size))
      tagList(
        tags$p(id = "image_caption", basename(current_file)),
        tags$img(src = paste0(sprintf("data:%s;base64,", mime_type), img_data), 
                 style = "max-width: 100%; max-height: 100%; object-fit: contain;")
      )
    }
  })
  
  output$second_image_preview <- renderUI({
    req(values$second_image_files)
    if (length(values$second_image_files) > 0) {
      current_file <- values$second_image_files[values$current_index]
      ext <- tools::file_ext(current_file)
      mime_type <- switch(ext,
                          jpg = "image/jpeg",
                          jpeg = "image/jpeg",
                          png = "image/png",
                          "application/octet-stream")
      
      img_data <- base64enc::base64encode(readBin(current_file, "raw", file.info(current_file)$size))
      tagList(
        tags$p(id = "second_image_caption", basename(current_file)),
        tags$img(src = paste0(sprintf("data:%s;base64,", mime_type), img_data), 
                 style = "max-width: 100%; max-height: 100%; object-fit: contain;")
      )
    }
  })
  
  observeEvent(input$cbf, {
    updateGrading("CBF")
  })
  
  observeEvent(input$vascular, {
    updateGrading("VASCULAR")
  })
  
  observeEvent(input$artifact, {
    updateGrading("ARTIFACT")
  })
  
  observeEvent(input$unknown, {
    updateGrading("UNKNOWN")
  })
  
  updateGrading <- function(grade) {
    values$grading_data$grading[values$current_index] <- grade
    values$current_index <- min(values$current_index + 1, length(values$image_files))
    dataTableProxy('grading_table') %>% replaceData(values$grading_data)
    updateUngradedCount()  # Update ungraded count after grading
  }
  
  observeEvent(input$previous, {
    values$current_index <- max(values$current_index - 1, 1)
    dataTableProxy('grading_table') %>% replaceData(values$grading_data)
  })
  
  observeEvent(input$next_image, {
    values$current_index <- min(values$current_index + 1, length(values$image_files))
    dataTableProxy('grading_table') %>% replaceData(values$grading_data)
  })
  
  output$grading_table <- renderDT({
    req(values$grading_data)
    total_rows <- nrow(values$grading_data)
    entries <- as.numeric(input$entries)
    
    start_row <- max(1, values$current_index - floor(entries/2))
    end_row <- min(total_rows, start_row + entries - 1)
    
    if (end_row == total_rows) {
      start_row <- max(1, total_rows - entries + 1)
    }
    
    subset_data <- values$grading_data[start_row:end_row, ]
    
    datatable(
      subset_data,
      options = list(
        pageLength = entries,
        lengthMenu = list(c(5, 10, 20, 50, 100), c('5', '10', '20', '50', '100')),
        searching = FALSE,
        info = FALSE,
        paging = FALSE,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'filename',
        target = 'row',
        backgroundColor = styleEqual(values$grading_data$filename[values$current_index], 'lightblue')
      )
  })
  
  observeEvent(input$saveCSV, {
    req(values$grading_data)
    
    # Construct the full file path
    file_path <- file.path(values$dir_path, input$csv_filename)
    
    # Check if the file already exists
    if (file.exists(file_path)) {
      # If it exists, show a confirmation dialog
      showModal(modalDialog(
        title = "File already exists",
        "The specified file already exists. Do you want to overwrite it?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirmOverwrite", "Overwrite")
        )
      ))
    } else {
      # If it doesn't exist, save the file
      saveCSV(file_path)
    }
  })
  
  # Add a new observer for the confirmation
  observeEvent(input$confirmOverwrite, {
    req(values$grading_data)
    file_path <- file.path(values$dir_path, input$csv_filename)
    saveCSV(file_path)
    removeModal()
  })
  
  # Create a function to save the CSV
  saveCSV <- function(file_path) {
    write.csv(values$grading_data, file = file_path, row.names = FALSE)
    showNotification(paste("Results saved to", basename(file_path)), type = "message")
  }
  
  # Function to update the ungraded count
  updateUngradedCount <- function() {
    ungraded_count <- sum(values$grading_data$grading == "")
    output$ungraded_count <- renderText({
      ungraded_count
    })
  }
}

# Run the app
shinyApp(ui, server)