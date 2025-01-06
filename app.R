library(shiny)
library(purrr)
library(dplyr)
library(bslib)
library(shinyjs) #for colapse sidebar

# Define UI for data upload app ----
ui <- fluidPage(
  useShinyjs(),
  navbarPage("",
             tabPanel("Gurukul Fees Status Dashboard",
                      div( id ="Sidebar",sidebarPanel(
                        fileInput("file1", "Choose CSV File",
                                  multiple = FALSE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")),
                        #tags$hr(),       # Horizontal line ----
                        
                        selectInput("selected_standard", 
                                    "Selected Standard", 
                                    choices = character(0)),  # Initialize with empty vector
                      )),
                      mainPanel(actionButton("toggleSidebar", "Toggle sidebar"),
                                tableOutput("contents"))
)))

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  # Create reactive data frame
  dataInput <- reactive({
    req(input$file1)
    
    tryCatch({
      df <- read.csv(input$file1$datapath,
                     header = FALSE,
                     sep = ",")
      
      # Rename columns
      new_names <- c("Serial number", "Standard", "Student Full Name", "Mobile Number", 
                     "Previous Pending Fee", "Summer Fee", "Current Fee", "Fund", 
                     "Total Fee", "Paid Fee", "Remaining Fee", "Remark")
      df %>% set_names(new_names)
    },
    error = function(e) {
      stop(safeError(e))
    })
  })
  
  # Update standard choices when data is loaded
  observe({
    req(dataInput())
    standards <- c("All", unique(dataInput()$Standard))
    updateSelectInput(session, 
                      "selected_standard",
                      choices = standards)
  })
  
  observeEvent(input$toggleSidebar, {
    shinyjs::toggle(id = "Sidebar")
  })
  
  # Create filtered reactive dataset
  filtered_data <- reactive({
    req(dataInput(), input$selected_standard)
    if (input$selected_standard == "All") {
      dataInput()
    } else {
      dataInput() %>%
        filter(Standard == input$selected_standard)
    }
  })
  
  # Render the filtered table
  output$contents <- renderTable({
    #options = list(
    #autoWidth = TRUE,
    #columnDefs = list(list(targets = "_all")),#, className = "dt-body-wrap")),
    filtered_data()
    
})
}

# Create Shiny app ----
shinyApp(ui, server)
  
  