library(shiny)
library(purrr)
library(dplyr)
library(bslib)
library(shinyjs) #for colapse sidebar
library(plotly)  # Add this new library

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
                        selectInput("selected_standard", 
                                    "Selected Standard", 
                                    choices = character(0)),
                        actionButton("clearFilters", "Clear All Filters",
                                     class = "btn btn-warning"),
                        uiOutput("filterInputs")
                      )),
                      mainPanel(actionButton("toggleSidebar", "Toggle sidebar"),
                                tabsetPanel(
                                  tabPanel("Data", tableOutput("contents")),
                                  tabPanel("Charts",
                                           fluidRow(
                                             column(12, plotlyOutput("feeBarChart", height = "400px"))
                                           ),
                                           br(), # Add bottom spacing
                                           fluidRow(
                                             column(12, plotlyOutput("feeByStandardChart", height = "400px"))
                                           ),
                                           br(), #br(), # Add double spacing
                                           #tags$hr(),
                                           fluidRow(
                                             column(12, plotlyOutput("feePieChart", height = "400px"))
                                           ),
                                           br(), #br(), # Add double spacing
                                           fluidRow(
                                             column(12, plotlyOutput("standardBarChart", height = "400px"))
                                           ),
                                           br() # Add bottom spacing
                                  )
                                )
                      )
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
      new_names <- c(#"Serial number", 
        "Standard", "Student Full Name", "Mobile Number", 
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
  
  # Create dynamic filter inputs
  output$filterInputs <- renderUI({
    req(dataInput())
    df <- dataInput()
    
    lapply(names(df), function(col) {
      if (col != "Standard") {  # Skip Standard as it's already handled
        unique_values <- unique(df[[col]])
        selectInput(
          inputId = paste0("filter_", col),
          label = paste("Filter", col),
          choices = c("All", unique_values),
          selected = "All"
        )
      }
    })
  })
  
  # Clear filters observer
  observeEvent(input$clearFilters, {
    req(dataInput())
    updateSelectInput(session, "selected_standard", selected = "All")
    
    for(col in names(dataInput())) {
      if (col != "Standard") {
        updateSelectInput(session, 
                          paste0("filter_", col), 
                          selected = "All")
      }
    }
  })
  
  # Create filtered reactive dataset
  filtered_data <- reactive({
    req(dataInput(), input$selected_standard)
    df <- dataInput()
    
    # Apply Standard filter
    if (input$selected_standard == "All") {
      df <- df
    } else {
      df <- df %>%
        filter(Standard == input$selected_standard)
    }
    
    # Apply column filters
    for(col in names(df)) {
      if (col != "Standard") {  # Skip Standard as it's already handled
        filter_input <- input[[paste0("filter_", col)]]
        if (!is.null(filter_input) && filter_input != "All") {
          df <- df %>% filter(!!sym(col) == filter_input)
        }
      }
    }
    
    df
  })
  
  # Render the filtered table
  output$contents <- renderTable({
    #options = list(
    #autoWidth = TRUE,
    #columnDefs = list(list(targets = "_all")),#, className = "dt-body-wrap")),
    filtered_data()
    
  })
  
  # Bar chart for fee distribution
  output$feeBarChart <- renderPlotly({
    req(filtered_data())
    
    # Prepare data for better visualization
    df <- filtered_data() %>%
      mutate(
        `Previous Pending Fee` = as.numeric(trimws(`Previous Pending Fee`)),
        `Paid Fee` = as.numeric(trimws(`Paid Fee`)),
        `Remaining Fee` = as.numeric(trimws(`Remaining Fee`))
      )
    
    plot_ly(df) %>%
      add_trace(
        x = ~`Student Full Name`,
        y = ~`Previous Pending Fee`,
        type = 'bar',
        name = 'Previous Pending',
        marker = list(color = '#3498db')  # Blue
      ) %>%
      add_trace(
        x = ~`Student Full Name`,
        y = ~`Paid Fee`,
        type = 'bar',
        name = 'Paid',
        marker = list(color = '#2ecc71')  # Green
      ) %>%
      add_trace(
        x = ~`Student Full Name`,
        y = ~`Remaining Fee`,
        type = 'bar',
        name = 'Remaining',
        marker = list(color = '#e74c3c')  # Red
      ) %>%
      layout(
        title = "Fee Distribution by Student",
        barmode = 'stack',
        xaxis = list(
          title = "Student Name",
          tickangle = 45
        ),
        yaxis = list(
          title = "Amount (₹)",
          hoverformat = "₹%{y:,.0f}"
        ),
        hovermode = 'x unified',
        showlegend = TRUE,
        legend = list(
          x = 1.1,
          y = 0.5,
          xanchor = 'left'
        ),
        margin = list(r = 150) # Add right margin for legend
      )
  })
  
  # Pie chart for overall fee status
  output$feePieChart <- renderPlotly({
    req(filtered_data())
    
    # First print the raw data to debug
    print("Raw fee data:")
    print(head(filtered_data()[, c("Paid Fee", "Remaining Fee")]))
    
    # Convert fees to numeric more carefully
    total_stats <- filtered_data() %>%
      transmute(
        Paid = as.numeric(trimws(`Paid Fee`)),  # Remove whitespace first
        Remaining = as.numeric(trimws(`Remaining Fee`))
      ) %>%
      summarise(
        Total_Paid = sum(Paid, na.rm = TRUE),
        Total_Remaining = sum(Remaining, na.rm = TRUE)
      )
    
    # Debug print
    print("Processed totals:")
    print(total_stats)
    
    # Create pie chart with simpler configuration
    plot_ly(
      labels = c("Paid Fees", "Remaining Fees"),
      values = unname(c(total_stats$Total_Paid, total_stats$Total_Remaining)),
      type = 'pie',
      textposition = 'inside',
      textinfo = 'label+value',
      insidetextfont = list(size = 14),
      hoverinfo = 'text',
      text = ~paste('₹', format(c(total_stats$Total_Paid, total_stats$Total_Remaining), 
                                big.mark=","))
    ) %>%
      layout(
        title = list(
          text = "Overall Fee Status",
          font = list(size = 20)
        ),
        showlegend = TRUE
      )
  })
  
  # Bar chart for student count by standard
  output$standardBarChart <- renderPlotly({
    req(dataInput())
    standard_count <- dataInput() %>%
      mutate(Standard = as.numeric(as.character(Standard))) %>%  # Convert to numeric
      group_by(Standard) %>%
      summarise(Count = n()) %>%
      arrange(Standard)
    
    plot_ly(standard_count,
            x = ~Standard,
            y = ~Count,
            type = "bar") %>%
      layout(title = "Students per Standard",
             xaxis = list(
               title = "Standard",
               type = "category",
               categoryorder = "array",
               categoryarray = sort(unique(standard_count$Standard))
             ),
             yaxis = list(title = "Number of Students"))
  })
  
  # Fee Distribution by Standard chart
  output$feeByStandardChart <- renderPlotly({
    req(dataInput())
    
    standard_fees <- dataInput() %>%
      mutate(Standard = as.numeric(as.character(Standard))) %>%  # Convert to numeric
      group_by(Standard) %>%
      summarise(
        Total_Previous = sum(as.numeric(trimws(`Previous Pending Fee`)), na.rm = TRUE),
        Total_Paid = sum(as.numeric(trimws(`Paid Fee`)), na.rm = TRUE),
        Total_Remaining = sum(as.numeric(trimws(`Remaining Fee`)), na.rm = TRUE)
      ) %>%
      arrange(Standard)
    
    plot_ly(standard_fees) %>%
      add_trace(
        x = ~Standard,
        y = ~Total_Previous,
        type = 'bar',
        name = 'Previous Pending',
        marker = list(color = '#3498db')
      ) %>%
      add_trace(
        x = ~Standard,
        y = ~Total_Paid,
        type = 'bar',
        name = 'Paid',
        marker = list(color = '#2ecc71')
      ) %>%
      add_trace(
        x = ~Standard,
        y = ~Total_Remaining,
        type = 'bar',
        name = 'Remaining',
        marker = list(color = '#e74c3c')
      ) %>%
      layout(
        title = "Fee Distribution by Standard",
        barmode = 'stack',
        xaxis = list(
          title = "Standard",
          type = "category",
          categoryorder = "array",
          categoryarray = sort(unique(standard_fees$Standard))
        ),
        yaxis = list(
          title = "Amount (₹)",
          hoverformat = "₹%{y:,.0f}"
        ),
        hovermode = 'x unified',
        showlegend = TRUE
      )
  })
}

# Create Shiny app ----
shinyApp(ui, server)

