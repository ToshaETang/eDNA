# Load libraries
library(shiny)
library(ggplot2)
library(vegan)
library(ggforce)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("Combined Shiny App"),
  tabsetPanel(
    tabPanel("NMDS Visualization",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file_nmds", "Choose CSV File",
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 helpText("Note: Please upload a CSV file with binary matrix data.")
               ),
               mainPanel(
                 plotOutput("nmdsPlot")
               )
             )
    ),
    tabPanel("CSV Viewer with Column Selection",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file_csv", "Choose CSV File",
                           accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                 selectInput("river_select", "Select River", NULL),
                 selectInput("species_select", "Select Species", NULL)
               ),
               mainPanel(
                 DTOutput("table"),
                 verbatimTextOutput("unique_species"),
                 verbatimTextOutput("unique_rivers"),
                 DTOutput("binary_matrix"),
                 downloadButton("save_button", "Save Binary Matrix")
               )
             )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Read data for NMDS
  data_LAN <- reactive({
    inFile <- input$file_nmds
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath, header = TRUE, row.names = 1)
  })
  
  # Perform NMDS and create plot
  output$nmdsPlot <- renderPlot({
    req(data_LAN())
    
    dfNmds <- metaMDS(data_LAN(), distance = "bray", k = 2, trymax = 100)
    data <- data.frame(dfNmds$points)
    
    ggplot(data, aes(x = MDS1, y = MDS2)) +
      geom_point(size = 2) +
      theme_classic() +
      geom_text(
        aes(label = rownames(data)),
        vjust = 1.5,
        size = 3,
        color = "black"
      ) +
      labs(
        subtitle = paste("stress=", round(dfNmds$stress, 3), sep = "")
      )
  })
  
  # Read data for CSV Viewer
  data_csv <- reactive({
    req(input$file_csv)  # Ensure file is selected
    read.csv(input$file_csv$datapath)
  })
  
  # Update dropdown options
  observe({
    col_options <- names(data_csv())
    updateSelectInput(session, "river_select", choices = col_options)
    updateSelectInput(session, "species_select", choices = col_options)
  })
  
  # Reactive value to store binary matrix
  binary_matrix <- reactiveVal(NULL)
  
  # Selected data for CSV Viewer
  selected_data <- reactive({
    req(input$river_select, input$species_select)
    data_selected <- data_csv()[, c(input$river_select, input$species_select), drop = FALSE]
    colnames(data_selected) <- c("River", "Species")
    
    # Create binary matrix
    binary_matrix_data <- table(data_selected$River, data_selected$Species)
    binary_matrix_data[binary_matrix_data > 1] <- 1
    binary_matrix(binary_matrix_data)  # Store binary matrix in reactive value
    
    data_selected
  })
  
  # Display selected rows
  output$table <- renderDT({
    datatable(selected_data())
  })
  
  # Display unique species
  output$unique_species <- renderPrint({
    unique_species <- unique(selected_data()$Species)
    cat("Unique Species: ", paste(unique_species, collapse = ", "))
  })
  
  # Display unique rivers
  output$unique_rivers <- renderPrint({
    unique_rivers <- unique(selected_data()$River)
    cat("Unique Rivers: ", paste(unique_rivers, collapse = ", "))
  })
  
  # Display binary matrix
  output$binary_matrix <- renderDT({
    datatable(binary_matrix())
  })
  
  # Save binary matrix
  output$save_button <- downloadHandler(
    filename = function() {
      paste("binary_matrix_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Use isolate to prevent reactiveValues from being accessed reactively
      write.csv(isolate(binary_matrix()), file, row.names = TRUE)
    }
  )
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
