library(shiny)
library(DT)

# 定義 UI
ui <- fluidPage(
  titlePanel("CSV Viewer with Column Selection"),
  
  # 上傳 CSV 檔案
  fileInput("file", "Choose CSV File",
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
  
  # 下拉式選單，用於選擇河川名稱
  selectInput("river_select", "Select River", NULL),
  
  # 下拉式選單，用於選擇物種
  selectInput("species_select", "Select Species", NULL),
  
  # 顯示表格
  DTOutput("table"),
  
  # 顯示不同的物種
  verbatimTextOutput("unique_species"),
  
  # 顯示所有不同的河川
  verbatimTextOutput("unique_rivers"),
  
  # 顯示二維表格
  DTOutput("binary_matrix")
)

# 定義 server 邏輯
server <- function(input, output, session) {
  
  # 讀取 CSV 檔案
  data <- reactive({
    req(input$file)  # 確保檔案已經被選取
    read.csv(input$file$datapath)
  })
  
  # 更新下拉式選單的選項
  observe({
    col_options <- names(data())
    updateSelectInput(session, "river_select", choices = col_options)
    updateSelectInput(session, "species_select", choices = col_options)
  })
  
  # 選擇河川名稱和物種的資料
  selected_data <- reactive({
    req(input$river_select, input$species_select)
    data_selected <- data()[, c(input$river_select, input$species_select), drop = FALSE]
    colnames(data_selected) <- c("River", "Species")
    data_selected
  })
  
  # 顯示選擇的列
  output$table <- renderDT({
    datatable(selected_data())
  })
  
  # 取得物種那一列的所有不同值
  output$unique_species <- renderPrint({
    unique_species <- unique(selected_data()$Species)
    cat("Unique Species: ", paste(unique_species, collapse = ", "))
  })
  
  # 取得河川那一列的所有不同值
  output$unique_rivers <- renderPrint({
    unique_rivers <- unique(selected_data()$River)
    cat("Unique Rivers: ", paste(unique_rivers, collapse = ", "))
  })
  
  # 顯示二維表格
  output$binary_matrix <- renderDT({
    # 創建二維表格
    binary_matrix <- table(selected_data()$River, selected_data()$Species)
    
    # 轉換成資料框
    binary_df <- as.data.frame.matrix(binary_matrix)
    
    # 顯示表格
    datatable(binary_df)
  })
}

# 執行應用程式
shinyApp(ui = ui, server = server)
