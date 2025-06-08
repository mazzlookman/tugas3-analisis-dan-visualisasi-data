library(shiny)
library(ggplot2)
library(DT)
library(plotly)
library(readr)

# Load dataset
data <- read_csv("weatherweather_data.csv")

# Pilih hanya kolom numerik untuk visualisasi (drop kolom kategori untuk x/y)
numeric_vars <- names(data)[sapply(data, is.numeric)]

# UI
ui <- fluidPage(
  titlePanel("Visualisasi Data Cuaca Interaktif"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "Pilih variabel X:", choices = numeric_vars, selected = numeric_vars[1]),
      selectInput("yvar", "Pilih variabel Y:", choices = numeric_vars, selected = numeric_vars[2]),
      radioButtons("plotType", "Pilih Jenis Visualisasi:",
                   choices = c("Scatter Plot" = "scatter",
                               "Line Plot" = "line",
                               "Bar Plot" = "bar",
                               "Tabel Data" = "table"))
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.plotType == 'table'",
        DTOutput("data_table")
      ),
      conditionalPanel(
        condition = "input.plotType != 'table'",
        plotlyOutput("plot")
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  output$data_table <- renderDT({
    datatable(data)
  })
  
  output$plot <- renderPlotly({
    req(input$xvar, input$yvar)
    
    p <- ggplot(data, aes_string(x = input$xvar, y = input$yvar))
    
    if (input$plotType == "scatter") {
      p <- p + geom_point()
    } else if (input$plotType == "line") {
      p <- p + geom_line()
    } else if (input$plotType == "bar") {
      p <- p + geom_col()
    }
    
    p <- p + theme_minimal()
    ggplotly(p)
  })
}

# Jalankan Aplikasi
shinyApp(ui = ui, server = server)