library(shiny)
dataset <- read.delim('https://raw.githubusercontent.com/HackBio-Internship/public_datasets/main/R/datasets/pca_sc_dataset.dat')
dataset <- subset(dataset, select = -c(gene, Fig4DE.genecluster, lineage))
View(dataset)
mygenedata <- colnames(dataset)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Gene Data!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      
      selectInput(
        inputId = "select_gene",
        label = "gene",
        choices = c(
          "E3",
          "E4",
          "EPI",
          "PE",
          "TE",
          "EPI.1",
          "PE.1",
          "TE.1",
          "EPI.2",
          "E5.pre.lineage"
          
        ),
        selected = "All",
        multiple = TRUE
      )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput("distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  output$distPlot <- renderPlot({
    
    x    <- mygenedata
    
    
    hist(x, breaks = bins, col = "#75AADB", border = "black",
         xlab = "Ozone level",
         main = "Histogram of Ozone level")
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
