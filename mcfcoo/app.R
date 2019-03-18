#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# OOOOO OOOOO
# O   O O   O
# O   O O   O
# O   O O   O
# OOOOO OOOOO



library(shiny)

source("mcfcoo01.R")

get_a_nice_table <- function(the_file_name="./risk_KA_11.csv") {
  
  q0 <-   mc$get_table(the_file_name) %>% mc$CMCTable()
  
   q0 $clean_table() %>% mc$CMCTable()
  
  
}

a_nice_table <- get_a_nice_table()

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  
  
    
    fluidRow(  titlePanel("Monty Carlo's Flying Circus")), # end  row 0
    
    fluidRow(
      column(width=4,
             selectInput("rt0", "Risk Type", a_nice_table $ GetRiskMitigation())),
      column(width=4,
             selectInput("bu0", "Business Unit", c("All", a_nice_table $ GetBusinessUnits() ))),
      column(width=4,
             selectInput("ed0", "Extract Date", a_nice_table $ GetExtractDates() ))
    ), # end row 1
    fluidRow(
      column(width=4,
             numericInput("repeats", "Repeats", 99)),
      column(width=4,
             numericInput("samplesize", "SampleSize", 9)),
      column(width=4,
             numericInput("cov0", "Covariance", -0.01) )
    ), # end row 2

    
    fluidRow(
      column(width=12,
             plotOutput("distPlot")
      ) ), # end row 3
    fluidRow(
      column(width=12,
             textOutput("some_nice_information")
      ) ), # end row 3
    
    fluidRow(
      column(width=12,
             tableOutput("tbl_info")
      )) # end row 4
    

) # end fluid page

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- 1:100
      bins <-7
      
      # draw the histogram with the specified number of bins
      
      the_mc_table <- a_nice_table$ get_filtered_data( input$bu0, input$ed0, input$rt0 ) %>% mc $CMCTable()
      
      hist(the_mc_table$many_collapses( input$repeats, input$samplesize))
   })  # end distPlot
   
   output$tbl_info <- renderTable(  {
     the_filtered_table <- a_nice_table$ get_filtered_data( input$bu0, input$ed0, input$rt0 ) %>% mc $ CMCTable()
     the_collapsed_table <- the_filtered_table $ get_collapsed_data () 

     the_collapsed_table[ order(the_collapsed_table$mu, decreasing = TRUE),]
      
    }    ) # end tbl_info
   
   output$some_nice_information <- renderText( {
     the_mc_table <- a_nice_table$ get_filtered_data( input$bu0, input$ed0, input$rt0 ) %>% mc $CMCTable()
     
      the_mc_table$my_table %>% nrow() %>% as.character()
     
      
     
     
     }) # end some_nice_information
}

# Run the application 
shinyApp(ui = ui, server = server)

