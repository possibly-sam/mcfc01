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


library(tidyverse)
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
      # column(width=4,             numericInput("cov0", "Covariance", -0.01) )
      column(width=3, actionButton("btn_refresh", "Refresh")    ) , 
      column(width=1, actionButton("btn_collapse", "..")    )  
    ), # end row 2

    
    fluidRow(
      column(width=6,  plotOutput("distPlot")),
      column(width=6,  plotOutput("distPlot2") )
      ),  # end row 3
    

    fluidRow(
      column(width=12,
             verbatimTextOutput("some_nice_information")
      ) ), # end row 3
    
    fluidRow(
      column(width=12,
             tableOutput("tbl_info")
      )) # end row 4
    

) # end fluid page






# Define server logic required to draw a histogram
server <- function(input, output) {
  
  g_filtered_table <-  NA
  g_collapsed_table <- NA
  g_scale <- 1000000
  g_costs <- 1:10
  
  g_collapsed_table_ <- reactive({

    input$btn_collapse %>% force()
    
    g_filtered_table <<- a_nice_table $ get_filtered_data( input$bu0, input$ed0, input$rt0 ) %>% mc $CMCTable()
    g_filtered_table $ get_collapsed_data ()
    
  })

  
  g_costs_ <- reactive( {
    input$btn_refresh %>% force()
    
    mc_collapsed_table <- g_collapsed_table_() %>%  mc $ CMCCollapsedTable ()
    
    # input$btn_collapse <- input$btn_collapse + 1
    g_filtered_table$many_collapses( input$repeats, input$samplesize) / g_scale
  }  )
  
  
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- 1:100
      bins <-7
      
      input$btn_refresh %>% force()

      
      # draw the histogram with the specified number of bins
      
      g_filtered_table <<- a_nice_table $ get_filtered_data( input$bu0, input$ed0, input$rt0 ) %>% mc $CMCTable()
      g_collapsed_table <<- g_filtered_table $ get_collapsed_data ()
      
      mc_collapsed_table <- g_collapsed_table %>%  mc $ CMCCollapsedTable ()
      
      # input$btn_collapse <- input$btn_collapse + 1
      g_costs <<- g_filtered_table$many_collapses( input$repeats, input$samplesize) / g_scale
      
      # TODO:  pimp the histogram up to ggplot2
      # TODO:  what we are doing here is creating a plot (histogram/boxplot)
      #        with some auxilliary information (instance, CI, etc..)
      #        this should be refactored into a function that takes 
      #        the costs and the accompaying information and then ca
      #        display it in different ways
      g_costs_() %>% hist( main="Costs (M)", breaks=20)
      
      mc_collapsed_table <- g_collapsed_table_() %>%  mc $ CMCCollapsedTable ()
      
      
      # cat (mc_collapsed_table$mu)
      abline(v=mc_collapsed_table$instance/ g_scale,col='red')
      abline(v=mc_collapsed_table$mu/ g_scale,col='blue')
      abline(v=(mc_collapsed_table$mu-mc_collapsed_table$sigma)/ g_scale,col='green')
      abline(v=(mc_collapsed_table$mu+mc_collapsed_table$sigma)/ g_scale,col='green')
      
      
   })  # end distPlot
   
   
   # TODO:  these plots all refer to different instances of a collapsed risk function
   #        they need to be made to refer to the same;
   output$distPlot2 <- renderPlot({
     
     input$btn_refresh %>% force()
     the_costs <- g_filtered_table$many_collapses( input$repeats, input$samplesize) / g_scale
     
     (g_costs_()+1) %>% log()%>% boxplot(main="log cost (M)")

   })  # end distPlot2
   
   # TODO:  this is one instance of a collapsed risk function.  make a red dot show up on the histogram where it hits?
   output$tbl_info <- renderTable(  {
     input$btn_collapse %>% force()
     
     
     #the_filtered_table <- a_nice_table $ get_filtered_data( input$bu0, input$ed0, input$rt0 ) %>% mc $ CMCTable()
     #the_collapsed_table <- the_filtered_table $ get_collapsed_data () 
     
     g_collapsed_table <- g_collapsed_table_()

     g_collapsed_table[ order(g_collapsed_table$mu, decreasing = TRUE),]
      
    }    ) # end tbl_info
   
   # TODO:  format information about the collapsed risk function nicely in HTML;
   output$some_nice_information <- renderPrint( {

      #printme <- paste( "mu:", g_costs_() %>% mean()  )
      #tags$html(printme) %>% as.character()
     
     g_costs_() %>% quantile()
     
     
     }) # end some_nice_information
}

# Run the application 
shinyApp(ui = ui, server = server)

