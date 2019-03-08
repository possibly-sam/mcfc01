#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#
# Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0) 
# Ramin Takin
# Phillip Abbott
# Liza French
# Annette Z
# Anne-Marie
# Ken Allan

library(shiny)
library(dplyr)
library(ggplot2)
library(MASS) # for mvrnorm

cat(getwd())

source("./mcfc01.R")
source("~/Documents/stt/mcfc01b/mcfc03.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Monty Carlo's Flying Circus"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("i0", "Risk Type", c("Target", "Residual")),
        numericInput("repeats", "Repeats", 99),
        numericInput("samplesize", "SampleSize", 9),
        selectInput("bu0", "Business Unit", c("All", mc $ get_business_units() )),
        numericInput("cov0", "Covariance", -0.01)
        # textInput("rn0", "Risk Name", "R12286"), 
        #selectInput("rt0", "Risk Type", mc $ get_risk_type ()), 
        #selectInput("ed0", "Extract Date", mc $ get_extract_date() ),
        #selectInput("status0", "Status", mc $ get_status () ), 
        #selectInput("de0", "Design Element", mc $ get_design_element() ), 
        #selectInput("ca0", "Contract", mc $ get_contractual ()), 
        #selectInput("mit0", "Mitigation", mc $ get_mitigation ()), 
        #selectInput("cat0", "All", mc $ get_category () ) # c("Environmental", "Health & Safety", "Reputation", "Quality", "Availability"))
        
        
      ),
        
        
        

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         textOutput("info"),
         textOutput("info2"),
         textOutput("info3"),
         tableOutput("tbl_info")
      )
   )
)

g_result <- 1:3

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      # bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      # hist(x, breaks = bins, col = 'darkgray', border = 'white')
       #mc $ manytimes(Risk, input$repeats, input$samplesize) %>% hist( breaks=29, main="Your Histogram")
      Risk_ <- Risk
      
      if (input$bu0 != 'All') {
        
        Risk_ <- Risk_[Risk$Business_Unit__c==input$bu0,]
        
      }
      # cat(input$cov0)
        
       reps <- min(input$samplesize, Risk_ %>% nrow() )
       Cost <- mc $ manytimes(Risk_, input$repeats, reps, input$cov0) 
       Cost <- (Risk_ %>% nrow()) * Cost / (1000000*reps)
       g_result <<- Cost
       b0 <- (g_result %>% length() )/2
       # qplot(Cost, geom="histogram", main="Cost k GBP") 
       mc$make_histogram(Cost)
       
         
   })
   
   output$info <- renderText( {
      #g_result %>% summary() 
     "\n\n\n\n\n\n\n\n."
   } )

   
   output$info2 <- renderText( {
     #g_result %>% summary() 
     "\n\n\n\n\n\n\n\n."
   } )
   
   output$info3 <- renderText( {
     #g_result %>% summary() 
     "\n\n\n\n\n\n\n\n."
   } )
   
   
  output$tbl_info <- renderTable({
    input$repeats %>% force() 
    input$samplesize %>% force()
    input$bu0 %>% force()
    input$cov0 %>% force()
    
    
    q0 <- g_result %>% summary()
    # data.frame(min=q0[['Min.']], max=q0[['Max.']] )
    mc $ some_nice_limits (g_result)
  }     , spacing="l",size="48")
  
} # end server

# Run the application 
shinyApp(ui = ui, server = server)

