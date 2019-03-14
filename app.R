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
  title="Monty Carlo's Flying Circus",
   
  fluidRow(  titlePanel("Monty Carlo's Flying Circus")), # end  row 0
  
  fluidRow(
    column(width=4,
        selectInput("i0", "Risk Type", c("Target", "Residual", "Inherent"))),
    column(width=4,
        selectInput("bu0", "Business Unit", c("All", mc $ get_business_units() ))),
    column(width=4,
        selectInput("ed0", "Extract Date", mc $ get_extract_date() ))
  ), # end row 1
  fluidRow(
    column(width=4,
        numericInput("repeats", "Repeats", 99)),
    column(width=4,
        numericInput("samplesize", "SampleSize", 9)),
    column(width=4,
        numericInput("cov0", "Covariance", -0.01) )
  ), # end row 2
        # textInput("rn0", "Risk Name", "R12286"), 
        #selectInput("rt0", "Risk Type", mc $ get_risk_type ()), 
        #selectInput("ed0", "Extract Date", mc $ get_extract_date() ),
        #selectInput("status0", "Status", mc $ get_status () ), 
        #selectInput("de0", "Design Element", mc $ get_design_element() ), 
        #selectInput("ca0", "Contract", mc $ get_contractual ()), 
        #selectInput("mit0", "Mitigation", mc $ get_mitigation ()), 
        #selectInput("cat0", "All", mc $ get_category () ) # c("Environmental", "Health & Safety", "Reputation", "Quality", "Availability"))
        
        
      fluidRow(
        column(width=12,
               plotOutput("distPlot")
        ) ), # end row 3
      fluidRow(
        column(width=12,
          tableOutput("tbl_info")
      )) # end row 4
        
   ) # end fluid page


g_result <- 1:3

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
       # mc $ manytimes(Risk, input$repeats, input$samplesize) %>% hist( breaks=29, main="Your Histogram")
     
     # filter based on the options & clean up the data appropriately
      Risk_ <- Risk %>% mc $ some_good_data( input$bu0, input$ed0, input$i0)
      
      # if (input$bu0 != 'All') {        Risk_ <- Risk_[Risk$Business_Unit__c==input$bu0,]      }
      # cat(input$cov0)
        
       reps <- min(input$samplesize, Risk_ %>% nrow() )
       Cost <- mc $ manytimes(Risk_, input$repeats, reps, input$cov0) 
       Cost <- (Risk_ %>% nrow()) * Cost / (1000000*reps)
       g_result <<- Cost
       b0 <- (g_result %>% length() )/2
       # qplot(Cost, geom="histogram", main="Cost k GBP") 
       mc$make_histogram_discrete(Cost, Risk_)
       
         
   })
   
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

