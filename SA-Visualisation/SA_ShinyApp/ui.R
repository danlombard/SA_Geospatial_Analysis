library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinycssloaders)
library(markdown)
source("global.R")

shinyUI(fluidPage(
  # line below to fix bug in ggiraph when deploying app
  tags$head( tags$style(type = "text/css", "text {font-family: sans-serif}")),
  theme = shinytheme("yeti"),
  titlePanel("Visualising South Africa's Crimes by Province"),
  
  sidebarLayout(
    sidebarPanel(
      br(),
      pickerInput(
        inputId = "variable",
        label = "Choose a variable to map", 
        choices = c( "Total GDP",                                              
                     "GDP per Capita",                                         
                     "GDP per Capita (USD)",                                      
                     "Population",                                              
                     "Murder",                                                  
                     "Sexual offences",                                        
                     "Attempted murder",                                       
                     "Assault with the intent to inflict grievous bodily harm",
                     "Common assault",                                          
                     "Common robbery",                                         
                     "Robbery with aggravating circumstances",                  
                     "Arson",                                                  
                     "Malicious damage to property",                            
                     "Burglary at nonresidential premises",                    
                     "Burglary at residential premises",                        
                     "Theft of motor vehicle and motorcycle",                  
                     "Theft out of or from motor vehicle",                      
                     "Stock theft",                                            
                     "Illegal possession of firearms and ammunition",           
                     "Drug related crime",                                     
                     "Driving under the influence of alcohol or drugs",         
                     "Sexual offences detected as a result of police action",  
                     "All theft not mentioned elsewhere",                       
                     "Commercial crime",                                       
                     "Shoplifting",                                             
                     "Community reported serious crimes",                     
                     "Carjacking",                                              
                     "Truck hijacking",                                        
                     "Robbery at residential premises",                         
                     "Robbery at non residential premises",                    
                     "Bank robbery",                                            
                     "Robbery of cash in transit",                             
                     "TRIO Crimes",                                             
                     "Rape",                                                   
                     "Sexual assault",                                          
                     "Attempted sexual offences",                              
                     "Contact sexual offences"),
        selected = "Population",
        width = '210px'
      ),
      pickerInput(
        inputId = "representation",
        label = "Choose a visual representation", 
        choices = c("Geographic", "Continuous Cartogram", 
                    "Non-continuous Cartogram", "Dorling Cartogram",
                    "Hexbin"),
        selected = "Geographic",
        width = '210px'
      ),
      pickerInput(
        inputId = "color_scheme",
        label = "Choose a colour scheme", 
        choices = c("magma", "inferno","plasma", "viridis", "cividis"),
        selected = "viridis",
        multiple = FALSE,
        width = '210px'
      ),
      br(),
      h6(em("N.B.: Please allow longer compute time for cartograms."))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Choropleth", 
          withSpinner(ggiraphOutput("mymap", height = "750px", width = "700px"))
        ),
        tabPanel(
          "Dotplot", 
          ggiraphOutput("dotplot", height = "650px")
        )
      )
    )
  )
))


