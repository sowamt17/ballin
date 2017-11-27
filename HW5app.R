################################################################################
# MSBA Data Visualization5
# November 2017
#
# HW5: Analysis with Shiny
#
# Your Name:
################################################################################

library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(ggvis)
setwd("C:\\Users\\tsowa\\Desktop\\6035\\")
gfm <- read.csv("HW5_GoFundMe.csv")
se <- function(x){ sqrt(var(x, na.rm=T)/length(x))}

################################################################################
# UI function
#renaming columns to fit submission formatting

colnames(gfm)[colnames(gfm)=="AmountRaised"]<-"raised"
colnames(gfm)[colnames(gfm)=="Category"]<- "category" 




#subsetting numeric variables from gfm dataset
namesolo<- names(gfm[sapply(gfm, is.numeric)]) 

#subsetting character variables from gfm dataset

charactersolo<- names(gfm[sapply(gfm, is.factor)])



ui <- fluidPage(
  titlePanel("HW 5: Go Fund Me Insights"),
  tabsetPanel(
    tabPanel("Explore",
             sidebarLayout(
               sidebarPanel(
                
                 # Question 1a: create the user inputs here.
                 selectInput(inputId = "xvar", 
                             label="Categorical Variable (X-axis)",
                             choices=charactersolo, 
                             selected="category"),
                 
                 selectInput(inputId = "yvar",
                             label="Numeric Variable (Y-axis)",
                             choices=namesolo, selected = "raised" 
                             ), actionButton('go', 'Go')
               ),
           
               
               mainPanel(
                 plotOutput("distPlot")
               ))
    ), tabPanel("Insight",
                ggvisOutput("plot"),
                uiOutput("plot_ui"),
                textOutput("insights"))
  )
)
################################################################################
# Server function: 
# Server logic required to make the plots
server <- function(input, output) {
  restrictedData <- eventReactive(input$go,{
    # This verifies that the user inputs are the right class.
    if (!class(gfm[,input$xvar])%in% c("factor", "character")) { return(NULL) }
    if (!class(gfm[,input$yvar])%in% c("numeric", "integer")) { return(NULL) }
    cbind.data.frame(x=gfm[,input$xvar],y=gfm[,input$yvar])
  })
  # Question 1b: Create a reactive dataset for the plot
  
   plotData <- reactive({
   restrictedData () %>% 
      group_by(x) %>% 
      summarise(avgy = mean(y, na.rm=TRUE), sey = se(y)) %>% 
      arrange(desc(avgy, na.rm=TRUE)) #%>% 
      head(n=10)
     
  })
   

   
    # end Question 1b  
    observeEvent(input$go, {
      output$distPlot <- renderPlot({
        if (is.null(plotData())) {return(NULL)}
        # Question 1c: Create the categorical plot here.
        ggplot() + geom_bar(data=plotData(), mapping=aes(x=reorder(x,-avgy), y=avgy),
                            stat="identity")+
          geom_errorbar()+
          ggtitle("Exploring the data")+
          theme(plot.title = element_text(hjust = .5))
        
      })
    }) }

    # Q2a: Insert the insight plot here.
  server<- function(input, output) {
    reactive({
      ihateshiny<- gfm %>% filter(Backers<1000)
      
      ihateshiny %>% 
        ggvis(x= ~Backers, y= input_select(names(ihateshiny)[6:11],
                                          selected="MeanHappiness",
                                          label="Choose", map=as.name),
              fill:="red") %>% 
                layer_points() %>% 
                add_axis("y", title="Choose Variable", title_offset=40) %>% 
                add_axis("x", title="Backers")
               
              
    }) %>% bind_shiny("plot", "plot_ui")
    
  
  output$insights <- renderText({
    
    "Th"
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
