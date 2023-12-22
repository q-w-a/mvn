#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(MASS)
library(tidyverse)
source(here::here("helpers.R"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Multivariate Normal Distribution"),
    withMathJax(),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
     
        sidebarPanel(
          numericInput("mu_x",
                       label = "$$\\mu_X$$",
                       value = 0),
       
          numericInput("mu_y",
                       label = "$$\\mu_Y$$",
                       value = 0),
        
    
          numericInput("var_x",
                       label = "$$\\text{Var}(X)$$",
                       value = 1,
                       min=0),
          numericInput("var_y",
                       label = "$$\\text{Var}(Y)$$",
                       value = 1,
                       min=0),
          uiOutput("ui"),
          # numericInput("cov_xy",
          #              label = "$$\\text{Cov}(X,Y)$$",
          #              value = 0),
          actionButton("goButton", "Submit", class = "btn-success")
          ),

        # Show a plot of the generated distribution
        mainPanel(
          withMathJax(),
          uiOutput("dist"),
          fluidRow(
            splitLayout(cellWidths = c("50%", "50%"), 
                        plotOutput("distPlot"), plotOutput("contoursPlot"))
          )

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$ui <- renderUI( {
    # tagList(
    sliderInput("cov_xy", label = withMathJax("$$\\text{Cov}(X,Y)$$"),
                value = 0, 
                min = -1*round(sqrt(input$var_x * input$var_y ),2), 
                max = 1*round(sqrt(input$var_x * input$var_y),2), step = .5)
    #)
  })
  
  
  
  
  plt_list <- reactive(
                       {
                         input$goButton

                         validate(
                           need(!is.null(input$cov_xy), "") 
                         )
                         
                         
                         isolate( { 

    sim <- MASS::mvrnorm(n = 10000, mu =c(input$mu_x,input$mu_y),
                       Sigma =  matrix(data=c(input$var_x, input$cov_xy,
                                              input$cov_xy,input$var_y),
                                       byrow=TRUE,
                                       nrow=2)) 
    bivn.kde <- MASS::kde2d(sim[,1], sim[,2], n = 100,
                            h = c( bandwidth.nrd(sim[,1]) + .4,
                                   bandwidth.nrd(sim[,2]) + .4  ) )
    
    
    p1 <- plot3D::persp3D(z=bivn.kde$z, x=bivn.kde$x,y=bivn.kde$y, colvar=NULL,
                    theta=140, phi=25, zlab="Density",
                    cex.lab=1.5,cex.axis=1,ticktype="detailed", shade=.1, 
                    contour=TRUE, lighting="diffuse",opaque.top=TRUE,
                    border="black",lwd=0.08,
                    col="gray") 
    p2 <-   tibble(x=sim[,1],y=sim[,2]) %>%
      ggplot(aes(x=x,y=y)) +
      geom_point(alpha = .05) +
      geom_density_2d() +
      geom_point(x= input$mu_x,y = input$mu_y, color="red", size=1.2) +
      theme_c()
    
    list(plot1= p1, plot2= p2) } ) })



    
    output$dist <- renderUI({
      
      validate(
        need(!is.null(input$cov_xy), "Loading") 
      )
      
      input$goButton
      isolate (  { 
     withMathJax( paste0("$$N_2\\left(\\begin{pmatrix}", input$mu_x, "\\\\", input$mu_y, 
              "\\end{pmatrix}, \\begin{pmatrix}", input$var_x, "&", input$cov_xy,
             "\\\\", input$cov_xy, "&", input$var_y,
             "\\end{pmatrix}", "\\right)", "$$")) } )  }) 
    
    
    
    
    output$distPlot <- renderPlot(
      { plt_list()[["plot1"]] })
    output$contoursPlot <- renderPlot({plt_list()[["plot2"]]})
    
      
  
}

# Run the application 
shinyApp(ui = ui, server = server)
