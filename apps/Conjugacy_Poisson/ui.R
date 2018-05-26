library(shiny)


fluidPage(title="Poisson distribution",
          navbarPage(h3(strong("Poisson distribution")),
                     
  navbarMenu(h3(strong("Conjugacy")),  
 
  tabPanel(h3(strong("Learning about Theta")),
  sidebarLayout(
    sidebarPanel(
      withMathJax(), 
      h3(strong('Prior Gamma distribution',
                style="color:black")),
      h3(numericInput(inputId="alpha1",label=HTML("&alpha;"),
                   value=2,min=0.1, max=20, step=0.1)),
      h3(numericInput(inputId="beta1",label=HTML("&beta;"),
                   value=3,min=0.1, max=20, step=0.1)),
      br(),
      br(),
      h3(strong('Poisson Likelihood function',style="color:black")),
      h3(numericInput(inputId="lambda",label=HTML("&theta;"),
                   value=1.6,min=0.1, max=10, step=0.1)),
      h3(numericInput(inputId="n1",
                      label="Sample size",
                      value=20,min=1, max=1000, step=1)),
      h3(strong("Reference")),
      h3(code("Andrew Gelman, John B. Carlin, Hal S. Stern, 
                    David B. Dunson, Aki Vehtari, Donald B. Rubin (2013). 
           Bayesian Data Analysis, Third Edition. 
           Chapman & Hall/CRC."))
      
      
    ),
    
    mainPanel(
      plotOutput("distPlot1")
      )
  )
  )#tabPanel("Learning about Theta"
  ),
  
  navbarMenu(h3(strong("Posterior Summary")),
             tabPanel(h3(strong("CI and Percentiles")),
                      sidebarLayout(
                        sidebarPanel(
                          withMathJax(), 
                          h3(strong('Posterior Gamma distribution',
                                    style="color:black")),
                          br(),
                          h3(numericInput(inputId="confint1",label="Credible Interval level",
                                       value=0.95,min=0.05, max=1, step=0.01)),
                          h3(numericInput(inputId="percentil1",label="Percentile",
                                       value=0.50,min=0.01, max=1, step=0.01)),
                          h3(numericInput(inputId="maximo_Poisson",
                                       label="Maximum  value for x axis",
                                       value=4,min=3, max=20, step=1)),
                          br(),
                          br(),
                          h3(strong("Reference")),
                          h3(code("Andrew Gelman, John B. Carlin, Hal S. Stern, 
                                  David B. Dunson, Aki Vehtari, Donald B. Rubin (2013). 
                                  Bayesian Data Analysis, Third Edition. 
                                  Chapman & Hall/CRC."))
                          
                          
                          
                          ),
                        
                        mainPanel(
                        plotOutput("distPlot3")
                          
                        )
                          )
                        )
             
             
  ),
  navbarMenu(h3(strong("Posterior Predictive  Distribution")),
             
             tabPanel(h3(strong("PPD")),
                      sidebarLayout(
                        sidebarPanel(
                          withMathJax(), 
                          h3(strong('Predicting Future Observations'),
                             style="color:black"),
                          br(),
                          h3(sliderInput(inputId="m1.linha1",
                                         label="",
                                       value=1,min=1, max=50, step=1,animate = TRUE)),
                          br(),
                          br(),
                          h3(strong("Reference")),
                          h3(code("Andrew Gelman, John B. Carlin, Hal S. Stern, 
                               David B. Dunson, Aki Vehtari, Donald B. Rubin (2013). 
                               Bayesian Data Analysis, Third Edition. 
                               Chapman & Hall/CRC."))
                          
                          
                          ),
                        
                      mainPanel(
                        plotOutput("distPlot2")
                        
                      )
                      )
             )
             )
  
  
  )
  
)
