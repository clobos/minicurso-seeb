library(shiny)

fluidPage(title="Binomial distribution",
    navbarPage(h3(strong("Binomial distribution")),

  navbarMenu(h3(strong("Conjugacy")),  
 
  tabPanel(h3(strong("Learning about Theta")),
  sidebarLayout(
    sidebarPanel(
      withMathJax(), 
      h3(strong('Prior Beta distribution'),
         style="color:black"),
      h3(numericInput(inputId="alpha",label=HTML("&alpha;"),
                   value=2,min=0.1, max=20, step=0.1)),
      h3(numericInput(inputId="beta",label=HTML("&beta;"),
                   value=5,min=0.1, max=20, step=0.1)),
      h3(strong('Binomial Likelihood function',style="color:black")),
      h3(numericInput(inputId="m",
                      label="Number of trials",
                      value=3,min=1, max=100, step=1)),
      h3(numericInput(inputId="prob",label=HTML("&theta;"),
                   value=0.8,min=0, max=1, step=0.1)),
      h3(numericInput(inputId="n",
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
                          h3(strong('Posterior Beta distribution',
                                    style="color:black")),
                          h3(numericInput(inputId="confint",label="Credible Interval level",
                                          value=0.95,min=0.01, max=1, step=0.01)),
                          h3(numericInput(inputId="percentil",label="Percentile",
                                          value=0.50,min=0.01, max=1, step=0.01)),
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
                          h3(strong('Posterior Preditive distribution'),
                             style="color:black"),
                          br(),
                          h3(sliderInput(inputId="m.linha",
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
#)


  
