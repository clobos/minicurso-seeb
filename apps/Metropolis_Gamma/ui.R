library("shiny")
 
fluidPage(title="Metropolis (dependence)",

    sidebarPanel(
      withMathJax(),
      h3(strong("Sampling from  Gamma distribution (target)
                with shape and rate parameter
                based on the proposal normal distribution
                with tuning parameter (sigma)",style="color:black")),
      br(),
      br(),
      h3(strong("Behaviour of the MCMC",style="color:black")),
      h3(sliderInput("Iterations",
                  "Iterations",
                  value = 100,min=100,max=100000, 
                  step=1000,animate=TRUE)),
      h3(sliderInput(inputId="burnin", 
                  label="Burnin", 
                  value=1, min = 1, max = 5000,
                  step=100,animate=TRUE)),
      h3(sliderInput(inputId="thin", 
                  label="Thin", 
                  value=1, min = 1, max = 1000,
                  step=10,animate=TRUE)),
      br(),
      br(),
      h3(strong("Initial Values",style="color:black")),
      h3(numericInput(inputId="theta.inicial.1", 
                   label=HTML("&theta;[1]"), 
                   value=1, min = -5, max = 5,
                   step=0.5)),
      h3(numericInput(inputId="theta.inicial.2", 
                   label=HTML("&theta;[2]"), 
                   value=5, min = -5, max = 5,
                   step=0.5)),
      h3(numericInput(inputId="theta.inicial.3", 
                   label=HTML("&theta;[3]"), 
                   value=12, min = -5, max = 5,
                   step=0.5)),
      h3(numericInput(inputId="tuning.parameter", 
                   label=HTML("&sigma;"), 
                   value=0.5, min = 0.5, max = 10,
                   step=0.5)),
      br(),
      br(),
      h3(strong("Target Gamma distribution",style="color:black")),
      h3(numericInput(inputId="shape", 
                   label="Shape parameter", 
                   value=2, min = 0.1, max = 10,
                   step=0.5)),
      h3(numericInput(inputId="rate", 
                   label="Rate parameter", 
                   value=5, min = 0.1, max = 10,
                   step=0.5)),
      br(),
      br(),
      h3(strong("Reference")),
      h3(code("Siddhartha Chib & Edward Greenberg (2012). 
              Understanding the Metropolis-Hastings 
              Algorithm, The American Statistician, 49:4, 
              327-335."))
      
        ),

    mainPanel(
      h1(strong("Metropolis algorithm (dependence sampling)"), align="center"),
      plotOutput("plot1"),
      plotOutput("plot2"),
      h3(strong("Summary of Chains",style="color:black")),
      verbatimTextOutput("summary1")
          
      )
    )
