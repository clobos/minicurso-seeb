library("shiny")
 
fluidPage(title="MH (independence)",

    sidebarPanel(
      withMathJax(),
      h3(strong("Sampling from Beta distribution (target) 
                based on the proposal gamma distribution",style="color:black")),
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
      h3(strong("Initial values",style="color:black")),
      h3(numericInput(inputId="theta.inicial.1", 
                   label=HTML("&theta;[1]"), 
                   value=0.1, min = -5, max = 5,
                   step=0.5)),
      h3(numericInput(inputId="theta.inicial.2",
                   label=HTML("&theta;[2]"), 
                   value=0.5, min = -5, max = 5,
                   step=0.5)),
      h3(numericInput(inputId="theta.inicial.3",
                   label=HTML("&theta;[3]"), 
                   value=0.1, min = -5, max = 5,
                   step=0.5)),
      h3(strong("Proposal Gamma distribution",style="color:black")),
      h3(numericInput(inputId="shape",
                   label="Shape parameter", 
                   value=0.3, min = 0.1, max = 4,
                   step=0.1)),
      h3(numericInput(inputId="rate", 
                   label="Rate parameter", 
                   value=0.03, min = 0.1, max = 4,
                   step=0.1)),
      h3(strong("Target Beta distribution",style="color:black")),
      h3(numericInput(inputId="shape1",
                   label="Shape1 parameter", 
                   value=2, min = 0.5, max = 10,
                   step=0.5)),
      h3(numericInput(inputId="shape2", 
                   label="Shape2 parameter", 
                   value=5, min = 0.5, max = 10,
                   step=0.5)),
      h3(strong("Reference")),
      h3(code("Siddhartha Chib & Edward Greenberg (2012). 
               Understanding the Metropolis-Hastings 
               Algorithm, The American Statistician, 49:4, 
               327-335."))
      
        ),

    mainPanel(
       h1(strong("Metropolis Hasting algorithm (independence sampling)"), align="center"),
       plotOutput("plot1"),
       plotOutput("plot2"),
       h3(strong("Summary of Chains",style="color:black")),
       verbatimTextOutput("summary1")
       )
    
    )
