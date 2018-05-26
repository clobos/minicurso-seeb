library("shiny")
 
fluidPage(title="Gibbs Sampling",
          
    sidebarPanel(
      withMathJax(),
      h3(strong("Sampling from X~Beta Binomial
                      based on X|Y~Bin(n,Y) and 
                      Y|X~Beta(x+alpha,n-x+beta)",
                      style="color:black")),
      h3(strong("Behaviour of the MCMC ",style="color:red")),
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
      h3(strong("Initial values",style="color:red")),
      h3(numericInput(inputId="theta.inicial.1", 
                   label=HTML("&theta;[1]"), 
                   value=0.1, min = 0, max = 1,
                   step=0.1)),
      h3(numericInput(inputId="theta.inicial.2",
                   label=HTML("&theta;[2]"), 
                   value=0.5, min = 0, max = 1,
                   step=0.1)),
      h3(numericInput(inputId="theta.inicial.3",
                   label=HTML("&theta;[3]"), 
                   value=0.9, min = 0, max = 1,
                   step=0.1)),
      br(),
      br(),
      h3(strong("Parameter of Beta Binomial distribution",style="color:red")),
      h3(numericInput(inputId="n",
                   label="n", 
                   value=16, min = 1, max = 100,
                   step=1)),
      h3(numericInput(inputId="alpha", 
                   label=HTML("&alpha;"), 
                   value=2, min = 0.5, max = 10,
                   step=0.5)),
      h3(numericInput(inputId="beta", 
                   label=HTML("&beta;"), 
                   value=4, min = 0.5, max = 10,
                   step=0.5)),
      br(),
      br(),
      h3(strong("Reference")),
      h3(code("George Casella & Edward I. 
               George (2012). Explaining the Gibbs Sampler, 
               The American Statistician, 46:3, 
               167-174."))
      
        ),

    mainPanel(
      h1(strong("Gibbs Sampling for Beta Binomial distribution"), align="center"),
      plotOutput("plot1"),
      plotOutput("plot2"),
      h3(strong("Summary of Chains",style="color:red")),
      verbatimTextOutput("summary1")
       )
)

