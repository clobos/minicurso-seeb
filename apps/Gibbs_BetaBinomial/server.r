library("shiny")
library("ggplot2")
library("gridExtra")
library("pander")

shinyServer(function(input, output) {
  
  
   resultados<- reactive({ 
     
     betaBinomial<-function(x,n,alpha, beta){
       num<- gamma(alpha+beta)*gamma(x+alpha)*gamma(n-x+beta)
       den<- gamma(alpha)*gamma(beta)*gamma(alpha+beta+n)
       choose(n,x)*(num/den)
     }
     
     
     x<- matrix(0, input$Iterations, 3)
     y<- matrix(0, input$Iterations, 3)
     
     y[1,]<- c(input$theta.inicial.1,
               input$theta.inicial.2,
               input$theta.inicial.3)
     
     for (i in 1:3){
       for (t in 2:input$Iterations){
         x[t-1,i]<- rbinom(1,size=input$n, prob = y[t-1,i])
         y[t,i]  <- rbeta(1,
                          shape1 = x[t-1,i]+ input$alpha, 
                          shape2 = input$n+input$beta-x[t-1,i])
       }
       
     }
     
     
     x<- x[seq(input$burnin,input$Iterations,input$thin),]
     y<- y[seq(input$burnin,input$Iterations,input$thin),]
     data.frame(x=x,y=y)
     })
   
  
   output$plot1 <- renderPlot({
     x<- resultados()
     names(x)<- NULL
     
     dados1<- data.frame(iter=1:length(x[,1]), 
                          x1=x[,1],x2=x[,2],x3=x[,3])
     traceplot<- ggplot() + 
       geom_line(aes(x=iter,y=x1), dados1, color="black", size=2)+
       geom_line(aes(x=iter,y=x2), dados1, color="red", size=2)+
       geom_line(aes(x=iter,y=x3), dados1,color="blue", size=2)+
       ylab("x")+
       xlab("Iterations")+
       theme(
         plot.title = element_text(color="black", size=24, face="bold"),
         axis.title.x = element_text(color="black", size=24, face="bold"),
         axis.title.y = element_text(color="black", size=24, face="bold"),
         axis.text.x = element_text(size=24, face="bold"),
         axis.text.y = element_text(size=24, face="bold")
       )
     traceplot
   },height = 350)
   
   output$plot2 <- renderPlot({
      x<- resultados()
      names(x)<- NULL
      dados<- data.frame(x1=x[,1],x2=x[,2],x3=x[,3])
      x.valores<- range(dados)
      y.valores<- max(hist(dados$x1,plot=T)$counts,
                      hist(dados$x2,plot=T)$counts,
                      hist(dados$x3,plot=T)$counts)     
     p1 <- with(dados, acf(x1, plot = FALSE))
     p2 <- with(dados, acf(x2, plot = FALSE))
     p3 <- with(dados, acf(x3, plot = FALSE))
     aux1 <- with(p1, data.frame(lag, acf))
     aux2 <- with(p2, data.frame(lag, acf))
     aux3 <- with(p3, data.frame(lag, acf))

     v1<- ggplot(aux1, aes(x=lag,y=acf))+
       geom_bar(stat="identity", position = "identity", width = 0.1,
                color="black")+
       theme(
         plot.title = element_text(color="black", size=24, face="bold"),
         axis.title.x = element_text(color="black", size=24, face="bold"),
         axis.title.y = element_text(color="black", size=24, face="bold"),
         axis.text.x = element_text(size=24, face="bold"),
         axis.text.y = element_text(size=24, face="bold")
       )
     v2<- ggplot(aux2, aes(x=lag,y=acf))+
       geom_bar(stat="identity", position = "identity", width = 0.1,
                color="red")+
       theme(
         plot.title = element_text(color="black", size=24, face="bold"),
         axis.title.x = element_text(color="black", size=24, face="bold"),
         axis.title.y = element_text(color="black", size=24, face="bold"),
         axis.text.x = element_text(size=24, face="bold"),
         axis.text.y = element_text(size=24, face="bold")
       )
     v3<- ggplot(aux3, aes(x=lag,y=acf))+
       geom_bar(stat="identity", position = "identity", width = 0.1,
                color="blue")+
       theme(
         plot.title = element_text(color="black", size=24, face="bold"),
         axis.title.x = element_text(color="black", size=24, face="bold"),
         axis.title.y = element_text(color="black", size=24, face="bold"),
         axis.text.x = element_text(size=24, face="bold"),
         axis.text.y = element_text(size=24, face="bold")
       )

     v4<- ggplot(dados, aes(x1))+
       geom_bar(fill="black", width=0.5)+
       xlim(x.valores)+ylim(0,y.valores)+
       xlab("x")+
       ylab("f(x)")+
       theme(
         plot.title = element_text(color="black", size=24, face="bold"),
         axis.title.x = element_text(color="black", size=24, face="bold"),
         axis.title.y = element_text(color="black", size=24, face="bold"),
         axis.text.x = element_text(size=24, face="bold"),
         axis.text.y = element_text(size=24, face="bold")
       )
     
     v5<- ggplot(dados, aes(x2))+
       geom_bar(fill="red", width=0.5)+
       xlim(x.valores)+ylim(0,y.valores)+
       xlab("x")+
       ylab("f(x)")+
       theme(
         plot.title = element_text(color="black", size=24, face="bold"),
         axis.title.x = element_text(color="black", size=24, face="bold"),
         axis.title.y = element_text(color="black", size=24, face="bold"),
         axis.text.x = element_text(size=24, face="bold"),
         axis.text.y = element_text(size=24, face="bold")
       )
       
     v6<- ggplot(dados, aes(x3))+
        geom_bar(fill="blue", width=0.5)+
        xlim(x.valores)+ylim(0,y.valores)+
        xlab("x")+
        ylab("f(x)")+
       theme(
         plot.title = element_text(color="black", size=24, face="bold"),
         axis.title.x = element_text(color="black", size=24, face="bold"),
         axis.title.y = element_text(color="black", size=24, face="bold"),
         axis.text.x = element_text(size=24, face="bold"),
         axis.text.y = element_text(size=24, face="bold")
       )
       
     grid.arrange(v1,v2,v3, v4,v5,v6, ncol=3)


   },height = 330)

   output$summary1<- renderPrint({
      x<- resultados()
      names(x)<- NULL
      theta<- c(x[,1], x[,2], x[,3])
      cadeias<- sort(rep(1:3,length(x[,1])))
      dados<- data.frame(theta=theta, cadeias=cadeias)
      teste<- with(dados,tapply(theta, cadeias, summary))
      # for (i in 1:3) {
      #   set.caption(sub(".", " ", 
      #                   paste("Summary for Chain",i), 
      #                   fixed = TRUE))
      #   pander(teste[[i]])
      # }
      set.caption(sub(".", " ", 
                      paste("Summary for Sampling"), 
                      fixed = TRUE))
      pander(pander(summary(dados$theta)))
   })
   

})
