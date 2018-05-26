library(shiny)
library(ggplot2)
library(gridExtra)

shinyServer(function(input, output) {
  
  Veross<- function(dados,theta,m){
    n<- length(dados)
    sy <- sum(dados)
    L.theta<- theta^sum(dados)*(1-theta)^(m*n-sum(dados))
    #L.theta<- L.theta/max(L.theta)
    L.theta <- dbeta(theta, sy + 1, m * n - sy + 1)
    return(L.theta)
  }
  
  Priori<- function(theta, alpha, beta){ 
    aux<- dbeta(theta, shape1=alpha, shape2=beta)  
    aux#/max(aux)
  }
  
  Posterior<- function(dados, theta, alpha, beta, m){
    n<- length(dados)
    aux<- dbeta(theta,shape1=alpha + sum(dados), 
                shape2 = beta + m*n -sum(dados))
    aux#/max(aux)
  }
  
  preditiva<- function(alpha, beta, dados, m, m.linha){
    yt<- 0:m.linha
    n<- length(dados)
    alpha.linha<- alpha + sum(dados) 
    beta.linha<- beta + n*m -sum(dados)
    aux1<- choose(m.linha,yt)
    aux3<- beta(alpha.linha, beta.linha)
    aux2<- beta(yt+alpha.linha,beta.linha-yt+m.linha)
    (aux1*aux2)/aux3      
  }
  
  
  
  
  
  output$distPlot1 <- renderPlot({ 
    set.seed(123)
    dados<- rbinom(n=input$n, size=input$m, prob=input$prob)
    theta<- seq(0.001,0.999,length.out=1000)
    
    aux1<- data.frame("Distribution" = "Likelihood",
                      x=theta,y=Veross(dados,theta,#=input$prob,
                                       m=input$m))
    aux2<- data.frame("Distribution" = "Prior",
                      x=theta, y=Priori(theta,
                                        alpha=input$alpha, 
                                        beta=input$beta))
    aux3<- data.frame("Distribution" = "Posterior",
                      x=theta, y=Posterior(dados, theta, 
                                           alpha=input$alpha, 
                                           beta=input$beta, 
                                           m=input$m))
    
    dados_Binomial <- reactive({
      data.frame(rbind(aux1, aux2, aux3))
    })  
    
    dados_preditiva<-  reactive({
      data.frame(x=as.factor(0:input$m.linha),
                 y=preditiva(alpha=input$alpha, beta=input$beta, 
                             dados, m=input$m,
                             m.linha = input$m.linha))
    })
    
    
    LI<- qbeta( (1-input$confint)/2,input$alpha + sum(dados), 
                input$beta + input$n*input$m -sum(dados) )
    LS<- qbeta((1+input$confint)/2,input$alpha + sum(dados), 
               input$beta + input$n*input$m - sum(dados))
    Percentil<- qbeta(input$percentil,input$alpha + sum(dados), 
                      input$beta + input$n*input$m - sum(dados))
    
    
    dados_Posteriori<-  reactive({ 
      data.frame("Distribution" = "Posterior",
                 x=theta, y=Posterior(dados, theta, 
                                      alpha=input$alpha, 
                                      beta=input$beta, 
                                      m=input$m), LI=LI, LS=LS,Percentil=Percentil)
    })
    
    v1<- ggplot(dados_Binomial(),
                aes(x=x,y=y,color=Distribution)) +
      scale_color_manual(values=c("blue", "black", "red"))+ 
      #likelihood, prior, posterior
      geom_line(size=3,linetype="solid") +
      xlab(expression(theta))+
      ylab("")+
      annotate("text", x = 0.90, y = max(aux1$y), 
               label = "Likelihood", color="blue", size=12)+
      annotate("text", x = 0.15, y = max(aux3$y)-2, 
               label = "Prior distribution", color="black", size=12)+
      annotate("text", x = 0.25, y = max(aux3$y), 
               label = "Posterior distribution", color="red", size=12)+
      labs( title= "")+#"Conjugate families of distributions"
      theme(legend.position="none")+
      theme(
        plot.title = element_text(color="black", size=24, face="bold"),
        axis.title.x = element_text(color="black", size=24, face="bold"),
        axis.title.y = element_text(color="black", size=24, face="bold"),
        axis.text.x = element_text(size=24, face="bold"),
        axis.text.y = element_text(size=24, face="bold")
      )
    v1
}, height = 710)
  
    output$distPlot2<- renderPlot({
      
      set.seed(123)
      dados<- rbinom(n=input$n, size=input$m, prob=input$prob)
      theta<- seq(0.001,0.999,length.out=1000)
      
      aux1<- data.frame("Distribution" = "Likelihood",
                        x=theta,y=Veross(dados,theta,#=input$prob
                                         m=input$m))
      aux2<- data.frame("Distribution" = "Prior",
                        x=theta, y=Priori(theta,
                                          alpha=input$alpha, 
                                          beta=input$beta))
      aux3<- data.frame("Distribution" = "Posterior",
                        x=theta, y=Posterior(dados, theta, 
                                             alpha=input$alpha, 
                                             beta=input$beta, 
                                             m=input$m))
      
      dados_Binomial <- reactive({
        data.frame(rbind(aux1, aux2, aux3))
      })  
      
      dados_preditiva<-  reactive({
        data.frame(x=as.factor(0:input$m.linha),
                   y=preditiva(alpha=input$alpha, beta=input$beta, 
                               dados, m=input$m,
                               m.linha = input$m.linha))
      })
      
      
      LI<- qbeta( (1-input$confint)/2,input$alpha + sum(dados), 
                  input$beta + input$n*input$m -sum(dados) )
      LS<- qbeta((1+input$confint)/2,input$alpha + sum(dados), 
                 input$beta + input$n*input$m - sum(dados))
      Percentil<- qbeta(input$percentil,input$alpha + sum(dados), 
                        input$beta + input$n*input$m - sum(dados))
      
      
      dados_Posteriori<-  reactive({ 
        data.frame("Distribution" = "Posterior",
                   x=theta, y=Posterior(dados, theta, 
                                        alpha=input$alpha, 
                                        beta=input$beta, 
                                        m=input$m), LI=LI, LS=LS,
                   Percentil=Percentil)
      })
      
     v2<- ggplot(data=dados_preditiva(), aes(x, y)) +
      geom_bar(stat="identity",width=0.5, fill = "black")+
      xlab("Future observations")+
      ylab(expression(pi(y[t]/data)))+
      ylim(0,1)+
      labs(title ="")+#Posterior Predictive Distribution
      theme(
        plot.title = element_text(color="black", size=24, face="bold"),
        axis.title.x = element_text(color="black", size=24, face="bold"),
        axis.title.y = element_text(color="black", size=24, face="bold"),
        axis.text.x = element_text(size=24, face="bold"),
        axis.text.y = element_text(size=24, face="bold")
      )
     v2
    }, height = 730)
    
    
    output$distPlot3<- renderPlot({
      set.seed(123)
      dados<- rbinom(n=input$n, size=input$m, prob=input$prob)
      theta<- seq(0.001,0.999,length.out=1000)
      
      aux1<- data.frame("Distribution" = "Likelihood",
                        x=theta,y=Veross(dados,theta=input$prob,
                                         m=input$m))
      aux2<- data.frame("Distribution" = "Prior",
                        x=theta, y=Priori(theta,
                                          alpha=input$alpha, 
                                          beta=input$beta))
      aux3<- data.frame("Distribution" = "Posterior",
                        x=theta, y=Posterior(dados, theta, 
                                             alpha=input$alpha, 
                                             beta=input$beta, 
                                             m=input$m))
      
      dados_Binomial <- reactive({
        data.frame(rbind(aux1, aux2, aux3))
      })  
      
      dados_preditiva<-  reactive({
        data.frame(x=as.factor(0:input$m.linha),
                   y=preditiva(alpha=input$alpha, beta=input$beta, 
                               dados, m=input$m,
                               m.linha = input$m.linha))
      })
      
      
      LI<- qbeta( (1-input$confint)/2,input$alpha + sum(dados), 
                  input$beta + input$n*input$m -sum(dados) )
      LS<- qbeta((1+input$confint)/2,input$alpha + sum(dados), 
                 input$beta + input$n*input$m - sum(dados))
      Percentil<- qbeta(input$percentil,input$alpha + sum(dados), 
                        input$beta + input$n*input$m - sum(dados))
      
      
      dados_Posteriori<-  reactive({ 
        data.frame("Distribution" = "Posterior",
                   x=theta, y=Posterior(dados, theta, 
                                        alpha=input$alpha, 
                                        beta=input$beta, 
                                        m=input$m), LI=LI, LS=LS,
                   Percentil=Percentil)
      })
      
    v3<- ggplot(dados_Posteriori(),
                aes(x=x,y=y)) +
      geom_line(size=4,colour="black") +
      xlab(expression(theta))+
      ylab(expression(pi(theta/data)))+
      labs(title = "") +#CI for the posterior distribution
      geom_area(mapping = aes(x = ifelse(x>LI & x<LS, x, 2)), 
                fill = "lightblue") +
      geom_point(mapping = aes(x = LI,y=0),color="red",size=10)+
      geom_point(mapping = aes(x = LS,y=0),color="red",size=10)+
      xlim(0,1)    +
      theme(
        plot.title = element_text(color="black", size=24, face="bold"),
        axis.title.x = element_text(color="black", size=24, face="bold"),
        axis.title.y = element_text(color="black", size=24, face="bold"),
        axis.text.x = element_text(size=24, face="bold"),
        axis.text.y = element_text(size=24, face="bold")
      )
    
    v4<- ggplot(dados_Posteriori(),
                aes(x=x,y=y)) +
      geom_line(size=4,colour="black") +
      xlab(expression(theta))+
      ylab(expression(pi(theta/data)))+
      labs(title = "") +#"Percentile of the posterior distribution"
      geom_area(mapping = aes(x = ifelse( x>0 & x<Percentil, x, 2)), 
                fill = "lightblue") +
      xlim(0, 1) +
      theme(
        plot.title = element_text(color="black", size=24, face="bold"),
        axis.title.x = element_text(color="black", size=24, face="bold"),
        axis.title.y = element_text(color="black", size=24, face="bold"),
        axis.text.x = element_text(size=24, face="bold"),
        axis.text.y = element_text(size=24, face="bold")
      )
    
    grid.arrange(v3, v4,ncol=2)
    }, height = 730)
  
}
)