library(shiny)
library(ggplot2)
library(gridExtra)

shinyServer(function(input, output) {
  
  Veross<- function(dados, theta){
    n<- length(dados)
    aux<- exp(-n*theta)*theta^(sum(dados))
    aux#/max(aux)
    dgamma(theta, n*mean(dados), n)
  }
  
  Priori<- function(theta,alpha,beta){
    aux<- dgamma(theta, shape=alpha, rate=beta)
    aux#/max(aux)
  }
  
  Posterior<- function(dados, theta, alpha, beta){
    n<- length(dados)
    aux<- dgamma(theta, shape=alpha+n*mean(dados), rate=n+beta)
    aux#/max(aux)
    
  }
  
  preditiva_Poisson<- function(dados, alpha, beta, m.linha){
    yp<- 0:m.linha
    n<- length(dados)
    alphap<- n*mean(dados)+alpha
    betap<- beta + n
    (betap)^(alphap)/gamma(alphap)* 1/factorial(yp)*gamma(yp+alphap)/(betap+1)^(yp+alphap)    
  }
  
  
  
  
  
  
  output$distPlot1 <- renderPlot({ 
    dados<- rpois(n=input$n1, input$lambda)
    theta<- seq(0.001,input$maximo_Poisson,length.out=1000)
    LI_Poisson<- qgamma((1-input$confint1)/2, 
                        shape=input$alpha1+input$n1*mean(dados), 
                        rate=input$n1+input$beta1)
    LS_Poisson<- qgamma((1+input$confint1)/2, 
                        shape=input$alpha1+input$n1*mean(dados), 
                        rate=input$n1+input$beta1)
    Percentil_Poisson<- qgamma(input$percentil1, 
                               shape=input$alpha1+input$n1*mean(dados), 
                               rate=input$n1+input$beta1)
    
    
    aux1<- data.frame("Distribution" = "Likelihood",
                      x=theta,y=Veross(dados,theta))
    aux2<- data.frame("Distribution" = "Prior",
                      x=theta, y=Priori(theta,
                                        alpha=input$alpha1, 
                                        beta=input$beta1))
    aux3<- data.frame("Distribution" = "Posterior",
                      x=theta, y=Posterior(dados, theta, 
                                           alpha=input$alpha1, 
                                           beta=input$beta1))
    
    dados_Poisson <- reactive({
      dados.ggplot<- data.frame(rbind(aux1, aux2, aux3))
    })  
    
    dados_preditiva<-  reactive({
      data.frame(x=as.factor(0:input$m.linha),
                 y=preditiva(alpha=input$alpha, beta=input$beta, 
                             dados, m=input$m,
                             m.linha = input$m.linha))
    })
    
    
    dados_Posterior_Poisson = reactive({ 
      data.frame("Distribution" = "Posterior",
                 x=theta, y=Posterior(dados, theta, 
                                      alpha=input$alpha1, 
                                      beta=input$beta1), 
                 LI_Poisson=LI_Poisson, 
                 LS_Poisson=LS_Poisson,
                 Percentil=Percentil_Poisson)})
    
    dados_preditiva_Poisson= reactive({ 
      data.frame(x=0:input$m1.linha1,#as.factor(
                 y=preditiva_Poisson(dados, 
                                     alpha=input$alpha1, 
                                     beta=input$beta1, 
                                     m.linha = input$m1.linha1)) })
    
    v1<- ggplot(dados_Poisson(),
                aes(x=x,y=y,color=Distribution)) +
      scale_color_manual(values=c("blue", "black", "red"))+ 
      #likelihood, prior, posterior
      geom_line(size=3,linetype="solid") +
      xlab(expression(theta))+
      ylab("")+
      annotate("text", x = 0.5, y = max(aux1$y), 
               label = "Likelihood", color="blue", size=12)+
      annotate("text", x = 1.0, y = max(aux2$y), 
               label = "Prior distribution", color="black", size=12)+
      annotate("text", x = 1.5, y = max(aux3$y), 
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
      
      dados<- rpois(n=input$n1, input$lambda)
      theta<- seq(0.001,input$maximo_Poisson,length.out=1000)
      LI_Poisson<- qgamma((1-input$confint1)/2, 
                          shape=input$alpha1+input$n1*mean(dados), 
                          rate=input$n1+input$beta1)
      LS_Poisson<- qgamma((1+input$confint1)/2, 
                          shape=input$alpha1+input$n1*mean(dados), 
                          rate=input$n1+input$beta1)
      Percentil_Poisson<- qgamma(input$percentil1, 
                                 shape=input$alpha1+input$n1*mean(dados), 
                                 rate=input$n1+input$beta1)
      
      
      aux1<- data.frame("Distribution" = "Likelihood",
                        x=theta,y=Veross(dados,theta))
      aux2<- data.frame("Distribution" = "Prior",
                        x=theta, y=Priori(theta,
                                          alpha=input$alpha1, 
                                          beta=input$beta1))
      aux3<- data.frame("Distribution" = "Posterior",
                        x=theta, y=Posterior(dados, theta, 
                                             alpha=input$alpha1, 
                                             beta=input$beta1))
      
      dados_Poisson <- reactive({
        dados.ggplot<- data.frame(rbind(aux1, aux2, aux3))
      })  
      
      dados_preditiva<-  reactive({
        data.frame(x=as.factor(0:input$m.linha),
                   y=preditiva(alpha=input$alpha, 
                               beta=input$beta, 
                               dados, m=input$m,
                               m.linha = input$m.linha))
      })
      
      
      dados_Posterior_Poisson = reactive({ 
        data.frame("Distribution" = "Posterior",
                   x=theta, y=Posterior(dados, theta, 
                                        alpha=input$alpha1, 
                                        beta=input$beta1), 
                   LI_Poisson=LI_Poisson, 
                   LS_Poisson=LS_Poisson,
                   Percentil=Percentil_Poisson)})
      
      dados_preditiva_Poisson= reactive({ 
        data.frame(x=0:input$m1.linha1,#as.factor(
                   y=preditiva_Poisson(dados, 
                                       alpha=input$alpha1, 
                                       beta=input$beta1, 
                                       m.linha = input$m1.linha1)) })
      
     v2<- ggplot(data=dados_preditiva_Poisson(), aes(x, y)) +
      geom_bar(stat="identity",width=0.5, fill = "black")+
       xlab("Future observations")+
      ylab(expression(pi(y[t]/data)))+
      #ylim(0,1)+
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
      dados<- rpois(n=input$n1, input$lambda)
      theta<- seq(0.001,input$maximo_Poisson,length.out=1000)
      LI_Poisson<- qgamma((1-input$confint1)/2, 
                          shape=input$alpha1+input$n1*mean(dados), 
                          rate=input$n1+input$beta1)
      LS_Poisson<- qgamma((1+input$confint1)/2, 
                          shape=input$alpha1+input$n1*mean(dados), 
                          rate=input$n1+input$beta1)
      Percentil_Poisson<- qgamma(input$percentil1, 
                                 shape=input$alpha1+input$n1*mean(dados), 
                                 rate=input$n1+input$beta1)
      
      
      aux1<- data.frame("Distribution" = "Likelihood",
                        x=theta,y=Veross(dados,theta))
      aux2<- data.frame("Distribution" = "Prior",
                        x=theta, y=Priori(theta,
                                          alpha=input$alpha1, 
                                          beta=input$beta1))
      aux3<- data.frame("Distribution" = "Posterior",
                        x=theta, y=Posterior(dados, theta, 
                                             alpha=input$alpha1, 
                                             beta=input$beta1))
      
      dados_Poisson <- reactive({
        dados.ggplot<- data.frame(rbind(aux1, aux2, aux3))
      })  
      
      dados_preditiva<-  reactive({
        data.frame(x=as.factor(0:input$m.linha),
                   y=preditiva(alpha=input$alpha, beta=input$beta, 
                               dados, m=input$m,
                               m.linha = input$m.linha))
      })
      
      
      dados_Posterior_Poisson = reactive({ 
        data.frame("Distribution" = "Posterior",
                   x=theta, y=Posterior(dados, theta, 
                                        alpha=input$alpha1, 
                                        beta=input$beta1), 
                   LI_Poisson=LI_Poisson, 
                   LS_Poisson=LS_Poisson,
                   Percentil=Percentil_Poisson)})
      
      dados_preditiva_Poisson= reactive({ 
        data.frame(x=0:input$m1.linha1,#as.factor(
                   y=preditiva_Poisson(dados, 
                                       alpha=input$alpha1, 
                                       beta=input$beta1, 
                                       m.linha = input$m1.linha1)) })
      
    v3<- ggplot(dados_Posterior_Poisson(),
                aes(x=x,y=y)) +
      geom_line(size=4,colour="black") +
      xlab(expression(theta))+
      ylab(expression(pi(theta/data)))+
      labs(title = "") +#CI for the posterior distribution
      geom_area(mapping = aes(x = ifelse(x>LI_Poisson & x<LS_Poisson, x, 100)), 
                fill = "lightblue") +
      geom_point(mapping = aes(x = LI_Poisson,y=0),color="red",size=10)+
      geom_point(mapping = aes(x = LS_Poisson,y=0),color="red",size=10)+
      xlim(0,input$maximo_Poisson)    +
      theme(
        plot.title = element_text(color="black", size=24, face="bold"),
        axis.title.x = element_text(color="black", size=24, face="bold"),
        axis.title.y = element_text(color="black", size=24, face="bold"),
        axis.text.x = element_text(size=24, face="bold"),
        axis.text.y = element_text(size=24, face="bold")
      )
    
    v4<- ggplot(dados_Posterior_Poisson(),
                aes(x=x,y=y)) +
      geom_line(size=4,colour="black") +
      xlab(expression(theta))+
      ylab(expression(pi(theta/data)))+
      labs(title = "") +#"Percentile of the posterior distribution"
      geom_area(mapping = aes(x = ifelse( x>0 & x<Percentil, x, 100)), 
                fill = "lightblue") +
      xlim(0, input$maximo_Poisson) +
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