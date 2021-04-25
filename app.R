#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(plotly)
library(bslib)

# Define UI 
ui <- fluidPage(
    
    theme = bs_theme(bg = "lightpink", fg = "indigo", primary = "dodgerblue",
                     base_font = font_google("Space Mono"),
                     code_font = font_google("Space Mono")),
    
    # Application title
    titlePanel("Plotting probability distributions"),

    # Sidebar with inputs
    sidebarLayout(
        sidebarPanel(
            selectInput("DISCRETEdistribution",
                        "Select a discrete random variable probability distribution:",
                        c("Binomial" = "binomial",
                        "Geometric" = "geometric",
                        "Hypergeometric" = "hypergeometric",
                        "Poisson" = "poisson",
                        "Negative binomial" = "negativebinom")),
            selectInput("CONTINUOUSdistribution",
                        "Select a continuous variable probability distribution:",
                        c("Uniform" = "uniform",
                          "Normal" = "normal",
                          "Exponential" = "exponential",
                          "Gamma" = "gamma",
                          "Chi-square" = "chisquare")),
            sliderInput("mean_value", "Select the mean:",
                       min = 0,
                       max = 100,
                       value = 50),
            sliderInput("sd_value", "[Normal] Select the standard deviation:",
                        min = 0,
                        max = 100,
                        value = 5),
            submitButton("Apply")
        ),

        # Show plot
        mainPanel(
            tabsetPanel(type = "tabs",
                tabPanel("Instructions", 
                        htmlOutput("instructions", height = 200)),
                tabPanel("Discrete PDF", 
                         imageOutput("discreteImg", height = 200),
                         plotlyOutput("discretePlot", height = 300)),
                tabPanel("Discrete CDF", 
                         imageOutput("discreteImg2", height = 200),
                         plotlyOutput("dcumulativePlot", height = 300)),
                tabPanel("Continuous PDF", 
                         imageOutput("continuousImg", height = 200),
                         plotlyOutput("continuousPlot", height = 300)),
                tabPanel("Continuous CDF", 
                         imageOutput("continuousImg2", height = 200),
                         plotlyOutput("ccumulativePlot", height = 300)))
        )
    )
)

# Define server 
server <- function(input, output) {
  
  output$instructions <- renderText({
    
    "This app was made to plot and compare discrete random variable probability distributions and continuous variable probability distributions, all with the same mean. <br>
    You can plot both the probability distribution function (PDF), as well as the cumulative distribution function (CDF). <br>
    To get started, just select the discrete and continuous variable probability distribution functions you want to plot, and also use the slider to select their mean. The standard deviation slider only works for the normal distribution, since both parameters need to be specified in order to plot such distribution. <br>
    On the tabs, you will find the equation for the function, the mean, the variance and the plot itself. <br>
    Please note the tabs order and how they relate to the input values:
    <ul>
    <li> Use the first drop down list for tabs 2 and 3 (discrete PDF and CDF). </li>
    <li> Use the second drop down list for tabs 4 and 5 (continuous PDF and CDF). </li>
    <li> Use the first slider (mean) for any tab. </li>
    <li> Use the second slider (standard deviation) for normal distribution. </li>
    </ul>"
    
  })
    
    x <- seq(1, 100, by = 1)
    size1 <- 100
    size2 <- 50
    
    q <- seq(1, 100, by = 1)
    n <- 100
    
    w <- 100
    b <- 100
    k <- 100
    
    min <- 0

    shape <- 0.5
    
    ncp <- 0
    
    shape1 <- 200
    
    prob_func <- reactive({
        prob <- input$mean_value/100
        return(prob)
    })
    
    k_func <- reactive({
        k <- input$mean_value/0.5
        return(k)
    })
    
    max_func <- reactive({
        max <- 2*input$mean_value+min
        return(max)
    })
    
    beta_func <- reactive({
        beta <- input$mean_value/shape
        return(beta)
    })
    
    shape2_func <- reactive({
        shape2 <- (shape1-shape1*input$mean_value)/input$mean_value
        return(shape2)
    })
    
    DISCRETEvalues <- reactive({
        
        if(input$DISCRETEdistribution == "binomial"){
            density <- dbinom(x, size1, prob_func())
            data <- data.frame(x, density)
            return(data)
        } else{
            if(input$DISCRETEdistribution == "geometric"){
                density <- dgeom(x, 1/(prob_func()*100))
                data <- data.frame(x, density)
                return(data)
            } else{
                if(input$DISCRETEdistribution == "hypergeometric"){
                    density <- dhyper(x, w, b, k_func())
                    data <- data.frame(x, density)
                    return(data)
                } else{
                    if(input$DISCRETEdistribution == "poisson"){
                        density <- dpois(x, prob_func()*100)
                        data <- data.frame(x, density)
                        return(data)
                    } else{
                        if(input$DISCRETEdistribution == "negativebinom"){
                            density <- dnbinom(x, size2, size/(prob_func()*100+size))
                            data <- data.frame(x, density)
                            return(data)
                        } 
                    }
                }
            }
        }
    })
    
    CONTINUOUSvalues <- reactive({
        
                            if(input$CONTINUOUSdistribution == "uniform"){
                                density <- dunif(x, min, max_func())
                                data <- data.frame(x, density)
                                return(data)
                            } else{
                                if(input$CONTINUOUSdistribution == "normal"){
                                    density <- dnorm(x, prob_func()*100, input$sd_value)
                                    data <- data.frame(x, density)
                                    return(data)
                                } else{
                                    if(input$CONTINUOUSdistribution == "exponential"){
                                        density <- dexp(x, 1/input$mean_value)
                                        data <- data.frame(x, density)
                                        return(data)
                                    } else{
                                        if(input$CONTINUOUSdistribution == "gamma"){
                                            density <- dgamma(x, shape, scale = beta_func())
                                            data <- data.frame(x, density)
                                            return(data)
                                        } else{
                                            if(input$CONTINUOUSdistribution == "chisquare"){
                                                density <- dchisq(x, input$mean_value, ncp)
                                                data <- data.frame(x, density)
                                                return(data)
                                            } 
                                        }
                                    }
                                }
                            }
    })
    
    DISCRETEnames <- reactive({
        if(input$DISCRETEdistribution == "binomial"){
            name <- "Binomial"
            return(name)
        } else{
            if(input$DISCRETEdistribution == "geometric"){
                name <- "Geometric"
                return(name)
            } else{
                if(input$DISCRETEdistribution == "hypergeometric"){
                    name <- "Hypergeometric"
                    return(name)
                } else{
                    if(input$DISCRETEdistribution == "poisson"){
                        name <- "Poisson"
                        return(name)
                    } else{
                        if(input$DISCRETEdistribution == "negativebinom"){
                            name <- "Negative binomial"
                            return(name)
                        } 
                    }
                }
            }
        }
    })
    
    CONTINUOUSnames <- reactive({
  
                            if(input$CONTINUOUSdistribution == "uniform"){
                                name <- "Uniform"
                                return(name)
                            } else{
                                if(input$CONTINUOUSdistribution == "normal"){
                                    name <- "Normal"
                                    return(name)
                                } else{
                                    if(input$CONTINUOUSdistribution == "exponential"){
                                        name <- "Exponential"
                                        return(name)
                                    } else{
                                        if(input$CONTINUOUSdistribution == "gamma"){
                                            name <- "Gamma"
                                            return(name)
                                        } else{
                                            if(input$CONTINUOUSdistribution == "chisquare"){
                                                name <- "Chi-square"
                                                return(name)
                                            } 
                                        }
                                    }
                                }
                            }

    })
    
    output$discretePlot <- renderPlotly({
        
        fig <- plot_ly(DISCRETEvalues(), x = ~x, y = ~density, type = 'bar')
        
        fig <- fig %>% layout(title = paste(DISCRETEnames(),"probability histogram"),
                              xaxis = list(title = 'y'),
                              yaxis = list(title = 'p(y)'))
        
        fig
        
    })
    
    output$continuousPlot <- renderPlotly({
        
        fig <- plot_ly(CONTINUOUSvalues(), x = ~x, y = ~density, 
                       type = 'scatter', mode = 'lines', fill = 'tozeroy')
        
        fig <- fig %>% layout(title = paste(CONTINUOUSnames(),"density distribution"),
                              xaxis = list(title = 'y'),
                              yaxis = list(title = 'p(y)'))
        
        fig
        
    })
    
    DISCRETEcumulative <- reactive({
        
        if(input$DISCRETEdistribution == "binomial"){
            cum <- pbinom(q, size1, prob_func())
            data <- data.frame(q, cum)
            colnames(data) <- c("x", "y")
            return(data)
        } else{
            if(input$DISCRETEdistribution == "geometric"){
                cum <- pgeom(q, 1/(prob_func()*100))
                data <- data.frame(q, cum)
                colnames(data) <- c("x", "y")
                return(data)
            } else{
                if(input$DISCRETEdistribution == "hypergeometric"){
                    cum <- phyper(q, w, b, k_func())
                    data <- data.frame(q, cum)
                    colnames(data) <- c("x", "y")
                    return(data)
                } else{
                    if(input$DISCRETEdistribution == "poisson"){
                        cum <- ppois(q, prob_func()*100)
                        data <- data.frame(q, cum)
                        colnames(data) <- c("x", "y")
                        return(data)
                    } else{
                        if(input$DISCRETEdistribution == "negativebinom"){
                            cum <- pnbinom(q, size2, size/(prob_func()*100+size))
                            data <- data.frame(q, cum)
                            colnames(data) <- c("x", "y")
                            return(data)
                        } 
                    }
                }
            }
        }
    })
    
    CONTINUOUScumulative <- reactive({
        
                            if(input$CONTINUOUSdistribution == "uniform"){
                                cum <- punif(q, min, max_func())
                                data <- data.frame(q, cum)
                                colnames(data) <- c("x", "y")
                                return(data)
                            } else{
                                if(input$CONTINUOUSdistribution == "normal"){
                                    cum <- pnorm(q, prob_func()*100, input$sd_value)
                                    data <- data.frame(q, cum)
                                    colnames(data) <- c("x", "y")
                                    return(data)
                                } else{
                                    if(input$CONTINUOUSdistribution == "exponential"){
                                        cum <- pexp(q, 1/input$mean_value)
                                        data <- data.frame(q, cum)
                                        colnames(data) <- c("x", "y")
                                        return(data)
                                    } else{
                                        if(input$CONTINUOUSdistribution == "gamma"){
                                            cum <- pgamma(q, shape, scale = beta_func())
                                            data <- data.frame(q, cum)
                                            colnames(data) <- c("x", "y")
                                            return(data)
                                        } else{
                                            if(input$CONTINUOUSdistribution == "chisquare"){
                                                cum <- pchisq(q, input$mean_value, ncp)
                                                data <- data.frame(q, cum)
                                                colnames(data) <- c("x", "y")
                                                return(data)
                                            } 
                                        }
                                    }
                                }
                            }
    })
    
    output$dcumulativePlot <- renderPlotly({
        
        fig <- plot_ly(DISCRETEcumulative(), x = ~x, y = ~y, type = 'bar')
        
        fig <- fig %>% layout(title = paste(DISCRETEnames(),"cumulative distribution function"),
                              xaxis = list(title = 'y'),
                              yaxis = list(title = 'P(y)'))
        
        fig
        
    })
    
    output$ccumulativePlot <- renderPlotly({
        
        fig <- plot_ly(CONTINUOUScumulative(), x = ~x, y = ~y, 
                       type = 'scatter', mode = 'lines', fill = 'tozeroy')
        
        fig <- fig %>% layout(title = paste(CONTINUOUSnames(),"cumulative distribution function"),
                              xaxis = list(title = 'p'),
                              yaxis = list(title = 'P(y)'))
        
        fig
        
    })
    
    output$discreteImg <- renderImage(
      
      if(input$DISCRETEdistribution == "binomial"){
        return(list(
          src = "binomial.png",
          filetype = "image/png"
        ))
      } else{
        if(input$DISCRETEdistribution == "geometric"){
          return(list(
            src = "geometric.png",
            filetype = "image/png"
          ))
        } else{
          if(input$DISCRETEdistribution == "hypergeometric"){
            return(list(
              src = "hypergeometric.png",
              filetype = "image/png"
            ))
          } else{
            if(input$DISCRETEdistribution == "poisson"){
              return(list(
                src = "poisson.png",
                filetype = "image/png"
              ))
            } else{
              if(input$DISCRETEdistribution == "negativebinom"){
                return(list(
                  src = "negativebinom.png",
                  filetype = "image/png"
                ))
              }
            }
          }
        }
      }

    )
    
    
    output$discreteImg2 <- renderImage(
      
      if(input$DISCRETEdistribution == "binomial"){
        return(list(
          src = "binomial.png",
          filetype = "image/png"
        ))
      } else{
        if(input$DISCRETEdistribution == "geometric"){
          return(list(
            src = "geometric.png",
            filetype = "image/png"
          ))
        } else{
          if(input$DISCRETEdistribution == "hypergeometric"){
            return(list(
              src = "hypergeometric.png",
              filetype = "image/png"
            ))
          } else{
            if(input$DISCRETEdistribution == "poisson"){
              return(list(
                src = "poisson.png",
                filetype = "image/png"
              ))
            } else{
              if(input$DISCRETEdistribution == "negativebinom"){
                return(list(
                  src = "negativebinom.png",
                  filetype = "image/png"
                ))
              }
            }
          }
        }
      }
      
    )
    
    
    output$continuousImg <- renderImage(
      
      if(input$CONTINUOUSdistribution == "uniform"){
        return(list(
          src = "uniform.png",
          filetype = "image/png"
        ))
      } else{
        if(input$CONTINUOUSdistribution == "normal"){
          return(list(
            src = "normal.png",
            filetype = "image/png"
          ))
        } else{
          if(input$CONTINUOUSdistribution == "exponential"){
            return(list(
              src = "exponential.png",
              filetype = "image/png"
            ))
          } else{
            if(input$CONTINUOUSdistribution == "gamma"){
              return(list(
                src = "gamma.png",
                filetype = "image/png"
              ))
            } else{
              if(input$CONTINUOUSdistribution == "chisquare"){
                return(list(
                  src = "chi-square.png",
                  filetype = "image/png"
                ))
              }
            }
          }
        }
      }
      
    )
    
    
    output$continuousImg2 <- renderImage(
      
      if(input$CONTINUOUSdistribution == "uniform"){
        return(list(
          src = "uniform.png",
          filetype = "image/png"
        ))
      } else{
        if(input$CONTINUOUSdistribution == "normal"){
          return(list(
            src = "normal.png",
            filetype = "image/png"
          ))
        } else{
          if(input$CONTINUOUSdistribution == "exponential"){
            return(list(
              src = "exponential.png",
              filetype = "image/png"
            ))
          } else{
            if(input$CONTINUOUSdistribution == "gamma"){
              return(list(
                src = "gamma.png",
                filetype = "image/png"
              ))
            } else{
              if(input$CONTINUOUSdistribution == "chisquare"){
                return(list(
                  src = "chi-square.png",
                  filetype = "image/png"
                ))
              }
            }
          }
        }
      }
      
    )

}

# Run the application 
shinyApp(ui = ui, server = server)
