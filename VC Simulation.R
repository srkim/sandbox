library(shiny)
library(truncnorm)
library(ggplot2)
library(poweRlaw)
library(dplyr)

nsim = 10000
uub <- 5*10^9
ulb <- 5*10^8
mtub <- ulb - 1
mtlb <- 1*10^6
lub <- mtlb - 1

val.sim <- function(fs = 30, 
                    nd = 30, 
                    ao = 0.3,
                    nu = 1, 
                    udist = 1, 
                    ulambda = 1, 
                    ualpha = 2, 
                    nmt = 6, 
                    mtdist = 1){
  nmt <- min(nmt, nd - nu)
  nl <- max(0, nd - nu - nmt)
  if (udist == 1){
    uval <- ulb + ulb*rexp(nu, ulambda)
  }else if (udist == 2){
    uval <- runif(nu, ulb, uub)
  }else{
    uval <- ulb*dplcon(nu, 1, ualpha)
  }
  if (mtdist == 1){
    mtval <- rtruncnorm(nmt, mtlb, mtub, (mtlb+mtub)/2, (mtub-mtlb)/2)
  }else{
    mtval <- runif(nmt, mtlb, mtub)
  }
  lval <- runif(nl, max = lub)
  pval <- c(uval, mtval, lval)
  return(pval)
}

do.sim.df <- function(nsim = 10000, 
                      fs = 30, 
                      nd = 30, 
                      ao = 0.3,
                      nu = 1, 
                      udist = 1, 
                      nmt = 6, 
                      mtdist = 1){
  val.df <- data.frame()
  for (i in 1:nsim){
    val.df <- rbind(val.df, val.sim(fs, nd, ao, nu, udist, nmt, mtdist))
  }
  names(val.df) <- paste0("deal", as.character(1:nd))
  return(val.df)
}

do.sim.v <- function(nsim = 10000, 
                     fs = 30, 
                     nd = 30, 
                     ao = 0.3,
                     nu = 1, 
                     udist = 1, 
                     ulambda = 1, 
                     ualpha = 2, 
                     nmt = 6, 
                     mtdist = 1){
  val.v <- c()
  for (i in 1:nsim){
    val.v <- c(val.v, sum(val.sim(fs, nd, ao, nu, udist, ulambda, ualpha, nmt, mtdist)) * ao / fs / 10^6)
  }
  #p <- hist(val.v, main = "Distribution of Multiples - 5,000 Simulations", xlab = "")
  #t <- setNames(stack(summary(val.v))[2:1], c('Statistics','Value'))
  return(val.v)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("VC Monte Carlo Simulation - One Round"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      
      sliderInput("fundSize",
                  "Fund size in millions: ",
                  min = 10,
                  max = 300,
                  value = 30, 
                  step = 5), 
      
      
      sliderInput("numOfDeals",
                  "Number of deals: ",
                  min = 10,
                  max = 300,
                  value = 30, 
                  step = 5), 
      
      sliderInput("avgOwnership",
                  "Average ownership: ",
                  min = 0.00,
                  max = 0.50,
                  step = 0.02, 
                  value = 0.10), 
      
      hr(), 
      
      h3("Unicorn (>$500M)"), 
      
      sliderInput("numOfUnicorn",
                  "Number of unicorn exits: ",
                  min = 0,
                  max = 10,
                  step = 1, 
                  value = 1), 
      
      radioButtons("unicornDist", "Unicorn exit valuation distribution: ",
                   choices = list("Exponential" = 1, 
                                  "Uniform capped at $5B" = 2), selected = 1), 
      
      sliderInput("exponentialLambda", 
                   "Exponential lambda: ", 
                   value = 1, min = 0.5, max = 5, step = 0.1), 
      
      helpText("Default = 1, lower value = higher likelihood getting extremely high valuation exits"), 
      
      # sliderInput("pLAlpha", 
      #             "Power Law Alpha: ", 
      #             value = 2, min = 1.2, max = 5, step = 0.1), 
      # 
      # helpText("Default = 2, lower value = higher likelihood getting extremely high valuation exits"), 
      
      hr(), 
      
      h3("Mid-Tier (>=$1M, <$500M)"), 
      
      sliderInput("numOfMT",
                  "Number of Mid-Tier exits: ",
                  min = 0,
                  max = 10,
                  step = 1, 
                  value = 3), 
      
      radioButtons("mtDist", 
                   "Mid-Tier exit valuation distribution: ",
                   choices = list("Truncated Normal" = 1, "Uniform" = 2), selected = 1), 
      
      hr(), 
      
      h3("Loss (<$1M)"), 
      
      helpText("All deals except unicorn and Mid-Tier exits.")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot"), 
      hr(), 
      verbatimTextOutput('table')
    ) 
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  results <- reactive({
    do.sim.v(nsim = 10000, 
             fs = input$fundSize, 
             nd = input$numOfDeals, 
             ao = input$avgOwnership,
             nu = input$numOfUnicorn, 
             udist = input$unicornDist, 
             ulambda = input$exponentialLambda, 
             ualpha = input$pLAlpha, 
             nmt = input$numOfMT, 
             mtdist = input$mtDist)
  })
  
  output$plot <- renderPlot({
    r <- results()
    r.df <- data.frame(Multiple = r)
    p <- ggplot(r.df, aes(x=Multiple)) + 
      geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 30) + 
      geom_density(alpha=.2, fill="#FF6666") + 
      theme_classic(base_size = 20) + 
      labs(title="Distribution of Multiples - 10,000 Simulations", y = "Density")
    #p <- hist(r, main = "Distribution of Multiples - 5,000 Simulations", xlab = "")
    print(p)
  })
  output$table <- renderPrint({
    df <- data.frame(Multiple = results())
    t(df %>% 
      summarise(Min = min(Multiple),
                FirstQu = quantile(Multiple, .25), 
                Median = quantile(Multiple, .50), 
                Mean = mean(Multiple),
                ThirdQu = quantile(Multiple, .75),
                Max = max(Multiple),
                n = n(), 
                StdDev = sd(Multiple)))  
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

