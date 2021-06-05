library(shiny)
library(dplyr)
library(readxl)
library(readr)
library(plotly)
source("functions.R")

ui <- fluidPage(
  h1("Ergodicity - Growth of $1M Investment Each Year"), 
  sidebarLayout(
    sidebarPanel(
      radioButtons("asset_class", label = "Asset Class",
                   choices = list("Early Stage VC", "Buyout"), 
                   selected = "Early Stage VC"), 
      hr(), 
      sliderInput("n_fund", 
                  label = "# of Funds Invested Each Year", 
                  min = 1,  
                  max = 10, 
                  value = 2, 
                  step = 1), 
      hr(), 
      sliderInput("vintage", 
                  label = "Vintage", 
                  min = 1990,  
                  max = 2015, 
                  value = c(1990, 2015), 
                  step = 1), 
      hr(), 
      selectInput("stats", 
                  label = "Summary Statistics to Use", 
                  choices = c("Pooled*", "5th", "25th", "50th", "75th", "95th"), 
                  selected = "Pooled*"), 
      hr(), 
      checkboxInput("skill", "Add a Skilled Manager",
                    value = FALSE), 
      sliderInput("skill_edge", 
                  label = "Skill Edge", 
                  min = 0,  
                  max = 1, 
                  value = 0.2, 
                  step = 0.2), 
      hr(), 
      sliderInput("opacity", 
                  label = "Line Opacity", 
                  min = 0,  
                  max = 1, 
                  value = 0.1, 
                  step = 0.05), 
      hr(), 
      actionButton("run", "Run")
    ),
    mainPanel(
      plotlyOutput("plot", height = "800px")
    )
  )
)

server <- function(input, output, clientData, session){
  
  observe({
    asset_class = input$asset_class
    if (asset_class == "Buyout"){
      min_vintage = min(irr_by_vintage_BO$Vintage)
      max_vintage = max((irr_by_vintage_BO$Vintage))
    }else{
      min_vintage = min(irr_by_vintage_eVC$Vintage)
      max_vintage = max((irr_by_vintage_eVC$Vintage))
    }
    updateSliderInput(session, "vintage",
                      min = min_vintage,
                      max = max_vintage, 
                      value = c(min_vintage, max_vintage))
  })
  
  observe({
    skill = input$skill
    if (skill){
      updateSliderInput(session, "skill_edge",
                        max = 1, 
                        value = 0.2)
    }else{
      updateSliderInput(session, "skill_edge",
                        max = 0, 
                        value = 0)
    }
  })
  
  data <- eventReactive(input$run, {
    year_start <- as.integer(input$vintage[1])
    year_end <- as.integer(input$vintage[2])
    stats <- input$stats
    asset_class = input$asset_class
    if (asset_class == "Buyout"){
      data_by_vintage <- irr_by_vintage_BO
      combined_tvpi_and_irr <- combined_tvpi_and_irr_BO
    }else{
      data_by_vintage <- irr_by_vintage_eVC
      combined_tvpi_and_irr <- combined_tvpi_and_irr_eVC
    }
    data_by_vintage <- data_by_vintage[(data_by_vintage$Vintage >= year_start)&(data_by_vintage$Vintage <= year_end), ]
    ensemble <- get_ensemble(data = data_by_vintage[stats], 
                             year_range = year_start:year_end, 
                             is_irr = "IRR" == "IRR")
    all_sims_df <- get_sims(combined_tvpi_and_irr, 
                            year_start:year_end, 
                            "IRR" == "IRR", 
                            input$n_fund)
    if (input$skill){
      skilled_manager <- get_skilled_manager(combined_tvpi_and_irr, year_start:year_end, "IRR" == "IRR", input$skill_edge, input$n_fund)
      ensemble_and_sims <- cbind(ensemble, 
                                 skilled_manager["Skilled.Manager"], 
                                 all_sims_df[, colnames(all_sims_df)[1:n_sim+1]])
    }else{
      ensemble_and_sims <- cbind(ensemble, 
                                 all_sims_df[, colnames(all_sims_df)[1:n_sim+1]])
    }
    return(list(ensemble, ensemble_and_sims))
  })
  
  output$plot <- renderPlotly({
    ensemble_and_sims <- data.frame(data()[2])
    # print(head(ensemble_and_sims))
    if (input$skill){
      plot_ensemble_and_skilled_manager_and_sims(ensemble_and_sims)
    }else{
      plot_ensemble_and_sims(ensemble_and_sims)
    }
  })
}

shinyApp(ui = ui, server = server)