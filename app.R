# Load packages ----
library(shiny)
library(bslib)
library(quantmod)
library(plotly)
library(dplyr)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(scales)
library(shiny)
library(bs4Dash)
library(shiny.fluent)

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  
  # Modal Dialog (Popup) triggered when the app is loaded
  tags$script(
    "$(document).on('shiny:connected', function() {
      $('#welcome_modal').modal('show');
    });"
  )
  ,
  tags$style(HTML("
    /* Main page background color */
    body {
      background-color: #fbf7de; /* Light grey background for the main page */
    }
    
    .sidebar {
    background-color: #e1e1bb !important;
    border: none !important;
    }

.sidebar .specific-class {
    background-color: #e1e1bb !important; /* Adjust the class as needed */
}

    
    /* Card styling */
    .card-container {
      display: flex;
      flex-wrap: wrap;
      gap: 15px;  /* Space between cards */
    }
    .card-container .card {
      flex: 1;
      min-width: 0;
    }
    
    /* Modal dialog styling */
    .modal-content {
      background-color: #ffffff; /* White background for modal */
    }
  ")),
  
  
  
  # The actual modal dialog content
  div(
    id = "welcome_modal", class = "modal fade", tabindex = "-1", role = "dialog",
    div(class = "modal-dialog", role = "document",
        div(class = "modal-content",
            div(class = "modal-header",
                h4(class = "modal-title", "Welcome to the Time Traveler's Portfolio")
            ),
            div(class = "modal-body",
                p("This is a simulation tool to help you understand the potential of stock market investments. Click the button to get started.")
            ),
            div(class = "modal-footer",
                actionButton("close_modal", "Let's Time Travel!", class = "btn btn-primary")
            )
        )
    )
  ),
  div(id = "investment_page", 
      page_sidebar(
        sidebar = sidebar(id = "My_sidebar",
          numericInput("investment", "How much would you like to invest?", value = 150, min = 1, max = 1000000),
          checkboxGroupInput("stocks", "Which stocks would you like to buy?",
                             choices = list("Apple" = "AAPL", "Microsoft" = "MSFT", "Google" = "GOOGL",
                                            "Tesla" = "TSLA", "Amazon" = "AMZN", "Nike" = "NKE", 
                                            "Ford" = "F", "Intel" = "INTC", "Nokia" = "NOK")),
          dateInput("start_date", "When would you like to start investing?", value = "2020-01-01", min = "2012-01-01", max = as.character(Sys.Date())),
          actionButton("submit_investment", "Submit Investment", 
                       style = "color: black; background-color: #bdc8c8; border-color: black; border-radius: 5px; font-size: 14px; font-family: Verdana;"
          ),
          tags$style(HTML(".sidebar { background-color: #e1e1bb !important; }"))
        )
        ,
        div(
          id = "tutorial",
          tags$h3("The Time Traveler's Portfolio"),
          tags$p("Have you ever wondered what your financial situation might look like if you had invested in the stock market a few years back? Imagine being able to turn back time, make that investment, and see where it could have taken you today. In this simulator, you can explore the different outcomes of your past investment decisions and learn about the potential of investing in the stock market. Whether you’re curious about specific stocks or want to see the overall growth of a portfolio, this tool will let you explore countless possibilities. Ready to start?"),
          
          tags$ol(
            tags$li(tags$b("Step 1: Input Your Investment Amount."), 
                    "Enter the amount of money you would like to invest in each of the stocks selected."),
            
            tags$li(tags$b("Step 2: Select Your Stocks."),
                    " Choose the stocks you want to invest in from the list provided. You can select multiple stocks by checking the boxes next to their names."),
            
            tags$li(tags$b("Step 3: Choose Your Investment Start Date."),
                    " Pick the date when you want your investment to start. The available date range is from January 1, 2012, to the current date."),
            
            tags$li(tags$b("Step 4: Submit Your Investment."),
                    " Once you've entered your investment amount, selected your stocks, and picked a start date, click the 'Submit Investment' button to see your results."),
            
            tags$li(tags$b("Step 5: Explore and Adjust."),
                    " Feel free to go back and adjust your investment amount, stock selections, and start date to see how different scenarios would have affected your portfolio.")
          ),
          
          tags$p("Enjoy your time travel and learn about investments!")
        )
        ,
        
        div(id = "main_dash", style = "display: none;",
              fluidRow(
                column(6, 
                       fluidRow(
                         column(6, 
                                uiOutput("profit_card")
                         ),
                         column(6, 
                                uiOutput("percent_card")
                         )
                       )
                       ,
                       fluidRow(
                         column(12, 
                                card(
                                  card_header("Return vs Investment",
                                              style = "font-size: 14px; 
                                        font-family: Verdana; 
                                        font-weight: bold; 
                                        color: #585858;
                                        text-align: center;
                                        background-color: #e1e1bb;"
                                  ),
                                  plotOutput("returnVSinvest"),
                                  style = "background-color: #f7f7e8; height: 200px;"
                                )
                         )
                       )
                ),
                column(6, 
                       card(
                         card_header("Percentage change per Stock",
                                     style = "font-size: 14px; 
                                        font-family: Verdana; 
                                        font-weight: bold; 
                                        color: #585858;
                                        text-align: center;
                                        background-color: #e1e1bb;"),
                         plotOutput("stockIncrease"),
                         style = "background-color: #f7f7e8; height: 320px;"
                       )
                )
              )
              ,
              column(12, card(
                card_header("Stock Prices Over Time",
                            style = "font-size: 14px; 
                                        font-family: Verdana; 
                                        font-weight: bold; 
                                        color: #585858;
                                        text-align: center;
                                        background-color: #e1e1bb;"),
                style = "background-color: #f7f7e8;",
                plotOutput("historicalStocks")
              )
              )
          )
  )
)
)


server <- function(input, output, session) {
  
  observeEvent(input$close_modal, {
    runjs("$('#welcome_modal').modal('hide');")
    show("investment_page")
  })
  
  observeEvent(input$submit_investment, {
    shinyjs::hide("tutorial")
    shinyjs::show("main_dash")
    
  })
  
  
  
  dataInput <- reactive({
    
    req(length(input$stocks) > 0)
    req(input$investment > 0)
    # Initialize an empty list to store data frames
    data_list <- list()
    historical <- list()
    # Loop over each selected symbol
    for (symbol in input$stocks) {
      # Retrieve the data for the current symbol
      stock_data <- getSymbols(symbol, src = "yahoo", auto.assign = FALSE, from = input$start_date)
      #Save Adjusted price in variable
      # Convert to data frame and add a column for the stock symbol
      stock_df <- data.frame(
        AdjustedPrice = stock_data[,6],
        Stock = symbol,
        Date = as.Date(index(stock_data))
      )
      
      colnames(stock_df)[1] <- "Price"
      
      start_prices <- stock_df[1, 1] 
      end_prices <- stock_df[nrow(stock_df), 1]
      
      # Calculate the percentage gains
      percentage_gains <- (end_prices - start_prices) / start_prices * 100
      
      stock_df$Stock = switch(symbol,
                              "AAPL" = "Apple",
                              "MSFT" = "Microsoft",
                              "GOOGL" = "Google",
                              "TSLA" = "Tesla", 
                              "AMZN" = "Amazon" , 
                              "NKE" = "Nike" ,
                              "F" = "Ford", 
                              "INTC" = "Intel", 
                              "NOK" = "Nokia")
      
      stock = switch(symbol,
                     "AAPL" = "Apple",
                     "MSFT" = "Microsoft",
                     "GOOGL" = "Google",
                     "TSLA" = "Tesla", 
                     "AMZN" = "Amazon" , 
                     "NKE" = "Nike" , 
                     "F" = "Ford", 
                     "INTC" = "Intel", 
                     "NOK" = "Nokia")
      
      # Create a data frame to store the results
      gain_data <- data.frame(
        Stock = stock,
        Start_Price = as.numeric(start_prices),
        End_Price = as.numeric(end_prices),
        Percentage_Gain = as.numeric(percentage_gains),
        Profits = input$investment * percentage_gains / 100
      )
      # Append to the list
      data_list[[symbol]] <- gain_data
      historical[[symbol]] <- stock_df
    }
    # Combine all data frames into a single data frame
    combined_data <- bind_rows(data_list)
    
    combined_data = combined_data[order(combined_data$Percentage_Gain),]
    historical_data <- bind_rows(historical)
    return(list(combined_data = combined_data, historical_data = historical_data))
    
  })
  
  
  returnTable = reactive({ 
    
    req(length(input$stocks) > 0)
    req(input$investment > 0)
    
    dfReturn = data.frame(
      Type = c("Investment", "Return"),
      Amount = c(input$investment * nrow(dataInput()$combined_data) , sum(dataInput()$combined_data[,"Profits"]) +  (input$investment)*(nrow(dataInput()$combined_data)))
    )
    return(dfReturn)
  })
  
  output$profit_card <- renderUI({
    
    req(length(input$stocks) > 0)
    req(input$investment > 0)
    
    profit_value <- round(sum(dataInput()$combined_data[,"Profits"]), 0)
    
    bg_color <- ifelse(profit_value > 0, "#d7f7d7", "#f7d7d7") # Greenish background if positive, reddish if negative
    text_color <- ifelse(profit_value > 0, "#39d224", "#d22424") # Green if positive, red if negative
    
    card(
      div(uiOutput("profit"), 
          style = "font-size: 14px; font-family: Verdana; color: #585858; font-weight: bold;"),  
      div(style = paste0("font-size: 20px; font-family: Verdana; font-weight: bold; color:", text_color, ";"), 
          paste("$", profit_value)),
      style = paste0("background-color:", bg_color, "; height: 105px;")
    )
  })
  
  output$profit <- renderUI({
    
    req(length(input$stocks) > 0)
    req(input$investment > 0)
    
    profit_value = round(sum(dataInput()$combined_data[,"Profits"]),0)
    profit_text <- ifelse(profit_value > 0, "Profit", "Loss") 
    div(profit_text)
  })
  
  output$percent_card <- renderUI({
    
    req(length(input$stocks) > 0)
    req(input$investment > 0)
    
    percent_value <- round(mean(dataInput()$combined_data[,"Percentage_Gain"]),0)
    
    bg_color <- ifelse(percent_value > 0, "#d7f7d7", "#f7d7d7") # Greenish background if positive, reddish if negative
    text_color <- ifelse(percent_value > 0, "#39d224", "#d22424") # Green if positive, red if negative
    
    card(
      div(uiOutput("percent"), 
          style = "font-size: 14px; font-family: Verdana; color: #585858; font-weight: bold;"),  
      div(style = paste0("font-size: 20px; font-family: Verdana; font-weight: bold; color:", text_color, ";"), 
          paste(percent_value, "%")),
      style = paste0("background-color:", bg_color, "; height: 105px;")
    )
  })
  
  
  output$percent <- renderUI({
    
    req(length(input$stocks) > 0)
    req(input$investment > 0)
    
    percent_value = round(mean(dataInput()$combined_data[,"Percentage_Gain"]),0)
    percent_text <- ifelse(percent_value > 0, "Increase", "Decrease") 
    div(percent_text)
  })
  
  
  output$returnVSinvest <- renderPlot({
    
    req(length(input$stocks) > 0)
    req(input$investment > 0)
    
    ggplot(returnTable(), aes(x = Amount, y = Type)) +
      geom_bar(stat = "identity", fill = "wheat2", color = "#f7f7e8") +
      geom_text(aes(label = paste0("$", scales::number(Amount, scale = 1, accuracy = 1))),
                hjust = 1.2, color = "#585858", size = 5, 
                family = "Verdana", fontface = "bold") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#f7f7e8", color = "#f7f7e8"),
        panel.background = element_rect(fill = "#f7f7e8", color = "#f7f7e8"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12, color = "#585858", face = "bold", family = "Verdana"),
        panel.grid = element_blank(),
        text = element_text(family = "Verdana", color = "#585858", face = "bold")
      )
    
    
  })
  
  
  output$stockIncrease <- renderPlot({
    
    req(length(input$stocks) > 0)
    req(input$investment > 0)
    # Assuming `dataInput()$combined_data` is sorted by `Percentage_Gain` in the desired order
    
    data <- dataInput()$combined_data
    
    # Reorder the levels of the Stock factor based on the sorted Percentage_Gain
    data$Stock <- factor(data$Stock, levels = data$Stock[order(data$Percentage_Gain)])
    
    # Calculate the number of bars
    num_bars <- nlevels(data$Stock)
    
    # Dynamically set text size based on the number of bars
    text_size <- ifelse(num_bars <= 3, 5, ifelse(num_bars <= 5, 4.5, ifelse(num_bars <= 7, 4, 3.5)))
    text_size2 <- ifelse(num_bars <= 3, 13.5, ifelse(num_bars <= 5, 12.5, ifelse(num_bars <= 7, 11, 10)))
    
    # Now plot using ggplot2
    ggplot(data, aes(x = Stock, y = Percentage_Gain, fill = Stock)) +
      geom_bar(stat = "identity", color = "#f7f7e8") +
      geom_text(aes(label = paste0(scales::number(Percentage_Gain, scale = 1, accuracy = 1), "%")), 
                color = "#585858", size = text_size, 
                family = "Verdana", 
                position = position_stack(vjust = 1.2)
                , fontface = "bold") +  # Set font to Verdana, bold, and color to #585858
      scale_fill_manual(values = c("Apple" = "turquoise3", "Microsoft" = "slateblue3", "Google" = "orange", 
                                   "Tesla" = "red3", "Amazon" = "orchid2", "Nike" = "seagreen",
                                   "Ford" = "steelblue1", "Intel" = "salmon4", "Nokia" = "yellow3")) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "#f7f7e8", color = "#f7f7e8"),
            panel.background = element_rect(fill = "#f7f7e8", color = "#f7f7e8"),
            axis.title.x = element_blank(),    # Remove x-axis label
            axis.title.y = element_blank(),    # Remove y-axis label
            axis.text.x = element_text(size = text_size2, color = "#585858", face = "bold", family = "Verdana"),  # Set x-axis text properties
            axis.text.y = element_blank(),     # Remove y-axis numbers
            panel.grid = element_blank(),
            legend.position = "none",          # Hide legend
            text = element_text(family = "Verdana", color = "#585858", face = "bold")  # Apply font, color, and bold to all text elements
      )
    
    
  })
  
  
  output$historicalStocks <- renderPlot({
    
    req(length(input$stocks) > 0)
    req(input$investment > 0)
    
    ggplot(dataInput()$historical_data, aes(x = Date, y = Price, color = Stock)) +
      geom_line(size = 0.5) +
      scale_color_manual(values = c("Apple" = "turquoise3", "Microsoft" = "slateblue3", "Google" = "orange", 
                                    "Tesla" = "red3", "Amazon" = "orchid2", "Nike" = "seagreen",
                                    "Ford" = "steelblue1", "Intel" = "salmon4", "Nokia" = "yellow3")) +
      labs(x = "Date",
           y = "Price",
           color = "Stock") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#f7f7e8", color = "#f7f7e8"),
        panel.background = element_rect(fill = "#f7f7e8", color = "#f7f7e8"),
        panel.grid.major = element_line(color = "gray80", size = 0.25),  # Major gridlines settings
        panel.grid.minor = element_line(color = "gray80", size = 0.25),  # Minor gridlines settings
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold", family = "Verdana", color = "#585858"),
        axis.title = element_text(size = 14, face = "bold", family = "Verdana", color = "#585858"),
        axis.text = element_text(size = 12, face = "bold", family = "Verdana", color = "#585858"),
        legend.title = element_text(size = 12, face = "bold", family = "Verdana", color = "#585858"),
        legend.text = element_text(size = 10, face = "bold", family = "Verdana", color = "#585858"),
        legend.position = "bottom"
      )
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
