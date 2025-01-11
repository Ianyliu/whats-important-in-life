#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(ggplot2)
library(truncnorm)
library(GGally) 
library(gtsummary)
library(gt)
library(cardx)
library(gridExtra)
library(effects)

plotly_legend_font_size <- 18

shinyServer(function(input, output, session) {
  
  # Reactive Function to Generate Risk-Seeking Probability
  # Uses truncated normal distribution constrained between 0 and 1
  # Parameters:
  # - input$n: Number of simulations
  # - input$risk_mu: Mean of the risk-seeking distribution
  # - input$risk_sd: Standard deviation of the risk-seeking distribution
  risk_seek <- reactive({
    rtruncnorm(input$n, a=0, b=1, mean = input$risk_mu, sd = input$risk_sd)
  })
  
  # Reactive Function to Generate Risk-Seeking Probability
  # Uses truncated normal distribution constrained between 0 and 1
  # Parameters:
  # - input$n: Number of simulations
  # - input$risk_mu: Mean of the risk-seeking distribution
  # - input$risk_sd: Standard deviation of the risk-seeking distribution
  init_capital <- reactive({
    rgamma(input$n, shape = input$gamma_alpha, scale = input$gamma_beta)
  })
  # Reactive Function to Generate Talent
  # Uses truncated normal distribution constrained between 0 and 1
  # Parameters:
  # - input$n: Number of simulations
  # - input$talent_mu: Mean of the talent distribution
  # - input$talent_sd: Standard deviation of the talent distribution
  talent <- reactive({
    rtruncnorm(input$n, a=0, b=1, mean = input$talent_mu, sd = input$talent_sd)
  })
  # Render Full Results Table
  # Displays the complete dataset with initial parameters
  output$results_table <- DT::renderDT({
    full_df()
  })
  
  # Function to Extract and Reshape Individual Data
  # Converts wide-format data to long-format for a specific individual
  # Parameters:
  # - df: Full dataframe
  # - individual_id: ID of the individual to extract
  # Returns: A long-format dataframe with time-series information
  extract_individual_data <- function(df, individual_id) {
    # Extract data for the specified individual
    individual_data <- df[individual_id, ]
    
    # Extract time points from column names
    time_points <- seq(1, input$timerange)

    # Initialize an empty long-format data frame
    long_data <- data.frame(
      time = unique(time_points),
      capital = numeric(length(unique(time_points))),
      good_n = numeric(length(unique(time_points))),
      good_seize_n = numeric(length(unique(time_points))),
      bad_n = numeric(length(unique(time_points))),
      bad_seize_n = numeric(length(unique(time_points))),
      luck = character(length(unique(time_points)))
    )
    
    # Populate the long-format data frame by extracting data for each time point
    for (t in seq_along(long_data$time)) {
      time <- long_data$time[t]
      long_data$capital[t] <- individual_data[[paste0("capital_", time)]]
      long_data$good_n[t] <- individual_data[[paste0("good_n", time)]]
      long_data$good_seize_n[t] <- individual_data[[paste0("good_seize_n", time)]]
      long_data$bad_n[t] <- individual_data[[paste0("bad_n", time)]]
      long_data$bad_seize_n[t] <- individual_data[[paste0("bad_seize_n", time)]]
      long_data$luck[t] <- as.character(individual_data[[paste0("luck_", time)]])
    }
    
    return(long_data)
  }
  
  # Reactive Function to Extract Individual Data
  # Uses the extract_individual_data function to get data for a selected individual
  individual_data <- reactive({
    df <- results_df()
    extract_individual_data(df, input$selected_individual)
  })
  
  # Render Individual Parameters
  # Prints key parameters and results for a selected individual
  output$print_individual_parameters <- renderPrint({
    paste("Individual #", input$selected_individual, "Parameters:",
          "Initial Capital=", round(init_capital()[input$selected_individual], digits = 2),
    "Final capital = ", 
    round(individual_data()$capital[length(individual_data()$capital)], digits = 2),
    "Initial Talent=", round(talent()[input$selected_individual], digits = 2), 
    "Risk Seeking Probability=", 
    round(risk_seek()[input$selected_individual], digits = 2))
  })
  
  # Render Individual Data Table
  output$print_individual_table <- DT::renderDT({
    individual_data()
  })
  
  # Plot Individual Capital Over Time
  # Creates a line plot showing an individual's capital trajectory
  output$individual_over_time <- renderPlot({
    df <- individual_data()
    
    ggplot(df, aes(x = time, y = capital, 
                   )) +
      geom_line() +
      geom_point() +
      geom_smooth(se = F) +
      theme_minimal() +
      labs(
        x = "Time",
        y = "Capital",
        title = paste("Individual", input$selected_individual, "Capital Over Time")
      )
  })
  
  # Render Individual Summary Statistics
  # Uses gtsummary to create a comprehensive summary table for an individual
  output$individual_summary_table <-
    render_gt(
      individual_data() |>
        # Summarize statistics for continuous and categorical variables
        tbl_summary(
          statistic = list(all_continuous() ~ 
                             "{mean} \n {median} ({p25}, {p75}) \n {sd}", 
                           all_categorical() ~
                             "{n} ({p}%)"),
        ) %>%
        add_stat_label() |>
        add_n() |>
        add_ci() |>
        # CONVERT TO A {gt} TABLE! VERY IMPORTANT STEP!
        as_gt() %>%
        tab_header(md("**Table 1. Summary Statistic for Individual**"))
    )
  
  # Reactive Function to Create Full DataFrame
  # Combines initial parameters with simulation results
  full_df <- reactive({
    df <- results_df()
    # Add initial parameters as columns
    df <- cbind(init_capital = init_capital(), talent = talent(), risk_seek = risk_seek(), df)
    df
  })
  
  # Render Summary Statistics Table
  # Creates a summary table for the entire dataset at a specific time point
  output$summary_table <-
    render_gt(
      full_df() |>
        select(init_capital, talent,risk_seek,
               paste0("capital_", input$timeslider), 
               paste0("luck_", input$timeslider), 
               paste0("good_n", input$timeslider),
               paste0("bad_n", input$timeslider),
               paste0("good_seize_n", input$timeslider),
               paste0("bad_seize_n", input$timeslider),
        )  |>
        # Summarize statistics for continuous and categorical variables
        tbl_summary(
          statistic = list(all_continuous() ~ 
                             "{mean} \n {median} ({p25}, {p75}) \n {sd}", 
                           all_categorical() ~
                             "{n} ({p}%)"),
        ) %>%
        add_stat_label() |>
        add_n() |>
        add_ci() |>
        # CONVERT TO A {gt} TABLE! VERY IMPORTANT STEP!
        as_gt() %>%
        tab_header(md("**Table 1. Summary Statistics**"))
    )
  
  # Parameter Distributions Histogram
  # Creates histograms to compare simulated and theoretical distributions
  output$parameter_histograms <- renderPlot({
    # Get final capital from results
    final_capital <- results_df()[[paste0("capital_", input$timerange)]]
    # Set up a 2x2 plot grid
    par(mfrow = c(2, 2))
    # Initial Capital Distribution
    hist(init_capital(), breaks = "FD", freq = FALSE, col = "lightblue", cex.lab=1.5, cex.main=1.5, cex.sub=1.5)
    # Overlay theoretical gamma distribution
    curve(dgamma(x, shape = input$gamma_alpha, scale = input$gamma_beta), col = "red", lwd = 2, add = TRUE)
    # Final Capital Distribution
    hist(final_capital, breaks = "FD", freq = FALSE, col = "lightblue", cex.lab=1.5, cex.main=1.5, cex.sub=1.5)
    # Talent Distribution
    hist(talent(), breaks = "FD", freq = FALSE, col = "lightblue", cex.lab=1.5, cex.main=1.5, cex.sub=1.5)
    # Overlay theoretical truncated normal distribution
    curve(truncnorm::dtruncnorm(x, a=0, b=1, mean = input$talent_mu, sd = input$talent_sd), add = TRUE, col = "red")
    # Risk-Seeking Distribution
    hist(risk_seek(), breaks = "FD", freq = FALSE, col = "lightblue", cex.lab=1.5, cex.main=1.5, cex.sub=1.5)
    # Overlay theoretical truncated normal distribution
    curve(truncnorm::dtruncnorm(x, a=0, b=1, mean = input$risk_mu, sd = input$risk_sd), add = TRUE, col = "red")
  })
  
  # Summary Boxplots
  # Creates boxplots to visualize distributions of key parameters
  output$summary_boxplots <- renderPlot({
    # Select relevant columns for visualization
    df <- full_df() |>
      select(init_capital, talent,risk_seek)
    # Get final capital
    final_capital <- results_df()[[paste0("capital_", input$timerange)]]
    
    # Create boxplots for initial capital
    boxplot_1 <- ggplot(df, aes(x = init_capital, y = "Initial Capital")) + 
      geom_boxplot(color="blue", outlier.shape=NA) + 
      geom_jitter(color="purple", size=2, width=0.2, alpha = 0.5) + 
      labs(x="Initial Capital", title="Initial Capital Distribution",
           y = "") + 
      theme_minimal()
    # Create boxplots for final capital
    boxplot_2 <- ggplot(df, aes(x = final_capital, y = "Final Capital")) + 
      geom_boxplot(color="blue", outlier.shape=NA) + 
      geom_jitter(color="purple", size=2, width=0.2, alpha = 0.5) + 
      labs(x="Final Capital", title="Final Capital Distribution",
           y = "") + 
      theme_minimal()
    
    # Create boxplots for risk-seeking probability
    boxplot_3 <- ggplot(df, aes(x = risk_seek, y = "Initial Risk Seeking")) + 
      geom_boxplot(color="blue", outlier.shape=NA) + 
      geom_jitter(color="purple", size=2, width=0.2, alpha = 0.5) + 
      labs(x="Initial Risk Seeking", title="Initial Risk Seeking Distribution",
           y = "") + 
      theme_minimal()
    
    # Create boxplots for talent
    boxplot_4 <- ggplot(df, aes(x = talent, y = "Initial Talent")) + 
      geom_boxplot(color="blue", outlier.shape=NA) + 
      geom_jitter(color="purple", size=2, width=0.2, alpha = 0.5) + 
      labs(x="Initial Talent", title="Initial Talent Distribution",
           y = "") + 
      theme_minimal()
    
    # Arrange boxplots in a 2x2 grid
    grid.arrange(boxplot_1, boxplot_2, boxplot_3, boxplot_4, nrow = 2)
    
  })
  
  # Correlation Heatmap Visualization
  # Creates a correlation heatmap for selected variables at a specific time point
  output$corr_heatmap <- renderPlot({
    # Select relevant columns for correlation analysis
    df <- full_df() |>
      select(init_capital, talent,risk_seek,
             paste0("capital_", input$timeslider),
             paste0("luck_", input$timeslider),
             paste0("good_seize_n", input$timeslider),
             paste0("bad_seize_n", input$timeslider),
      )
    # Generate correlation heatmap using ggally package
    # Uses RdBu color palette (Red-Blue) to show positive and negative correlations
    # Adds correlation values as labels
    ggcorr(df, palette = "RdBu", label = TRUE)
    
  })
  
  # Scatterplot Matrix Visualization
  # Creates an interactive scatterplot matrix to explore relationships between variables
  output$scatterplot_matrix <- renderPlot({
    # Select key variables for visualization
    df <- full_df() |>
      select(init_capital, talent,risk_seek,
             paste0("capital_", input$timeslider), 
             paste0("luck_", input$timeslider)
      ) 
    # Generate scatterplot matrix using ggpairs
    # Includes:
    # - Smooth trend lines for continuous variables
    # - Color coding based on luck at current time point
    ggpairs(df,
            # Specify columns to include in the matrix
            columns = c("init_capital", "talent", "risk_seek", 
                        paste0("capital_", input$timeslider) 
                        ),
            # Use smooth trend lines for continuous variable relationships
            lower = list(continuous = "smooth"),
            # Color and transparency based on luck factor
            aes(
              color = df[[paste0("luck_", input$timeslider)]],alpha = 0.5)
            ) + theme_minimal()
  })
  
  # Data Download Handler for Full Simulation Data
  # Allows users to download the entire simulation dataset as a CSV
  output$downloadData <- downloadHandler(
    # Generate filename with current date
    filename = function() {
      paste("simulation_data", Sys.Date(), ".csv", sep = "")
    },
    # Write full dataset to the specified file
    content = function(file) {
      write.csv(full_df(), file)
    }
  )
  
  # Data Download Handler for Individual Simulation Data
  # Allows users to download data for a specific individual
  output$downloadIndData <- downloadHandler(
    # Generate filename with individual ID and current date
    filename = function() {
      paste("simulation_individual_", input$selected_individual, "-", 
            Sys.Date(), ".csv", sep = "")
    },
    # Write individual data to the specified file
    content = function(file) {
      write.csv(individual_data(), file)
    }
  )

  # Dynamic Time Slider with Animation Options
  # Creates an interactive slider that can be animated over time
  output$slider_to_anim <- renderUI({
    # Generate slider input with animation capabilities
    sliderInput("timeslider",
                "Time/Animation",
                min = 1,
                max = input$timerange,
                value = input$timerange,
                # Set animation interval based on user-defined speed
                animate = animationOptions(interval = input$speed * 1000))
  })
  
  # Primary Simulation Results Generator
  # Generates a comprehensive dataset simulating individual capital trajectories
  results_df <- reactive({
    
    print("Simulating from time = 1")
    # Initialize dataframe with initial conditions
    df <- data.frame(matrix(nrow = input$n, ncol = 0))
    df[[paste0("capital_", 1)]] <- init_capital()
    df[[paste0("good_n", 1)]] <- rep(0, input$n)
    df[[paste0("bad_n", 1)]] <- rep(0, input$n)
    df[[paste0("good_seize_n", 1)]] <- rep(0, input$n)
    df[[paste0("bad_seize_n", 1)]] <- rep(0, input$n)
    df[[paste0("luck_", 1)]] <- rep("neither", input$n)
    # Initialize tracking variables
    capitals <- matrix(NA, nrow = input$n, ncol = input$timerange)
    capital <- init_capital()
    good_n <- rep(0, input$n)
    bad_n <- rep(0, input$n)
    good_seize_n <- rep(0, input$n)
    bad_seize_n <- rep(0, input$n)
    
    # Simulate capital trajectory over multiple time points
    for(i in 2:input$timerange){
      # Calculate probabilities based on current capital distribution
      capital_pr <- rank(capital) - 0.5
      capital_pr <- (capital_pr / length(capital))
      # Set seed for reproducibility
      set.seed(42)
      # Generate luck probabilities (good and bad events)
      # Probabilities are a function of capital distribution
      good_p <- rtruncnorm(input$n, a=0, b=1, 
                           mean = (capital - min(capital)) / (max(capital) - min(capital)) * input$capital_mapping + 0.5, 
                           sd = input$good_luck_sd)   # Luck is a function of capital
      bad_p <- rtruncnorm(input$n, a=0, b=1,
                          mean = 1- ((capital - min(capital)) / (max(capital) - min(capital)) * input$capital_mapping + 0.5), 
                          sd = input$bad_luck_sd)   
      # print(bad_p[bad_p >=1 | bad_p <=0])
      
      # Determine occurrence of good and bad events
      good <- rbinom(n = input$n, size = 1, prob = good_p)
      bad <- rbinom(n = input$n, size = 1, prob = bad_p)
      
      # Generate seizing probabilities based on talent
      good_seize <- rbinom(n = input$n, size = 1, prob = (talent() - input$talent_mu) * input$talent_mapping + 0.5)
      bad_seize <- rbinom(n = input$n, size = 1, prob = (input$talent_mu - talent()) * input$talent_mapping + 0.5)
      # Update event counters
      good_n <- good_n + good
      bad_n <- bad_n + bad
      
      # Calculate capital changes based on events and risk-seeking behavior
      up <- good_seize * good
      down <- bad_seize * bad
      good_seize_n <- good_seize_n + up
      bad_seize_n <- bad_seize_n + down
      # Modify capital based on risk-seeking and events
      capital <- capital * (1 - risk_seek()) + capital * risk_seek() * (1 + input$roi * up - input$roi * down)
      capital[capital < 0] <- 0
      # Store capital trajectory
      capitals[, i] <- capital
      # set to 0 if capital <0 
      # Update dataframe with simulation results
      df[[paste0("capital_", i)]] <- capital
      df[[paste0("luck_", i)]] <- ifelse(
        good == 1 & bad == 1, "both",
        ifelse(good == 1, "good",
               ifelse(bad == 1, "bad", "neither")
        )
      ) |> factor()
      df[[paste0("good_n", i)]] <- good_n
      df[[paste0("bad_n", i)]] <- bad_n
      df[[paste0("good_seize_n", i)]] <- good_seize_n
      df[[paste0("bad_seize_n", i)]] <- bad_seize_n
    }
    print("Simulation Complete")
    df
  })
  

  output$lm1_print <- renderPrint({
    lm1() |> summary()
  })
  
  output$lm2_print <- renderPrint({
    lm2() |> summary()
  })
  
  output$effectsplot <- renderPlot({
    if(input$timeslider > 1) {
    plot(allEffects(lm2()))
    }
  })
  
  # Linear Model for Capital Prediction (Simple Model)
  # Creates a linear regression model at a specific time point
  lm1 <- reactive({
    # Get current time point from slider
    curr_time <- input$timeslider
    # Prepare dataframe with current capital and predictors
    df <- full_df()
    df$cur_capital <- df[[paste0("capital_", curr_time)]]
    df$good_luck_count <- df[[paste0("good_n", curr_time)]]
    df$bad_luck_count <- df[[paste0("bad_n", curr_time)]]
    df$talent
    # Fit linear model predicting current capital
    lm(cur_capital ~
         talent + init_capital  + good_luck_count + bad_luck_count  + good_luck_count + risk_seek,
       data = df)
  })
  
  # Coefficient Plot for Simple Linear Model
  # Visualizes the coefficients and their significance from lm1
  output$lm1_plot <- renderPlot({
    if (input$timeslider > 1) {
      # Extract the coefficients and standard errors
      coefs <- lm1()$coefficients
      se <- summary(lm1())$coefficients[, "Std. Error"]
      
      # Prepare data for plotting
      plot_data <- data.frame(
        variable = names(coefs),
        coef = coefs,
        se = se
      ) |>
        arrange(coefs)
      
      # Sort by absolute value of coefficients
      plot_data <- plot_data[order(abs(plot_data$coef), decreasing = TRUE), ]

      # Create coefficient bar plot with error bars
      coef_plot <- ggplot(plot_data, aes(x = reorder(variable, abs(coef)), y = coef)) +
        geom_bar(aes(fill= ifelse(coef > 0, "positive", "negative")), 
                 stat = "identity",
                 ) +
        geom_errorbar(aes(ymin = coef - se, ymax = coef + se), width = 0.2, 
                      alpha = 0.5,
                      ) +
        theme(
          text = element_text(size = 15),
          axis.text.x = element_text(size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)),
          axis.title.x = element_text(size = rel(1.5)),
          axis.title.y = element_text(size = rel(1.5)),
        ) +
        labs(x = "Variable", y = "Coefficient",
             color = "Positive Or Negative Coefficient",
             fill = "Positive Or Negative Coefficient",
             main = "Adjusted Effects of Each Factor") +
        theme_minimal()
      coef_plot
    } 

  })
  
  output$step_lm_plot <- renderPlot({
    if (input$timeslider > 1) {
      # Extract the coefficients and standard errors
      coefs <- stepwise_lm()$coefficients
      se <- summary(stepwise_lm())$coefficients[, "Std. Error"]
      
      # Create a data frame for plotting
      plot_data <- data.frame(
        variable = names(coefs),
        coef = coefs,
        se = se
      ) |>
        arrange(coefs)
      
      # Sort by absolute value of coefficients
      plot_data <- plot_data[order(abs(plot_data$coef), decreasing = TRUE), ]
      
      # Create the bar plot with error bars
      coef_plot <- ggplot(plot_data, aes(x = reorder(variable, abs(coef)), y = coef)) +
        geom_bar(aes(fill= ifelse(coef > 0, "positive", "negative")), 
                 stat = "identity",
        ) +
        geom_errorbar(aes(ymin = coef - se, ymax = coef + se), width = 0.2, 
                      alpha = 0.5,
                      # color = "darkgrey"
        ) +
        theme(
          text = element_text(size = 15),
          axis.text.x = element_text(size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)),
          axis.title.x = element_text(size = rel(1.5)),
          axis.title.y = element_text(size = rel(1.5)),
        ) +
        labs(x = "Variable", y = "Coefficient",
             color = "",
             fill = "Positive Or Negative Coefficient",
             main = "Adjusted Effects of Each Factor") +
        theme_minimal()
      coef_plot
    } 
    
  })
  
  # Linear Model with Interaction Terms
  # Creates a more complex linear regression model with interaction terms
  lm2 <- reactive({
    curr_time <- input$timeslider
    df <- full_df()
    df$cur_capital <- df[[paste0("capital_", curr_time)]]
    df$good_luck_count <- df[[paste0("good_n", curr_time)]]
    df$bad_luck_count <- df[[paste0("bad_n", curr_time)]]
    df$talent
    # Fit linear model with interaction terms
    lm(cur_capital ~
         talent * init_capital  + good_luck_count + bad_luck_count  + good_luck_count * risk_seek,
       data = df)
  })
  # Stepwise Model Selection
  # Performs automated model selection using both forward and backward steps
  stepwise_lm <- reactive({
    curr_time <- input$timeslider
    df <- full_df()
    df$cur_capital <- df[[paste0("capital_", curr_time)]]
    df$good_luck_count <- df[[paste0("good_n", curr_time)]]
    df$bad_luck_count <- df[[paste0("bad_n", curr_time)]]
    # Start with full model and perform stepwise selection
    lm(cur_capital ~
         talent + init_capital  + good_luck_count + bad_luck_count  + good_luck_count + risk_seek,
       data = df) |>
      stats::step(
        direction = "both", # Allow both forward and backward selection
        scope = . ~ .^2, # Consider all two-way interactions
        trace=0) # Suppress detailed output
  })
  
  output$stepwise_lm_print <- renderPrint({
    stepwise_lm() |> summary()
  })
  
  # Richest Individuals Plot
  # Visualizes the top 10 individuals by capital at a specific time point
  output$richest_plot <- renderPlot({
    # Prepare data: sort by capital at current time point
    df <- full_df()
    df <- df |>
      arrange(desc(df[[paste0("capital_", input$timeslider)]])) |>
      head(10)
    # Create bar plot of top 10 individuals
    ggplot(df, aes(x = reorder(individual, desc(.data[[paste0("capital_", input$timeslider)]])), y = .data[[paste0("capital_", input$timeslider)]], fill = .data[[paste0("luck_", input$timeslider)]], alpha = 0.5)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(
        x = "Individual",
        y = paste("Capital at Time", input$timeslider),
        title = paste("Top 10 Individuals at Time", input$timeslider),
        fill = "Luck"
      )
  })
  
  output$regress_plot1 <- renderPlot({
    curr_time <- input$timeslider
    df <- results_df()
    ggplot(data = df) +
      geom_point(aes(x = init_capital(), y = .data[[paste0("capital_", curr_time)]],
                     color = talent(),
                     )) +
      geom_smooth(aes(x = init_capital(), y = .data[[paste0("capital_", curr_time)]]), method = "lm",
                  color = "black") +
      theme_minimal() +
      geom_text(aes(x = 0.5, y = 0.5, label = paste("β:", round(lm(df[[paste0("capital_", curr_time)]] ~ init_capital())$coefficients[2], digits = 2))),
                color = "brown", size = 5) +
      theme(
        text = element_text(size = 15),
      ) +
      labs(
        x = "Initial Capital",
        size = "Good Luck Count",
        color = "Talent",
        y = paste("Capital at Time", curr_time),
        title = paste("Capital vs Initial Capital at ", curr_time),
      )
  })
  
  output$capital_bar_plot <- renderPlot({
    curr_time <- input$timeslider
    df <- results_df() 
    df$individual_n<-1:nrow(df)
    
    # sort df by capital at time
    df <- df |>
      arrange(desc(df[[paste0("capital_", curr_time)]])) 
      
    df$ranked <- 1:nrow(df)
    head_df <- df |>
      head(10)
    tail_df <- df |>
      tail(10)
    
    

    p1 <- ggplot(data = head_df) +
      geom_col(
        aes(x = ranked, y = .data[[paste0("capital_", curr_time)]], fill = .data[[paste0("luck_", curr_time)]])
      ) +
      geom_text(aes(x = ranked, y = .data[[paste0("capital_", curr_time)]], 
                    label = individual_n,
                    ), vjust = -0.5) +
      theme_minimal() +
      theme(
        text = element_text(size = 15),
      ) +
      labs(
        x = "Rank of Capital  (left = most capital)",
        color = paste("Luck at Time", curr_time),
        fill = paste("Luck at Time", curr_time),
        y = paste("Capital at Time", curr_time),
        title = paste('Top 10 "Richest" Individuals at Time: ', curr_time),
      )
    p2 <- ggplot(data = tail_df) +
      geom_col(
        aes(x = ranked, y = .data[[paste0("capital_", curr_time)]], fill = .data[[paste0("luck_", curr_time)]])
      ) +
      geom_text(aes(x = ranked, y = .data[[paste0("capital_", curr_time)]], 
                    label = individual_n,
                    ), vjust = -0.5) +
      theme_minimal() +
      theme(
        text = element_text(size = 15),
      ) +
      labs(
        x = "Rank of Capital (right = least capital)",
        color = paste("Luck at Time", curr_time),
        fill = paste("Luck at Time", curr_time),
        y = paste("Capital at Time", curr_time),
        title = paste('Top 10 "Poorest" Individuals at Time: ', curr_time),
      )
    
    grid.arrange(p1, p2, nrow = 1)
  })
  
  output$talent_bar_plot <- renderPlot({
    curr_time <- input$timeslider
    
    df <- full_df() 
    
    df$individual_n<-1:nrow(df)
    df$ranked <- row_number(df[[paste0("capital_", curr_time)]])

    df <- df |>
      arrange(desc(talent)) 
    
    df$talent_ranked <- 1:nrow(df)
    
    head_df <- df |>
      head(10)
    
    tail_df <- df |>
      tail(10)
    
    p1 <- ggplot(data = head_df) +
      geom_col(
        aes(x = talent_ranked, y = .data[[paste0("capital_", curr_time)]], fill = .data[[paste0("luck_", curr_time)]])
      ) +
      geom_text(aes(x = talent_ranked, y = .data[[paste0("capital_", curr_time)]], 
                    label = paste(individual_n),
                    ), vjust = -0.5) +
      theme_minimal() +
      theme(
        text = element_text(size = 15),
      ) +
      labs(
        x = "Talent Rank (left = most talented)",
        color = paste("Luck at Time", curr_time),
        fill = paste("Luck at Time", curr_time),
        y = paste("Capital at Time", curr_time),
        title = paste("Top 10 Most Talented Individuals"),
      )
    p2 <- ggplot(data = tail_df) +
      geom_col(
        aes(x = talent_ranked, y = .data[[paste0("capital_", curr_time)]], fill = .data[[paste0("luck_", curr_time)]])
      ) +
      geom_text(aes(x = talent_ranked, y = .data[[paste0("capital_", curr_time)]], 
                    label = paste(individual_n),
      ), vjust = -0.5) +
      theme_minimal() +
      theme(
        text = element_text(size = 15),
      ) +
      labs(
        x = "Talent Rank (right = least talented)",
        color = paste("Luck at Time", curr_time),
        fill = paste("Luck at Time", curr_time),
        y = paste("Capital at Time", curr_time),
        title = paste("Top 10 Least Talented Individuals"),
      )
    grid.arrange(p1, p2, nrow =1)
  })
  
  output$risk_seek_bar_plot <- renderPlot({
    curr_time <- input$timeslider
    
    df <- full_df() 
    
    df$individual_n<-1:nrow(df)
    df$ranked <- row_number(df[[paste0("capital_", curr_time)]])
    df <- df |>
      arrange(desc(risk_seek))
    
    df$risk_seek_ranked <- 1:nrow(df)
    
    head_df <- df |>
      head(10)
    
    tail_df <- df |>
      tail(10)
    
    
    p1 <- ggplot(data = head_df) +
      geom_col(
        aes(x = risk_seek_ranked, y = .data[[paste0("capital_", curr_time)]], fill = .data[[paste0("luck_", curr_time)]])
      ) +
      geom_text(aes(x = risk_seek_ranked, y = .data[[paste0("capital_", curr_time)]], 
                    label = paste(individual_n),
      ), vjust = -0.5) +
      theme_minimal() +
      theme(
        text = element_text(size = 15),
      ) +
      labs(
        x = "Risk Seeking Rank (left = most risk-seeking)",
        color = paste("Luck at Time", curr_time),
        fill = paste("Luck at Time", curr_time),
        y = paste("Capital at Time", curr_time),
        title = paste("Top 10 Most Risk Seeking Individuals"),
      )
    p2 <- ggplot(data = tail_df) +
      geom_col(
        aes(x = risk_seek_ranked, y = .data[[paste0("capital_", curr_time)]], fill = .data[[paste0("luck_", curr_time)]])
      ) +
      geom_text(aes(x = risk_seek_ranked, y = .data[[paste0("capital_", curr_time)]],
                    label = paste(individual_n),
      ), vjust = -0.5) +
    theme_minimal() +
    theme(
      text = element_text(size = 15),
    ) +
    labs(
      x = "Risk Seeking Rank (right = least risk-seeking)",
      color = paste("Luck at Time", curr_time),
      fill = paste("Luck at Time", curr_time),
      y = paste("Capital at Time", curr_time),
      title = paste("Top 10 Least Risk Seeking Individuals"),
    )
    grid.arrange(p1, p2, nrow = 1)
  })

  output$regress_plot2 <- renderPlot({
    curr_time <- input$timeslider
    df <- results_df()
    ggplot(data = df) +
      geom_point(aes(x = talent(), y = .data[[paste0("capital_", curr_time)]],
                     color = talent(),
      )) +
      geom_smooth(aes(x = talent(),, y = .data[[paste0("capital_", curr_time)]]), method = "lm",
                  color = "black") +
      theme_minimal() +
      geom_text(aes(x = 0.5, y = 0.5, label = paste("β:", round(lm(df[[paste0("capital_", curr_time)]] ~ talent())$coefficients[2], digits = 2))),
                color = "brown", size = 5) +
      theme(
        text = element_text(size = 15),
        # axis.text.x = element_text(size = rel(1.0)),
        # axis.text.y = element_text(size = rel(1.0)), 
        # axis.title.x = element_text(size = rel(1.0)),
        # axis.title.y = element_text(size = rel(1.0)),
      ) +
      labs(
        x = "Talent",
        # alpha = "Bad Luck Count",
        size = "Good Luck Count",
        color = "Talent",
        y = paste("Capital at Time", curr_time),
        title = paste("Capital vs Talent at ", curr_time),
      )
  })
  
  output$regress_plot3 <- renderPlot({
    curr_time <- input$timeslider
    df <- results_df()
    ggplot(data = df) +
      geom_point(aes(x = risk_seek(), y = .data[[paste0("capital_", curr_time)]],
                     color = talent(),
      )) +
      geom_smooth(aes(x = risk_seek(),, y = .data[[paste0("capital_", curr_time)]]), method = "lm",
                  color = "black") +
      theme_minimal() +
      geom_text(aes(x = 0.5, y = 0.5, label = paste("β:", round(lm(df[[paste0("capital_", curr_time)]] ~ risk_seek())$coefficients[2], digits = 2))),
                color = "brown", size = 5) +
      theme(
        text = element_text(size = 15),
      ) + 
      labs(
        x = "Risk Seeking Probability",
        size = "Good Luck Count",
        color = "Talent",
        y = paste("Capital at Time", curr_time),
        title = paste("Capital vs Risk Seeking Probability at ", curr_time),
      )
  })
  
  output$good_luck_plots <- renderPlot({
    curr_time <- input$timeslider
    df <- results_df()
    good_luck_plots <- ggplot(data = df) +
      geom_histogram(aes(x = .data[[paste0("good_n", curr_time)]], y=..density..), fill = "lightblue") +
      geom_density(aes(x = .data[[paste0("good_n", curr_time)]], y=..density..), color = "red") +
      theme_minimal() +
      theme(
        text = element_text(size = 15),
      ) +
      labs(
        x = "Cumulative Number of Good Luck Encounters",
        title = paste("Cumulative Number of Good Luck at Time", curr_time),
      )
    good_luck_seized_plots <- ggplot(data = df) +
      geom_histogram(aes(x = .data[[paste0("good_seize_n", curr_time)]], y=..density..), fill = "lightblue") +
      geom_density(aes(x = .data[[paste0("good_seize_n", curr_time)]], y=..density..,), color = "red") +
      theme_minimal() +
      theme(
        text = element_text(size = 15),
      ) +
      labs(
        x = "Cumulative Number of Good Luck Seizes",
        title = paste("Cumulative Number of Good Luck Seized at Time ", curr_time),
      )
    grid.arrange(good_luck_plots, good_luck_seized_plots, nrow = 1)
  })
  
  output$bad_luck_plots <- renderPlot({
    curr_time <- input$timeslider
    df <- results_df()
    bad_luck_plots <- ggplot(data = df) +
      geom_histogram(aes(x = .data[[paste0("bad_n", curr_time)]], y=..density..), fill = "lightblue") +
      geom_density(aes(x = .data[[paste0("bad_n", curr_time)]], y=..density..,), color = "red") +
      theme_minimal() +
      theme(
        text = element_text(size = 15),
      ) +
      labs(
        x = "Cumulative Number of Bad Luck Encounters",
        title = paste("Cumulative Number of Bad Luck at Time", curr_time),
      )
    bad_luck_seized_plots <- ggplot(data = df) +
      geom_histogram(aes(x = .data[[paste0("bad_seize_n", curr_time)]], y=..density..), fill = "lightblue") +
      geom_density(aes(x = .data[[paste0("bad_seize_n", curr_time)]], y=..density..,), color = "red") +
      theme_minimal() +
      theme(
        text = element_text(size = 15),
      ) +
      labs(
        x = "Cumulative Number of Bad Luck Seizes",
        title = paste("Cumulative Number of Bad Luck Seized at Time ", curr_time),
      )
    grid.arrange(bad_luck_plots, bad_luck_seized_plots, nrow = 1)
  })
    
  output$plot1 <- renderPlotly({
    curr_time <- input$timeslider

    p <- plot_ly(
      data = results_df(),
      y = ~init_capital(),
      x = ~.data[[paste0("capital_", curr_time)]],
      color = ~results_df()[[paste0("luck_", curr_time)]],
      type = 'scatter',
      mode = 'markers',
      text = ~paste0(
        "Good Luck Count:", .data[[paste0("good_n", curr_time)]],
        "<br>Bad Luck Count:", .data[[paste0("bad_n", curr_time)]],
        "<br>Good Seize Count:", .data[[paste0("good_seize_n", curr_time)]],
        "<br>Bad Seize Count:", .data[[paste0("bad_seize_n", curr_time)]],
        "<br> Inital Talent:", round(talent(), digits = 2),
        "<br> Inital Capital:", round(init_capital(), digits = 2),
        "<br> Risk Seek Probability:", round(risk_seek(), digits = 2)
        )
    ) %>%
      layout(
        title = paste("Capital vs Initial Capital at Time", curr_time),
        yaxis = list(title = "Initial Capital"),
        xaxis = list(title = paste("Capital at Time", curr_time)),
        plot_bgcolor  = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        legend = list(font = list(size = plotly_legend_font_size))
      )
    p
  })
  
  output$plot2 <- renderPlotly({
    curr_time <- input$timeslider
    
    p <- plot_ly(
      data = results_df(),
      y = ~talent(),
      x = ~.data[[paste0("capital_", curr_time)]],
      color = ~.data[[paste0("luck_", curr_time)]],
      type = 'scatter',
      mode = 'markers',
      text = ~paste0(
        "Good Luck Count:", .data[[paste0("good_n", curr_time)]],
        "<br>Bad Luck Count:", .data[[paste0("bad_n", curr_time)]],
        "<br>Good Seize Count:", .data[[paste0("good_seize_n", curr_time)]],
        "<br>Bad Seize Count:", .data[[paste0("bad_seize_n", curr_time)]],
        "<br> Inital Talent:", round(talent(), digits = 2),
        "<br> Inital Capital:", round(init_capital(), digits = 2),
        "<br> Risk Seek Probability:", round(risk_seek(), digits = 2)
      )
    ) %>%
      layout(
        title = paste("Capital vs Initial Talent at Time", curr_time),
        yaxis = list(title = "Initial Talent"),
        xaxis = list(title = paste("Capital at Time", curr_time)),
        legend = list(font = list(size = plotly_legend_font_size))
      )
    
    p
  })
  
  output$plot3 <- renderPlotly({
    curr_time <- input$timeslider

    p <- plot_ly(
      data = results_df(),
      x = ~.data[[paste0("capital_", curr_time)]],
      color = ~.data[[paste0("luck_", curr_time)]],
      type = 'scatter',
      mode = 'markers',
      text = ~paste0(
        "Good Luck Count:", .data[[paste0("good_n", curr_time)]],
        "<br>Bad Luck Count:", .data[[paste0("bad_n", curr_time)]],
        "<br>Good Seize Count:", .data[[paste0("good_seize_n", curr_time)]],
        "<br>Bad Seize Count:", .data[[paste0("bad_seize_n", curr_time)]],
        "<br> Inital Talent:", round(talent(), digits = 2),
        "<br> Inital Capital:", round(init_capital(), digits = 2),
        "<br> Risk Seek Probability:", round(risk_seek(), digits = 2)
      )
    ) %>%
      layout(
        title = paste("Capital vs Risk Seeking Probability at Time", curr_time),
        yaxis = list(title = "Risk Seeking (Adventurous or Careful)"),
        xaxis = list(title = paste("Capital at Time", curr_time)),
        legend = list(font = list(size = plotly_legend_font_size))
      )
    
    p
  })


})


