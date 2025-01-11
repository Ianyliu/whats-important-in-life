#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(ggplot2)
library(truncnorm)
library(GGally) 
library(shiny)
library(plotly)
library(bslib)
library(gtsummary)
library(DT)
library(gt)
library(tidyverse)
library(cardx)


# Define the UI
shinyUI(fluidPage(
  withMathJax(),
  titlePanel("What's more important in life?"),
  theme = bs_theme(
    bg = "white", 
    fg = "#4E3629",    
    secondary = "#59CBE8", 
    base_font = font_google("Roboto"),
  ),
  
  sidebarLayout(
    sidebarPanel(
      h1("Talent vs Initial Capital vs Risk Seeking"),
      style = "height: 90vh; overflow-y: auto;", 
      h3("Controls"),
      uiOutput("slider_to_anim"),
      sliderInput("speed",
                  "Speed of Animation (in seconds):",
                  min = 0.45,
                  max = 5,
                  value = 1.0),
      sliderInput("n", "Number of Individuals", min = 1, max = 5000, value = 100),
      sliderInput("gamma_alpha", "Gamma Parameter 1 α: (shape of  initial capital)", min = 0, max = 10, value = 3),
      sliderInput("gamma_beta", "Gamma Parameter 2 β: (rate of  initial capital)", min = 0, max = 10, value = 3),
      sliderInput("talent_mu", "Talent Mean μ: (mean of  talent)", min = 0.1, max = 0.9, value = 0.5),
      sliderInput("talent_sd", "Talent Mean σ: (sd of  talent)", min = 0.1, max = 1, value = 0.2),
      sliderInput("risk_mu", "Risk Seeking μ: (mean of risk seeking)", min = 0.1, max = 0.9, value = 0.5),
      sliderInput("risk_sd", "Risk Seeking σ: (sd of risk seeking)", min = 0.1, max = 1, value = 0.2),
      sliderInput("roi", "Return on Investment (After Risk Seek)", min = 0.05, max = 1, value = 0.05),
      sliderInput("talent_mapping", "Talent Mapping (Scale)", min = 0.1, max = 1, value = 0.1),
      sliderInput("capital_mapping", "Capital Mapping (Scale)", min = 0.1, max = 1, value = 0.1),
      sliderInput("good_luck_sd", "Good Luck Probability Standard Deviation", min = 0.1, max = 1, value = 0.1),
      sliderInput("bad_luck_sd", "Bad Luck Probability Standard Deviation", min = 0.1, max = 1, value = 0.1),
      sliderInput("timerange",
                  "Time Range (upper bound of timeslider):",
                  min = 10,
                  max = 150,
                  value = 75
      ),
    ),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Project Description",
                 withMathJax(),
                 style = "height: 90vh; overflow-y: auto;", 
                 h3("Project Introduction", style = "text-align: center;"),
                 br(),
                 tags$img(src = "./R Final Project.drawio.svg", style = "display: block; margin-left: auto; margin-right: auto; width: 80%;"),
                 br(),
                 p("For our simulation project, we aim to investigate the impact of external factors like luck and initial capital on individual success, alongside intrinsic qualities such as talent and risk-taking.  "),
                 p("The app allows players to simulate their model based on their understanding of the world by adjusting the parameters of the distribution. Explore how each factor influences the capital and observe the interactions among factors to determine their correlation with capital. Identify which factors have a greater impact on capital. Additionally, you can examine specific individuals' capital trajectories to understand why their capital value reached a particular level. We hope this app inspires you. Enjoy the game!"),
                 h3("Model Background Settings", style = "text-align: center;"),
                 tags$a(href = "https://arxiv.org/abs/1802.07068", "Link to the reference paper"),
                 p("In our project, we've developed a wealth simulation game
                   that allows individuals to explore how wealth can fluctuate
                   due to various factors, such as talent, luck, initial capital,
                   and risk tolerance. This game is designed to reflect both
                   internal and external influences that can impact one's
                   financial status over time. Each of these main effects not
                   only contributes to the outcome but also interacts with the
                   others, creating a dynamic model of wealth evolution."),
                 p("Here’s how each factor plays a role:"),
                 tags$ul(
                   tags$li(tags$b("Initial Capital:"), " This represents an
                           individual’s starting financial condition, analogous
                           to family wealth in real life. Those with higher
                           initial capital have a greater probability of
                           encountering good luck, whereas those with less
                           capital are more prone to bad luck."),
                   tags$li(tags$b("Talent Level:"), " This determines a person's
                           capability to capitalize on opportunities. Higher
                           talent increases the likelihood of leveraging good
                           luck and reduces the chances of succumbing to bad
                           luck."),
                   tags$li(tags$b("Luck:"), " Each instance of luck can either
                           amplify or diminish the proportion of capital wagered
                           in the game, directly affecting the player's wealth.
                           Luck depends on two aspects: whether they encounter
                           good luck (1 or 0) and the ability to seize luck (1
                           or 0). Only when both conditions are met does the
                           individual experience luck."),
                   tags$li(tags$b("Risk Tolerance:"), " This influences the
                           amount of capital a player is willing to risk. Higher
                           risk tolerance leads to larger bets, and lower
                           tolerance results in more conservative betting.")
                 ),
                 p("In our model, attributes such as talent level, the
                   likelihood of good or bad luck, initial capital, and risk
                   tolerance are quantified based on specific distributions.
                   While the values for initial capital, talent, and risk
                   tolerance remain constant for each individual, the
                   probability of experiencing good or bad luck changes at each
                   simulation step, adding a layer of unpredictability and
                   realism to the game. This setup allows players to see how
                   wealth could potentially evolve under different scenarios and
                  decisions."),
                 p("The \"Parameter Distribution\" section: It
                           displays the initial distribution of four key factors
                           for the population: initial capital, final capital,
                           talent, and risk-seeking. You can modify the
                           distribution's shape and rate using the sidebar
                           controls based on your understanding of the world.
                           Below that, there are four corresponding boxplots
                           that present detailed population information for each
                           factor, offering a clear overview of the population's
                           characteristics based on your parameter settings."),
                 p("Here is the instruction for parameter adjusting and animation setting through the sidebar on the left: "),
                 tags$ul(
                   tags$li(tags$b("Animation:"), " To control the animation condition such as start or pause for better observe the outcome on the time t.  The range is from one to the maximum time in the time range setting."),
                   tags$li(tags$b("Speed:"), " To control the animation speed."),
                   tags$li(tags$b("Time Range:"), " To set the time range for
                           your simulation game."),
                   tags$li(tags$b("Gamma Parameter 1 (Alpha, Shape of Initial Capital):"),
                           "Adjust the shape value for the initial capital distribution for the population based on your understanding of the world. A larger alpha value concentrates the distribution, resulting in a symmetrical and balanced spread of initial capital values across the population. The capital values would be more evenly distributed throughout the range, preventing extreme concentrations or dispersions"),
                   tags$li(tags$b("Gamma Parameter 2 (Beta, Rate of Initial Capital):"),
                           " Adjust the rate value for the initial capital distribution for the population based on your understanding of the world. A larger rate value concentrates the distribution around lower capital values, leading to a tighter clustering near the origin. This adjustment helps ensure that capital values are more evenly spread throughout the range, minimizing the occurrence of extreme concentrations or dispersions"),
                   tags$li(tags$b("Talent Mean (Mean of Talent):"), " Adjust the average talent level of the population by modifying the talent mean. A higher talent mean indicates a higher average talent within the population. Individuals with higher talent are more likely to capitalize on good opportunities, whereas those with lower talent may be more prone to encountering setbacks."),
                   tags$li(tags$b("Talent Variance (Standard Deviation of Talent):"),
                           " Adjust the scale of talent spread within the population. A higher standard deviation indicates a wider distribution of talent levels, reflecting greater diversity in skills and abilities among individuals. Conversely, a lower standard deviation suggests a more uniform talent distribution, with less variation between individuals."),
                   tags$li(tags$b("Risk Seeking (Mean of Risk Seeking):"),
                           "Adjust the mean value of risk-seeking behavior in the population. Increasing the mean value raises the average level of risk tolerance among individuals. An individual with high risk-seeking behavior would increase the proportion of capital bet on an event."),
                   tags$li(tags$b("Risk Seeking (SD of Risk Seeking): "),
                           "Adjust the standard deviation (SD) to control the variability of risk-seeking behavior in the population. A higher SD indicates a broader range of risk-seeking levels among individuals, signifying greater diversity in their willingness to take risks. Conversely, a lower SD suggests that the population's risk-seeking behaviors are more uniform, with minimal variation between individuals."),
                   tags$li(tags$b("Return on Investment(After Risk Seek): "),
                           "Adjust the return rate for each unit of betting capital on each event. A higher return rate results in higher pay-offs for people's investments when they capitalize on good opportunities but also leads to higher losses when they encounter unfavorable outcomes."),
                   ),
                 h3("Model Equations", style = "text-align: center;"),
                 p("The following equations describe the capital evolution in the simulation:"),
                 tags$div("$$
                    \\begin{aligned}
                    \\textbf{Model Parameters:} \\\\
                    n &= 1000 \\quad \\text{(Number of individuals)} \\\\
                    \\mu_{\\text{talent}} &= 0.5, \\, \\sigma_{\\text{talent}} = 0.2 \\quad \\text{(Truncated normal distribution for talent)} \\\\
                    \\mu_{\\text{risk-seeking}} &= 0.5, \\, \\sigma_{\\text{risk-seeking}} = 0.2 \\quad \\text{(Truncated normal distribution for risk-seeking)} \\\\
                    \\text{Initial Capital: } C_{i} &\\sim \\text{Gamma}(\\text{shape} = 3, \\text{scale} = 3) \\\\
                    \\text{Capital Scaling Factor: } \\text{capital_mapping} &= 0.1 \\\\
                    \\text{Talent Scaling Factor: } \\text{talent_mapping} &= 0.1 \\\\
                    \\text{Return on Investment: } \\text{roi} &= 0.05 \\\\
                    \\sigma_{\\text{good luck}} &= 0.1, \\, \\sigma_{\\text{bad luck}} = 0.1 \\\\
                    \\textbf{Capital Update for Individual } i: \\\\
                    \\text{Capital}_{i, t+1} &= \\text{Capital}_{i, t} \\cdot (1 - r_{i}) + \\text{Capital}_{i, t} \\cdot r_{i} \\cdot \\Big( 1 + \\text{roi} \\cdot \\text{Up}_{i} - \\text{roi} \\cdot \\text{Down}_{i} \\Big) \\\\
                    \\textbf{Components of Capital Update:} \\\\
                    \\text{Up}_{i} &= S_{G_i} \\cdot G_{i} \\quad \\text{(Good luck seized)} \\\\
                    \\text{Down}_{i} &= S_{B_i} \\cdot B_{i} \\quad \\text{(Bad luck seized)} \\\\
                    \\textbf{Luck and Seizure Mechanisms:} \\\\
                    \\text{Good Luck Seized: } S_{G_i} &\\sim \\text{Binomial}\\left(1, \\left((T_i - \\mu_{\\text{talent}}) \\cdot \\text{talent_mapping} + 0.5\\right)\\right) \\\\
                    \\text{Bad Luck Seized: } S_{B_i} &\\sim \\text{Binomial}\\left(1, \\left((\\mu_{\\text{talent}} - T_i)\\cdot \\text{talent_mapping} + 0.5\\right)\\right) \\\\
                    \\text{Good Luck: }G_{i} &\\sim N\\left(\\frac{\\text{Capital}_{i, t} - \\min(\\text{Capital})}{\\max(\\text{Capital}) - \\min(\\text{Capital})} \\cdot \\text{capital_mapping} + 0.5, \\sigma_{\\text{good luck}}^2\\right) \\\\
                    \\text{Bad Luck: }B_{i} &\\sim N\\left(1 - \\frac{\\text{Capital}_{i, t} - \\min(\\text{Capital})}{\\max(\\text{Capital}) - \\min(\\text{Capital})} \\cdot \\text{capital_mapping} + 0.5, \\sigma_{\\text{bad luck}}^2\\right) \\\\
                    \\textbf{Distributions:} \\\\
                    \\text{Talent: } T_{i} &\\sim N(\\mu_{\\text{talent}}, \\sigma_{\\text{talent}}^2)  \\\\
                    \\text{Risk-seeking: }r_{i} &\\sim N(\\mu_{\\text{risk-seeking}}, \\sigma_{\\text{risk-seeking}}^2) \\\\
                    \\textbf{Initialization:} \\\\
                    \\text{At } t = 0, \\, \\text{Capital}_{i, 0} &= C_{i} \\\\
                    \\end{aligned}
                          $$"),
                 h3("Outcome Display Format", style = "text-align: center;"),
                 p("Our simulation results are displayed across nine sections, each illustrating the effects of various factors within our model."),
                 p('In the "Overall Regression" section, two regression models and three partial regression plots are displayed:'),
                 tags$ul(
                   tags$li(tags$b("First regression model:"), " it focuses on the independent effects of variables. X_axis: the factors, Y_axis: The coefficient value. It illustrates how the coefficients of the main effect variables change over time."),
                   tags$li(tags$b("Second regression model:"), " it incorporates interaction terms and functions similarly to the first, enhancing our understanding of the complex interplay between factors."),
                   tags$li(tags$b("Three partial plots:"), " they provide insights into the correlation between each factor and capital. "),
                 ),
                 p("In the 'Risk Seeking, Talent, Initial Capital' sections, three plots are displayed to comprehensively illustrate the influence of each factor:"),
                 tags$ul(
                   tags$li(tags$b("Plot 1:"), "The x-axis represents time, while the y-axis displays the factor values. The points within the plot indicate the type of luck each individual experiences at time t. Throughout the animation, you can observe how individuals with varying levels of these factors change their capital values at each point in time."),
                   tags$li(tags$b("Plot 2(Regression Model with Only Risk Seeking):"), "The x-axis represents the factor value, while the y-axis shows the capital value at time t. The points in the plot indicate the condition of each individual (x = factor level, y = capital value), with the color representing the individual’s talent level. This plot allows you to observe the correlation between the factor and capital, as well as the distribution trend of talent."),
                   tags$li(tags$b("Plot 3(Top 10 Most & Least Risk-Seeking Individuals): "), "This plot illustrates the relationship between factor rank, capital, and the luck experienced by individuals at time t. Individuals are ranked based on their factor level, with the top 10 and bottom 10 in terms of factor highlighted. The x-axis shows the rank level, while the y-axis displays the capital level at time t. The numbers displayed on the columns indicate each individual's index. For more detailed information about these individuals, refer to the \"Individual\" section using these index numbers. "),
                 ),
                 p("In the 'Luck' section, two pairs of plots are presented."),
                 tags$ul(
                   tags$li(" The first pair, situated on the top row, displays data on the population's good luck occurrences alongside instances where this good luck was capitalized upon. "),
                   tags$li(" The second pair, on the bottom row, illustrates data on bad luck occurrences and the instances where bad luck was mitigated."),
                   tags$li(tags$b(" Plots(Cumulative Number of #Luck at time #):"), "The x-axis represents the cumulative number of good luck encounters in the population, and the y-axis represents the probability density value for each count of luck."),
                 ),
                 p("In ”Individual” section, there are three plots."),
                 tags$ul(
                   tags$li(tags$b("Plot 1(Individual Trajectory): "),
                           "You can select an individual by using their index number, as shown on the rank plot in each factor's section, to observe the individual's capital trajectory on the plot."),
                   tags$li(tags$b("Plot 2(Regression Model with Only Risk Seeking):"),
                           "To see the summary statistics for the individual."),
                   tags$li(tags$b("Plot 3(Top 10 Most & Least Risk-Seeking Individuals): "),
                           "This plot allows you to examine the details of an individual's factors at each time t. Use the up arrow to sort the data in ascending order or the down arrow for descending order."),
                 ),
                 p("In “Dataset” section, You can view information about the population's factors at different time t, and sort the list by various variables displayed in the first row. Click the up arrow for ascending order or the down arrow for descending order."),
                 p("In “Summary Statistics” section: It shows the population’s summary statistics for all factors. "),
        ),
        tabPanel("Parameter Distribution", 
                 style = "height: 90vh; overflow-y: auto;", 
                 verbatimTextOutput("Distribution of Parameters"),
                 plotOutput("parameter_histograms"),
                 plotOutput("summary_boxplots")
        ),
        tabPanel("Overall Regression", 
                 style = "height: 90vh; overflow-y: auto;", 
                 p("Regression Model with All Main Effects"),
                 plotOutput("lm1_plot"),
                 p("Regression Results"),
                 verbatimTextOutput("lm1_print"),
                 p("Stepwise Regression Model with a Maximum of 2 Interactions"),
                 verbatimTextOutput("stepwise_lm_print"),
                 plotOutput("step_lm_plot"),
                 plotOutput("effectsplot", height = "1300px"),
        ),
        tabPanel("Risk Seeking", 
                 style = "height: 90vh; overflow-y: auto;",
                 br(),
                 plotlyOutput("plot3"),
                 p("Regression Model with Only Risk Seeking"),
                 plotOutput("regress_plot3"),
                 h2("Top 10 Most & Least Risk-Seeking Individuals"),
                 p("Numbers above bars indicate individual IDs"),
                 plotOutput("risk_seek_bar_plot"),
        ),
        tabPanel("Talent", 
                 style = "height: 90vh; overflow-y: auto;", 
                 br(),
                 plotlyOutput("plot2"),
                 p("Regression Model with Only Talent"),
                 plotOutput("regress_plot2"),
                 h2("Top 10 Most & Least Talented"),
                 p("Numbers above bars indicate individual IDs"),
                 plotOutput("talent_bar_plot"),
        ),
        tabPanel("Capital", 
                 style = "height: 90vh; overflow-y: auto;", 
                 br(),
                 plotlyOutput("plot1"),
                 p("Regression Model with Only Initial Capital"),
                 plotOutput("regress_plot1"), 
                 h2('Top 10 Most & Least "Rich"'),
                 p("Numbers above bars indicate individual IDs"),
                 plotOutput("capital_bar_plot")
                 
        ),
        tabPanel("Luck", 
                 style = "height: 90vh; overflow-y: auto;", 
                 br(),
                 p("Good Luck"),
                 plotOutput("good_luck_plots"),
                 p("Bad Luck"),
                 plotOutput("bad_luck_plots"),
        ),
        tabPanel("Individual", 
                 style = "height: 90vh; overflow-y: auto;", 
                 numericInput("selected_individual", "Select Individual:", min = 1, max = 1000, value = 1),
                 plotOutput("individual_over_time"),
                 verbatimTextOutput("print_individual_parameters"),
                 gt_output(outputId = "individual_summary_table"),
                 br(),
                 downloadButton("downloadIndData", "Download Individual Data"),
                 br(),
                 DTOutput(outputId = "print_individual_table"),
                 br(),
        ),
        tabPanel("Dataset", 
                 h4("Dataset:"),
                 downloadButton("downloadData", "Download Data"),
                 br(),
                 br(),
                 DTOutput("results_table")
        ),
        tabPanel("Summary Statistics", 
                 style = "height: 90vh; overflow-y: auto;", 
                 br(),
                 h4("Summary Statistics"),
                 p("Please be patient and wait for the table to load"),
                 gt_output(outputId = "summary_table")
        ),
        tabPanel("Bivariate Plots", 
                 style = "height: 90vh; overflow-y: auto;", 
                 br(),
                 h4("Scatterplot Matrix"),
                 p("Please be patient and wait for the plot to load"),
                 plotOutput("scatterplot_matrix"),
                 h4("Correlation Heatmap"),
                 plotOutput("corr_heatmap")
        ),
      )
    )
  ),
  tags$footer(
    div(style = "background-color: #4E3629; padding: 10px;",
        p("© 2024 Brown University - School of Public Health", 
          style = "color: #FFFFFF; text-align: center; font-size: 14px;")
    )
  )
))

