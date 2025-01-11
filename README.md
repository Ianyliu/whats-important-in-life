<h1 align="center">Internal Factor vs External Factor: The Role of Randomness in Success and Failure</h1>

Part of a R Final Project

**Simulation** by Man-Fang Liang, **Report** **& Description** by Shan Su, **R Shiny App** by Ian Liu 

**R SHINY APP LINK:** [https://iantheliu.shinyapps.io/RFinalProj/](https://iantheliu.shinyapps.io/RFinalProj/)


<img width="1000" src="www/R%20Final%20Project.drawio.svg" alt="R Final Project Workflow" align = "center">

Here is the paper source we cite for our project: [https://arxiv.org/abs/1802.07068](https://arxiv.org/abs/1802.07068) 

- This paper attempts to determine whether internal factors, such as talent, have a greater influence on people's success compared to external factors, such as luck. We developed our model based on the existing research but added additional factors, including characteristic risk tolerance and the ability to capitalize on opportunities, to better reflect reality.

**Project Introduction:** 

For our simulation project, we aim to investigate the impact of external factors like luck and initial capital on individual success, alongside intrinsic qualities such as talent and risk-taking.  

The app allows players to simulate their model based on their understanding of the world by adjusting the parameters of the distribution. Explore how each factor influences the capital and observe the interactions among factors to determine their correlation with capital. Identify which factors have a greater impact on capital. Additionally, you can examine specific individuals' capital trajectories to understand why their capital value reached a particular level. We hope this app inspires you. Enjoy the game!

**Model Background Settings:** 

In our project, we've developed a wealth simulation game that allows individuals to explore how wealth can fluctuate due to various factors, such as talent, luck, initial capital, and risk tolerance. This game is designed to reflect both internal and external influences that can impact one's financial status over time. Each of these main effects not only contributes to the outcome but also interacts with the others, creating a dynamic model of wealth evolution.

Here’s how each factor plays a role:

- **Initial Capital**: This represents an individual’s starting financial condition, analogous to family wealth in real life. Those with higher initial capital have a greater probability of encountering good luck, whereas those with less capital are more prone to bad luck.
- **Talent Level**: This determines a person's capability to capitalize on opportunities. Higher talent increases the likelihood of leveraging good luck and reduces the chances of succumbing to bad luck.
- **Luck**: Each instance of luck can either amplify or diminish the proportion of capital wagered in the game, directly affecting the player's wealth. To have luck or not is depends on the two aspect, one is whether he/she meet a good luck(1 or 0) * the ability to seize luck(1 or 0). Only both two situation meets, the individual would have a luck.
- **Risk Tolerance**: This influences the amount of capital a player is willing to risk. Higher risk tolerance leads to larger bets, and lower tolerance results in more conservative betting.

In our model, attributes such as talent level, the likelihood of good or bad luck, initial capital, and risk tolerance are quantified based on specific distributions. While the values for initial capital, talent, and risk tolerance remain constant for each individual, the probability of experiencing good or bad luck changes at each simulation step, adding a layer of unpredictability and realism to the game. This setup allows players to see how wealth could potentially evolve under different scenarios and decisions.

The "Parameter Distribution" section: It displays the initial distribution of four key factors for the population: initial capital, final capital, talent, and risk-seeking. You can modify the distribution's shape and rate using the sidebar controls based on your understanding about the world. Below that, there are four corresponding boxplots that present detailed population information for each factor, offering a clear overview of the population's characteristics based on your parameter settings.

Here is the instruction for parameter adjusting and animation setting through the **sidebar **on the left: 

Animation: To control the animation condition such as start or pause for better observe the outcome on the time t.  The range is from one to the maximum time in the time range setting. 

Speed: To control the animation speed

Time Range: To set the time range for your simulation game

Gamma Parameter 1 (Alpha, Shape of Initial Capital): Adjust the shape value for the initial capital distribution for the population based on your understanding of the world. A larger alpha value concentrates the distribution, resulting in a symmetrical and balanced spread of initial capital values across the population. The capital values would be more evenly distributed throughout the range, preventing extreme concentrations or dispersions

Gamma Parameter 2 (Beta, Rate of Initial Capital): Adjust the rate value for the initial capital distribution for the population based on your understanding of the world. A larger rate value concentrates the distribution around lower capital values, leading to a tighter clustering near the origin. This adjustment helps ensure that capital values are more evenly spread throughout the range, minimizing the occurrence of extreme concentrations or dispersions

Talent Mean (Mean of Talent): Adjust the average talent level of the population by modifying the talent mean. A higher talent mean indicates a higher average talent within the population. Individuals with higher talent are more likely to capitalize on good opportunities, whereas those with lower talent may be more prone to encountering setbacks.

Talent Variance (Standard Deviation of Talent): Adjust the scale of talent spread within the population. A higher standard deviation indicates a wider distribution of talent levels, reflecting greater diversity in skills and abilities among individuals. Conversely, a lower standard deviation suggests a more uniform talent distribution, with less variation between individuals.

Risk Seeking (Mean of Risk Seeking): Adjust the mean value of risk-seeking behavior in the population. Increasing the mean value raises the average level of risk tolerance among individuals. An individual with high risk-seeking behavior would increase the proportion of capital bet on an event.

Risk Seeking (SD of Risk Seeking): Adjust the standard deviation (SD) to control the variability of risk-seeking behavior in the population. A higher SD indicates a broader range of risk-seeking levels among individuals, signifying greater diversity in their willingness to take risks. Conversely, a lower SD suggests that the population's risk-seeking behaviors are more uniform, with minimal variation between individuals.

Return on Investment (After Risk Seek): Adjust the return rate for each unit of betting capital on each event. A higher return rate results in higher pay-offs for people's investments when they capitalize on good opportunities but also leads to higher losses when they encounter unfavorable outcomes.

**Outcome Display Formate:** 

Our simulation results are displayed across nine sections, each illustrating the effects of various factors within our model.

In the "Overall Regression" section, two regression models and three partial regression plots are displayed:

- First regression model: it focuses on the independent effects of variables. X_axis: the factors, Y_axis: The coefficient value. It illustrates how the coefficients of the main effect variables change over time.
- Second regression model: it incorporates interaction terms and functions similarly to the first, enhancing our understanding of the complex interplay between factors.
- Three partial plots: they provide insights into the correlation between each factor and capital.

In the 'Risk Seeking, Talent, Initial Capital' sections, three plots are displayed to comprehensively illustrate the influence of each factor:

- Plot 1: The x-axis represents time, while the y-axis displays the factor values. The points within the plot indicate the type of luck each individual experiences at time t. Throughout the animation, you can observe how individuals with varying levels of these factors change their capital values at each point in time.

- Plot 2(Regression Model with Only Risk Seeking): The x-axis represents the factor value, while the y-axis shows the capital value at time t. The points in the plot indicate the condition of each individual (x = factor level, y = capital value), with the color representing the individual’s talent level. This plot allows you to observe the correlation between the factor and capital, as well as the distribution trend of talent.

- Plot 3(**Top 10 Most & Least Risk-Seeking Individuals)**: This plot illustrates the relationship between factor rank, capital, and the luck experienced by individuals at time t. Individuals are ranked based on their factor level, with the top 10 and bottom 10 in terms of factor highlighted. The x-axis shows the rank level, while the y-axis displays the capital level at time t. The numbers displayed on the columns indicate each individual's index. For more detailed information about these individuals, refer to the "Individual" section using these index numbers.

In the 'Luck' section, two pairs of plots are presented. 

- The first pair, situated on the top row, displays data on the population's good luck occurrences alongside instances where this good luck was capitalized upon.
- The second pair, on the bottom row, illustrates data on bad luck occurrences and the instances where bad luck was mitigated.
- Plots(Cumulative Number of #Luck at time #): The x-axis represents the cumulative number of good luck encounters in the population, and the y-axis represents the probability density value for each count of luck.

In ”Individual” section, there are three plots.

- Plot 1(Individual Trajectory): You can select an individual by using their index number, as shown on the rank plot in each factor's section, to observe the individual's capital trajectory on the plot.

- Plot 2(summary statistics for individual): To see the summary statistics for the individual.

- Plot 3 (Individual's Past Path Record): This plot allows you to examine the details of an individual's factors at each time t. Use the up arrow to sort the data in ascending order or the down arrow for descending order.

In “Dataset” section, You can view information about the population's factors at different time t, and sort the list by various variables displayed in the first row. Click the up arrow for ascending order or the down arrow for descending order.

In “Summary Statistics” section: It shows the population’s summary statistics for all factors. 

In “Bivariate Plots” section: You will have a clear view about the correlation between the various variables through the both scatter plot matrix and the correlation heatmap. Under scatterplot matrix, you can observe all possible bivariate relationships while stratifying by luck category at current time point t. The correlation heatmap also quantifies the relationship between factors. 

$$
\begin{aligned}
\textbf{Model Parameters:} \\
n &= 1000 \quad \text{(Number of individuals)} \\
\mu_{\text{talent}} &= 0.5, \, \sigma_{\text{talent}} = 0.2 \quad \text{(Truncated normal distribution for talent)} \\
\mu_{\text{risk-seeking}} &= 0.5, \, \sigma_{\text{risk-seeking}} = 0.2 \quad \text{(Truncated normal distribution for risk-seeking)} \\

\text{Initial Capital: } C_{i} &\sim \text{Gamma}(\text{shape} = 3, \text{scale} = 3) \\
\text{Capital Scaling Factor: } \text{capital\_mapping} &= 0.1 \\
\text{Talent Scaling Factor: } \text{talent\_mapping} &= 0.1 \\
\text{Return on Investment: } \text{roi} &= 0.05 \\
\sigma_{\text{good luck}} &= 0.1, \, \sigma_{\text{bad luck}} = 0.1 \\

\textbf{Capital Update for Individual } i: \\
\text{Capital}_{i, t+1} &= \text{Capital}_{i, t} \cdot (1 - r_{i}) + \text{Capital}_{i, t} \cdot r_{i} \cdot \Big( 1 + \text{roi} \cdot \text{Up}_{i} - \text{roi} \cdot \text{Down}_{i} \Big) \\
\textbf{Components of Capital Update:} \\
\text{Up}_{i} &= S_{G_i} \cdot G_{i} \quad \text{(Good luck seized)} \\
\text{Down}_{i} &= S_{B_i} \cdot B_{i} \quad \text{(Bad luck seized)} \\

\textbf{Luck and Seizure Mechanisms:} \\
\text{Good Luck Seized: } S_{G_i} &\sim \text{Binomial}\left(1, \left((T_i - \mu_{\text{talent}}) \cdot \text{talent\_mapping} + 0.5\right)\right) \\
\text{Bad Luck Seized: } S_{B_i} &\sim \text{Binomial}\left(1, \left((\mu_{\text{talent}} - T_i)\cdot \text{talent\_mapping} + 0.5\right)\right) \\
\text{Good Luck: }G_{i} &\sim \mathcal{N}\left(\frac{\text{Capital}_{i, t} - \min(\text{Capital})}{\max(\text{Capital}) - \min(\text{Capital})} \cdot \text{capital\_mapping} + 0.5, \sigma_{\text{good luck}}^2\right) \\
\text{Bad Luck: }B_{i} &\sim \mathcal{N}\left(1 - \frac{\text{Capital}_{i, t} - \min(\text{Capital})}{\max(\text{Capital}) - \min(\text{Capital})} \cdot \text{capital\_mapping} + 0.5, \sigma_{\text{bad luck}}^2\right) \\
                    \textbf{Distributions:} \\
                    \text{Talent: } T_{i} &\sim \mathcal{N}(\mu_{\text{talent}}, \sigma_{\text{talent}}^2) \\
                    \text{Risk-seeking: }r_{i} &\sim \mathcal{N}(\mu_{\text{risk-seeking}}, \sigma_{\text{risk-seeking}}^2) \\
\textbf{Initialization:} \\
\text{At } t = 0, \, \text{Capital}_{i, 0} &= C_{i} \\
\end{aligned}
$$
