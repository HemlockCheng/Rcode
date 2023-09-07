# Load required libraries
library(ggplot2)
library(stats)

# Define the parameters for the bimodal distributions (mu and sigma for A and B)
mu_A <- c(-1, 1)
sigma_A <- 0.5

mu_B <- c(0, 2)
sigma_B <- 0.5

# Define a range of x values
x_values <- seq(-4, 4, length = 500)

# Calculate the probabilities using the bimodal distribution formula for A and B
probabilities_A <- 0.5 * (dnorm(x_values, mean = mu_A[1], sd = sigma_A) + dnorm(x_values, mean = mu_A[2], sd = sigma_A))
probabilities_B <- 0.5 * (dnorm(x_values, mean = mu_B[1], sd = sigma_B) + dnorm(x_values, mean = mu_B[2], sd = sigma_B))

# Calculate the Ignorance Scores for A and B
ignorance_scores_A <- -log(probabilities_A)
ignorance_scores_B <- -log(probabilities_B)

# Calculate the relative Ignorance Scores
relative_ignorance_scores <- ignorance_scores_A - ignorance_scores_B

# Downscale the relative Ignorance Scores for visualization purposes
downscaled_relative_ignorance_scores <- relative_ignorance_scores/20

# Create data frames for plotting
data_prob_A <- data.frame(x = x_values, Probability_A = probabilities_A)
data_prob_B <- data.frame(x = x_values, Probability_B = probabilities_B)
data_relative_ignorance <- data.frame(x = x_values, Relative_Ignorance_Score = downscaled_relative_ignorance_scores)

# Create the plot using ggplot2
plot <- ggplot() +
  geom_line(data = data_prob_A, aes(x = x, y = Probability_A, color = "Probability A"), size = 2.5) +
  geom_line(data = data_prob_B, aes(x = x, y = Probability_B, color = "Probability B"), size = 2.5) +
  geom_line(data = data_relative_ignorance, aes(x = x, y = Relative_Ignorance_Score, color = "Relative Ignorance Score"), size = 2.5) +
  labs(x = "X", y = "") +
  scale_x_continuous(limits = c(-3,4))+
  scale_y_continuous(limits = c(-0.4, .4))+
  scale_color_manual(values = c("Probability A" = "blue", "Probability B" = "red", "Relative Ignorance Score" = "green"),
                     labels = c("Distribution A", "Distribution B", "Downscaled Relative Ignorance Score")) +
  theme_bw() +
  geom_hline(aes(yintercept=0),linetype="dashed",color="black",size=1.5)+
  theme(legend.position = c(0.6, 0.2),
        legend.box = "horizontal",
        legend.title = element_text(size = 26),
        legend.text = element_text(size = 26),
        legend.key.size = unit(1.5, "lines"))+
  theme(axis.title.x = element_text(size = 30))+
  theme(axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 24))+
  theme(axis.text.y = element_text(size = 24))

# Display the plot
print(plot)
