# Load required library
library(ggplot2)

# Define the parameters for the beta distribution (alpha and beta for A)
alpha_A <- 2
beta_A <- 2

# Define the parameters for the beta distribution (alpha and beta for B)
alpha_B <- 2
beta_B <- 4

# Define a range of x values
x_values <- seq(0, 1, length = 1000)

# Calculate the probabilities using the beta distribution formula for A
probabilities_A <- dbeta(x_values, shape1 = alpha_A, shape2 = beta_A)

# Calculate the probabilities using the beta distribution formula for B
probabilities_B <- dbeta(x_values, shape1 = alpha_B, shape2 = beta_B)

# Calculate the relative ignorance scores by subtracting -log(p(x)) for B from -log(p(x)) for A
relative_ignorance_scores <- -log(probabilities_A) - (-log(probabilities_B))

# Downscale the relative ignorance scores by a factor of 10 for visualization
downscaled_relative_ignorance_scores <- relative_ignorance_scores / 10

# Create data frames for plotting
data_A <- data.frame(x = x_values, PDF_A = probabilities_A)
data_B <- data.frame(x = x_values, PDF_B = probabilities_B)
data_downscaled_relative <- data.frame(x = x_values, downscaled_relative_ignorance_score = downscaled_relative_ignorance_scores)

# Create the plot using ggplot2
plot <- ggplot() +
  geom_line(data = data_A, aes(x = x, y = PDF_A, color = "Distribution A"), size = 2.5) +
  geom_line(data = data_B, aes(x = x, y = PDF_B, color = "Distribution B"), size = 2.5) +
  geom_line(data = data_downscaled_relative, aes(x = x, y = downscaled_relative_ignorance_score, color = "Downscaled Relative Ignorance Score"), size = 2.5) +
  labs(x = "X", y = "") +
  scale_color_manual(values = c("Distribution A" = "blue", "Distribution B" = "red", "Downscaled Relative Ignorance Score" = "green"),
                     labels = c("Distribution A", "Distribution B", "Downscaled Relative Ignorance Score")) +
  theme_bw() +
  geom_hline(aes(yintercept=0),linetype="dashed",color="black",size=1.5)+
  theme(legend.position = c(0.45, 0.2), 
        legend.box = "horizontal",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1, "lines"))+
  theme(axis.title.x = element_text(size = 30))+
  theme(axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 24))+
  theme(axis.text.y = element_text(size = 24))

# Display the plot
print(plot)
