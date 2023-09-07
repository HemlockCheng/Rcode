# Define the parameters for the binomial distribution (n and p for A)
n_A <- 20
p_A <- 0.5

# Define the parameters for the binomial distribution (n and p for B)
n_B <- 40
p_B <- 0.5

# Define a range of x values
x_values <- seq(0, n_B, length = n_B + 1)

# Calculate the probabilities using the binomial distribution formula for A
probabilities_A <- dbinom(x_values, size = n_A, prob = p_A)

# Calculate the probabilities using the binomial distribution formula for B
probabilities_B <- dbinom(x_values, size = n_B, prob = p_B)

# Calculate the relative ignorance scores by subtracting -log(p(x)) for B from -log(p(x)) for A
relative_ignorance_scores <- -log(probabilities_A) - (-log(probabilities_B))

# Downscale the relative ignorance scores by a factor of 40 for visualization
downscaled_relative_ignorance_scores <- relative_ignorance_scores / 40

# Create data frames for plotting
data_A <- data.frame(x = x_values, PDF_A = probabilities_A)
data_B <- data.frame(x = x_values, PDF_B = probabilities_B)
data_downscaled_relative <- data.frame(x = x_values, downscaled_relative_ignorance_score = downscaled_relative_ignorance_scores)

# Load required libraries
library(ggplot2)

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
  theme(legend.position = c(0.65, 0.3), 
        legend.box = "horizontal",
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.key.size = unit(1, "lines"))+
  theme(axis.title.x = element_text(size = 30))+
  theme(axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 24))+
  theme(axis.text.y = element_text(size = 24))


# Display the plot
print(plot)
