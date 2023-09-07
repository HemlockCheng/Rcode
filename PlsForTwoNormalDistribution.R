# Load required libraries
library(ggplot2)
library(stats)
x <- c(rnorm(2500, mean = -3, sd = 0.5), rnorm(2500, mean =3, sd = 1))
# Define the parameters for the normal distributions (mu and sigma for A and B)
mu_A <- 0
sigma_A <- 0.5

mu_B <- 0
sigma_B <- 4

# Define a range of x values
x_values <- seq(-3, 3, length = 5000)

# Calculate the probabilities using the normal distribution formula for A and B
probabilities_A <- dnorm(x_values, mean = mu_A, sd = sigma_A)
probabilities_B <- dnorm(x_values, mean = mu_B, sd = sigma_B)

integrand_A <- function(x){((1/(sqrt(2*pi)*sigma_A))*exp(-((x-mu_A)^2)/(2*sigma_A^2)))^2}
integrate_A <- integrate(integrand_A, lower = -Inf, upper = Inf)
integrand_B <- function(x){((1/(sqrt(2*pi)*sigma_B))*exp(-((x-mu_B)^2)/(2*sigma_B^2)))^2}
integrate_B <- integrate(integrand_B, lower = -Inf, upper = Inf)
integrate_A$value
probabilities_A
integrate_B$value
probabilities_B

# Calculating the PLS differences using the corrected approach
pls_diff1 <- integrate_A$value - 2 * probabilities_A
pls_diff2 <- integrate_B$value - 2 * probabilities_B

pls_diff <- (pls_diff1 - pls_diff2)/2


data_prob_A <- data.frame(x = x_values, Probability_A = probabilities_A)
data_prob_B <- data.frame(x = x_values, Probability_B = probabilities_B)
data_pls_diff <- data.frame(x = x_values, PLS_Difference = pls_diff)

# Create the plot using ggplot2
plot <- ggplot() +
  geom_line(data = data_prob_A, aes(x = x, y = Probability_A, color = "Probability A"), size = 2.5) +
  geom_line(data = data_prob_B, aes(x = x, y = Probability_B, color = "Probability B"), size = 2.5) +
  geom_line(data = data_pls_diff, aes(x = x, y = PLS_Difference, color = "PLS Difference"), size = 2.5) +
  labs(x = "X", y = "") +
  scale_color_manual(values = c("Probability A" = "blue", "Probability B" = "red", "PLS Difference" = "green"),
                     labels = c( "PLS relative score","Distribution A", "Distribution B")) +
  theme_minimal() +
  geom_vline(aes(xintercept=-1),linetype="dashed",color="black",size=1.5)+
  geom_vline(aes(xintercept=1),linetype="dashed",color="black",size=1.5)+
  geom_vline(aes(xintercept=-0.5),linetype="dashed",color="orange",size=1.5)+
  geom_vline(aes(xintercept=0.5),linetype="dashed",color="orange",size=1.5)+
  theme(legend.position = c(0.7, 0.2),
        legend.box = "horizontal",
        legend.title = element_text(size = 28),
        legend.text = element_text(size = 28),
        legend.key.size = unit(1.5, "lines"))+
  theme(axis.title.x = element_text(size = 30))+
  theme(axis.title.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 24))+
  theme(axis.text.y = element_text(size = 24))

# Display the plot
print(plot)

