# Load required libraries
library(ggplot2)
library(stats)
# Define the parameters for the beta distribution (alpha and beta for A)
lambda_A = 0.5

# Define the parameters for the beta distribution (alpha and beta for B)
lambda_B = 1.5

x <- c(rexp(500, lambda_A), rexp(500, lambda_B))

# Define a range of x values
x_values <- seq(0, 3, length = 1000)

# Calculate the probabilities using the normal distribution formula for A and B
probabilities_A <- dexp(x_values, lambda_A)
probabilities_B <- dexp(x_values, lambda_B)

integrand_A <- function(x){(lambda_A*exp(-lambda_A*x))^2}
integrate_A <- integrate(integrand_A, lower = 0, upper = Inf)
integrand_B <- function(x){(lambda_B*exp(-lambda_B*x))^2}
integrate_B <- integrate(integrand_B, lower = 0, upper = Inf)
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
  geom_line(data = data_prob_A, aes(x = x, y = Probability_A, color = "Probability A"), size = 2) +
  geom_line(data = data_prob_B, aes(x = x, y = Probability_B, color = "Probability B"), size = 2) +
  geom_line(data = data_pls_diff, aes(x = x, y = PLS_Difference, color = "PLS Difference"), size = 2) +
  labs(x = "X", y = "") +
  scale_color_manual(values = c("Probability A" = "blue", "Probability B" = "red", "PLS Difference" = "green"),
                     labels = c( "PL relative score","PDF A", "PDF B")) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_hline(aes(yintercept=0),linetype="dashed",color="black",size=1.5)+
  geom_vline(aes(xintercept=0.6),linetype="dashed",color="black",size=1.5)+
  geom_vline(aes(xintercept=1.1),linetype="dashed",color="orange",size=1.5)+
  scale_x_continuous(expand = c(0,0),breaks = c(0,0.4,0.6,0.8,1.1,1.6,2,2.4,2.8), labels = c("0","0.4","E","0.8","F","1.6","2","2.4","2.8"))+
  theme(legend.position = c(0.7, 0.7),
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

