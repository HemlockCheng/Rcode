# Load required libraries
library(ggplot2)

# Create a data frame with the provided data
data <- data.frame(
  lambda = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
  score = c(4.71, 4.49, 4.95, 5.78, 6.90, 9.00)
)

# Create the plot using ggplot2
plot <- ggplot(data, aes(x = lambda, y = score)) +
  geom_point(color = "grey", size = 3) +
  geom_smooth(color = "black", size = 2.5) +

  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  xlab(expression(lambda))+
  ylab('Relative scores')+
  geom_hline(aes(yintercept=5.78),linetype="dashed",color="green",size=2)+
  geom_vline(aes(xintercept=0.8),linetype="dashed",color="green",size=2)+
  
  theme(axis.title.x = element_text(size = 42))+
  theme(axis.title.y = element_text(size = 42))+
  theme(axis.text.x = element_text(size = 32))+
  theme(axis.text.y = element_text(size = 32))



# Display the plot
print(plot)
