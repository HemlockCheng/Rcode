library(ggplot2)

scoreSigma<-read.csv("C:\\Users\\lenovo\\Desktop\\Dissertation\\PLStoCube.csv")

ggplot(scoreSigma, aes(x=sigma,y=PLStoCube))+ 
  geom_path(color='blue',size=2.5)+
  scale_x_continuous(breaks = c(1,3,5,7,9,11.1,13,15))+
  scale_y_continuous(breaks = c(-0.4,-0.2,0,0.2,0.32))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  xlab(expression(sigma))+
  ylab('Pls Cube Relative scores')+
  theme(axis.title.x = element_text(size = 42))+
  theme(axis.title.y = element_text(size = 42))+
  theme(axis.text.x = element_text(size = 32))+
  theme(axis.text.y = element_text(size = 32))+
  geom_hline(aes(yintercept=0),linetype="dashed",color="green",size=2)+
  geom_hline(aes(yintercept=0.318),linetype="dashed",color="green",size=2)+
  geom_vline(aes(xintercept=11.1),color="red",size=2)

