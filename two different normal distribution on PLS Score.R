scoreIGN<-read.csv("C:\\Users\\lenovo\\Desktop\\Dissertation\\IgnoranceScore.csv")

ggplot(scoreIGN, aes(x=sigma,y=PLS)) + geom_smooth(color='blue',size=2.5) + 
  scale_y_continuous(breaks = c(0,-.2,-.4,-.6,-.8,-1))+
  scale_x_continuous(expand = c(0,0),breaks = c(1,1.5,2,2.5,3,3.5,4,4.5,5,5.5))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  xlab(expression(sigma))+
  ylab('Relative scores')+
  
  theme(axis.title.x = element_text(size = 42))+
  theme(axis.title.y = element_text(size = 42))+
  theme(axis.text.x = element_text(size = 32))+
  theme(axis.text.y = element_text(size = 32))

