x=rnorm(1000000,mean = 0,sd=1)
library(ggplot2)

#WHEN SIGMA = 1
scoreIGN_A1=mean(-log((1/(sqrt(2*pi)*1))*exp(-((x-0)^2)/(2*1^2)),base=10))


scoreIGN_B1=mean(-log((1/(sqrt(2*pi)*1/1))*exp(-((x-0)^2)/(2*(1/1)^2)),base=10))


scoreIGN_1 = scoreIGN_A1 - scoreIGN_B1
scoreIGN_1


#WHEN SIGMA = 1.5
scoreIGN_A1.5=mean(-log((1/(sqrt(2*pi)*1.5))*exp(-((x-0)^2)/(2*1.5^2)),base=10))


scoreIGN_B1.5=mean(-log((1/(sqrt(2*pi)*1/1.5))*exp(-((x-0)^2)/(2*(1/1.5)^2)),base=10))


scoreIGN_1.5 = scoreIGN_A1.5 - scoreIGN_B1.5
scoreIGN_1.5

#WHEN SIGMA = 2
scoreIGN_A2=mean(-log((1/(sqrt(2*pi)*2))*exp(-((x-0)^2)/(2*2^2)),base=10))


scoreIGN_B2=mean(-log((1/(sqrt(2*pi)*1/2))*exp(-((x-0)^2)/(2*(1/2)^2)),base=10))


scoreIGN_2 = scoreIGN_A2 - scoreIGN_B2
scoreIGN_2

#WHEN SIGMA = 2.5
scoreIGN_A2.5=mean(-log((1/(sqrt(2*pi)*2.5))*exp(-((x-0)^2)/(2*2.5^2)),base=10))


scoreIGN_B2.5=mean(-log((1/(sqrt(2*pi)*1/2.5))*exp(-((x-0)^2)/(2*(1/2.5)^2)),base=10))


scoreIGN_2.5 = scoreIGN_A2.5 - scoreIGN_B2.5
scoreIGN_2.5

#WHEN SIGMA = 3
scoreIGN_A3=mean(-log((1/(sqrt(2*pi)*3))*exp(-((x-0)^2)/(2*3^2)),base=10))


scoreIGN_B3=mean(-log((1/(sqrt(2*pi)*1/3))*exp(-((x-0)^2)/(2*(1/3)^2)),base=10))


scoreIGN_3 = scoreIGN_A3 - scoreIGN_B3
scoreIGN_3

#WHEN SIGMA = 3.5
scoreIGN_A3.5=mean(-log((1/(sqrt(2*pi)*3.5))*exp(-((x-0)^2)/(2*3.5^2)),base=10))


scoreIGN_B3.5=mean(-log((1/(sqrt(2*pi)*1/3.5))*exp(-((x-0)^2)/(2*(1/3.5)^2)),base=10))


scoreIGN_3.5 = scoreIGN_A3.5 - scoreIGN_B3.5
scoreIGN_3.5

#WHEN SIGMA = 4
scoreIGN_A4=mean(-log((1/(sqrt(2*pi)*4))*exp(-((x-0)^2)/(2*4^2)),base=10))


scoreIGN_B4=mean(-log((1/(sqrt(2*pi)*1/4))*exp(-((x-0)^2)/(2*(1/4)^2)),base=10))


scoreIGN_4 = scoreIGN_A4 - scoreIGN_B4
scoreIGN_4

#WHEN SIGMA = 4.5
scoreIGN_A4.5=mean(-log((1/(sqrt(2*pi)*4.5))*exp(-((x-0)^2)/(2*4.5^2)),base=10))


scoreIGN_B4.5=mean(-log((1/(sqrt(2*pi)*1/4.5))*exp(-((x-0)^2)/(2*(1/4.5)^2)),base=10))


scoreIGN_4.5 = scoreIGN_A4.5 - scoreIGN_B4.5
scoreIGN_4.5

#WHEN SIGMA = 5
scoreIGN_A5=mean(-log((1/(sqrt(2*pi)*5))*exp(-((x-0)^2)/(2*5^2)),base=10))


scoreIGN_B5=mean(-log((1/(sqrt(2*pi)*1/5))*exp(-((x-0)^2)/(2*(1/5)^2)),base=10))


scoreIGN_5 = scoreIGN_A5 - scoreIGN_B5
scoreIGN_5

#WHEN SIGMA = 5.5
scoreIGN_A5.5=mean(-log((1/(sqrt(2*pi)*5.5))*exp(-((x-0)^2)/(2*5.5^2)),base=10))


scoreIGN_B5.5=mean(-log((1/(sqrt(2*pi)*1/5.5))*exp(-((x-0)^2)/(2*(1/5.5)^2)),base=10))


scoreIGN_5.5 = scoreIGN_A5.5 - scoreIGN_B5.5
scoreIGN_5.5

#WHEN SIGMA = 6
scoreIGN_A6=mean(-log((1/(sqrt(2*pi)*6))*exp(-((x-0)^2)/(2*6^2)),base=10))


scoreIGN_B6=mean(-log((1/(sqrt(2*pi)*1/6))*exp(-((x-0)^2)/(2*(1/6)^2)),base=10))


scoreIGN_6 = scoreIGN_A6 - scoreIGN_B6
scoreIGN_6

scoreIGN<-read.csv("C:\\Users\\lenovo\\Desktop\\Dissertation\\IgnoranceScore.csv")

ggplot(scoreIGN, aes(x=sigma,y=IGN)) + geom_smooth(color=43,size=2.5) + 
  scale_y_continuous(breaks = c(0,-1,-2,-3,-4,-5,-6))+
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



#plot(scoreIGN$sigma, scoreIGN$IGN,lwd=2, col="blue",type = "l")
#lines(scoreIGN$sigma, scoreIGN$IGN,xlab="sigma",ylab="Relative scores",lwd=2, col="blue")
