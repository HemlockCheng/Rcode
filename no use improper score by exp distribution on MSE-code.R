set.seed(1000)        
x<-rexp(5000, 0.8)
z0.5<-rexp(5000, .5)
z0.6<-rexp(5000, .6)
z0.7<-rexp(5000, .7)
z0.8<-rexp(5000, .8)
z0.9<-rexp(5000, .9)
z1.0<-rexp(5000, 1)

scoreMSE_0.5 = mean((x-z0.5)^2*0.5*exp(0.5*z))
scoreMSE_0.5

scoreMSE_0.6 = mean((x-z0.6)^2*0.6*exp(0.6*z))
scoreMSE_0.6

scoreMSE_0.7 = mean((x-z0.7)^2*0.7*exp(0.7*z))
scoreMSE_0.7

scoreMSE_0.8 = mean((x-z0.8)^2*0.8*exp(0.8*z))
scoreMSE_0.8

scoreMSE_0.9 = mean((x-z0.9)^2*0.9*exp(0.9*z))
scoreMSE_0.9

scoreMSE_1.0 = mean((x-z1.0)^2*1.0*exp(1.0*z))
scoreMSE_1.0
