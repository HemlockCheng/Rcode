z=rnorm(1000000,mean = 0,sd=1)
x=rnorm(1000000,mean = 0,sd=1)

#WHEN SIGMA = 1

integrand_A1 <- function(z){((1/(sqrt(2*pi)*1))*exp(-((z-0)^2)/(2*1^2)))^2}
integrate_A1 <- integrate(integrand_A1, lower = -Inf, upper = Inf)

mf_A1 = 2*mean((1/(sqrt(2*pi)*1))*exp(-((x^3-0)^2)/(2*1^2)))
mf_A1

ScorePLS_A1 = integrate_A1$value - mf_A1
ScorePLS_A1

integrand_B1 <- function(z){((1/(sqrt(2*pi)*1/1))*exp(-((z-0)^2)/(2*(1/1)^2)))^2}
integrate_B1 <- integrate(integrand_B1, lower = -Inf, upper = Inf)

mf_B1 = 2*mean((1/(sqrt(2*pi)*1/1))*exp(-((x^3-0)^2)/(2*(1/1)^2)))
mf_B1

ScorePLS_B1 = integrate_B1$value - mf_B1

ScorePLS_1 = ScorePLS_A1 - ScorePLS_B1
ScorePLS_1


#WHEN SIGMA = 1.5

integrand_A1.5 <- function(z){((1/(sqrt(2*pi)*1.5))*exp(-((z-0)^2)/(2*1.5^2)))^2}
integrate_A1.5 <- integrate(integrand_A1.5, lower = -Inf, upper = Inf)
integrate_A1.5$value

mf_A1.5 = 2*mean((1/(sqrt(2*pi)*1.5))*exp(-((x^3-0)^2)/(2*1.5^2)))
mf_A1.5

ScorePLS_A1.5 = integrate_A1.5$value - mf_A1.5
ScorePLS_A1.5

integrand_B1.5 <- function(z){((1/(sqrt(2*pi)*1/1.5))*exp(-((z-0)^2)/(2*(1/1.5)^2)))^2}
integrate_B1.5 <- integrate(integrand_B1.5, lower = -Inf, upper = Inf)

mf_B1.5 = 2*mean((1/(sqrt(2*pi)*1/1.5))*exp(-((x^3-0)^2)/(2*(1/1.5)^2)))
mf_B1.5

ScorePLS_B1.5 = integrate_B1.5$value - mf_B1.5
ScorePLS_B1.5

ScorePLS_1.5 = ScorePLS_A1.5 - ScorePLS_B1.5
ScorePLS_1.5

#WHEN SIGMA = 2

integrand_A2 <- function(z){((1/(sqrt(2*pi)*2))*exp(-((z-0)^2)/(2*2^2)))^2}
integrate_A2 <- integrate(integrand_A2, lower = -Inf, upper = Inf)
integrate_A2$value

mf_A2 = 2*mean((1/(sqrt(2*pi)*2))*exp(-((x^3-0)^2)/(2*2^2)))
mf_A2

ScorePLS_A2 = integrate_A2$value - mf_A2
ScorePLS_A2

integrand_B2 <- function(z){((1/(sqrt(2*pi)*1/2))*exp(-((z-0)^2)/(2*(1/2)^2)))^2}
integrate_B2 <- integrate(integrand_B2, lower = -Inf, upper = Inf)

mf_B2 = 2*mean((1/(sqrt(2*pi)*1/2))*exp(-((x^3-0)^2)/(2*(1/2)^2)))
mf_B2

ScorePLS_B2 = integrate_B2$value - mf_B2
ScorePLS_B2

ScorePLS_2 = ScorePLS_A2 - ScorePLS_B2
ScorePLS_2

#WHEN SIGMA = 2.5

integrand_A2.5 <- function(z){((1/(sqrt(2*pi)*2.5))*exp(-((z-0)^2)/(2*2.5^2)))^2}
integrate_A2.5 <- integrate(integrand_A2.5, lower = -Inf, upper = Inf)
integrate_A2.5$value

mf_A2.5 = 2*mean((1/(sqrt(2*pi)*2.5))*exp(-((x^3-0)^2)/(2*2.5^2)))
mf_A2.5

ScorePLS_A2.5 = integrate_A2.5$value - mf_A2.5
ScorePLS_A2.5

integrand_B2.5 <- function(z){((1/(sqrt(2*pi)*1/2.5))*exp(-((z-0)^2)/(2*(1/2.5)^2)))^2}
integrate_B2.5 <- integrate(integrand_B2.5, lower = -Inf, upper = Inf)

mf_B2.5 = 2*mean((1/(sqrt(2*pi)*1/2.5))*exp(-((x^3-0)^2)/(2*(1/2.5)^2)))
mf_B2.5

ScorePLS_B2.5 = integrate_B2.5$value - mf_B2.5
ScorePLS_B2.5

ScorePLS_2.5 = ScorePLS_A2.5 - ScorePLS_B2.5
ScorePLS_2.5

#WHEN SIGMA = 3

integrand_A3 <- function(z){((1/(sqrt(2*pi)*3))*exp(-((z-0)^2)/(2*3^2)))^2}
integrate_A3 <- integrate(integrand_A3, lower = -Inf, upper = Inf)
integrate_A3$value

mf_A3 = 2*mean((1/(sqrt(2*pi)*3))*exp(-((x^3-0)^2)/(2*3^2)))
mf_A3

ScorePLS_A3 = integrate_A3$value - mf_A3
ScorePLS_A3

integrand_B3 <- function(z){((1/(sqrt(2*pi)*1/3))*exp(-((z-0)^2)/(2*(1/3)^2)))^2}
integrate_B3 <- integrate(integrand_B3, lower = -Inf, upper = Inf)

mf_B3 = 2*mean((1/(sqrt(2*pi)*1/3))*exp(-((x^3-0)^2)/(2*(1/3)^2)))
mf_B3

ScorePLS_B3 = integrate_B3$value - mf_B3
ScorePLS_B3

ScorePLS_3 = ScorePLS_A3 - ScorePLS_B3
ScorePLS_3

#WHEN SIGMA = 3.5

integrand_A3.5 <- function(z){((1/(sqrt(2*pi)*3.5))*exp(-((z-0)^2)/(2*3.5^2)))^2}
integrate_A3.5 <- integrate(integrand_A3.5, lower = -Inf, upper = Inf)
integrate_A3.5$value

mf_A3.5 = 2*mean((1/(sqrt(2*pi)*3.5))*exp(-((x^3-0)^2)/(2*3.5^2)))
mf_A3.5

ScorePLS_A3.5 = integrate_A3.5$value - mf_A3.5
ScorePLS_A3.5

integrand_B3.5 <- function(z){((1/(sqrt(2*pi)*1/3.5))*exp(-((z-0)^2)/(2*(1/3.5)^2)))^2}
integrate_B3.5 <- integrate(integrand_B3.5, lower = -Inf, upper = Inf)

mf_B3.5 = 2*mean((1/(sqrt(2*pi)*1/3.5))*exp(-((x^3-0)^2)/(2*(1/3.5)^2)))
mf_B3.5

ScorePLS_B3.5 = integrate_B3.5$value - mf_B3.5
ScorePLS_B3.5

ScorePLS_3.5 = ScorePLS_A3.5 - ScorePLS_B3.5
ScorePLS_3.5

#WHEN SIGMA = 4

integrand_A4 <- function(z){((1/(sqrt(2*pi)*4))*exp(-((z-0)^2)/(2*4^2)))^2}
integrate_A4 <- integrate(integrand_A4, lower = -Inf, upper = Inf)
integrate_A4$value

mf_A4 = 2*mean((1/(sqrt(2*pi)*4))*exp(-((x^3-0)^2)/(2*4^2)))
mf_A4

ScorePLS_A4 = integrate_A4$value - mf_A4
ScorePLS_A4

integrand_B4 <- function(z){((1/(sqrt(2*pi)*1/4))*exp(-((z-0)^2)/(2*(1/4)^2)))^2}
integrate_B4 <- integrate(integrand_B4, lower = -Inf, upper = Inf)

mf_B4 = 2*mean((1/(sqrt(2*pi)*1/4))*exp(-((x^3-0)^2)/(2*(1/4)^2)))
mf_B4

ScorePLS_B4 = integrate_B4$value - mf_B4
ScorePLS_B4

ScorePLS_4 = ScorePLS_A4 - ScorePLS_B4
ScorePLS_4

#WHEN SIGMA = 4.5

integrand_A4.5 <- function(z){((1/(sqrt(2*pi)*4.5))*exp(-((z-0)^2)/(2*4.5^2)))^2}
integrate_A4.5 <- integrate(integrand_A4.5, lower = -Inf, upper = Inf)
integrate_A4.5$value

mf_A4.5 = 2*mean((1/(sqrt(2*pi)*4.5))*exp(-((x^3-0)^2)/(2*4.5^2)))
mf_A4.5

ScorePLS_A4.5 = integrate_A4.5$value - mf_A4.5
ScorePLS_A4.5

integrand_B4.5 <- function(z){((1/(sqrt(2*pi)*1/4.5))*exp(-((z-0)^2)/(2*(1/4.5)^2)))^2}
integrate_B4.5 <- integrate(integrand_B4.5, lower = -Inf, upper = Inf)

mf_B4.5 = 2*mean((1/(sqrt(2*pi)*1/4.5))*exp(-((x^3-0)^2)/(2*(1/4.5)^2)))
mf_B4.5

ScorePLS_B4.5 = integrate_B4.5$value - mf_B4.5
ScorePLS_B4.5

ScorePLS_4.5 = ScorePLS_A4.5 - ScorePLS_B4.5
ScorePLS_4.5

#WHEN SIGMA = 5

integrand_A5 <- function(z){((1/(sqrt(2*pi)*5))*exp(-((z-0)^2)/(2*5^2)))^2}
integrate_A5 <- integrate(integrand_A5, lower = -Inf, upper = Inf)
integrate_A5$value

mf_A5 = 2*mean((1/(sqrt(2*pi)*5))*exp(-((x^3-0)^2)/(2*5^2)))
mf_A5

ScorePLS_A5 = integrate_A5$value - mf_A5
ScorePLS_A5

integrand_B5 <- function(z){((1/(sqrt(2*pi)*1/5))*exp(-((z-0)^2)/(2*(1/5)^2)))^2}
integrate_B5 <- integrate(integrand_B5, lower = -Inf, upper = Inf)

mf_B5 = 2*mean((1/(sqrt(2*pi)*1/5))*exp(-((x^3-0)^2)/(2*(1/5)^2)))
mf_B5

ScorePLS_B5 = integrate_B5$value - mf_B5
ScorePLS_B5

ScorePLS_5 = ScorePLS_A5 - ScorePLS_B5
ScorePLS_5

#WHEN SIGMA = 5.5

integrand_A5.5 <- function(z){((1/(sqrt(2*pi)*5.5))*exp(-((z-0)^2)/(2*5.5^2)))^2}
integrate_A5.5 <- integrate(integrand_A5.5, lower = -Inf, upper = Inf)
integrate_A5.5$value

mf_A5.5 = 2*mean((1/(sqrt(2*pi)*5.5))*exp(-((x^3-0)^2)/(2*5.5^2)))
mf_A5.5

ScorePLS_A5.5 = integrate_A5.5$value - mf_A5.5
ScorePLS_A5.5

integrand_B5.5 <- function(z){((1/(sqrt(2*pi)*1/5.5))*exp(-((z-0)^2)/(2*(1/5.5)^2)))^2}
integrate_B5.5 <- integrate(integrand_B5.5, lower = -Inf, upper = Inf)

mf_B5.5 = 2*mean((1/(sqrt(2*pi)*1/5.5))*exp(-((x^3-0)^2)/(2*(1/5.5)^2)))
mf_B5.5

ScorePLS_B5.5 = integrate_B5.5$value - mf_B5.5
ScorePLS_B5.5

ScorePLS_5.5 = ScorePLS_A5.5 - ScorePLS_B5.5
ScorePLS_5.5

#WHEN SIGMA = 6

integrand_A6 <- function(z){((1/(sqrt(2*pi)*6))*exp(-((z-0)^2)/(2*6^2)))^2}
integrate_A6 <- integrate(integrand_A6, lower = -Inf, upper = Inf)
integrate_A6$value

mf_A6 = 2*mean((1/(sqrt(2*pi)*6))*exp(-((x^3-0)^2)/(2*6^2)))
mf_A6

ScorePLS_A6 = integrate_A6$value - mf_A6
ScorePLS_A6

integrand_B6 <- function(z){((1/(sqrt(2*pi)*1/6))*exp(-((z-0)^2)/(2*(1/6)^2)))^2}
integrate_B6 <- integrate(integrand_B6, lower = -Inf, upper = Inf)

mf_B6 = 2*mean((1/(sqrt(2*pi)*1/6))*exp(-((x^3-0)^2)/(2*(1/6)^2)))
mf_B6

ScorePLS_B6 = integrate_B6$value - mf_B6
ScorePLS_B6

ScorePLS_6 = ScorePLS_A6 - ScorePLS_B6
ScorePLS_6

#WHEN SIGMA = 8

integrand_A8 <- function(z){((1/(sqrt(2*pi)*8))*exp(-((z-0)^2)/(2*8^2)))^2}
integrate_A8 <- integrate(integrand_A8, lower = -Inf, upper = Inf)
integrate_A8$value

mf_A8 = 2*mean((1/(sqrt(2*pi)*8))*exp(-((x^3-0)^2)/(2*8^2)))
mf_A8

ScorePLS_A8 = integrate_A8$value - mf_A8
ScorePLS_A8

integrand_B8 <- function(z){((1/(sqrt(2*pi)*1/8))*exp(-((z-0)^2)/(2*(1/8)^2)))^2}
integrate_B8 <- integrate(integrand_B8, lower = -Inf, upper = Inf)

mf_B8 = 2*mean((1/(sqrt(2*pi)*1/8))*exp(-((x^3-0)^2)/(2*(1/8)^2)))
mf_B8

ScorePLS_B8 = integrate_B8$value - mf_B8
ScorePLS_B8

ScorePLS_8 = ScorePLS_A8 - ScorePLS_B8
ScorePLS_8

#WHEN SIGMA = 10

integrand_A10 <- function(z){((1/(sqrt(2*pi)*10))*exp(-((z-0)^2)/(2*10^2)))^2}
integrate_A10 <- integrate(integrand_A10, lower = -Inf, upper = Inf)

mf_A10 = 2*mean((1/(sqrt(2*pi)*10))*exp(-((x^3-0)^2)/(2*10^2)))
mf_A10

ScorePLS_A10 = integrate_A10$value - mf_A10
ScorePLS_A10

integrand_B10 <- function(z){((1/(sqrt(2*pi)*1/10))*exp(-((z-0)^2)/(2*(1/10)^2)))^2}
integrate_B10 <- integrate(integrand_B10, lower = -Inf, upper = Inf)

mf_B10 = 2*mean((1/(sqrt(2*pi)*1/10))*exp(-((x^3-0)^2)/(2*(1/10)^2)))
mf_B10

ScorePLS_B10 = integrate_B10$value - mf_B10

ScorePLS_10 = ScorePLS_A10 - ScorePLS_B10
ScorePLS_10

#WHEN SIGMA = 10.5

integrand_A10.5 <- function(z){((1/(sqrt(2*pi)*10.5))*exp(-((z-0)^2)/(2*10.5^2)))^2}
integrate_A10.5 <- integrate(integrand_A10.5, lower = -Inf, upper = Inf)

mf_A10.5 = 2*mean((1/(sqrt(2*pi)*10.5))*exp(-((x^3-0)^2)/(2*10.5^2)))
mf_A10.5

ScorePLS_A10.5 = integrate_A10.5$value - mf_A10.5
ScorePLS_A10.5

integrand_B10.5 <- function(z){((1/(sqrt(2*pi)*1/10.5))*exp(-((z-0)^2)/(2*(1/10.5)^2)))^2}
integrate_B10.5 <- integrate(integrand_B10.5, lower = -Inf, upper = Inf)

mf_B10.5 = 2*mean((1/(sqrt(2*pi)*1/10.5))*exp(-((x^3-0)^2)/(2*(1/10.5)^2)))
mf_B10.5

ScorePLS_B10.5 = integrate_B10.5$value - mf_B10.5

ScorePLS_10.5 = ScorePLS_A10.5 - ScorePLS_B10.5
ScorePLS_10.5

#WHEN SIGMA = 11

integrand_A11 <- function(z){((1/(sqrt(2*pi)*11))*exp(-((z-0)^2)/(2*11^2)))^2}
integrate_A11 <- integrate(integrand_A11, lower = -Inf, upper = Inf)

mf_A11 = 2*mean((1/(sqrt(2*pi)*11))*exp(-((x^3-0)^2)/(2*11^2)))
mf_A11

ScorePLS_A11 = integrate_A11$value - mf_A11
ScorePLS_A11

integrand_B11 <- function(z){((1/(sqrt(2*pi)*1/11))*exp(-((z-0)^2)/(2*(1/11)^2)))^2}
integrate_B11 <- integrate(integrand_B11, lower = -Inf, upper = Inf)

mf_B11 = 2*mean((1/(sqrt(2*pi)*1/11))*exp(-((x^3-0)^2)/(2*(1/11)^2)))
mf_B11

ScorePLS_B11 = integrate_B11$value - mf_B11

ScorePLS_11 = ScorePLS_A11 - ScorePLS_B11
ScorePLS_11

#WHEN SIGMA = 11.5

integrand_A11.5 <- function(z){((1/(sqrt(2*pi)*11.5))*exp(-((z-0)^2)/(2*11.5^2)))^2}
integrate_A11.5 <- integrate(integrand_A11.5, lower = -Inf, upper = Inf)

mf_A11.5 = 2*mean((1/(sqrt(2*pi)*11.5))*exp(-((x^3-0)^2)/(2*11.5^2)))
mf_A11.5

ScorePLS_A11.5 = integrate_A11.5$value - mf_A11.5
ScorePLS_A11.5

integrand_B11.5 <- function(z){((1/(sqrt(2*pi)*1/11.5))*exp(-((z-0)^2)/(2*(1/11.5)^2)))^2}
integrate_B11.5 <- integrate(integrand_B11.5, lower = -Inf, upper = Inf)

mf_B11.5 = 2*mean((1/(sqrt(2*pi)*1/11.5))*exp(-((x^3-0)^2)/(2*(1/11.5)^2)))
mf_B11.5

ScorePLS_B11.5 = integrate_B11.5$value - mf_B11.5

ScorePLS_11.5 = ScorePLS_A11.5 - ScorePLS_B11.5
ScorePLS_11.5

#WHEN SIGMA = 12

integrand_A12 <- function(z){((1/(sqrt(2*pi)*12))*exp(-((z-0)^2)/(2*12^2)))^2}
integrate_A12 <- integrate(integrand_A12, lower = -Inf, upper = Inf)
integrate_A12$value

mf_A12 = 2*mean((1/(sqrt(2*pi)*12))*exp(-((x^3-0)^2)/(2*12^2)))
mf_A12

ScorePLS_A12 = integrate_A12$value - mf_A12
ScorePLS_A12

integrand_B12 <- function(z){((1/(sqrt(2*pi)*1/12))*exp(-((z-0)^2)/(2*(1/12)^2)))^2}
integrate_B12 <- integrate(integrand_B12, lower = -Inf, upper = Inf)

mf_B12 = 2*mean((1/(sqrt(2*pi)*1/12))*exp(-((x^3-0)^2)/(2*(1/12)^2)))
mf_B12

ScorePLS_B12 = integrate_B12$value - mf_B12
ScorePLS_B12

ScorePLS_12 = ScorePLS_A12 - ScorePLS_B12
ScorePLS_12

#WHEN SIGMA = 14

integrand_A14 <- function(z){((1/(sqrt(2*pi)*14))*exp(-((z-0)^2)/(2*14^2)))^2}
integrate_A14 <- integrate(integrand_A14, lower = -Inf, upper = Inf)
integrate_A14$value

mf_A14 = 2*mean((1/(sqrt(2*pi)*14))*exp(-((x^3-0)^2)/(2*14^2)))
mf_A14

ScorePLS_A14 = integrate_A14$value - mf_A14
ScorePLS_A14

integrand_B14 <- function(z){((1/(sqrt(2*pi)*1/14))*exp(-((z-0)^2)/(2*(1/14)^2)))^2}
integrate_B14 <- integrate(integrand_B14, lower = -Inf, upper = Inf)

mf_B14 = 2*mean((1/(sqrt(2*pi)*1/14))*exp(-((x^3-0)^2)/(2*(1/14)^2)))
mf_B14

ScorePLS_B14 = integrate_B14$value - mf_B14
ScorePLS_B14

ScorePLS_14 = ScorePLS_A14 - ScorePLS_B14
ScorePLS_14

#WHEN SIGMA = 16

integrand_A16 <- function(z){((1/(sqrt(2*pi)*16))*exp(-((z-0)^2)/(2*16^2)))^2}
integrate_A16 <- integrate(integrand_A16, lower = -Inf, upper = Inf)
integrate_A16$value

mf_A16 = 2*mean((1/(sqrt(2*pi)*16))*exp(-((x^3-0)^2)/(2*16^2)))
mf_A16

ScorePLS_A16 = integrate_A16$value - mf_A16
ScorePLS_A16

integrand_B16 <- function(z){((1/(sqrt(2*pi)*1/16))*exp(-((z-0)^2)/(2*(1/16)^2)))^2}
integrate_B16 <- integrate(integrand_B16, lower = -Inf, upper = Inf)

mf_B16 = 2*mean((1/(sqrt(2*pi)*1/16))*exp(-((x^3-0)^2)/(2*(1/16)^2)))
mf_B16

ScorePLS_B16 = integrate_B16$value - mf_B16
ScorePLS_B16

ScorePLS_16 = ScorePLS_A16 - ScorePLS_B16
ScorePLS_16

score<-read.csv("C:\\Users\\lenovo\\Desktop\\Dissertation\\PLStoCube.csv")

plot(score$sigma, score$PLStoCube,lwd=2, col="green")
abline(h=0.0,lty=2,col="red")
abline(v=11.1,lty=1,col="red")

lines(score$sigma, score$PLStoCube,lwd=3, col="green")
