set.seed(123)
z=rnorm(10000,mean = 5,sd=2)
zc=rnorm(10000,mean = 5,sd=2)
x=rnorm(10000,mean = 0,sd=1)
#WHEN SIGMA = 5, test Pls score
#for cube3

integrand_Ac5 <- function(zc){((1/(sqrt(2*pi)*5))*exp(-((zc-0)^2)/(2*5^2)))^2}
integrate_Ac5 <- integrate(integrand_Ac5, lower = -Inf, upper = Inf)
integrate_Ac5$value

mf_Ac5 = 2*mean((1/(sqrt(2*pi)*5))*exp(-((x^3-0)^2)/(2*5^2)))
mf_Ac5

ScorePLS_Ac5 = integrate_Ac5$value - mf_Ac5
ScorePLS_Ac5

integrand_Bc5 <- function(zc){((1/(sqrt(2*pi)*1/5))*exp(-((zc-0)^2)/(2*(1/5)^2)))^2}
integrate_Bc5 <- integrate(integrand_Bc5, lower = -Inf, upper = Inf)
integrate_Bc5$value

mf_Bc5 = 2*mean((1/(sqrt(2*pi)*1/5))*exp(-((x^3-0)^2)/(2*(1/5)^2)))
mf_Bc5

ScorePLS_Bc5 = integrate_Bc5$value - mf_Bc5
ScorePLS_Bc5

ScorePLS_c5 = ScorePLS_Ac5 - ScorePLS_Bc5
ScorePLS_c5

#for 1

integrand_A5 <- function(z){((1/(sqrt(2*pi)*5))*exp(-((z-0)^2)/(2*5^2)))^2}
integrate_A5 <- integrate(integrand_A5, lower = -Inf, upper = Inf)
integrate_A5$value

mf_A5 = 2*mean((1/(sqrt(2*pi)*5))*exp(-((x-0)^2)/(2*5^2)))
mf_A5

ScorePLS_A5 = integrate_A5$value - mf_A5
ScorePLS_A5

integrand_B5 <- function(z){((1/(sqrt(2*pi)*1/5))*exp(-((z-0)^2)/(2*(1/5)^2)))^2}
integrate_B5 <- integrate(integrand_B5, lower = -Inf, upper = Inf)
integrate_B5$value

mf_B5 = 2*mean((1/(sqrt(2*pi)*1/5))*exp(-((x-0)^2)/(2*(1/5)^2)))
mf_B5

ScorePLS_B5 = integrate_B5$value - mf_B5
ScorePLS_B5

ScorePLS_5 = ScorePLS_A5 - ScorePLS_B5
ScorePLS_5

ScorePlsCubeTest = ScorePLS_5*ScorePLS_c5
ScorePlsCubeTest
