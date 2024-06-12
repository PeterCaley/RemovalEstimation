################################################################################
# Filename: functions.R
TEST <- FALSE

# Function to estimate parameters a and b for a desired "refuge" density
ab.est <- function(c.max, x.crit, d) {
  b.hat <- c.max*(exp(d*x.crit) - 1)
  a.hat <- c.max + b.hat
  c(a.hat, b.hat)  
}
if(TEST) {
  ab.est(c.max=40, x.crit=2, d=0.5)
}

# Ivlev Function
Ivlev <- function(x, a=NULL, b=NULL, c.max=NULL, x.min=NULL, d, no.neg=TRUE) {
  # Args:
  # x -- prey density
  #	x.min -- rate of offtake at zero population density, which helps determine
  # The density below which no prey can be shot
  #	c.max -- asymptotic maximum rate of kill at high prey density
  #	d -- effect of declining prey density on shooting efficiency 
  # no.neg -- do you want to plot negative vlues, or overwrite with zeros?
  if(is.null(a) & is.null(b)) {
    a <- ab.est(c.max, x.min, d)[1]
    b <- ab.est(c.max, x.min, d)[2]
  }  
  # returns cull   
  c <- -b + a*(1 - exp(-d*x))
  if(no.neg) 
    c[x < -log(1-b/a)/d] <- 0
  return(c)
} 

if(TEST) {
  Ivlev(x=2, a=108.73, b=68.73, d=0.5)
  Ivlev(x=2, c.max=40, x.min=2, d=0.5)
}

# Function used by Choquenot et al 1999
Choquenot <- function(x=20, a=40, b=1, d=0.5) {
  # b -- population density below which no prey can (effectively) be shot
  # a -- asymptotic maximum rate of kill at high prey density
  # d -- effect of declining prey density on shooting efficiency 
  # x -- prey density   
  kill.rate <- a*(1 - exp(-(x-b)*d))
  # Adjust for "unshootable"  population
  kill.rate[x<=b] <- 0
  return(kill.rate)
}

# Holling Type III Functional response used by Pech et al 1992  

TypeIII <- function(x, beta0, beta1, k=2) {
  # Args:
  #   beta0 -- Parameter reflecting shooting saturation c.max
  #   beta1 -- Parameter determining the rate at which c.max is approached
  #    k -- parameter
  c <- beta0*x^k/(beta1^2+x^k)
  return(c)
}

if(TEST) {
  plot(seq(0,20,1), TypeIII(seq(0,20,1), beta0=40, beta1=10, k=2), type = 'l', ylim=c(0,40))
  lines(seq(0,20,1), TypeIII(seq(0,20,1), beta0=40, beta1=2, k=2), lty=2)
  lines(seq(0,20,1), TypeIII(seq(0,20,1), beta0=40, beta1=2, k=3), lty=2)
  lines(seq(0,20,1), TypeIII(seq(0,20,1), beta0=40, beta1=10, k=3), lty=3)
  lines(seq(0,20,1), TypeIII(seq(0,20,1), beta0=40, beta1=10, k=4), lty=4)
}

TypeIIIGeneral <- function(x, beta0, beta1, k) {
  # Args:
  #   beta0 -- ToDo
  #   beta1 -- Parameter determining saturation rate c.max = 1/beta1
  #   k -- Parameter controlling rate at which saturation is approached
  c <- beta0*x^k/(1 + beta0*beta1*x^k)
  return(c)
}


################################################################################