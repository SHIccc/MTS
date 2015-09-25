# ---------------------------------------------------------------------
# Project:      Bachelor Thesis
# ---------------------------------------------------------------------
# Quantlet:     Hist_pdf_binomial
# ---------------------------------------------------------------------
# Description:  Produces a histogram of a standardized binomial 
#               distribution on behalf of empirical samples and shows 
#               the possible approximation for large n
#               throughout the standard normal distribution  
# ---------------------------------------------------------------------
# Output:       Histogram ("n100.png")
# ---------------------------------------------------------------------
# Keywords:     approximation, standardized binomial, distribution, 
#               graphical representation, visualization, histogram, pdf,
#               CLT, moivre-laplace
# ---------------------------------------------------------------------
# Author:       Fabrice Wunderlich,  2015/08/01
# ---------------------------------------------------------------------



# Clear loaded variables and close graphics
closeAllConnections()
rm(list=ls(all=TRUE))
graphics.off()

# Parameter settings of the binomial random variable and of the sample size N
n = 100
p = 0.2
N = 1000000

# Sample of the N stadardized binomial random variables
bsample = (rbinom(N,n,p)-(n*p))/(sqrt(n*p*(1-p)))

# Plot 
breakingpoints = seq(from=min(bsample-1),to=max(bsample+1),by=0.25)
hist(bsample,freq=FALSE,breaks=breakingpoints,xlab="(k-np)/sqrt(np(1-p))",ylab="b(n,p;k)",xlim=c(min(bsample-1),max(bsample+1)),main=paste("n=",n,", p=",p))

x     = seq(min(bsample-1),max(bsample+1), by=0.02)
norm  = dnorm(x,0,1)
lines(x,norm,col="dark red")

