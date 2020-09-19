library(rethinking)
library(tidyverse)

#============
# Chapter 4
# In Text
#============

data("Howell1")
d <- Howell1
dens(d$height)

curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )

sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )


#height relationship with weight
p <- ggplot(d, aes(x=weight,y=height))+
  geom_point()


#grid approx no model
mu.list <- seq( from=150, to=160 , length.out=100 )
sigma.list <- seq( from=7 , to=9 , length.out=100 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) sum(
  dnorm( d$height , post$mu[i] , post$sigma[i] , log=TRUE ) ) )
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )

#lines of simulated priors
set.seed(2971)
height.priors <- tibble(N = 100,a = rnorm( 100 , 178 , 20 ),b = rnorm( 100 , 0 , 10 ))

ggplot(height.priors)+
  scale_x_continuous(limits = c(-10,10))+
  scale_y_continuous(limits = c(-100,400))+
  geom_abline(aes(intercept = a, slope = b), alpha = .5)
  
#quadratic approximation of height~weight model
xbar <- mean(d$weight)
d$weight_s <- ( d$weight - mean(d$weight) )/sd(d$weight)
d$w2 <- d$weight^2

mod <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*(weight) + b2*w2,
    a ~ dnorm( 0 , 100 ) ,
    b1 ~ dlnorm( 0 , 1) ,
    b2 ~ dnorm(0,1),
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d)

precis(mod)


#plot means of both 

samples <- extract.samples(mod,n = 1000)


p + geom_abline(intercept =  mean(samples$a), slope = mean(samples$b1),color = "blue", size = 1.5)#mean

p + geom_abline(data = samples, aes(intercept = a, slope = b), alpha = .01)+#mean and uncertianty
  geom_abline(intercept =  mean(samples$a), slope = mean(samples$b), color = "blue",size = 1.5)

ggplot(samples, aes(x=b1))+
  geom_density()+
  geom_vline(xintercept = mean(samples$b1))
  
f <- function(x){
  mu <- mean(samples$a)+mean(samples$b1)*x+mean(samples$b2)*(x^2)
  return(mu)
}
p + stat_function(fun = f, color = "blue",size = 1)


#==================
# Chapter 4
# Practice Problems
#==================

#===========
# Chapter 5
# In Text
#===========

data("WaffleDivorce")
d <- WaffleDivorce

d$A <- scale(d$MedianAgeMarriage)
d$D <- scale(d$Divorce)

mod <- quap(
  alist(
    D ~ dnorm(mu, sig),
    mu ~ a + ba * A,
    a ~ dnorm(0,1),
    ba ~ dnorm(0,.5),
    sig~ dexp(1)
  ),
  data = d
)

samp <- extract.samples(mod)

ggplot(d, aes(x=A,y=D))+
  geom_point()+
  geom_abline(intercept = mean(samp$a),slope = mean(samp$b), color = "blue", size = 1)


precis(mod)
ggplot(samp, aes(x = b))+
  geom_density()+
  geom_vline(xintercept = mean(samp$b))


# Conditional Model
d$M <- scale(d$Marriage)

mod2 <- quap(
  alist(
    M ~ dnorm(mu, sig),
    mu ~ a + ba * A ,
    a ~ dnorm(0,1),
    ba ~ dnorm(0,.5),
    sig~ dexp(1)
  ),
  data = d
)

precis(mod)

samps <- extract.samples(mod2)

select(samps,ba)%>%
  ggplot(aes(x=ba))+
  geom_density()+
  geom_vline(xintercept = 0, linetype = "dashed")

plot(coeftab(mod,mod2), par = c("ba","bm"))

#milk plot
data("milk")
d <- milk

str(d)
d$E <- scale(d$kcal.per.g)
d$M <- scale(d$mass)
d$B <- scale(d$neocortex.perc)

dcc <- d[complete.cases(d),]

biv <- quap(
  alist(
    E ~ dnorm(mu, sig),
    mu ~ a + b * B,
    a ~ dnorm(0,.2),
    b ~ dnorm(0,.5),
    sig ~ dexp(1)
  ),
  data = dcc
)

precis(biv)

xseq <- seq( from=min(dcc$E)-0.15 , to=max(dcc$E)+0.15 , length.out=30 )
mu <- link( biv ,data = list(E=xseq))


#Catagorical Variables
rm(list = ls())

data("Howell1")
d <- Howell1

str(d)

d <- filter(d, age >= 18)

d$h <- scale(d$height)
d$w <- scale(d$weight)





# Chapter 5
# Practice
#===========
rm(list = ls())

data(foxes)
d <- foxes


#5H.1
# model of weight based on territory

with(d, plot(area,weight))


mod.1 <- quap(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + b * area,
    a ~ dunif(0,5),
    b ~ dnorm(0,1),
    sigma ~ dexp(1)
  ), data = d 
)

sim(mod.1,data = list(area = seq(0,)))


#===========
# Chapter 6
# In Text
#===========
rm(list = ls())
#Multicollinearity
data(milk)
d <- milk

d$K <- scale( d$kcal.per.g )
d$F <- scale( d$perc.fat )
d$L <- scale( d$perc.lactose )

mod.fat <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu ~ a + bf*`F`,
    a ~ dnorm(0,.2),
    bf ~ dnorm(0,.5),
    sigma ~ dexp(1)
  ), data = d)

mod.lac <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu ~ a + bl*`L`,
    a ~ dnorm(0,.2),
    bl ~ dnorm(0,.5),
    sigma ~ dexp(1)
  ), data = d)

precis(mod.fat)
precis(mod.lac)

#model with both
mod.both <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu ~ a + bl*`L`+ bf*`F`,
    a ~ dnorm(0,.2),
    bl ~ dnorm(0,.5),
    bf ~ dnorm(0,.5),
    sigma ~ dexp(1)
  ), data = d)

plot(precis(mod.both))

#Post Treatment bias
N <- 100
# simulate initial heights
h0 <- rnorm(N,10,2)
# assign treatments and simulate fungus and growth
treatment <- rep( 0:1 , each=N/2 )
fungus <- rbinom( N , size=1 , prob=0.5 - treatment*0.4 )
h1 <- h0 + rnorm(N, 5 - 3*fungus)
# compose a clean data frame
d <- data.frame( h0=h0 , h1=h1 , treatment=treatment , fungus=fungus )

#height model
m6.6 <- quap(
  alist(
    h1 ~ dnorm( mu , sigma ),
    mu <- h0*p,
    p ~ dlnorm( 0 , 0.25 ),
    sigma ~ dexp( 1 )
  ), data=d )
plot(precis(m6.6))

#model with treatments
m6.7 <- quap(
  alist(
    h1 ~ dnorm( mu , sigma ),
    mu <- h0 * p,
    p <- a + bt*treatment,
    a ~ dlnorm( 0 , 0.2 ) ,
    bt ~ dnorm( 0 , 0.5 ),
    sigma ~ dexp( 1 )
  ), data=d )
plot(precis(m6.7))

#new data with M
N <- 1000
h0 <- rnorm(N,10,2)
treatment <- rep( 0:1 , each=N/2 )
M <- rbern(N)
fungus <- rbinom( N , size=1 , prob=0.5 - treatment*0.4 + 0.4*M )
h1 <- h0 + rnorm( N , 5 + 3*M )
d2 <- data.frame( h0=h0 , h1=h1 , treatment=treatment , fungus=fungus )

#rerun model
m6.7 <- quap(
  alist(
    h1 ~ dnorm( mu , sigma ),
    mu <- h0 * p,
    p <- a + bt*treatment + bf*fungus,
    a ~ dlnorm( 0 , 0.2 ) ,
    bt ~ dnorm( 0 , 0.5 ),
    bf ~ dnorm( 0, .5),
    sigma ~ dexp( 1 )
  ), data=d2 )
plot(precis(m6.7))


d <- read_excel("~/Book1.xlsx")

d$Target.Defendent <- d$`CONVICTED FRAUD DEFENDANTS` == "April Parks"
d$`AMOUNT STOLEN`[9] <- 31000000
d$`AMOUNT STOLEN` <- log(as.numeric(d$`AMOUNT STOLEN`))
d$log.sent <- log(d$SENTENCE)
ggplot(d,aes(x=`AMOUNT STOLEN`, y=log(SENTENCE), color = Target.Defendent))+
  geom_point()+
  geom_abline(slope = .27,intercept = .10)


d.noapril <- d[d$Target.Defendent == FALSE,]

mod<-quap(
 alist(
   log.sent ~ dnorm(mu,sig),
   mu ~ b*`AMOUNT STOLEN`,
   b ~ dlnorm(0,.25),
   sig ~ dexp(1)
 ), data = d.noapril 
)

stolen.seq <- seq(0,20,length.out = 1000)

samps <- extract.samples(mod)

plot(precis(mod))

simul <- sim(mod, data = list(`AMOUNT STOLEN` = log(554397.71)))

simul.pi <- apply(simul,2,PI)

simul.sentances <- tibble(sentance = exp(simul[,1]))


ggplot(simul.sentances, aes(x = sentance))+
  geom_density()+
  geom_vline(xintercept = 192)

mean(simul.sentances$sentance)

