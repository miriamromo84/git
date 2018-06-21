#### año 2014
#### librerias
library(alabama)
library(gamlss)
library(survey)
library(laeken)

data<-read.csv("D:\\survey\\2014\\enigh2014tradicional.csv")
data<-read.csv("D:\\survey\\2014\\mcs2014.csv")
#### ingreso bruto cuentas nacionales 


data<-read.dta("D:\\survey\\2016\\concentradohogar.dta")
tot<- 14997493000000/4 



data<-read.csv("D:\\survey\\2010\\2010mcs_concen.csv")
## solo tenemos un trimestre 
tot<-  13064257000000/4   # total ingreso por trimestre
# total ingreso por trimestre

#### mcs 2015  tot<-14088820000000/4
muestra<-data$ing_cor
fac<-data$factor
head(muestra)
#### cambio de datos del SAT para precios 2015
##muestraf2014[32]*(1/0.973514171746496)/4
###mean(muestraf2014[1:32]*(1/0.973514171746496)/4)

### leer datos 
tot<-  13064257000000/4   # total ingreso por trimestre 2014

data_info<-fleer(2014,1,tot)   #especificar  entidad 

muestra<-data_info[[1]]
fac<-data_info[[2]]
s<-seq(min(muestra),max(muestra),length.out = 10000)  ## para calcular integrales 
ci<-round(data_info[[3]])  # valor del promedio CN
tot_hog<-data_info[[4]]






## NOTA QUITAREMOS TODOS LOS VALORES 

tot_hog<-sum(fac) # total de hogares de la poblacion 
##  QUITAREMOS TODOS LOS VALORES MENORES A 2000
w<-c()
w<-which(muestra<2000)
if(length(w)==0)
{muestra<-muestra} else 
{muestra<-muestra[-w]
 fac<-fac[-w]
}


fquitados<-tot_hog-sum(fac)  ## re-austar los valores de expansión 
coci<-fquitados/length(fac)
nfac<-(fac+coci)
fac<-nfac  ## nuevos factores ajustados 
sum(fac)


ci <- c(tot/tot_hog)
ci


c1<-tot_hog*0.000001  ### 32 mas ricos
c2<-tot_hog*0.00001   ### 317 mas ricos 

head(muestraf2014) ## el vector muestra2014 información del SATordenada  traerla desde el archivo "obtener_info_sat.R"
cantidad1<-muestraf2014[round(c1)]  ## 
cantidad2<-muestraf2014[round(c2)] ##  
q1<-cantidad1/4 # para 0.000001   77712761  resticcion de percentil por trimestre 

q2<-cantidad2/4  # para 0.00001   18283053   restriccion de percentil por trimestre 

qi<-q1
p=0.999999
qi<-q2
p=0.99999

probs = c(0.1, 0.5, 1, 2, 5, 10,20,30,40, 50,60,70,80,90,95,98,99,99.5,99.9)/100  # vector probabilidades para quantiles
s<-seq(min(muestra),max(muestra),length.out = 10000)  ## para calcular valores de distri solo para lognormal y beta II

########################AJUSTES##############################

### Distribución GB 4 parámetros 

hin<-function(x)  # desigualdad todos los parámetros positivos 
{
  h <- rep(NA, 3)
  h[1]<-x[1]
  h[2]<-x[3]
  h[3]<-x[4]
  h
}


heq<- function(x)  #esperanza igual a constante ci, cuentas nacionales y percentil .99 coincidente 
  
{ h <- rep(NA, 2)
  p<-p
  w <- qf(p, 2 * x[3], 2 * x[4])
  q <- x[1] * (((x[3]/x[4]) * w)^(1/x[2]))
  h[1]<-qi-q  
  h[2]<-ci-(x[1]*beta(x[3]+(1/x[2]),x[4]-(1/x[2]))/beta(x[3],x[4]))  
  h
}



# estimadores de la muestra caso: sin restricción 
modbe2<-gamlss(muestra~1, family=GB2,weights=fac,control=gamlss.control(c.crit = 0.001, n.cyc = 50))

modbe2<-refit(modbe2)   ## si que no converge volver a ajustar
#fit <- vglm(muestra ~ 1, genbetaII, trace = TRUE)
## valores iniciales de los estiomadores
theta<-as.vector(c(fitted(modbe2,c("mu"))[1],fitted(modbe2,c("sigma"))[1],fitted(modbe2,c("nu"))[1],fitted(modbe2,c("tau"))[1]))  
betamues<- constrOptim.nl(par=theta, fn=fbeta2, hin=hin)
xGBsin<-betamues$par # estimadores sin restricción
x<-betamues$par  # solo para calculos
ExGBsin<-x[1]*beta(x[3]+(1/x[2]),x[4]-(1/x[2]))/beta(x[3],x[4])
vGBsin<-betamues$value  # valor verosimilitud

set.seed(10)
ma<- rGB2(100000, x[1], x[2],x[3],x[4])
giniGBsin<-gb2.gini(x[2],x[3],x[4]) # gini
#meanGBsin<-mean(ma)  # promedio 
par<-x
# quantiles 
qGBsin<-qGB2(probs, mu=par[1],sigma=par[2],nu=par[3],tau=par[4], lower.tail = TRUE, 
             log.p = FALSE) 


intGBsin<-c()
for ( i in 1: length(s))
{intGBsin[i]<-integrate(function(x) dGB2(x=x, mu=par[1], sigma=par[2],nu=par[3],tau=par[4]), 
                        lower=0, upper=s[i] )$v}



### caso restringido
ansbeta<-c()
ansbeta <- constrOptim.nl(par=xGBsin, fn=fbeta2, heq=heq,hin=hin)
xGBcon<-ansbeta$par #estimadores de parámetros de distribición
x<-ansbeta$par
ExGBcon<-x[1]*beta(x[3]+(1/x[2]),x[4]-(1/x[2]))/beta(x[3],x[4])
ExGBcon<-round(ExGBcon,2)
#if ((ci-ExGBcon)<1) 
 # ExGBcon<-ExGBcon else { asig<-paste("GB",años[j],sep='')
  #                        theta<-as.matrix(na.omit(val[which(names(val)==asig)]))
   #                       ansbeta<- constrOptim.nl(par=theta, fn=fbeta2, heq=heq,hin=hin)
    #                      xGBcon<-ansbeta$par #estimadores de parámetros de distribición
     #                     x<-ansbeta$par
      #                    ExGBcon<-x[1]*beta(x[3]+(1/x[2]),x[4]-(1/x[2]))/beta(x[3],x[4])
  #}

vGBcon<-ansbeta$value

set.seed(10)
ma<- rGB2(1000000, x[1], x[2],x[3],x[4])
meanGBcon<-mean(ma)
giniGBcon<-gini(ma)
par<-x
qGBcon<-qGB2(probs, mu=par[1],sigma=par[2],nu=par[3],tau=par[4], lower.tail = TRUE, 
             log.p = FALSE)


# integrales para ver desigualdad de 10 y 90 porciento
q10<-qGB2(p=c(.10),mu=par[1],sigma=par[2],nu=par[3],tau=par[4])
q90<-qGB2(p=c(.90),mu=par[1],sigma=par[2],nu=par[3],tau=par[4])

den<-integrate(function(x) x*dGB2(x=x, mu=par[1], sigma=par[2],nu=par[3],tau=par[4]), 
               lower= 0, upper=q10 )
numa<-integrate(function(x) x*dGB2(x=x, mu=par[1], sigma=par[2],nu=par[3],tau=par[4]), 
                lower= 0, upper=q90)
num<-ExGBcon-numa$v
resGB<-num/den$v
numGB<-num
denGB<-den$v


mGB<-max(muestra)
i<-integrate(function(x) x*dGB2(x=x, mu=par[1], sigma=par[2],nu=par[3],tau=par[4]), 
             lower=0, upper=mGB)
cocGB<-ExGBcon-i$v


intGBcon<-c()
for ( i in 1: length(s))
{intGBcon[i]<-integrate(function(x) dGB2(x=x, mu=par[1], sigma=par[2],nu=par[3],tau=par[4]), 
                        lower=0, upper=s[i] )$v}


par<-xGBcon
pes<-c(0, 0.1,.2,.3,.4,.5,.6,.7,.8,.9,.99,0.999,0.9999,0.99999,0.999999)

pes<-c(0.90,0.95,0.99,0.999,0.9999,0.99999,0.999999)
for ( i in 1:length(pes))
    qq[i]<-qGB2(p=pes[i],mu=par[1],sigma=par[2],nu=par[3],tau=par[4])

ii<-c()
for ( i in 2:15)
  ii[i]<-integrate(function(x) x*dGB2(x=x, mu=par[1],sigma=par[2],nu=par[3],tau=par[4]), 
                   lower= qq[i-1], upper=qq[i])$v

qq<-c()
qq[1]<-0
for ( i in 2:15)
  
  qq[i]<-qGB2(p=pes[i],mu=par[1],sigma=par[2],nu=par[3],tau=par[4])
for ( i in 2:15)
  ii[i]<-integrate(function(x) x*dGB2(x=x, mu=par[1],sigma=par[2],nu=par[3],tau=par[4]), 
                   lower= qq[i-1], upper=qq[i])$v


ii[i+1]<-  ii[i+1]<-integrate(function(x) x*dGB2(x=x, mu=par[1],sigma=par[2],nu=par[3],tau=par[4]), 
                              lower= qq[15], upper=10000000000000)$v
ii<-ii[-1]
ii[16]<-sum(ii)
if (ii[15]==ExGBcon) 
  ii[15]<-ii[15] else 
  {ii[15]<-ExGBcon-sum(ii[1:14])
   ii[16]<-ExGBcon}
iiGB<-ii
perGB<-qq[15]  ## percentil .9999999 
t(t(ii))


###caso restringido por promedio ( restriccion integral )
p=0.999999
prom<-mean(muestraf2014[1:32]/4)
prom<-prom*0.000001
ci<-103124.8
qi=q1 ##  

p=0.99999
prom<-mean(muestraf2014[1:315]/4)
prom<-prom*0.00001
qi=q2   ## 


hin<-function(x)  # desigualdad todos los parámetros positivos 
{
  h <- rep(NA, 3)
  h[1]<-x[1]
  h[2]<-x[3]
  h[3]<-x[4]
  h
}


heq<- function(x)  ## restricciones de igualdad, igual promedio CN y integral a partir de un percentil 
{
  #print(x)
  p<-p
  par<-x
  h <- rep(NA, 2)
  umb<-qi
  alfa<-1-p
  inte<-c()
  inte <-try(integrate(function(y) y*(dGB21(y, mu=par[1],sigma=par[2],nu=par[3],tau=par[4])), lower=qi, upper=1000000000000,stop.on.error = FALSE)$v,silent=TRUE)
  if (is.character(inte))
    inte<-1000
  h[1]<-inte-prom
  h[2]<-ci-(x[1]*beta(x[3]+(1/x[2]),x[4]-(1/x[2]))/beta(x[3],x[4])) 
  h
  # print(h)
}


dGB21<-function (x, mu = 1, sigma = 1, nu = 1, tau = 0.5, log = FALSE) 
{
  #if (any(mu < 0)) 
  # stop(paste("mu must be positive", "\n", ""))
  #if (any(nu < 0)) 
  # stop(paste("nu must be positive", "\n", ""))
  #if (any(tau < 0)) 
  #  stop(paste("tau must be positive", "\n", ""))
  z <- (x/mu)^sigma
  loglik <- nu * log(z) + log(abs(sigma)) - log(x) - lgamma(nu) - 
    lgamma(tau) + lgamma(nu + tau) - (nu + tau) * log(1 + z)
  if (log == FALSE) 
    ft <- exp(loglik)
  else ft <- loglik
  ft
}


ansbeta <- constrOptim.nl(par=xGBcon, fn=fbeta2, heq=heq,hin=hin)
par<-ansbeta$par  
xGBcon<-par
x<-ansbeta$par
ExGBcon<-x[1]*beta(x[3]+(1/x[2]),x[4]-(1/x[2]))/beta(x[3],x[4])
ExGBcon<-round(ExGBcon,2)
qGB2(p, mu=par[1],sigma=par[2],nu=par[3],tau=par[4], lower.tail = TRUE, 
     log.p = FALSE)

vGBcon<-ansbeta$value

set.seed(10)
#ma<- rGB2(1000000, x[1], x[2],x[3],x[4])
#meanGBcon<-mean(ma)
giniGBcon<-gb2.gini(x[2],x[3],x[4])
giniGBcon
par<-x
qGBcon<-qGB2(probs, mu=par[1],sigma=par[2],nu=par[3],tau=par[4], lower.tail = TRUE, 
             log.p = FALSE)


# integrales para ver desigualdad de 10 y 90 porciento
q10<-qGB2(p=c(.10),mu=par[1],sigma=par[2],nu=par[3],tau=par[4])
q90<-qGB2(p=c(.90),mu=par[1],sigma=par[2],nu=par[3],tau=par[4])

den<-integrate(function(x) x*dGB2(x=x, mu=par[1], sigma=par[2],nu=par[3],tau=par[4]), 
               lower= 0, upper=q10 )
numa<-integrate(function(x) x*dGB2(x=x, mu=par[1], sigma=par[2],nu=par[3],tau=par[4]), 
                lower= 0, upper=q90 )
num<-ExGBcon-numa$v
resGB<-num/den$v
numGB<-num
denGB<-den$v


mGB<-max(muestra)
i<-integrate(function(x) x*dGB2(x=x, mu=par[1], sigma=par[2],nu=par[3],tau=par[4]), 
             lower=0, upper=mGB)
cocGB<-ExGBcon-i$v


intGBcon<-c()
for ( i in 1: length(s))
{intGBcon[i]<-integrate(function(x) dGB2(x=x, mu=par[1], sigma=par[2],nu=par[3],tau=par[4]), 
                        lower=0, upper=s[i] )$v}


par<-xGBcon
pes<-c(0, 0.1,.2,.3,.4,.5,.6,.7,.8,.9,.99,0.999,0.9999,0.99999,0.999999)
qq<-c()
qq[1]<-0
for ( i in 2:15)
  
  qq[i]<-qGB2(p=pes[i],mu=par[1],sigma=par[2],nu=par[3],tau=par[4])
for ( i in 2:15)
  ii[i]<-integrate(function(x) x*dGB2(x=x, mu=par[1],sigma=par[2],nu=par[3],tau=par[4]), 
                   lower= qq[i-1], upper=qq[i])$v


ii[i+1]<-  ii[i+1]<-integrate(function(x) x*dGB2(x=x, mu=par[1],sigma=par[2],nu=par[3],tau=par[4]), 
                              lower= qq[15], upper=1000000000000)$v
ii<-ii[-1]
ii[16]<-sum(ii)
if (ii[15]==ExGBcon) 
  ii[15]<-ii[15] else 
  {ii[15]<-ExGBcon-sum(ii[1:14])
   ii[16]<-ExGBcon}
iiGB<-ii
perGB<-qq[15]  ## percentil .9999999 
t(t(ii))




###########dISTRIBUCION PARETO 
library(actuar)


hin<-function(x)
{
  h <- rep(NA, 2)
  h[1]<-x[1]-1
  h[2]<-x[2]
  h
}


heq<- function(x)  #esperanza igual a constante 
{  h <- rep(NA, 2)
   h[1]<-qi-(qpareto(p,x[1],x[2]))
   h[2]<-ci-(mpareto(1,x[1],x[2]))
   h
}

#caso no restringido 

fparet2<-function(x,m=muestra,w=fac) ## funcion de verosi
{ 
  fi<-c()
  fi<- -1*w*(dpareto(m, x[1],x[2],log=TRUE))
  sumaf<-sum(fi)
  return(sumaf)
  
}


parmues <- constrOptim.nl(par=c(10,10), fn=fparet2, hin=hin)            
xPsin<-parmues$par
x<-parmues$par
vPsin<-parmues$value
ExPsin<-mpareto(1,x[1],x[2])
set.seed(10)
ma<- rpareto(100000, x[1], x[2])
#meanLNsin<-mean(ma)
giniPsin<-gini(ma)
par<-x
qPsin<-qpareto(probs, shape=par[1],scale=par[2])

intPsin<-c()
for ( i in 1: length(s))
{intPsin[i]<-integrate(function(x) dpareto(x=x, shape=par[1],scale=par[2]), 
                       lower=0, upper=s[i] )$v}

## caso restringido
anspar<- constrOptim.nl(par=xPsin, fn=fparet2, heq=heq,hin=hin)
xPcon<-anspar$par
x<- xPcon
ExPcon <-mpareto(1,x[1],x[2])  ## esperanza
ExPcon<-round(ExPcon,2)
qpareto(0.999999, shape=x[1],scale=x[2])
vPcon<-anspar$value               ### verosimilitud

set.seed(10)
ma<- rpareto(100000, x[1], x[2])
#meanLNcon<-mean(ma)
giniPcon<-gini(ma)
par<-x
qPcon<-qpareto(probs, shape=par[1],scale=par[2])

q10<-qpareto(p=c(.10),shape=par[1],scale=par[2])
q90<-qpareto(p=c(.90),shape=par[1],scale=par[2])

den<-integrate(function(x) x*dpareto(x=x, par[1],par[2]), 
               lower= 0, upper=q10 )
numa<-integrate(function(x) x*dpareto(x=x, par[1], par[2]), 
                lower= 0, upper=q90 )
num<- ExPcon-numa$v
resP<-num/den$v
numP<-num
denP<-den$v

mP<-max(muestra)
i<-integrate(function(x) x*dpareto(x=x, par[1], par[2]), 
             lower= 0, upper=mP )
cocP<- ExPcon-i$v


intPcon<-c()
for ( i in 1: length(s))
  
{intPcon[i]<-integrate(function(x) dpareto(x=x,par[1], par[2]), 
                       lower=0, upper=s[i] )$v}



par<-xPcon
pes<-c(0, 0.1,.2,.3,.4,.5,.6,.7,.8,.9,.99,0.999,0.9999,0.99999,0.999999)
qq<-c()
qq[1]<-0

qq<-c()
qq[1]<-0
for ( i in 2:15)
  
  qq[i]<-qpareto(p=pes[i],par[1],par[2])


ii<-c()
for ( i in 2:15)
  ii[i]<-integrate(function(x) x*dpareto(x=x, par[1], par[2]), 
                   lower= qq[i-1], upper=qq[i])$v


ii[i+1]<-  ii[i+1]<-integrate(function(x) x*dpareto(x=x, par[1], par[2]), 
                              lower= qq[15], upper=1000000000)$v
ii<-ii[-1]
ii[16]<-sum(ii)
if (ii[15]==ExPcon) 
  ii[15]<-ii[15] else 
  {ii[15]<-abs(ExPcon-sum(ii[1:14]))
   ii[16]<-ExPcon}
iiP<-ii
perP<-qq[15]  ## percentil .9999999 
t(t(iiP))




############ distribución log-normal 



hin<-function(x)
{
  h <- rep(NA, 2)
  h[1]<-x[1]
  h[2]<-x[2]
}


heq<- function(x)  #esperanza igual a constante 
{ h <- rep(NA, 2)
  q <- qlnorm(p, meanlog = x[1], sdlog =x[2])
  h[1]<-ci-((exp(x[2]^2))^(0.5)*exp(x[1]))
  h[2]<-qi-q
  h
}

#caso no restringido 

log<-gamlss(muestra~1, family=LOGNO,weights=fac)
mu<-fitted(log,"mu")[1]
sigma<-fitted(log,"sigma")[1]
theta<-c(mu,sigma)

logmues <- constrOptim.nl(par=theta, fn=flog2, hin=hin)            
xLNsin<-logmues$par
x<-logmues$par
vLNsin<-logmues$value
ExLNsin<-(exp(x[2]^2))^(0.5)*exp(x[1])
set.seed(10)
ma<- rLOGNO(100000, x[1], x[2])
#meanLNsin<-mean(ma)
giniLNsin<-gini(ma)
par<-x
qLNsin<-qLOGNO(probs, mu=par[1],sigma=par[2], lower.tail = TRUE, 
               log.p = FALSE)

intLNsin<-c()
for ( i in 1: length(s))
{intLNsin[i]<-integrate(function(x) dLOGNO(x=x, mu=par[1], sigma=par[2]), 
                        lower=0, upper=s[i] )$v}


## caso restringido
anslog<-c()
anslog <- constrOptim.nl(par=xLNsin, fn=flog2, heq=heq,hin=hin)
xLNcon<-anslog$par
x<-anslog$par
ExLNcon<-(exp(x[2]^2))^(0.5)*exp(x[1])  ## esperanza
ExLNcon<-round(ExLNcon,2)
#if ((ci-ExLNcon)<1) 
 # ExLNcon<-ExLNcon else {asig<-paste("LN",años[j],sep='')
  #                       theta<-as.matrix(na.omit(val[which(names(val)==asig)]))
   #                      anslog<- constrOptim.nl(par=theta, fn=flog2, heq=heq,hin=hin)
    #                     xLNcon<-anslog$par
    #                     x<-anslog$par
    #                     ExLNcon<-(exp(x[2]^2))^(0.5)*exp(x[1])
  #}

vLNcon<-anslog$value               ### verosimilitud

set.seed(10)
ma<- rLOGNO(100000, x[1], x[2])
#meanLNcon<-mean(ma)
giniLNcon<-gini(ma)
par<-x
qLNcon<-qLOGNO(probs, mu=par[1],sigma=par[2], lower.tail = TRUE, 
               log.p = FALSE)

q10<-qLOGNO(p=c(.10),mu=par[1],sigma=par[2])
q90<-qLOGNO(p=c(.90),mu=par[1],sigma=par[2])

den<-integrate(function(x) x*dLOGNO(x=x, mu=par[1], sigma=par[2]), 
               lower= 0, upper=q10 )
numa<-integrate(function(x) x*dLOGNO(x=x, mu=par[1], sigma=par[2]), 
                lower= 0, upper=q90 )
num<- ExLNcon-numa$v
resLN<-num/den$v
numLN<-num
denLN<-den$v

mLN<-max(muestra)
i<-integrate(function(x) x*dLOGNO(x=x, mu=par[1], sigma=par[2]), 
             lower= 0, upper=mLN )
cocLN<- ExLNcon-i$v


intLNcon<-c()
for ( i in 1: length(s))
  
{intLNcon[i]<-integrate(function(x) dLOGNO(x=x, mu=par[1], sigma=par[2]), 
                        lower=0, upper=s[i] )$v}



par<-xLNcon
pes<-c(0, 0.1,.2,.3,.4,.5,.6,.7,.8,.9,.99,0.999,0.9999,0.99999,0.999999)
qq<-c()
qq[1]<-0
for ( i in 2:15)
  
  qq[i]<-qLOGNO(p=pes[i],mu=par[1],sigma=par[2])
for ( i in 2:15)
  ii[i]<-integrate(function(x) x*dLOGNO(x=x, mu=par[1],sigma=par[2]), 
                   lower= qq[i-1], upper=qq[i])$v


ii[i+1]<-  ii[i+1]<-integrate(function(x) x*dLOGNO(x=x, mu=par[1],sigma=par[2]), 
                              lower= qq[15], upper=10000000000000)$v
ii<-ii[-1]
ii[16]<-sum(ii)
if (ii[15]==ExLNcon) 
  ii[15]<-ii[15] else 
  {ii[15]<-ExLNcon-sum(ii[1:14])
   ii[16]<-ExLNcon}
iiLN<-ii
perLN<-qq[15]  ## percentil .9999999 
t(t(iiLN))



####################### distribucion gamma generalizada 3 parametros 


hin<-function(x)
{
  h <- rep(NA, 2)
  h[1]<-x[1]
  h[2]<-x[2]
  h
}


heq<- function(x)  
{ 
  h <- rep(NA, 2)
  p=p
  mu<-x[1]
  sigma<-x[2]
  nu<-x[3]
  t<-1/(x[2]^2*x[3]^2)
  
  if (length(nu) > 1) {
    p <- ifelse(nu > 0, p, 1 - p)
    z <- ifelse(abs(nu) > 1e-06, qGA2(p, mu = 1, sigma = sigma * 
                                        abs(nu)), qNO(p, mu = log(mu), sigma = sigma, ))
    y <- ifelse(abs(nu) > 1e-06, mu * z^(1/nu), exp(z))
  }   else if (abs(nu) > 1e-06) {
    p <- if (nu > 0) 
      p
    else 1 - p
    z <- qGA2(p, mu = 1, sigma = sigma * abs(nu))
    y <- mu * z^(1/nu)
  }   else {
    z <- qNO(p, mu = log(mu), sigma = sigma)
    y <- exp(z)
  }
  h[1]<-qi-y
  h[2]<-ci-(x[1]*gamma(t+(1/x[3]))/(t^(1/x[3])*gamma(t)))
  h
}


33884.2 0.854982 -0.0893391 

34282 0.85269 -0.0895639 

35636.2 0.845051 -0.090413

37082.8 0.798757 -0.923591 
#caso no restringido
gammues<-gamlss(muestra~1, family=GG,weights=fac)
gmag<-refit(gammues)
mu<-fitted(gmag,"mu")[1]
sigma<-fitted(gmag,"sigma")[1]
nu<-fitted(gmag,"nu")[1]
theta<-c(mu,sigma,nu)

gammues<-c()
x<-c()
gammues<-constrOptim.nl(par=theta, fn=fgamG, hin=hin)
xGGsin<-gammues$par
x<-gammues$par
t<-1/(x[2]^2*x[3]^2)
ExGGsin<-(x[1]*gamma(t+(1/x[3])))/(t^(1/x[3])*gamma(t))
vGGsin<-gammues$value

set.seed(10)
ma<- rGG(1000000, x[1], x[2],x[3])
#meanGGsin<-mean(ma)
giniGGsin<-gini(ma)
par<-x
qGGsin<-qGG(probs, mu=par[1],sigma=par[2], nu=par[3],lower.tail = TRUE, 
            log.p = FALSE)




qGA2<-function (p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
{
  q <- qgamma(p, shape = 1/sigma^2, scale = mu * sigma^2, lower.tail = lower.tail, 
              log.p = log.p)
  q
}



41237.4 0.816385 -0.0935657
41239.4 0.816385 -0.0935657 
41249.4 0.816385 -0.0935657
41296.5 0.816385 -0.0935657
theta<-c(41314.4, 0.816385 ,-0.0935657)
### caso restringido 
ansgam<-c()

ansgam<- constrOptim.nl(par=xGGsin, fn=fgamG, heq=heq,hin=hin,control.optim=list(method="L-BFGS-B"))

xGGcon<-ansgam$par
x<-ansgam$par
t<-1/(x[2]^2*x[3]^2)
ExGGcon<-x[1]*gamma(t+(1/x[3]))/(t^(1/x[3])*gamma(t))
vGGcon<-ansgam$value
vGGcon
set.seed(10)
ma<- rGG(10000000, x[1], x[2],x[3])
#meanGGcon<-mean(ma)
giniGGcon<-gini(ma)
giniGGcon
par<-x
qGG(p, mu=par[1],sigma=par[2], nu=par[3],lower.tail = TRUE, log.p = FALSE)
qGGcon<-qGG(probs, mu=par[1],sigma=par[2], nu=par[3],lower.tail = TRUE,log.p = FALSE)
t(t(qGGcon))

if ((ci-ExGGcon)<1) 
  ExGGcon<-ExGGcon else 
  {asig<-paste("GG",años[j],sep='')
   theta<-as.matrix(na.omit(val[which(names(val)==asig)]))
   ansgam<- constrOptim.nl(par=theta, fn=fgamG, heq=heq,hin=hin)
   xGGcon<-ansgam$par
   x<-ansgam$par
   t<-1/(x[2]^2*x[3]^2)
   ExGGcon<-x[1]*gamma(t+(1/x[3]))/(t^(1/x[3])*gamma(t))
  }



q10<-qGG(p=c(.10),mu=par[1],sigma=par[2],nu=par[3])
q90<-qGG(p=c(.90),mu=par[1],sigma=par[2],nu=par[3])

den<-integrate(function(x) x*dGG(x=x, mu=par[1], sigma=par[2],nu=par[3]), 
               lower= 0, upper=q10)
numa<-integrate(function(x) x*dGG(x=x, mu=par[1], sigma=par[2],nu=par[3]), 
                lower= 0, upper=q90 )
num<-ExGGcon-numa$v
resGG<-num/den$v
numGG<-num
denGG<-den$v

mGG<-max(muestra)

i<-integrate(function(x) x*dGG(x=x, mu=par[1], sigma=par[2],nu=par[3]), 
             lower= 0, upper=mGG )
cocGG<-ExGGcon-i$v



par<-xGGcon
pes<-c(0, 0.1,.2,.3,.4,.5,.6,.7,.8,.9,.99,0.999,0.9999,0.99999,0.999999)
qq<-c()
qq[1]<-0
for ( i in 2:15)
  
  qq[i]<-qGG(p=pes[i],mu=par[1],sigma=par[2],nu=par[3])
for ( i in 2:15)
  ii[i]<-integrate(function(x) x*dGG(x=x, mu=par[1],sigma=par[2],nu=par[3]), 
                   lower= qq[i-1], upper=qq[i])$v

ii[i+1]<-  ii[i+1]<-integrate(function(x) x*dGG(x=x, mu=par[1],sigma=par[2],nu=par[3]), 
                              lower= qq[15], upper=10000000000000)$v
ii<-ii[-1]
ii[16]<-sum(ii)
if (ii[15]==ExGGcon) 
  ii[15]<-ii[15] else 
  {ii[15]<-ExGGcon-sum(ii[1:14])
   ii[16]<-ExGGcon}
iiGG<-ii
perGG<-qq[15]  ## percentil .9999999 
t(t(iiGG))


############### caso restringido promedio 

p=0.999999
prom<-mean(muestra2012[1:32]/4)
prom<-prom*0.000001
ci<-92170.8
qi=q1 ##  177,965,502.00

p=0.99999
prom<-mean(muestra2012[1:315])
prom<-prom*0.00001
ci<-92170.8
qi=q2   ##  21,062,723

hin<-function(x)
{
  h <- rep(NA, 2)
  h[1]<-x[1]
  h[2]<-x[2]
  h
}


heq<- function(x)  
{
  #print(x)
  p<-p
  par<-x
  t<-1/(x[2]^2*x[3]^2)
  h <- rep(NA, 2)
  inte<-c()
  inte <-try(integrate(function(y) y*(dGG1(y, mu=par[1],sigma=par[2],nu=par[3])), lower=qi, upper=1000000000000,stop.on.error = FALSE)$v,silent=TRUE)
  if (is.character(inte))
    inte<-1000
  h[1]<-inte-prom
  h[2]<-ci-(x[1]*gamma(t+(1/x[3]))/(t^(1/x[3])*gamma(t)))
  h
  # print(h)
}



dGG1<-function (x, mu = 1, sigma = 0.5, nu = 1, log = FALSE) 
{
  z <- (x/mu)^nu
  theta <- 1/(sigma^2 * nu^2)
  if (length(nu) > 1) 
    loglik <- ifelse(abs(nu) > 1e-06, dGA1(z, mu = 1, sigma = sigma * 
                                             abs(nu), log = TRUE) + log(abs(nu) * z/x), -log(x) - 
                       0.5 * log(2 * pi) - log(sigma) - (1/(2 * sigma^2)) * 
                       (log(x) - log(mu))^2)
  else if (abs(nu) > 1e-06) 
    loglik <- dGA1(z, mu = 1, sigma = sigma * abs(nu), log = TRUE) + 
    log(abs(nu) * z/x)
  else loglik <- -log(x) - 0.5 * log(2 * pi) - log(sigma) - 
    (1/(2 * sigma^2)) * (log(x) - log(mu))^2
  if (log == FALSE) 
    ft <- exp(loglik)
  else ft <- loglik
  ft
}
dGA1<-function (x, mu = 1, sigma = 1, log = FALSE) 
{
  log.lik <- (1/sigma^2) * log(x/(mu * sigma^2)) - x/(mu * 
                                                        sigma^2) - log(x) - lgamma(1/sigma^2)
  if (log == FALSE) 
    fy <- exp(log.lik)
  else fy <- log.lik
  fy
}

ansGG <- constrOptim.nl(par=xGGsin, fn=fgamG, heq=heq,hin=hin)
par<-ansGG$par 

qGG(p, mu=par[1],sigma=par[2], nu=par[3],lower.tail = TRUE, log.p = FALSE)

xGGcon<-ansGG$par
x<-ansGG$par
t<-1/(x[2]^2*x[3]^2)
ExGGcon<-x[1]*gamma(t+(1/x[3]))/(t^(1/x[3])*gamma(t))

vGGcon<-ansGG$value
set.seed(10)
ma<- rGG(100000, x[1], x[2],x[3])
#meanGGcon<-mean(ma)
giniGGcon<-gini(ma)
giniGGcon<-gGG.gini(x[1],x[2],x[3])
par<-x
qGGcon<-qGG(probs, mu=par[1],sigma=par[2], nu=par[3],lower.tail = TRUE, 
            log.p = FALSE)

q10<-qGG(p=c(.10),mu=par[1],sigma=par[2],nu=par[3])
q90<-qGG(p=c(.90),mu=par[1],sigma=par[2],nu=par[3])

den<-integrate(function(x) x*dGG(x=x, mu=par[1], sigma=par[2],nu=par[3]), 
               lower= 0, upper=q10)
numa<-integrate(function(x) x*dGG(x=x, mu=par[1], sigma=par[2],nu=par[3]), 
                lower= 0, upper=q90 )
num<-ExGGcon-numa$v
resGG<-num/den$v
numGG<-num
denGG<-den$v

mGG<-max(muestra)

i<-integrate(function(x) x*dGG(x=x, mu=par[1], sigma=par[2],nu=par[3]), 
             lower= 0, upper=mGG )
cocGG<-ExGGcon-i$v





par<-xGGcon
ii<-c()
pes<-c(0, 0.1,.2,.3,.4,.5,.6,.7,.8,.9,.99,0.999,0.9999,0.99999,0.999999)
qq<-c()
qq[1]<-0
for ( i in 2:15)
  
  qq[i]<-qGG(p=pes[i],mu=par[1],sigma=par[2],nu=par[3])
for ( i in 2:15)
  ii[i]<-integrate(function(x) x*dGG(x=x, mu=par[1],sigma=par[2],nu=par[3]), 
                   lower= qq[i-1], upper=qq[i])$v


ii[i+1]<-  ii[i+1]<-integrate(function(x) x*dGG(x=x, mu=par[1],sigma=par[2],nu=par[3]), 
                              lower= qq[15], upper=10000000000000)$v
ii<-ii[-1]
ii[16]<-sum(ii)
if (ii[15]==ExGGcon) 
  ii[15]<-ii[15] else 
  {ii[15]<-ExGGcon-sum(ii[1:14])
   ii[16]<-ExGGcon}

iiGG<-ii
perGG<-qq[15]  ## percentil .9999999 
t(t(iiGG))



par<-xGGsin
tem<-data$ing_cor
w<-which(tem==0)
tem<-tem[-w]
pp<-pGG(tem,mu=par[1],sigma=par[2],nu=par[3])
par<-xGGcon
qq<-qGG((1-pp),mu=par[1],sigma=par[2],nu=par[3])

datosnuevo<-cbind(tem,(1-pp),qq)
write.csv(datosnuevo,"D://survey//2010//datos_mcs_2010.csv")
##### fin de ajuste de distribuciones 
#### datos de la muestra 


if (length(w)==0)
{data.desp <- svydesign(id=~1,weights=~factor,data=data)} else
{data.desp <- svydesign(id=~1,weights=~factor,data=data[-w,])
}

media<-svymean(~muestra,data.desp)
quantdis<- svyquantile(~muestra, data.desp, probs,ci=TRUE)


##### salida de información#############


a1<-c(vPcon,vLNcon,vGGcon,vGBcon)
a2<-c(vPsin,vLNsin,vGGsin,vGBsin)
a3<-c(giniPsin,giniLNsin,giniGGsin,giniGBsin)
#a3<-c(giniGsin$value,giniLNsin$value,giniGGsin$value,giniGBsin$value)/100
a4<-c(as.numeric(ExPsin),as.numeric(ExLNsin),as.numeric(ExGGsin),as.numeric(ExGBsin))
av1<-c(0,0 ,0 ,0)
di<-c("-","-","-","-")
d<-matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),4,4)
d[1:2,1]<-t(t(xPsin))
d[1:2,2]<-t(t(xLNsin))
d[1:3,3]<-t(t(xGGsin))
d[,4]<-t(t(xGBsin))
av2<-c(0,0 ,0 ,0)
a5<-cbind(qPsin,qLNsin,qGGsin,qGBsin)
#a5<-a5[-(20:21),]
salida1<-(rbind(a1,a2,a3,a4,di,d,di,a5))
colnames(salida1)<-c("P","L-N","GG","GB2")
rownames(salida1)<-c("Verosi-restringido","Verosi-no restingido","Gini-SR",
                     "Esperanza-SR","parámetros estimados-SR","mu","sigma","nu","tau","Percentiles-SR","0.1"," 0.5", "1", "2", "5", "10","20","30","40", "50","60","70","80","90","95","98","99","99.5","99.9")
#write.csv(salida1,"salida1.csv")
b3<-c(giniPcon,giniLNcon,giniGGcon,giniGBcon)
#b3<-c(giniGcon$value,giniLNcon$value,giniGGcon$value,giniGBcon$value)/100
b4<-c(as.numeric(ExPcon),as.numeric(ExLNcon),as.numeric(ExGGcon),as.numeric(ExGBcon))
bv1<-c(0,0 ,0 ,0)
d2<-matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),4,4)
d2[1:2,1]<-t(t(xPcon))
d2[1:2,2]<-t(t(xLNcon))
d2[1:3,3]<-t(t(xGGcon))
d2[1:4,4]<-t(t(xGBcon))

b5<-cbind(qPcon,qLNcon,qGGcon,qGBcon)
#b5<-b5[-(20:21),]
b6<-c(numP,numLN,numGG,numGB)
b7<-c(denP,denLN,denGG,denGB)
b8<-c(resP,resLN,resGG,resGB)
b9<-c(cocP,cocLN,cocGG,cocGB)
b10<-b9/b4
b11<-cbind(iiP,iiLN,iiGG,iiGB) ## integrales 
b12<-c(perP,perLN,perGG,perGB)
mG<-max(muestra)
vm<-c(mG,"-","-","-") # valor mayor de la muestra
promi<-c(matrix (media)[1],"-","-","-") # promedio enigh
sdi<-c(as.numeric(cbind(c(data.frame(media)[2]))),"-","-","-") # desviacion del promediop
tem1<-c(tot,"-","-","-")   ## total de ingreso CN trimestre
tem2<-c(tot_hog,"-","-","-")  ## total hogares 

tem3<-c(qi,"-","-","-")
tem4<-c(prom/(1-p),"-","-","-")
totencu<-c(sum(muestra*fac),"-","-","-")  ## total ingreso segun encuesta 

salida2<-rbind(b3,b4,di,d2,di,b5,b12,b6,b7,b8,di,b9,b10,vm,promi,sdi,tem1,totencu,tem2,tem3,tem4)

colnames(salida2)<-c("P","L-N","GG","GB2")
rownames(salida2)<-c("Gini-CR",
                     "Esperanza-CR","Parámetros estimados-CR","mu","sigma","nu","tau","Percentiles-CR","0.1"," 0.5", "1", "2", "5", "10","20","30","40", "50","60","70","80","90","95","98","99","99.5","99.9",
                     get("p"),"numerador","denominador","cociente","Relación acumulados mayores al máximo observado a ingresos totales","valor","cociente",
                     "valor máximo de la muestra","promedio ENIGH","sd promedio","tot_SCNM","tot_encuesta","Hogares","valor de percentil","promedio")

### probabilidades mj
c1<-cbind(intGBsin,intGBcon,intLNsin,intLNcon)
colnames(c1)<-c("P(x<mj)GB2_sin_restrición","P(x<mj)GB2_con_restrición","P(x<mj)LN_sin_restrición","P(x<mj)LN_con_restrición")
s<-as.character(s)
rownames(c1)<-s

#write.csv(c1,"pxmj.csv")

## percentiles muestra expandida 

ddd<-cbind(t(quantdis$qua),t(matrix(quantdis$CI,nrow=2)))
#ddd<-cbind(t(t(quantdis)),cbind(t(t(quantdis)),t(t(quantdis))))
ddd<-cbind(ddd,rep(0,19))
colnames(ddd)<-c("percentil","límite inferior","límite superior","-")
rownames(ddd)<-c("0.1","0.5", "1", "2", "5", "10","20","30","40", "50","60","70","80","90","95","98","99","99.5","99.9")

salida<-paste("salida",i,sep="")
integrales<-b11
rownames(integrales)<-c(
  "0---10",
  "10---20",
  "20---30",
  "30---40",
  "40---50",
  "50---60",
  "60---70",
  "70---80",
  "80---90",
  "90---99",
  "99---99.9",
  "99.9---99.99",
  "99.99---99.999",
  "99.999---99.9999",
  "99.9999--Inf",
  "suma")


salida3<-(rbind(salida1,c("-","-","-","-"),c("-","-","-","-"),c("P","L-N","GG","GB2"),salida2,c("-","-","-","-"),c("-","-","-","-"),c("percentil","límite inferior","límite superior","-"),ddd,c("-","-","-","-"),c("-","-","-","-"),c("P_GB2_SR","P_GB2_CR","PLN_SR","PLN_CR"),c1,c("-","-","-","-"),c("-","-","-","-"),c("iP","iLN","iGG","iGB"),integrales))

carpeta<-paste("D:","//","survey","//",años[j],"//",ei,sep="")
write.csv(salida3,paste(carpeta,"//","salida_proMCS_datosMCS",ei,".","csv",sep=""))
write.csv(salida3,"D:survey//2014//prueba.csv")
}

medacu<-c()
for (i in 1:length(muestra))
{medacu[i]<-integrate(function(x) x*dGG(x=x, mu=par[1],sigma=par[2],nu=par[3]), 
           lower=0 , upper=muestra[i])$v
}

head(medacu)


proacu<-c()
for (i in 1:length(muestra))
{proacu[i]<-integrate(function(x) dGG(x=x, mu=par[1],sigma=par[2],nu=par[3]), 
                      lower=0 , upper=muestra[i])$v
}

head(proacu)

tem<-cbind(medacu,proacu)
write.csv(tem,"D://tem.csv")


tem<-function(x){ y<-numeric(1) 
             y[1]<-c1-((I1*F1+I2*F2+I3*x)/(F1+F2+x))
             y}



ansGG <- constrOptim.nl(par=par, fn=fgamG, heq=heq,hin=hin)
par<-ansGG$par 


heq<- function(x)  
{ 
  #print(x)
  par<-x
  h <- rep(NA, 1)
  inte<-c()
  inte <-try(integrate(function(y) y*(dGG1(y, mu=par[1],sigma=par[2],nu=par[3])), lower=0, upper=100000000,stop.on.error = FALSE)$v,silent=TRUE)
  if (is.character(inte))
    inte<-1000
  h[1]<-inte-ci
   h
  # print(h)
}

