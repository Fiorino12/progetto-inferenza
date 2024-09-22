library(car)
library(faraway)
library(leaps)
library(MASS)
library(GGally)
library(rgl)
library(dplyr)
library(data.table)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(AID)
library(onewaytests)



#Carico il dataset
Foot=read.csv("Football teams.csv", header=TRUE);
head(Foot)

#summarydeidati
summary(Foot)


#controllo correlazione covariate
ggpairs(data = Foot[c('Goals','Shots.pg','yellow_cards','red_cards','Possession.','Pass.','AerialsWon','Rating')], title ="Relationships between predictors & response",lower = list(continuous=wrap("points", alpha = 0.5, size=0.1)))


#creo il modello dove y è Goals con TUTTE le covariate disponibili(Tranne le categoriche)
g=lm(Goals~.-Tournament-Team, Foot)
summary(g)

# R2 adj e R2 molto alti-->ok
# tra le covariate, quelle significative sono Shots_yellowcards_AerialsWon_Rating---->infatti il pvalue dell'Ftest è basso--> esiste almeno una covariata sign

#controllo correlazione tra covariate
X=Foot[c(4:10)]
cor(X)
corrplot( cor(X), method='number')

#probabilmente il modello va ridotto perché alcune covariate sono molto correlate tra loro, tipo Goals-Shots o Pass-possession

#controllo omoschedasticità residui
plot(g,which=1)
#per ora torna tutto

#shapiro benisssimo
shapiro.test(g$residuals)

#qqnorm bello
qqnorm( g$residuals, ylab = "Raw Residuals", pch = 16 )
qqline( g$residuals )

#effettuiamo la pulizia dei dati per migliorare il dataset --> controllando i punti leva


#lev = hatvalues( g )  
#lev

#p = g$rank # p = 98
#n = dim(Foot)[1] # n = 8 , estrae il numero di righe

#plot( g$fitted.values, lev, ylab = "Leverages", main = "Plot of Leverages",
  #    pch = 16, col = 'black' )
#abline( h = 2 * p/n, lty = 2, col = 'red' )
#watchout_points_lev = lev[ which( lev > 2 * p/n ) ]
#watchout_ids_lev = seq_along( lev )[ which( lev > 2 * p/n ) ]
#points( g$fitted.values[ watchout_ids_lev ], watchout_points_lev, col = 'red', pch = 16 )


#watchout_points_lev

#influence.measures(g)

# abbiamo fatto un doppio controllo sui punti influenti. Sia con l'infMeasures che con il plot otteniamo 3 punti di merda che andremo a togliere

#g_post_lev = lm(Rating~.-Team-Tournament,Foot, subset = ( lev<2*p/n)  )
#summary(g_post_lev)

#shapiro.test(g_post_lev$residuals)

#gs = summary(g)
#res_std = g$res/gs$sigma
#cerco quelli >2
#watchout_ids_rstd = which( abs( res_std ) > 2 )
#watchout_rstd = res_std[ watchout_ids_rstd ]
#watchout_rstd   #16
#stud = rstandard( g )

#watchout_ids_stud = which( abs( stud ) > 2 )
#watchout_stud = stud[ watchout_ids_stud ]
#watchout_stud



#g_post_both = lm( Rating~.-Team-Tournament, Foot, subset = ( abs(stud)<2 | lev<2*p/n ))


#shapiro.test(g_post_both$residuals)

#ancora lo shapiro viene basso->BOXCOX

#b=boxcox(g_post_both)
#best_lambda=b$x[which.max(b$y)]
#best_lambda

#il nuovo modello avra y=(y_old^-2-1)/(-2)

#g_post_bc=lm(((Rating)^best_lambda-1)/(best_lambda)~.-Team-Tournament, Foot, subset = ( lev<2*p/n))
#summary(g_post_bc)
#shapiro.test(g_post_bc$residuals)
#qqnorm(g_post_bc$residuals)
#qqline(g_post_bc$residuals)

#LE HP SONOVERIFICATE

#SELEZIONO LE COVARIATE SIGNIFICATIVE

AIC(g)

step(g,direction="backward", trace=T)

#il modello con AIC minore è Goals=shots+yellow+Pass+AerialsWon+Rating

g_step=lm(Goals~.-Team-Tournament-Possession.-red_cards,Foot)
summary(g_step)

shapiro.test(g_step$residuals)
plot(g_step,which=1)
AIC(g_step) 
#l'AIC è diminuito-->daje

#ANOVA--->VOGLIAMO CAPIRE SE LE MEDIE DEI GOL SONO UGUALI NEI DIVERSI CAMPIONATI-->+DIFESA O ATTACCO


#boxplotclassi

my_colors = brewer.pal( 5, 'Set1')
boxplot(boxplot( Foot$Goals ~ Foot$Tournament , xlab = 'Tournament', ylab = 'Goals',col = my_colors ))
abline(h=mean(Foot$Goals),col='red')
#VERIFICO LE IPOTESI DI ANOVA
#normalità intragruppo

Ps=tapply(Foot$Goals,Foot$Tournament,function(x) (shapiro.test(x)$p))
Ps #conferma che la serie A è troppo sbilanciata

#omoschedasticità

Var=tapply(Foot$Goals,Foot$Tournament,var)
mean=tapply(Foot$Goals,Foot$Tournament,mean)

Var

leveneTest(Foot$Goals, Foot$Tournament)

bartlett.test(Foot$Goals, Foot$Tournament)

#tutto bene

#ANOVA



mod=lm(Foot$Goals~Foot$Tournament,Foot)
summary(mod)
anova(mod)
step(mod,direction='backward')


