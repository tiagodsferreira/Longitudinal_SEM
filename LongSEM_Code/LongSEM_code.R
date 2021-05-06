# "Análise de dados longitudinais (Parte 3): Modelos longitudinais"
# author: "Tiago Ferreira"

## Importação de dados
choose.files()
## 
DF_mood <- read.csv("G:\\My Drive\\FPCEUP\\R trainning\\GitRepo\\Longitudinal SEM\\Longitudinal_SEM\\LongSEM_Data\\df_mood.csv")
names(DF_mood)
str(DF_mood)

## Estatística descritiva  
library(psych) # se necessário: install.packages("psych")
psych::describe(DF_mood[ ,1:6])

## Visualização de dados 
library(ggplot2) # se necessário: install.packages("ggplot2")
DF_pos <- data.frame(var=c(rep("PosAFF_1", 368), rep("PosAFF_2", 368), rep("PosAFF_3", 368)), 
                     value = c(DF_mood$PosAFF11, DF_mood$PosAFF21, DF_mood$PosAFF31))

ggplot(DF_pos, aes(x=value)) +
  geom_histogram(aes(fill=var),color="grey80", binwidth = 0.4) + facet_grid(var~.)

DF_neg <- data.frame(var=c(rep("NegAFF_1", 368), rep("NegAFF_2", 368), rep("NegAFF_3", 368)), 
                     value = c(DF_mood$NegAFF11, DF_mood$NegAFF21, DF_mood$NegAFF31))
ggplot(DF_neg, aes(x=value)) +
  geom_histogram(aes(fill=var),color="grey80", binwidth = 0.4) + facet_grid(var~.)

## Análise Fatorial Confirmatória
library(lavaan)# se necessário: install.packages("lavaan")
library(semPlot)# se necessário: install.packages("semPlot")

model.5 <- "
## Definir variáveis latentes
Pos1 =~ PosAFF11 + PosAFF21 + PosAFF31
Neg1 =~ NegAFF11 + NegAFF21 + NegAFF31
"
fit.5 <- cfa(model.5, data = DF_mood, meanstructure=T)
summary(fit.5, standardized=TRUE, fit.measures=TRUE, rsquare = TRUE)

fit.5.1 <- cfa(model.5, data = DF_mood, meanstructure=T, std.lv=TRUE)
summary(fit.5.1, standardized=TRUE, fit.measures=TRUE, rsquare = TRUE)

compareFit(fit.5, fit.5.1, nested = TRUE) 

modindices(fit.5.1, sort.=TRUE, minimum.value=6)

inspect(fit.5)

model.6 <- "
## Definir variáveis latentes
Pos1 =~ PosAFF11 + PosAFF21 + PosAFF31
Neg1 =~ NegAFF11 + NegAFF21 + NegAFF31

PosAFF11 ~~ PosAFF21
"
fit.6 <- cfa(model.6, data = DF_mood, meanstructure=T, std.lv=T)
summary(fit.6, standardized=TRUE, fit.measures=TRUE, rsquare = TRUE)
anova(fit.5, fit.6)
compareFit(fit.5, fit.6, nested = TRUE) 


# Análise fatorial confirmatória com dados longitudinais
## CFA com dados longitudinais
modelo.7 <- "
# Definir variáveis latentes para os 3 momentos
Pos_T1 =~ PosAFF11 + PosAFF21 + PosAFF31
Pos_T2 =~ PosAFF12 + PosAFF22 + PosAFF32
Pos_T3 =~ PosAFF13 + PosAFF23 + PosAFF33

Neg_T1 =~ NegAFF11 + NegAFF21 + NegAFF31
Neg_T2 =~ NegAFF12 + NegAFF22 + NegAFF32
Neg_T3 =~ NegAFF13 + NegAFF23 + NegAFF33
				
# Correlações entre resíduos ao longo do tempo
PosAFF11 ~~ PosAFF12 + PosAFF13 
PosAFF12 ~~ PosAFF13
PosAFF21 ~~ PosAFF22 + PosAFF23
PosAFF22 ~~ PosAFF23
PosAFF31 ~~ PosAFF32 + PosAFF33
PosAFF32 ~~ PosAFF33
				
NegAFF11 ~~ NegAFF12 + NegAFF13 
NegAFF12 ~~ NegAFF13
NegAFF21 ~~ NegAFF22 + NegAFF23
NegAFF22 ~~ NegAFF23
NegAFF31 ~~ NegAFF32 + NegAFF33
NegAFF32 ~~ NegAFF33
"
fit.7 <- cfa(modelo.7, data = DF_mood, meanstructure=T, std.lv=T)
summary(fit.7, standardized=TRUE, fit.measures=TRUE, rsquare = TRUE)
fitMeasures(fit.7)
reliability(fit.7)
semTools::clipboard(fit.7)

# CONTEÚDO OPCIONAL | Invariância de medida longitudinal
modelo.7 <- "
# Definir variáveis latentes para os 3 momentos
Pos_T1 =~ PosAFF11 + PosAFF21 + PosAFF31 
Pos_T2 =~ PosAFF12 + PosAFF22 + PosAFF32 
Pos_T3 =~ PosAFF13 + PosAFF23 + PosAFF33
Neg_T1 =~ NegAFF11 + NegAFF21 + NegAFF31 
Neg_T2 =~ NegAFF12 + NegAFF22 + NegAFF32 
Neg_T3 =~ NegAFF13 + NegAFF23 + NegAFF33

# Correlações entre resíduos ao longo do tempo
PosAFF11 ~~ PosAFF12 + PosAFF13; PosAFF12 ~~ PosAFF13; 
PosAFF21 ~~ PosAFF22 + PosAFF23; PosAFF22 ~~ PosAFF23; 
PosAFF31 ~~ PosAFF32 + PosAFF33; PosAFF32 ~~ PosAFF33
NegAFF11 ~~ NegAFF12 + NegAFF13; NegAFF12 ~~ NegAFF13; 
NegAFF21 ~~ NegAFF22 + NegAFF23; NegAFF22 ~~ NegAFF23; 
NegAFF31 ~~ NegAFF32 + NegAFF33; NegAFF32 ~~ NegAFF33
"
fit.7 <- cfa(modelo.7, data = DF_mood, meanstructure=T, std.lv=T)
summary(fit.7, standardized=TRUE, fit.measures=TRUE, rsquare = TRUE)

library(semTools)# se necessário: install.packages("semTools")
?measEq.syntax

longFacNames <- list(Pos = c("Pos_T1","Pos_T2", "Pos_T3"), Neg = c("Neg_T1","Neg_T2", "Neg_T3"))
## configural model: no constraints across groups or repeated measures
syntax.config <- measEq.syntax(configural.model = modelo.7, data = DF_mood,
                               ID.fac = "std.lv", longFacNames = longFacNames)
summary(syntax.config) ## print a summary of model features
syntax.config <- as.character(syntax.config) # save as text
configural.fit <- cfa(syntax.config, data = DF_mood, meanstructure=T)
summary(configural.fit, standardized=TRUE, fit.measures=TRUE, rsquare = TRUE)

compareFit(fit.7, configural.fit) # o mesmo modelo

### Invariância fraca ou métrica (iguais loadings)  
syntax.metric <- measEq.syntax(configural.model = syntax.config, data = DF_mood,
                               ID.fac = "std.lv", longFacNames = longFacNames,
                               long.equal  = c("loadings"))
summary(syntax.metric) # summarize model features
mod.metric <- as.character(syntax.metric) # save as text
metric.fit <- cfa(mod.metric, data = DF_mood, meanstructure=T)
summary(metric.fit, standardized=TRUE, fit.measures=TRUE, rsquare = TRUE)

compareFit(configural.fit, metric.fit)

### Invariância forte ou escalar (iguais loadings e intercepts)  
syntax.scalar <- measEq.syntax(configural.model = syntax.config, data = DF_mood,
                               ID.fac = "std.lv", longFacNames = longFacNames,
                               long.equal  = c("loadings", "intercepts"))
summary(syntax.scalar ) # summarize model features
mod.scalar <- as.character(syntax.scalar ) # save as text
scalar.fit <- cfa(mod.scalar, data = DF_mood, meanstructure=T)
summary(scalar.fit, standardized=TRUE, fit.measures=TRUE, rsquare = TRUE)

compareFit(metric.fit, scalar.fit)
### Invariância estrita (iguais loadings + intercepts + residuals)  
syntax.strict <- measEq.syntax(configural.model = syntax.config, data = DF_mood,
                               ID.fac = "std.lv", longFacNames = longFacNames,
                               long.equal  = c("loadings", "intercepts", "residuals"))
summary(syntax.strict ) # summarize model features
mod.strict <- as.character(syntax.strict ) # save as text
strict.fit <- cfa(mod.strict, data = DF_mood, meanstructure=T)
summary(strict.fit, standardized=TRUE, fit.measures=TRUE, rsquare = TRUE)

compareFit(scalar.fit, strict.fit)



# Modelos de crescimento latente (LGC)
DF_LGM <- read.csv("G:\\My Drive\\FPCEUP\\R trainning\\GitRepo\\Longitudinal SEM\\Longitudinal_SEM\\LongSEM_Data\\lgm.csv")
names(DF_LGM)
class(DF_LGM)
str(DF_LGM)

library(psych) # se necessário: install.packages("psych")
psych::describe(DF_LGM[ ,1:8])

library(ggplot2) # se necessário: install.packages("ggplot2")

DF_pos <- data.frame(var=c(rep("PosAFF_T1", nrow(DF_LGM)), rep("PosAFF_T2", nrow(DF_LGM)), 
                           rep("PosAFF_T3", nrow(DF_LGM)), rep("PosAFF_T4", nrow(DF_LGM))), 
                     value = c(DF_LGM$PosAFF_T1, DF_LGM$PosAFF_T2, DF_LGM$PosAFF_T3, DF_LGM$PosAFF_T4))
ggplot(DF_pos, aes(x=var,y=value, fill=var)) +
  geom_boxplot() +
  geom_jitter(width=0.08, alpha=0.03)

DF_neg <- data.frame(var=c(rep("NegAFF_T1", nrow(DF_LGM)), rep("NegAFF_T2", nrow(DF_LGM)), 
                           rep("NegAFF_T3", nrow(DF_LGM)), rep("NegAFF_T4", nrow(DF_LGM))), 
                     value = c(DF_LGM$NegAFF_T1, DF_LGM$NegAFF_T2, DF_LGM$NegAFF_T3, DF_LGM$NegAFF_T4))

ggplot(DF_neg, aes(x=var,y=value, fill=var)) +
  geom_boxplot() +
  geom_jitter(width=0.08, alpha=0.03)

modelo.8 <- "
## média inicial e trajetória linear de crescimento
intercept =~ 1*NegAFF_T1 + 1*NegAFF_T2 + 1*NegAFF_T3 + 1*NegAFF_T4
slope =~ 0*NegAFF_T1 + 1*NegAFF_T2 + 2*NegAFF_T3 + 3*NegAFF_T4
"
fit.8 <- growth(modelo.8, data=DF_LGM)
summary(fit.8, standardized=T, fit.measures=T)

modelo.8.1 <- "
intercept =~ 1*NegAFF_T1 + 1*NegAFF_T2 + 1*NegAFF_T3 + 1*NegAFF_T4
slope =~ 0*NegAFF_T1 + 1*NegAFF_T2 + 2*NegAFF_T3 + 3*NegAFF_T4

## fixar os intercepts das variáveis observadas a zero e 
## libertar as médias latentes (intecept e crescimento médio)
intercept ~ 1
slope ~ 1
NegAFF_T1 ~ 0*1
NegAFF_T2 ~ 0*1
NegAFF_T3 ~ 0*1
NegAFF_T4 ~ 0*1
"
fit.8.1 <- sem(modelo.8.1, data=DF_LGM)
summary(fit.8.1, standardized=T, fit.measures=T)
compareFit(fit.8, fit.8.1, nested=FALSE)


## Step 1: Modelo não condicionado  
modelo.9 <- "
## média inicial e trajetória linear de crescimento
intercept =~ 1*NegAFF_T1 + 1*NegAFF_T2 + 1*NegAFF_T3 + 1*NegAFF_T4
slope =~ 0*NegAFF_T1 + NegAFF_T2 + NegAFF_T3 + 1*NegAFF_T4

## intercepts e médias
intercept ~ 1
slope ~ 1
NegAFF_T1 ~ 0*1
NegAFF_T2 ~ 0*1
NegAFF_T3 ~ 0*1
NegAFF_T4 ~ 0*1
"
fit.9 <- sem(modelo.9, data=DF_LGM, meanstructure=TRUE)
summary(fit.9, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)

## STEP 2: Modelo condicionado  
modelo.10 <- "
## média inicial e trajetória linear de crescimento
my_intercept =~ 1*NegAFF_T1 + 1*NegAFF_T2 + 1*NegAFF_T3 + 1*NegAFF_T4
my_slope =~ 0*NegAFF_T1 + 1*NegAFF_T2 + 2*NegAFF_T3 + 3*NegAFF_T4

## intercepts e médias
my_intercept ~ 1
my_slope ~ 1
NegAFF_T1 ~ 0*1
NegAFF_T2 ~ 0*1
NegAFF_T3 ~ 0*1
NegAFF_T4 ~ 0*1
"
fit.10 <- sem(modelo.10, data=DF_LGM, meanstructure=TRUE)
summary(fit.10, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)
anova(fit.9, fit.10)

## LGC: Constrangimentos ao modelo  
modelo.10.1 <- "
## média inicial e trajetória linear de crescimento
intercept =~ 1*NegAFF_T1 + 1*NegAFF_T2 + 1*NegAFF_T3 + 1*NegAFF_T4
slope =~ 0*NegAFF_T1 + 1*NegAFF_T2 + 2*NegAFF_T3 + 3*NegAFF_T4

## intercepts e médias
intercept ~ 1
slope ~ 1
NegAFF_T1 ~ 0*1
NegAFF_T2 ~ 0*1
NegAFF_T3 ~ 0*1
NegAFF_T4 ~ 0*1

## Fixar as variâncias residuais ao mesmo valor
NegAFF_T1 ~~ res1*NegAFF_T1
NegAFF_T2 ~~ res1*NegAFF_T2
NegAFF_T3 ~~ res1*NegAFF_T3
NegAFF_T4 ~~ res1*NegAFF_T4
"
fit.10.1 <- sem(modelo.10.1, data=DF_LGM, meanstructure=TRUE)
summary(fit.10.1, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)
compareFit(fit.10, fit.10.1)

## LGC: Visualização (OPCIONAL)  
DF_FIT.10.1 <- as.data.frame(cbind(1:nrow(DF_LGM), predict(fit.10.1)))
names(DF_FIT.10.1)[1] <- "ID"
DF_FIT.10.1$ID <- rep("subjets", nrow(DF_FIT.10.1))
DF_FIT.10.1.sel <- DF_FIT.10.1[sample(1:nrow(DF_FIT.10.1),100), ]

DF_coef_fit.10.1 <- as.data.frame(matrix(coef(fit.10.1)[1:2], ncol=2, byrow=TRUE))
DF_coef_fit.10.1 <- as.data.frame(c("Average Est", DF_coef_fit.10.1), col.names=names(DF_FIT.10.1))

ggplot() + 
  scale_y_continuous(limits=c(0,4)) + 
  geom_abline(data= DF_FIT.10.1.sel, aes(slope=slope, intercept=intercept), colour="grey") +
  geom_abline(data= DF_coef_fit.10.1, aes(slope=slope, intercept=intercept, colour="estimate"), size=1) +
  theme(legend.title = element_blank()) +
  scale_x_continuous("TIME", limits=c(0,10)) 


## LGC: Modelo de crescimento com covariáveis fixas no tempo  
modelo.11 <- "
## média inicial e trajetória linear de crescimento
intercept =~ 1*NegAFF_T1 + 1*NegAFF_T2 + 1*NegAFF_T3 + 1*NegAFF_T4
slope =~ 0*NegAFF_T1 + 1*NegAFF_T2 + 2*NegAFF_T3 + 3*NegAFF_T4

## intercepts e médias
intercept ~ 1; 
slope ~ 1
NegAFF_T1 ~ 0*1; 
NegAFF_T2 ~ 0*1; 
NegAFF_T3 ~ 0*1; 
NegAFF_T4 ~ 0*1

## Fixar as variâncias residuais ao mesmo valor
NegAFF_T1 ~~ res1*NegAFF_T1; 
NegAFF_T2 ~~ res1*NegAFF_T2; 
NegAFF_T3 ~~ res1*NegAFF_T3; 
NegAFF_T4 ~~ res1*NegAFF_T4

# regressions
intercept ~ gender_male + Black + Hispanic
slope ~ gender_male + Black + Hispanic
"
fit.11 <- sem(modelo.11, data=DF_LGM, meanstructure=TRUE)
summary(fit.11, fit.measures=TRUE, rsquare=TRUE)


## LGC: Modelo de crescimento linear multivariado   
modelo.12 <- "
## média inicial e trajetória linear de crescimento
Neg_intercept =~ 1*NegAFF_T1 + 1*NegAFF_T2 + 1*NegAFF_T3 + 1*NegAFF_T4
Neg_slope =~ 0*NegAFF_T1 + 1*NegAFF_T2 + 2*NegAFF_T3 + 3*NegAFF_T4

Pos_intercept =~ 1*PosAFF_T1 + 1*PosAFF_T2 + 1*PosAFF_T3 + 1*PosAFF_T4
Pos_slope =~ 0*PosAFF_T1 + 1*PosAFF_T2 + 2*PosAFF_T3 + 3*PosAFF_T4

## médias
Neg_intercept ~ 1; Neg_slope ~ 1
Pos_intercept ~ 1; Pos_slope ~ 1

# intercepts 
NegAFF_T1 ~ 0*1; NegAFF_T2 ~ 0*1; NegAFF_T3 ~ 0*1; NegAFF_T4 ~ 0*1; 
PosAFF_T1 ~ 0*1; PosAFF_T2 ~ 0*1; PosAFF_T3 ~ 0*1; PosAFF_T4 ~ 0*1

## Fixar as variâncias residuais ao mesmo valor
NegAFF_T1 ~~ res1*NegAFF_T1; NegAFF_T2 ~~ res1*NegAFF_T2
NegAFF_T3 ~~ res1*NegAFF_T3; NegAFF_T4 ~~ res1*NegAFF_T4
PosAFF_T1 ~~ res2*PosAFF_T1; PosAFF_T2 ~~ res2*PosAFF_T2
PosAFF_T3 ~~ res2*PosAFF_T3; PosAFF_T4 ~~ res2*PosAFF_T4
"
fit.12 <- sem(modelo.12, data=DF_LGM, meanstructure=TRUE)
summary(fit.12, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)
