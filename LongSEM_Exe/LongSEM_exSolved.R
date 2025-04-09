# PARTE I ----
## 1. Iniciar o R e Importar base de dados no ficheiro "df_parinv_1.csv"
df_parinv_1 <- read.csv("G:\\My Drive\\FPCEUP\\AtividadeDocente\\Longitudinal_SEM\\LongSEM_Data\\df_parinv_1.csv")

## 2. Verificar as dimensões e nomes das variáveis na base de dados;  
dim(df_parinv_1)
names(df_parinv_1)

## 3. Descrever a estrutura da base dados usando a função str();  
str(df_parinv_1)

## 4. Fazer o sumário das variáveis na base de dados;  
summary(df_parinv_1)

## 5. Carregar os pacotes lavaan e semTools;   
library(lavaan)
library(semTools)

## 6. Testar e avaliar, usando o pacote lavaan, o modelo de medida de envolvimento usando para isso os dados reportados pelos pais nos três momentos de avaliação. Cada variável latente deve ser definida com 3 indicadores, p1, p2 e p3. Sumarie os resultados e analise, com base nos índices de modificação, a necessidade de introduzir alguma re-especificação ao modelo. Se necessário compare o modelo inicial com o modelo re-especificado;  
modelo1 <- "
# Definir variáveis latentes para os 3 momentos
father_t1 =~ Fath_t1.p1 + Fath_t1.p2 + Fath_t1.p3
father_t2 =~ Fath_t2.p1 + Fath_t2.p2 + Fath_t2.p3
father_t3 =~ Fath_t3.p1 + Fath_t3.p2 + Fath_t3.p3

# Correlações entre resíduos ao longo do tempo
Fath_t1.p1 ~~ Fath_t2.p1 + Fath_t3.p1
Fath_t2.p1 ~~ Fath_t3.p1
Fath_t1.p2 ~~ Fath_t2.p2 + Fath_t3.p2
Fath_t2.p2 ~~ Fath_t3.p2
Fath_t1.p3 ~~ Fath_t2.p3 + Fath_t3.p3
Fath_t2.p3 ~~ Fath_t3.p3
"

fit_model1 <- cfa(modelo1, data = df_parinv_1, meanstructure=T, std.lv=T)
summary(fit_model1, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
modificationindices(fit_model1, sort. = TRUE, minimum.value = 30, op="~~")


modelo2 <- "
# Definir variáveis latentes para os 3 momentos
father_t1 =~ Fath_t1.p1 + Fath_t1.p2 + Fath_t1.p3
father_t2 =~ Fath_t2.p1 + Fath_t2.p2 + Fath_t2.p3
father_t3 =~ Fath_t3.p1 + Fath_t3.p2 + Fath_t3.p3

# Correlações entre resíduos ao longo do tempo
Fath_t1.p1 ~~ Fath_t2.p1 + Fath_t3.p1
Fath_t2.p1 ~~ Fath_t3.p1
Fath_t1.p2 ~~ Fath_t2.p2 + Fath_t3.p2
Fath_t2.p2 ~~ Fath_t3.p2
Fath_t1.p3 ~~ Fath_t2.p3 + Fath_t3.p3
Fath_t2.p3 ~~ Fath_t3.p3

# correlação entre resíduos
Fath_t2.p2 ~~ Fath_t2.p3
Fath_t3.p1 ~~ Fath_t3.p2
"
fit_model2 <- cfa(modelo2, data = df_parinv_1, meanstructure=T, std.lv=T)
summary(fit_model2, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
compareFit(fit_model1,fit_model2)

# 7. Confirmar que o mesmo modelo de medida de envolvimento parental anteriormente testado se aplica igualmente aos scores de envolvimento reportados pelas mães.  
modelo3 <- "
# Definir variáveis latentes para os 3 momentos
mother_t1 =~ Moth_t1.p1 + Moth_t1.p2 + Moth_t1.p3
mother_t2 =~ Moth_t2.p1 + Moth_t2.p2 + Moth_t2.p3
mother_t3 =~ Moth_t3.p1 + Moth_t3.p2 + Moth_t3.p3

# Correlações entre resíduos ao longo do tempo
Moth_t1.p1 ~~ Moth_t2.p1 + Moth_t3.p1
Moth_t2.p1 ~~ Moth_t3.p1
Moth_t1.p2 ~~ Moth_t2.p2 + Moth_t3.p2
Moth_t2.p2 ~~ Moth_t3.p2
Moth_t1.p3 ~~ Moth_t2.p3 + Moth_t3.p3
Moth_t2.p3 ~~ Moth_t3.p3
"
fit_model3 <- cfa(modelo3, data = df_parinv_1, meanstructure=T, std.lv=T)
summary(fit_model3, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
modificationindices(fit_model3, sort. = TRUE, minimum.value = 20, op="~~")

modelo4 <- "
# Definir variáveis latentes para os 3 momentos
mother_t1 =~ Moth_t1.p1 + Moth_t1.p2 + Moth_t1.p3
mother_t2 =~ Moth_t2.p1 + Moth_t2.p2 + Moth_t2.p3
mother_t3 =~ Moth_t3.p1 + Moth_t3.p2 + Moth_t3.p3

# Correlações entre resíduos ao longo do tempo
Moth_t1.p1 ~~ Moth_t2.p1 + Moth_t3.p1
Moth_t2.p1 ~~ Moth_t3.p1
Moth_t1.p2 ~~ Moth_t2.p2 + Moth_t3.p2
Moth_t2.p2 ~~ Moth_t3.p2
Moth_t1.p3 ~~ Moth_t2.p3 + Moth_t3.p3
Moth_t2.p3 ~~ Moth_t3.p3

# correlação entre resíduos
Moth_t2.p1 ~~ Moth_t2.p3
Moth_t2.p1 ~~ Moth_t2.p2"
fit_model4 <- cfa(modelo4, data = df_parinv_1, meanstructure=T, std.lv=T)
summary(fit_model4, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
compareFit(fit_model3,fit_model4)

# PARTE II ----
  
# 8. Importar base de dados no ficheiro "df_parinv_2.csv";  
ParInv_2 <- read.csv("G:\\My Drive\\FPCEUP\\AtividadeDocente\\Longitudinal_SEM\\LongSEM_Data\\df_parinv_2.csv")
# pode usar a choose.files() para selecionar mais facilmente o ficheiro

# 9. Analisar a estatística descritiva das diferentes variáveis na base de dados usando a função describe() do pacote psych;  
library(psych)
psych::describe(ParInv_2)

# 10. Teste um modelo de crescimento latente para o envolvimento materno. A slope deve ser linear baseada em três momentos de avaliação ("T1_Minv", "T2_Minv", "T3_Minv");  
model5 <-
  'int_mother =~ 1*T1_Minv + 1*T2_Minv + 1*T3_Minv
sl_mother =~ 0*T1_Minv + 1*T2_Minv + 2*T3_Minv

## fixar os intercepts das variáveis observadas a zero e libertar as médias latentes (intecept e crescimento médio)
int_mother ~ 1
sl_mother ~ 1
T1_Minv ~ 0*1
T2_Minv ~ 0*1
T3_Minv ~ 0*1

' 
fit_model5 <- sem(model5, data=ParInv_2, meanstructure=TRUE)
summary(fit_model5, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)

# 11. Adicione ao modelo anterior a trajetória de crescimento do envolvimento paterno também nos três momentos ("T1_Finv", "T2_Finv", "T3_Finv"). Especifique uma trajetória linear de crescimento à semelhança do especificado para o envolvimento materno;  

model6 <-"
  int_mother =~ 1*T1_Minv + 1*T2_Minv + 1*T3_Minv
sl_mother =~ 0*T1_Minv + 1*T2_Minv + 2*T3_Minv

int_father =~ 1*T1_Finv + 1*T2_Finv + 1*T3_Finv
sl_father =~ 0*T1_Finv + 1*T2_Finv + 2*T3_Finv

## fixar os intercepts das variáveis observadas a zero e libertar as médias latentes (intecept e crescimento médio)
int_mother ~ 1
sl_mother ~ 1
int_father ~ 1
sl_father ~ 1

T1_Minv ~ 0*1
T2_Minv ~ 0*1
T3_Minv ~ 0*1
T1_Finv ~ 0*1
T2_Finv ~ 0*1
T3_Finv ~ 0*1

#fixar resíduos
T1_Minv ~~ res1*T1_Minv
T2_Minv ~~ res1*T2_Minv
T3_Minv ~~ res1*T3_Minv
T1_Finv ~~ res2*T1_Finv
T2_Finv ~~ res2*T2_Finv
T3_Finv ~~ res2*T3_Finv

#correlação entre resíduos
T1_Minv ~~ T1_Finv
T2_Minv ~~ T2_Finv
T3_Minv ~~ T3_Finv
" 
fit_model6 <- sem(model6, data=ParInv_2, meanstructure=TRUE)
summary(fit_model6, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)

# 12. Verifique os efeitos das variáveis idade ("childage"), sexo ("childsex") e habilidades de auto-controle ("T1_childSC") da sobre os momentos iniciais e crescimento médio do envolvimento materno e paterno;

model7 <-"
int_mother =~ 1*T1_Minv + 1*T2_Minv + 1*T3_Minv
sl_mother =~ 0*T1_Minv + 1*T2_Minv + 2*T3_Minv

int_father =~ 1*T1_Finv + 1*T2_Finv + 1*T3_Finv
sl_father =~ 0*T1_Finv + 1*T2_Finv + 2*T3_Finv

## fixar os intercepts das variáveis observadas a zero e libertar as médias latentes (intecept e crescimento médio)
int_mother ~ 1
sl_mother ~ 1
int_father ~ 1
sl_father ~ 1

T1_Minv ~ 0*1
T2_Minv ~ 0*1
T3_Minv ~ 0*1
T1_Finv ~ 0*1
T2_Finv ~ 0*1
T3_Finv ~ 0*1

#fixar resíduos
T1_Minv ~~ res1*T1_Minv
T2_Minv ~~ res1*T2_Minv
T3_Minv ~~ res1*T3_Minv
T1_Finv ~~ res2*T1_Finv
T2_Finv ~~ res2*T2_Finv
T3_Finv ~~ res2*T3_Finv

#correlação entre resíduos
T1_Minv ~~ T1_Finv
T2_Minv ~~ T2_Finv
T3_Minv ~~ T3_Finv

# Regressões
int_mother ~ childage + childsex + T1_childSC
sl_mother ~ childage + childsex + T1_childSC
int_father ~ childage + childsex + T1_childSC
sl_father ~ childage + childsex + T1_childSC
"
fit_model7 <- sem(model7, data=ParInv_2, meanstructure=TRUE)
summary(fit_model7, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)
