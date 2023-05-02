# In this code we will do the Ob with IV to understand the effect of informal employment on educational HIs

library(lfe)
library(haven)
library(readr)
library(dplyr)
library(ineq)
library(stargazer)
library(oaxaca)
library(AER)
library(plm)
library(feather)
library(dineq)

#Load Enemdu 

Enemdu <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Educ_HIs/Data/Enemdu2.feather")

###########################  Instrumental Variables with a logit ############

Enemdu = Enemdu[Enemdu$province != "GALAPAGOS" & Enemdu$province != "ZONAS NO DELIMITADAS" & Enemdu$period == 2019 & Enemdu$employment == 1,]


#log transformation to avoid extremes

Enemdu$leduc <- ifelse(Enemdu$education_y < 1,0,log(Enemdu$education_y))

Enemdu$lfam_educ <- ifelse(Enemdu$educ_house < 1 ,0,log(Enemdu$educ_house))

Enemdu$lincome <- ifelse(Enemdu$h_tot_labour_income_pc < 1,0,log(Enemdu$h_tot_labour_income_pc))

Enemdu$lage <- ifelse(Enemdu$age < 1 ,0,log(Enemdu$age))


# IV model

model1 <- lm(leduc ~ informal + lfam_educ + age  + sex + lincome + rural +
               nhousehold.y + h_kid + h_teen + h_adult, data=Enemdu)

stargazer(model1, type = "text")

model2  <- glm(informal ~ leduc + formal_per_house + migrant + rural +
                 nhousehold.y + h_kid + h_teen + h_adult +
                 lfam_educ + age  + sex + lincome +
                 agriculture + manufacturing + construction + retail + information + financial + realestate + scientific + public + other_services 
               , data=Enemdu, family=binomial(link=logit))

#saveRDS(model2, "model_2.rds")

table(Enemdu$informal)

stargazer(model2, type = "text")

Enemdu$IVinf <- model2$fitted.values

Enemdu$resid2SLS <- model2$residuals

remove(model2)

model3 <- lm(leduc ~ informal + lfam_educ + lage  + sex + lincome + rural +
               nhousehold.y + h_kid + h_teen + h_adult +
               resid2SLS , data=Enemdu)

# save the model 3 object to a file
#saveRDS(model3, "model_3.rds")


#model3<- readRDS("C:/Users/ignac/OneDrive/Documentos/PHD/Educ_HIs/Data/model_3.rds")

stargazer(coeftest(model3, vcov=vcovHC(model3,type="HC0")), type = "text")

#Perfect before the IV we see that informality has a positive coefficient after using the IV we see that it is negative 


#################################### Decomposition ############################


OB_reg <- oaxaca(leduc ~ informal + lfam_educ + lage  + sex + lincome + rural +
                   nhousehold.y + h_kid + h_teen + h_adult +
                  + resid2SLS | indigenous, data = Enemdu)


OB_reg$threefold$variables

plot(OB_reg)


############################## Informal employment on Horizontal Inequalities FE ###


Enemdu <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Educ_HIs/Data/Enemdu2.feather")

Enemdu = Enemdu[Enemdu$province != "GALAPAGOS" & Enemdu$province != "ZONAS NO DELIMITADAS" & Enemdu$period < 2020,]

Enemdu$leduc <- ifelse(Enemdu$education_y < 1,0,log(Enemdu$education_y))

Enemdu$lfam_educ <- ifelse(Enemdu$educ_house < 1 ,0,log(Enemdu$educ_house))

Enemdu$lincome <- ifelse(Enemdu$h_tot_labour_income_pc < 1,0,log(Enemdu$h_tot_labour_income_pc))

Enemdu$lage <- ifelse(Enemdu$age < 1 ,0,log(Enemdu$age))

Enemdu$tool1 <- rep(1,length(Enemdu$employment))
Enemdu$etnia <- rep("no indigena", length(Enemdu$employment))
Enemdu$etnia[Enemdu$indigenous==1]="indigena"


table(Enemdu$etnia)

HI <- Enemdu %>%
  group_by(province, period,etnia) %>%
  summarise_at(vars(education_y,tool1, informal),
               sum) %>%
  ungroup()

HI$educ_mean_e <- HI$education_y / HI$tool1


HI2 <- Enemdu %>%
  group_by(province, period) %>%
  summarise_at(vars(education_y,tool1, informal),
               sum) %>%
  ungroup()

HI2$educ_mean <- HI2$education_y / HI2$tool1
HI2$informal_rate <- HI2$informal / HI2$tool1


HI$HI_educ <- rep(0, length(HI$province))

HI_indigenas <- HI[HI$etnia=="indigena",]
HI_noindigena <- HI[HI$etnia=="no indigena",]

table(HI_indigenas$province)
table(HI_noindigena$province)

table(HI_indigenas$province, HI_indigenas$period)

HI2$pi <- HI_indigenas$tool1 / HI2$tool1

HI2$pni <- HI_noindigena$tool1 / HI2$tool1

HI2$test <- HI2$pi +HI2$pni

summary(HI2$test)

HI2$HI <- (HI2$pi * HI2$pni * abs(HI_noindigena$educ_mean_e - HI_indigenas$educ_mean_e)) / HI2$educ_mean

HI2$GGini2 <- (1/(2*HI2$educ_mean))*(HI2$pi * HI2$pni * abs(HI_noindigena$educ_mean_e - HI_indigenas$educ_mean_e))

HI <- merge(HI,HI2[,c("province","period", "informal_rate", "HI","educ_mean","GGini2")], by = c("province", "period"))

controls <- Enemdu %>%
  group_by(province, period) %>%
  summarise_at(vars("lincome","lfam_educ"),
               sum) %>%
  ungroup()

HI <- merge(HI,controls, by = c("province", "period"))

mod_within <- felm(GGini2 ~ informal_rate  + educ_mean + lincome + lfam_educ | period + province | 0 | period + province, data = HI)

summary(mod_within)


######################## Sub groups regressions ###############################

#mujeres

mujeres <- Enemdu[Enemdu$sex == 1,]

OB_reg <- oaxaca(leduc ~ informal + lfam_educ + lage + lincome + rural +
                   nhousehold.y + h_kid + h_teen + h_adult +
                   + resid2SLS | indigenous, data = mujeres)


OB_reg$threefold$variables

plot(OB_reg)

remove(mujeres)


#hombres

hombres <- Enemdu[Enemdu$sex == 0,]

OB_reg <- oaxaca(leduc ~ informal + lfam_educ + lage + lincome + rural +
                   nhousehold.y + h_kid + h_teen + h_adult +
                   + resid2SLS | indigenous, data = hombres)


OB_reg$threefold$variables

plot(OB_reg)


remove(hombres)

# rural

rural <- Enemdu[Enemdu$rural == 1,]

OB_reg <- oaxaca(leduc ~ informal + lfam_educ + lage + sex + lincome +
                   nhousehold.y + h_kid + h_teen + h_adult +
                   + resid2SLS | indigenous, data = rural)


OB_reg$threefold$variables

plot(OB_reg)


# rural

rural <- Enemdu[Enemdu$rural == 1,]

OB_reg <- oaxaca(leduc ~ informal + lfam_educ + lage + sex + lincome +
                   nhousehold.y + h_kid + h_teen + h_adult +
                   + resid2SLS | indigenous, data = rural)


OB_reg$threefold$variables

plot(OB_reg)

remove(rural)

#urban 

urban <- Enemdu[Enemdu$rural == 0,]

OB_reg <- oaxaca(leduc ~ informal + lfam_educ + lage + sex + lincome +
                   nhousehold.y + h_kid + h_teen + h_adult +
                   + resid2SLS | indigenous, data = urban)


OB_reg$threefold$variables

plot(OB_reg)

remove(urban)
remove(OB_reg)
remove(model1,model3)


#Q1 

Q1 <- Enemdu[Enemdu$quintil == 1,]

OB_reg_Q1 <- oaxaca(leduc ~ informal + lfam_educ + lage + sex + lincome + rural +
                   nhousehold.y + h_kid + h_teen + h_adult +
                   + resid2SLS | indigenous, data = Q1)


OB_reg_Q1$threefold$variables

plot(OB_reg_Q1)

remove(Q1)

#Q2 

Q2 <- Enemdu[Enemdu$quintil == 2,]

OB_reg_Q2 <- oaxaca(leduc ~ informal + lfam_educ + lage + sex + lincome + rural +
                      nhousehold.y + h_kid + h_teen + h_adult +
                      + resid2SLS | indigenous, data = Q2)


OB_reg_Q2$threefold$variables

plot(OB_reg_Q2)

remove(Q2)

#Q3

Q3 <- Enemdu[Enemdu$quintil == 3,]

OB_reg_Q3 <- oaxaca(leduc ~ informal + lfam_educ + lage + sex + lincome + rural +
                      nhousehold.y + h_kid + h_teen + h_adult +
                      + resid2SLS | indigenous, data = Q3)


OB_reg_Q3$threefold$variables

plot(OB_reg_Q3)

remove(Q3)


#Q4

Q4 <- Enemdu[Enemdu$quintil == 4,]

OB_reg_Q4 <- oaxaca(leduc ~ informal + lfam_educ + lage + sex + lincome + rural +
                      nhousehold.y + h_kid + h_teen + h_adult +
                      + resid2SLS | indigenous, data = Q4)


OB_reg_Q4$threefold$variables

plot(OB_reg_Q4)

remove(Q4)


#Q5 

Q5 <- Enemdu[Enemdu$quintil == 5,]

OB_reg_Q5 <- oaxaca(leduc ~ informal + lfam_educ + lage + sex + lincome + rural +
                      nhousehold.y + h_kid + h_teen + h_adult +
                      + resid2SLS | indigenous, data = Q5)


OB_reg_Q5$threefold$variables

plot(OB_reg_Q5)

table(Q5$indigenous)

remove(Q5, OB_reg_Q1, OB_reg_Q2, OB_reg_Q3, OB_reg_Q4, OB_reg_Q5)


################### Robustness Checks ########################################

#Now we will do the same kind of work using the different kinds of informal employment

#Lower_tier of informal employment : informal employees

# IV model

table(Enemdu$informal_lt)

model1 <- lm(leduc ~ informal_lt + lfam_educ + age  + sex + lincome + rural +
               nhousehold.y + h_kid + h_teen + h_adult, data=Enemdu)

stargazer(model1, type = "text")

model2  <- glm(informal_lt ~ leduc + formal_per_house + migrant + rural +
                 nhousehold.y + h_kid + h_teen + h_adult +
                 lfam_educ + age  + sex + lincome +
                 agriculture + manufacturing + construction + retail + information + financial + realestate + scientific + public + other_services 
               , data=Enemdu, family=binomial(link=logit))

#saveRDS(model2, "model_2.rds")

table(Enemdu$informal)

stargazer(model2, type = "text")

Enemdu$IVinf_lt <- model2$fitted.values

Enemdu$resid2SLS_lt <- model2$residuals

remove(model2)

model3 <- lm(leduc ~ informal_lt + lfam_educ + lage  + sex + lincome + rural +
               nhousehold.y + h_kid + h_teen + h_adult +
               resid2SLS_lt , data=Enemdu)

# save the model 3 object to a file
#saveRDS(model3, "model_3.rds")


#model3<- readRDS("C:/Users/ignac/OneDrive/Documentos/PHD/Educ_HIs/Data/model_3.rds")

stargazer(coeftest(model3, vcov=vcovHC(model3,type="HC0")), type = "text")

#Perfect before the IV we see that informality has a positive coefficient after using the IV we see that it is negative 


### Decomposition

OB_reg <- oaxaca(leduc ~ informal_lt + lfam_educ + lage  + sex + lincome + rural +
                   nhousehold.y + h_kid + h_teen + h_adult +
                   + resid2SLS_lt | indigenous, data = Enemdu)


OB_reg$threefold$variables

plot(OB_reg)

remove(model1,model3)

#Upper tier : self employed 

# IV model

table(Enemdu$informal_up)

model1 <- lm(leduc ~ informal_up + lfam_educ + age  + sex + lincome + rural +
               nhousehold.y + h_kid + h_teen + h_adult, data=Enemdu)

stargazer(model1, type = "text")

model2  <- glm(informal_up ~ leduc + formal_per_house + migrant + rural +
                 nhousehold.y + h_kid + h_teen + h_adult +
                 lfam_educ + age  + sex + lincome +
                 agriculture + manufacturing + construction + retail + information + financial + realestate + scientific + public + other_services 
               , data=Enemdu, family=binomial(link=logit))


stargazer(model2, type = "text")

Enemdu$IVinf_up <- model2$fitted.values

Enemdu$resid2SLS_up <- model2$residuals

remove(model2)

model3 <- lm(leduc ~ informal_up + lfam_educ + lage  + sex + lincome + rural +
               nhousehold.y + h_kid + h_teen + h_adult +
               resid2SLS_up , data=Enemdu)


stargazer(coeftest(model3, vcov=vcovHC(model3,type="HC0")), type = "text")


### Decomposition

OB_reg <- oaxaca(leduc ~ informal_up + lfam_educ + lage  + sex + lincome + rural +
                   nhousehold.y + h_kid + h_teen + h_adult +
                   + resid2SLS_up | indigenous, data = Enemdu)


OB_reg$threefold$variables

plot(OB_reg)

remove(model1,model3)

## Jacknife method 

table(Enemdu$informal, Enemdu$province)

## First, we are going to left out Guayas because it has a lot of individuals and there are more informals than formals in our sample
## Than we do the same for Pichincha because there are a lot of individuals and there are more formals than informals 

#Guayas

guayas <- Enemdu[Enemdu$province != "GUAYAS",]

OB_reg <- oaxaca(leduc ~ informal + lfam_educ + lage + sex + lincome + rural +
                   nhousehold.y + h_kid + h_teen + h_adult +
                   + resid2SLS | indigenous, data = guayas)


OB_reg$threefold$variables

plot(OB_reg)

remove(guayas)


#Pichincha

pichincha <- Enemdu[Enemdu$province != "PICHINCHA",]

OB_reg <- oaxaca(leduc ~ informal + lfam_educ + lage + sex + lincome + rural +
                   nhousehold.y + h_kid + h_teen + h_adult +
                   + resid2SLS | indigenous, data = pichincha)


OB_reg$threefold$variables

plot(OB_reg)

remove(pichincha)
