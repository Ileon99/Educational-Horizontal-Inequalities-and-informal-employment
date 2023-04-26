# In this code we will do the Ob with IV to understand the effect of informal employment on educational HIs

library(haven)
library(readr)
library(dplyr)
library(ineq)
library(stargazer)
library(oaxaca)
library(AER)
library(plm)
library(feather)

###########################  Instrumental Variables with a logit ############

#log transformation to avoid extremes

summary(Enemdu$education_y)

Enemdu$leduc <- ifelse(Enemdu$education_y < 1,0,log(Enemdu$education_y))

Enemdu$lfam_educ <- ifelse(Enemdu$educ_house < 1 ,0,log(Enemdu$educ_house))

Enemdu$lincome <- ifelse(Enemdu$h_tot_labour_income_pc < 1,0,log(Enemdu$h_tot_labour_income_pc))

Enemdu$lage <- ifelse(Enemdu$age < 1 ,0,log(Enemdu$age))

Enemdu$lformal_house <- ifelse(Enemdu$formal_per_house < 1 ,0,log(Enemdu$formal_per_house))

Enemdu$lnhousehold <- ifelse(Enemdu$nhousehold.y < 1 ,0,log(Enemdu$nhousehold.y))

Enemdu$lkid_house <- ifelse(Enemdu$h_kid < 1 ,0,log(Enemdu$h_kid))

# IV model

model1 <- lm(leduc ~ informal + lfam_educ + lage  + sex + lincome + rural +
               agriculture + manufacturing + construction + retail + information + financial + realestate + scientific + public + other_services +
               managers + professionals + technicians + clerical_support + services_and_sales + skilled_agricultural + craft_related_trades + plant_machine_operators + elementary_occupations + armed_forces 
             + as.factor(province), data=Enemdu)

stargazer(model1, type = "text")

model2  <- glm(informal ~ leduc + lformal_house + migrant + rural + lnhousehold + lkid_house +
                 lfam_educ + lage  + sex + lincome +
                 agriculture + manufacturing + construction + retail + information + financial + realestate + scientific + public + other_services +
                 managers + professionals + technicians + clerical_support + services_and_sales + skilled_agricultural + craft_related_trades + plant_machine_operators + elementary_occupations + armed_forces 
               , data=Enemdu, family=binomial(link=logit))

# Estimate fixed-effects logistic regression model with household fixed effects
model2 <- plm(informal ~ lformal_house + migrant + rural + lnhousehold + lkid_house +
                lfam_educ + lage + sex + lincome +
                agriculture + manufacturing + construction + retail + information + financial + realestate + scientific + public + other_services +
                managers + professionals + technicians + clerical_support + services_and_sales + skilled_agricultural + craft_related_trades + plant_machine_operators + elementary_occupations + armed_forces +
                as.factor(province),
              data = Enemdu, 
              index = "panel_id", 
              model = "within",
              effect = "twoways",
              effect.labels = c("Household", "Time"),
              family = binomial(link = logit))

stargazer(model2, type = "text")

Enemdu$IVinf <- model2$fitted.values

model3 <- lm(leduc ~ IVinf + lfam_educ + lage  + sex + lincome  + as.factor(period), data=Enemdu)

stargazer(coeftest(model3, vcov=vcovHC(model3,type="HC0")), type = "text")

#Perfect before the IV we see that informality has a positive coefficient after using the IV we see that it is negative 

Enemdu$resid2 <- model3$residuals

test_valid <- lm(resid2 ~ lformal_house + migrant + rural + lnhousehold + lkid_house + lfam_educ + lage  + sex + lincome + as.factor(period), data = Enemdu)

summary(test_valid)

#################################### Decomposition ############################

OB_reg <- oaxaca(leduc ~ IVinf + lfam_educ + lage  + sex + lincome + as.factor(period) + as.factor(province)| indigenous, data = Enemdu)

