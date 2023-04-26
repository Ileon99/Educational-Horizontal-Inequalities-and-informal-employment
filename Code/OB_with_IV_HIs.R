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

#Load Enemdu 

Enemdu <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Educ_HIs/Data/Enemdu.feather")

###########################  Instrumental Variables with a logit ############

#log transformation to avoid extremes

Enemdu$leduc <- ifelse(Enemdu$education_y < 1,0,log(Enemdu$education_y))

Enemdu$lfam_educ <- ifelse(Enemdu$educ_house < 1 ,0,log(Enemdu$educ_house))

Enemdu$lincome <- ifelse(Enemdu$h_tot_labour_income_pc < 1,0,log(Enemdu$h_tot_labour_income_pc))

Enemdu$lage <- ifelse(Enemdu$age < 1 ,0,log(Enemdu$age))


# IV model

model1 <- lm(leduc ~ informal + lfam_educ + lage  + sex + lincome + rural +
               nhousehold.y + h_kid + h_teen + h_adult +
               agriculture + manufacturing + construction + retail + information + financial + realestate + scientific + public + other_services +
               managers + professionals + technicians + clerical_support + services_and_sales + skilled_agricultural + craft_related_trades + plant_machine_operators + elementary_occupations + armed_forces 
             + as.factor(province) + as.factor(period), data=Enemdu)

remove(model1)

stargazer(model1, type = "text")

model2  <- glm(informal ~ leduc + formal_per_house + migrant + rural +
                 nhousehold.y + h_kid + h_teen + h_adult +
                 lfam_educ + age  + sex + lincome +
                 agriculture + manufacturing + construction + retail + information + financial + realestate + scientific + public + other_services +
                 managers + professionals + technicians + clerical_support + services_and_sales + skilled_agricultural + craft_related_trades + plant_machine_operators + elementary_occupations + armed_forces 
               + as.factor(province) + as.factor(period)
               , data=Enemdu, family=binomial(link=logit))

#saveRDS(model2, "model_2.rds")

remove(model2)

model2<- readRDS("C:/Users/ignac/OneDrive/Documentos/PHD/Educ_HIs/Data/model_2.rds")

stargazer(model2, type = "text")

Enemdu$IVinf <- model2$fitted.values

Enemdu$resid2SLS <- model2$residuals

remove(model2)

model3 <- lm(leduc ~ informal + lfam_educ + lage  + sex + lincome + rural +
               nhousehold.y + h_kid + h_teen + h_adult +
               agriculture + manufacturing + construction + retail + information + financial + realestate + scientific + public + other_services +
               managers + professionals + technicians + clerical_support + services_and_sales + skilled_agricultural + craft_related_trades + plant_machine_operators + elementary_occupations + armed_forces 
             + as.factor(province) + as.factor(period) + resid , data=Enemdu)

# save the model 3 object to a file
#saveRDS(model3, "model_3.rds")

remove(model3)

#model3<- readRDS("C:/Users/ignac/OneDrive/Documentos/PHD/Educ_HIs/Data/model_3.rds")

stargazer(coeftest(model3, vcov=vcovHC(model3,type="HC0")), type = "text")

#Perfect before the IV we see that informality has a positive coefficient after using the IV we see that it is negative 


#################################### Decomposition ############################


Enemdu2019 <- Enemdu[Enemdu$period == 2019,]

#reference for occupations are clerical support
#reference for secort is agriculture

OB_reg <- oaxaca(leduc ~ informal + lfam_educ + lage  + sex + lincome + rural +
                   nhousehold.y + h_kid + h_teen + h_adult +
                  manufacturing + construction + retail + information + financial + realestate + scientific + public + other_services +
                   managers + professionals + technicians + services_and_sales + skilled_agricultural + craft_related_trades + plant_machine_operators + elementary_occupations + armed_forces + resid2SLS | indigenous, data = Enemdu2019)


OB_reg$threefold$variables


