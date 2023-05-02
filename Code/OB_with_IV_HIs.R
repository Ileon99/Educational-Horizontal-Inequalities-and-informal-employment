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


#reference for occupations are clerical support
#reference for secort is agriculture

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


######################## On individuals ######################################










