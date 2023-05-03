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
library(tidyr)
library(xtable)

#Load Enemdu 

Enemdu <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Educ_HIs/Data/Enemdu2.feather")

###########################  Instrumental Variables with a logit ############

Enemdu = Enemdu[Enemdu$period == 2019 & Enemdu$employment == 1 & Enemdu$province != "ZONAS NO DELIMITADAS",]

#log transformation to avoid extremes

Enemdu$leduc <- ifelse(Enemdu$education_y < 1,0,log(Enemdu$education_y))

Enemdu$lfam_educ <- ifelse(Enemdu$educ_house < 1 ,0,log(Enemdu$educ_house))

Enemdu$lincome <- ifelse(Enemdu$h_tot_labour_income_pc < 1,0,log(Enemdu$h_tot_labour_income_pc))

Enemdu$lage <- ifelse(Enemdu$age < 1 ,0,log(Enemdu$age))

# Sierra

table(Enemdu$province)

Enemdu$sierra <- rep(0, length(Enemdu$employment))
Enemdu$sierra[Enemdu$province == "AZUAY" |
              Enemdu$province == "CHIMBORAZO"|
              Enemdu$province == "SANTO DOMINGO DE LOS TSACHILAS"|
              Enemdu$province == "BOLIVAR"|
              Enemdu$province == "COTOPAXI"|
              Enemdu$province == "CAÑAR"|
              Enemdu$province == "IMBABURA"|
              Enemdu$province == "PICHINCHA"|
              Enemdu$province == "TUNGURAHUA"|
              Enemdu$province == "CARCHI"|
              Enemdu$province == "LOJA"]=1

table(Enemdu$sierra)

Enemdu$costa <- rep(0, length(Enemdu$employment))
Enemdu$costa[Enemdu$province == "EL ORO" |
                Enemdu$province == "ESMERALDAS"|
                Enemdu$province == "GUAYAS"|
                Enemdu$province == "LOS RIOS"|
                Enemdu$province == "MANABI"|
                Enemdu$province == "SANTA ELENA"]=1

Enemdu$amazonia <- rep(0, length(Enemdu$employment))
Enemdu$amazonia[Enemdu$province == "MORONA SANTIAGO" |
                Enemdu$province == "NAPO"|
                Enemdu$province == "ORELLANA"|
                Enemdu$province == "PASTAZA"|
                Enemdu$province == "SUCUMBIOS"|
                Enemdu$province == "ZAMORA CHINCHIPE"]=1

Enemdu$galapagos <- rep(0, length(Enemdu$employment))
Enemdu$galapagos[Enemdu$province == "GALAPAGOS"]=1


# IV model

model1 <- lm(leduc ~ informal + lfam_educ + age  + sex + lincome + rural +
               nhousehold.y + h_kid + h_teen + h_adult+
               sierra + amazonia + galapagos, data=Enemdu)

stargazer(model1, type = "text")

model2  <- glm(informal ~ leduc + formal_per_house + migrant + rural +
                 nhousehold.y + h_kid + h_teen + h_adult +
                 lfam_educ + age  + sex + lincome +
                 sierra + amazonia + galapagos +
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
               sierra + amazonia + galapagos +
               resid2SLS , data=Enemdu)

Enemdu$residfin <- model3$residuals

# save the model 3 object to a file
#saveRDS(model3, "model_3.rds")


#model3<- readRDS("C:/Users/ignac/OneDrive/Documentos/PHD/Educ_HIs/Data/model_3.rds")

stargazer(coeftest(model3, vcov=vcovHC(model3,type="HC0")), type = "text")

#Perfect before the IV we see that informality has a positive coefficient after using the IV we see that it is negative 


#################################### Decomposition ############################


OB_reg <- oaxaca(leduc ~ informal + lfam_educ + lage  + sex + lincome + rural +
                   nhousehold.y + h_kid + h_teen + h_adult +
                   sierra + amazonia + galapagos +
                  + resid2SLS | indigenous, data = Enemdu)


OB_reg$threefold$variables

plot(OB_reg)

remove(model1, model3, OB_reg)

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

remove(HI, HI2, controls, HI_noindigena, HI_indigenas)

######################## Validity of the instruments ##########################

esti_2sls=ivreg(formula = leduc ~ informal + lfam_educ + age  + sex + lincome + rural +
                  nhousehold.y + h_kid + h_teen + h_adult | leduc + formal_per_house + migrant + rural +
                  nhousehold.y + h_kid + h_teen + h_adult +
                  lfam_educ + age  + sex + lincome +
                  agriculture + manufacturing + construction + retail + information + financial + realestate + scientific + public + other_services, data = Enemdu)

summary(esti_2sls)

summary(esti_2sls, diagnostics = TRUE)

#Hansen-Sargan test :

HS_test <- lm(residfin ~ leduc + formal_per_house + migrant + rural +
                nhousehold.y + h_kid + h_teen + h_adult +
                lfam_educ + age  + sex + lincome +
                agriculture + manufacturing + construction + retail + information + financial + realestate + scientific + public + other_services, data = Enemdu)

summary(HS_test)

######################## Sub groups regressions ###############################

#mujeres

mujeres <- Enemdu[Enemdu$sex == 1,]

OB_reg <- oaxaca(leduc ~ informal + lfam_educ + lage + lincome + rural +
                   nhousehold.y + h_kid + h_teen + h_adult +
                   sierra + amazonia + galapagos +
                   + resid2SLS | indigenous, data = mujeres)


OB_reg$threefold$variables

plot(OB_reg)

remove(mujeres)


#hombres

hombres <- Enemdu[Enemdu$sex == 0,]

OB_reg <- oaxaca(leduc ~ informal + lfam_educ + lage + lincome + rural +
                   nhousehold.y + h_kid + h_teen + h_adult +
                   sierra + amazonia + galapagos +
                   + resid2SLS | indigenous, data = hombres)


OB_reg$threefold$variables

plot(OB_reg)


remove(hombres)

# rural

rural <- Enemdu[Enemdu$rural == 1,]

OB_reg <- oaxaca(leduc ~ informal + lfam_educ + lage + sex + lincome +
                   nhousehold.y + h_kid + h_teen + h_adult +
                   sierra + amazonia + galapagos +
                   + resid2SLS | indigenous, data = rural)


OB_reg$threefold$variables

plot(OB_reg)


# rural

rural <- Enemdu[Enemdu$rural == 1,]

OB_reg <- oaxaca(leduc ~ informal + lfam_educ + lage + sex + lincome +
                   nhousehold.y + h_kid + h_teen + h_adult +
                   sierra + amazonia + galapagos +
                   + resid2SLS | indigenous, data = rural)


OB_reg$threefold$variables

plot(OB_reg)

remove(rural)

#urban 

urban <- Enemdu[Enemdu$rural == 0,]

OB_reg <- oaxaca(leduc ~ informal + lfam_educ + lage + sex + lincome +
                   nhousehold.y + h_kid + h_teen + h_adult +
                   sierra + amazonia + galapagos +
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
                     sierra + amazonia + galapagos +
                   + resid2SLS | indigenous, data = Q1)


OB_reg_Q1$threefold$variables

plot(OB_reg_Q1)

remove(Q1)

#Q2 

Q2 <- Enemdu[Enemdu$quintil == 2,]

OB_reg_Q2 <- oaxaca(leduc ~ informal + lfam_educ + lage + sex + lincome + rural +
                      nhousehold.y + h_kid + h_teen + h_adult +
                      sierra + amazonia + galapagos +
                      + resid2SLS | indigenous, data = Q2)


OB_reg_Q2$threefold$variables

plot(OB_reg_Q2)

remove(Q2)

#Q3

Q3 <- Enemdu[Enemdu$quintil == 3,]

OB_reg_Q3 <- oaxaca(leduc ~ informal + lfam_educ + lage + sex + lincome + rural +
                      nhousehold.y + h_kid + h_teen + h_adult +
                      sierra + amazonia + galapagos +
                      + resid2SLS | indigenous, data = Q3)


OB_reg_Q3$threefold$variables

plot(OB_reg_Q3)

remove(Q3)


#Q4

Q4 <- Enemdu[Enemdu$quintil == 4,]

OB_reg_Q4 <- oaxaca(leduc ~ informal + lfam_educ + lage + sex + lincome + rural +
                      nhousehold.y + h_kid + h_teen + h_adult +
                      sierra + amazonia + galapagos +
                      + resid2SLS | indigenous, data = Q4)


OB_reg_Q4$threefold$variables

plot(OB_reg_Q4)

remove(Q4)


#Q5 

Q5 <- Enemdu[Enemdu$quintil == 5,]

OB_reg_Q5 <- oaxaca(leduc ~ informal + lfam_educ + lage + sex + lincome + rural +
                      nhousehold.y + h_kid + h_teen + h_adult +
                      sierra + amazonia + galapagos +
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
               nhousehold.y + h_kid + h_teen + h_adult+
               sierra + amazonia + galapagos +, data=Enemdu)

stargazer(model1, type = "text")

model2  <- glm(informal_lt ~ leduc + formal_per_house + migrant + rural +
                 nhousehold.y + h_kid + h_teen + h_adult +
                 lfam_educ + age  + sex + lincome +
                 sierra + amazonia + galapagos +
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
               sierra + amazonia + galapagos +
               resid2SLS_lt , data=Enemdu)

# save the model 3 object to a file
#saveRDS(model3, "model_3.rds")


#model3<- readRDS("C:/Users/ignac/OneDrive/Documentos/PHD/Educ_HIs/Data/model_3.rds")

stargazer(coeftest(model3, vcov=vcovHC(model3,type="HC0")), type = "text")

#Perfect before the IV we see that informality has a positive coefficient after using the IV we see that it is negative 


### Decomposition

OB_reg <- oaxaca(leduc ~ informal_lt + lfam_educ + lage  + sex + lincome + rural +
                   nhousehold.y + h_kid + h_teen + h_adult +
                   sierra + amazonia + galapagos +
                   + resid2SLS_lt | indigenous, data = Enemdu)


OB_reg$threefold$variables

plot(OB_reg)

remove(model1,model3)

#Upper tier : self employed 

# IV model

table(Enemdu$informal_up)

model1 <- lm(leduc ~ informal_up + lfam_educ + age  + sex + lincome + rural +
               nhousehold.y + h_kid + h_teen + h_adult+
               sierra + amazonia + galapagos +, data=Enemdu)

stargazer(model1, type = "text")

model2  <- glm(informal_up ~ leduc + formal_per_house + migrant + rural +
                 nhousehold.y + h_kid + h_teen + h_adult +
                 lfam_educ + age  + sex + lincome +
                 sierra + amazonia + galapagos +
                 agriculture + manufacturing + construction + retail + information + financial + realestate + scientific + public + other_services 
               , data=Enemdu, family=binomial(link=logit))


stargazer(model2, type = "text")

Enemdu$IVinf_up <- model2$fitted.values

Enemdu$resid2SLS_up <- model2$residuals

remove(model2)

model3 <- lm(leduc ~ informal_up + lfam_educ + lage  + sex + lincome + rural +
               nhousehold.y + h_kid + h_teen + h_adult +
               sierra + amazonia + galapagos +
               resid2SLS_up , data=Enemdu)


stargazer(coeftest(model3, vcov=vcovHC(model3,type="HC0")), type = "text")


### Decomposition

OB_reg <- oaxaca(leduc ~ informal_up + lfam_educ + lage  + sex + lincome + rural +
                   nhousehold.y + h_kid + h_teen + h_adult +
                   sierra + amazonia + galapagos +
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
                   sierra + amazonia + galapagos +
                   + resid2SLS | indigenous, data = guayas)


OB_reg$threefold$variables

plot(OB_reg)

remove(guayas)


#Pichincha

pichincha <- Enemdu[Enemdu$province != "PICHINCHA",]

OB_reg <- oaxaca(leduc ~ informal + lfam_educ + lage + sex + lincome + rural +
                   nhousehold.y + h_kid + h_teen + h_adult +
                   sierra + amazonia + galapagos +
                   + resid2SLS | indigenous, data = pichincha)


OB_reg$threefold$variables

plot(OB_reg)

remove(pichincha)

#################### Descriptive Statistics #################################

Enemdu <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/Educ_HIs/Data/Enemdu2.feather")

Enemdu = Enemdu[Enemdu$province != "GALAPAGOS" & Enemdu$province != "ZONAS NO DELIMITADAS" & Enemdu$period == 2019 & Enemdu$employment == 1,]


Enemdu$tool1 <- rep(1,length(Enemdu$employment))
Enemdu$etnia <- rep("no indigena", length(Enemdu$employment))
Enemdu$etnia[Enemdu$indigenous==1]="indigena"

desc_stat <- Enemdu %>%
  group_by(etnia) %>%
  summarise_at(vars(education_y,tool1, informal),
               mean) %>%
  ungroup()

desc_stat$informal[desc_stat$etnia == "indigena"]


desc_stat2 <- Enemdu %>%
  group_by(period) %>%
  summarise_at(vars(education_y,tool1, informal),
               mean) %>%
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

remove(desc_stat, desc_stat2)

################### Descriptive Statistics Education ##########################

#sex

Enemdu$etnia<- rep("non-indigenous", length(Enemdu$employment))
Enemdu$etnia[Enemdu$indigenous == 1]="indigenous"

Enemdu$mujer<- rep("men", length(Enemdu$employment))
Enemdu$mujer[Enemdu$sex == 1]="women"

etnias_mujer <- Enemdu %>%
  group_by(etnia, mujer) %>%
  summarise_at(vars(education_y),
               mean) %>%
  ungroup()

#age

Enemdu$edad<- rep("<25 years", length(Enemdu$employment))
Enemdu$edad[Enemdu$age >24 & Enemdu$age < 51 ]="25 - 50 years"
Enemdu$edad[Enemdu$age >50  ]=">50 years"

etnias_edad <- Enemdu %>%
  group_by(etnia, edad) %>%
  summarise_at(vars(education_y),
               mean) %>%
  ungroup()

#household

table(Enemdu$h_adult)

Enemdu$vivienda<- rep("Other household", length(Enemdu$employment))
Enemdu$vivienda[Enemdu$h_adult == 1]="One adult"
Enemdu$vivienda[Enemdu$h_adult == 2]="Two adults"
Enemdu$vivienda[Enemdu$h_adult > 2 & Enemdu$h_adult < 5]="3 or 4 adults"
Enemdu$vivienda[Enemdu$h_adult > 4]="> 4 adults"

etnias_vivienda <- Enemdu %>%
  group_by(etnia, vivienda) %>%
  summarise_at(vars(educ_house),
               mean) %>%
  ungroup()

# Rural

Enemdu$area<- rep("urbano", length(Enemdu$employment))
Enemdu$area[Enemdu$rural == 1]="rural"

etnias_area <- Enemdu %>%
  group_by(etnia, area) %>%
  summarise_at(vars(education_y),
               mean) %>%
  ungroup()

# Formal en casa 

table(Enemdu$formal_per_house)

Enemdu$formal_in_house <- rep("Formal in the house", length(Enemdu$employment))
Enemdu$formal_in_house[Enemdu$formal_per_house == 0]= "Not formal in the house"

etnias_formal_house <- Enemdu %>%
  group_by(etnia, formal_in_house) %>%
  summarise_at(vars(education_y),
               mean) %>%
  ungroup()


# Migrant 


Enemdu$migrant1 <- rep("Migrant", length(Enemdu$employment))
Enemdu$migrant1[Enemdu$migrant == 0]= "No Migrant"

etnias_migrant <- Enemdu %>%
  group_by(etnia, migrant) %>%
  summarise_at(vars(education_y),
               mean) %>%
  ungroup()


# Universidad en casa 


h_univ <- Enemdu %>%
  group_by(id_hogar) %>%
  summarise_at(vars(university),
               sum) %>%
  ungroup()

names(h_univ) <- c("id_hogar", "h_univ")

Enemdu <- merge(Enemdu, h_univ, by = "id_hogar")

table(Enemdu$h_univ)

Enemdu$h_univ_c <- rep("0", length(Enemdu$h_univ))
Enemdu$h_univ_c[Enemdu$h_univ == 1]= "1"
Enemdu$h_univ_c[Enemdu$h_univ > 1]= " 2 or more"

table(Enemdu$h_univ_c)

h_univ2 <- Enemdu %>%
  group_by(etnia, h_univ_c) %>%
  summarise_at(vars(education_y),
               mean) %>%
  ungroup()



## Putting all in one table 

etnias_mujer <- spread(etnias_mujer, etnia, education_y)
names(etnias_mujer) <- c("variable", "indigenous", "non-indigenous")

etnias_area <- spread(etnias_area, etnia, education_y)
names(etnias_area) <- c("variable", "indigenous", "non-indigenous")


etnias_edad <- spread(etnias_edad, etnia, education_y)
names(etnias_edad) <- c("variable", "indigenous", "non-indigenous")


etnias_formal_house <- spread(etnias_formal_house, etnia, education_y)
names(etnias_formal_house) <- c("variable", "indigenous", "non-indigenous")

names(etnias_vivienda) <- c("etnia", "vivenda", "education_y")
etnias_vivienda <- spread(etnias_vivienda, etnia, education_y)
names(etnias_vivienda) <- c("variable", "indigenous", "non-indigenous")

h_univ2 <- spread(h_univ2, etnia, education_y)
names(h_univ2) <- c("variable", "indigenous", "non-indigenous")

etnias_migrant <- spread(etnias_migrant, etnia, education_y)
names(etnias_migrant) <- c("variable", "indigenous", "non-indigenous")

desc_stat <- rbind(etnias_area, etnias_edad, etnias_formal_house, etnias_migrant, etnias_mujer, etnias_vivienda, h_univ2)

my_table <- xtable(desc_stat)

# Add a caption to the table
attr(my_table, "caption") <- "Years of Education decomposition"

# Change the table label
attr(my_table, "label") <- "educ_stat"

# Change the font size
print(my_table, size = "footnotesize")

# Change the table alignment
print(my_table, align = c("l", "c", "r"))

remove(etnias_area, etnias_edad, etnias_formal_house, etnias_migrant, etnias_mujer, etnias_vivienda, h_univ2)
remove(desc_stat, h_univ, my_table)

################  Descriptive Statistics Informal Employment ##################



#sex

etnias_mujer <- Enemdu %>%
  group_by(etnia, mujer) %>%
  summarise_at(vars(informal),
               mean) %>%
  ungroup()

#age

etnias_edad <- Enemdu %>%
  group_by(etnia, edad) %>%
  summarise_at(vars(informal),
               mean) %>%
  ungroup()

#household

etnias_vivienda <- Enemdu %>%
  group_by(etnia, vivienda) %>%
  summarise_at(vars(informal),
               mean) %>%
  ungroup()

# Rural

etnias_area <- Enemdu %>%
  group_by(etnia, area) %>%
  summarise_at(vars(informal),
               mean) %>%
  ungroup()

# Formal en casa 

etnias_formal_house <- Enemdu %>%
  group_by(etnia, formal_in_house) %>%
  summarise_at(vars(formal_per_house),
               mean) %>%
  ungroup()


# Migrant 

etnias_migrant <- Enemdu %>%
  group_by(etnia, migrant) %>%
  summarise_at(vars(informal),
               mean) %>%
  ungroup()


# Universidad en casa 

table(Enemdu$h_univ_c)

h_univ2 <- Enemdu %>%
  group_by(etnia, h_univ_c) %>%
  summarise_at(vars(informal),
               mean) %>%
  ungroup()



## Putting all in one table 

etnias_mujer <- spread(etnias_mujer, etnia, informal)
names(etnias_mujer) <- c("variable", "indigenous", "non-indigenous")

etnias_area <- spread(etnias_area, etnia, informal)
names(etnias_area) <- c("variable", "indigenous", "non-indigenous")


etnias_edad <- spread(etnias_edad, etnia, informal)
names(etnias_edad) <- c("variable", "indigenous", "non-indigenous")

names(formal_in_house) <- c("etnia","formal_in_house","formal_per_house")
etnias_formal_house <- spread(etnias_formal_house, etnia, informal)
names(etnias_formal_house) <- c("variable", "indigenous", "non-indigenous")

names(etnias_vivienda) <- c("etnia", "vivienda", "informal")
etnias_vivienda <- spread(etnias_vivienda, etnia, informal)
names(etnias_vivienda) <- c("variable", "indigenous", "non-indigenous")

h_univ2 <- spread(h_univ2, etnia, informal)
names(h_univ2) <- c("variable", "indigenous", "non-indigenous")

etnias_migrant <- spread(etnias_migrant, etnia, informal)
names(etnias_migrant) <- c("variable", "indigenous", "non-indigenous")

desc_stat <- rbind(etnias_area, etnias_edad, etnias_formal_house, etnias_migrant, etnias_mujer, etnias_vivienda, h_univ2)

my_table <- xtable(desc_stat)

# Add a caption to the table
attr(my_table, "caption") <- "Years of Education decomposition"

# Change the table label
attr(my_table, "label") <- "inf_stat"

# Change the font size
print(my_table, size = "footnotesize")

# Change the table alignment
print(my_table, align = c("c", "c", "c"))

remove(etnias_area, etnias_edad, etnias_formal_house, etnias_migrant, etnias_mujer, etnias_vivienda, h_univ2)
remove(desc_stat, h_univ, my_table)

#######  Descriptive statistics per sector ######

table(Enemdu$rama2)

sector_educ <- Enemdu %>%
  group_by(etnia, rama2) %>%
  summarise_at(vars(education_y),
               mean) %>%
  ungroup()


sector_educ <- spread(sector_educ, etnia, education_y)
names(sector_educ) <- c("variable", "indigenous", "non-indigenous")

my_table <- xtable(sector_educ)

# Add a caption to the table
attr(my_table, "caption") <- "Years of Education by Economic Sector and Ethny for the Employed Population"

# Change the table label
attr(my_table, "label") <- "educ_sector_stat"

# Change the font size
print(my_table, size = "footnotesize")

# Change the table alignment
print(my_table, align = c("c", "c", "c"))

#informality

sector_inf <- Enemdu %>%
  group_by(etnia, rama2) %>%
  summarise_at(vars(informal),
               mean) %>%
  ungroup()


sector_inf <- spread(sector_inf, etnia, informal)
names(sector_educ) <- c("variable", "indigenous", "non-indigenous")

my_table <- xtable(sector_inf)

# Add a caption to the table
attr(my_table, "caption") <- "Frequencies of informal employment for the Employed Population"

# Change the table label
attr(my_table, "label") <- "inf_sector_stat"

# Change the font size
print(my_table, size = "footnotesize")

# Change the table alignment
print(my_table, align = c("c", "c", "c"))

remove(etnias_area, etnias_edad, etnias_formal_house, etnias_migrant, etnias_mujer, etnias_vivienda, h_univ2)
remove(desc_stat, h_univ, my_table)

#######  Descriptive statistics per region ######

Enemdu$region <- rep(0, length(Enemdu$province))
Enemdu$region[Enemdu$sierra == 1]="Sierra" 
Enemdu$region[Enemdu$costa == 1]="Costa" 
Enemdu$region[Enemdu$amazonia == 1]="Amazonía"
Enemdu$region[Enemdu$galapagos == 1]="Galapagos" 

table(Enemdu$region)


region_educ <- Enemdu %>%
  group_by(etnia, region) %>%
  summarise_at(vars(education_y),
               mean) %>%
  ungroup()


region_educ <- spread(region_educ, etnia, education_y)
names(region_educ) <- c("Region", "Indigenous", "Non-indigenous")

my_table <- xtable(region_educ)

# Add a caption to the table
attr(my_table, "caption") <- "Years of Education by Region and Ethny for the Employed Population in Ecuador"

# Change the table label
attr(my_table, "label") <- "educ_region_stat"

# Change the font size
print(my_table, size = "footnotesize")

# Change the table alignment
print(my_table, align = c("c", "c", "c"))

#informality

sector_inf <- Enemdu %>%
  group_by(etnia, rama2) %>%
  summarise_at(vars(informal),
               mean) %>%
  ungroup()


sector_inf <- spread(sector_inf, etnia, informal)
names(sector_educ) <- c("variable", "indigenous", "non-indigenous")

my_table <- xtable(sector_inf)

# Add a caption to the table
attr(my_table, "caption") <- "Frequencies of informal employment for the Employed Population"

# Change the table label
attr(my_table, "label") <- "inf_sector_stat"

# Change the font size
print(my_table, size = "footnotesize")

# Change the table alignment
print(my_table, align = c("c", "c", "c"))

remove(etnias_area, etnias_edad, etnias_formal_house, etnias_migrant, etnias_mujer, etnias_vivienda, h_univ2)
remove(desc_stat, h_univ, my_table)