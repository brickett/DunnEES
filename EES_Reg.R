#Import packages
library(readxl)
library(broom)

#set directory to GitHub repo
setwd("C:/Users/bjr21/Documents/GitHub/DunnEES")

#Import file
df <- read_excel("2017EESStatsClean.xlsx", sheet=2)

#Create factor variables from string variables
df$Race.f <- factor(df$Race, levels = c("White","American Indian or Alaska Native","Asian Indian","Black/African American","Chinese","Filipino","Guamanian or Chamorro","Hispanic, Latino, or of Spanish Origin","Japanese","Korean","Native Hawaiian","Other (please specify)","Other Pacific Islander","Samoan","Vietnamese",NA))
is.factor(df$Race.f)

df$Sex.f <- factor(df$Sex, levels = c("Male","Female"))
is.factor(df$Sex.f)

df$Tenure.f <- factor(df$Tenure, levels=c("0-2 years","2-5 years","5-15 years","15+ years",NA))
is.factor(df$Tenure.f)

df$Union.f <- factor(df$Union, levels = c("Union member","Fair share employee","Non-union supervisor","None of the above",NA))
is.factor(df$Union.f)

df$Agency.f <- factor(df$Agency)
is.factor(df$Agency.f)

#Run Regressions and save outputs into CSV
Retention <- summary(lm(RetentionComp ~ Tenure.f + Union.f + Race.f + Sex.f + Agency.f, data = df))
tidy_Retention <- tidy(Retention)
write.csv(tidy_Retention, "tidy_Retention.csv")

Talent <- summary(lm(TalentComp ~ Tenure.f + Union.f + Race.f + Sex.f + Agency.f, data = df))
tidy_Talent <- tidy(Talent)
write.csv(tidy_Talent, "tidy_Talent.csv")

Enviro <- summary(lm(EnviroComp ~ Tenure.f + Union.f + Race.f + Sex.f + Agency.f, data = df))
tidy_Enviro <- tidy(Enviro)
write.csv(tidy_Enviro, "tidy_Enviro.csv")

Eval <- summary(lm(EvalComp ~ Tenure.f + Union.f + Race.f + Sex.f + Agency.f, data = df))
tidy_Eval <- tidy(Eval)
write.csv(tidy_Eval, "tidy_Eval.csv")

Customer <- summary(lm(CustomerComp ~ Tenure.f + Union.f + Race.f + Sex.f + Agency.f, data = df))
tidy_Customer <- tidy(Customer)
write.csv(tidy_Customer, "tidy_Customer.csv")

Unit <- summary(lm(UnitComp ~ Tenure.f + Union.f + Race.f + Sex.f + Agency.f, data = df))
tidy_Unit <- tidy(Unit)
write.csv(tidy_Unit, "tidy_Unit.csv")

Super <- summary(lm(SuperComp ~ Tenure.f + Union.f + Race.f + Sex.f + Agency.f, data = df))
tidy_Super <- tidy(Super)
write.csv(tidy_Super, "tidy_Super.csv")

Leader <- summary(lm(LeaderComp ~ Tenure.f + Union.f + Race.f + Sex.f + Agency.f, data = df))
tidy_Leader <- tidy(Leader)
write.csv(tidy_Leader, "tidy_Leader.csv")

State <- summary(lm(StateComp ~ Tenure.f + Union.f + Race.f + Sex.f + Agency.f, data = df))
tidy_State <- tidy(State)
write.csv(tidy_State, "tidy_State.csv")

#Check residuals
plot(Retention$residuals)
plot(Talent$residuals)
plot(Enviro$residuals)
plot(Eval$residuals)
plot(Customer$residuals)
plot(Unit$residuals)
plot(Super$residuals)
plot(Leader$residuals)
plot(State$residuals)

#print the r-squared values
print(Retention$r.squared)
print(Talent$r.squared)
print(Enviro$r.squared)
print(Eval$r.squared)
print(Customer$r.squared)
print(Unit$r.squared)
print(Super$r.squared)
print(Leader$r.squared)
print(State$r.squared)

