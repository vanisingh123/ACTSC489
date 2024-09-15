library(tidyverse)
library(statmod)
library(effects)
require(statmod)
require(insuranceData)
require(data.table)

#loading the dataset:
collDatasetTrain <- read.csv("collDatasetTrain.csv")
coll_dataset<-collDatasetTrain
coll_dataset_Test<-read.csv("collDatasetTest.csv")

#creating the partition dataset for claims:
set.seed(12345)
rand_sample2 = runif(nrow(coll_dataset), 0, 1)
coll_dataset$partition = ifelse(rand_sample2 < 0.7, "Training", "Holdout")

#Making AY_factor
coll_dataset = coll_dataset %>%
  mutate(AY_factor = factor(Accident_year, levels=c("2019", "2020", "2021")))

coll_dataset_Test = coll_dataset_Test %>%
  mutate(Accident_year=2021)

coll_dataset_Test = coll_dataset_Test %>%
  mutate(AY_factor = factor(Accident_year, levels=c("2019", "2020", "2021")))

coll_dataset$severity = coll_dataset$Collision_incurred_amount/
  coll_dataset$Collision_claim_count

#removing non-claim_records
coll_dataset_claims = filter(coll_dataset, partition=="Training")
coll_dataset_claims = coll_dataset_claims[coll_dataset_claims$Collision_incurred_amount != 0,]
coll_dataset_claims$partition="Training" 

#creating the severity variate
coll_dataset_claims$severity = coll_dataset_claims$Collision_incurred_amount/
  coll_dataset_claims$Collision_claim_count

#severity capping:
summary(coll_dataset_claims$severity)
quantile(coll_dataset_claims$severity, 
         c(0.8,0.85,0.90,0.95,0.96,0.97,0.98,0.99,0.995,1)) 

sev = coll_dataset_claims$severity
qs = c(0.9, 0.95, 0.97, 0.98, 0.985, 0.99, 0.995, 0.998, 0.999, 1)
qt = quantile(sev, qs)
num_claims_above = sapply(qt,function(q){sum(sev>q)})

plotdat = data.frame(qs, qt, num_claims_above)
data.frame(quantile = round(qt), num_claims_above)

#visualize the above
ggplot(plotdat, aes(x = qs, y = qt)) +
  geom_line() +
  geom_text(aes(label=qs), hjust=0, vjust=0) +
  ggtitle("Loss Size at Various Quantiles") +
  xlab("Quantile") +
  ylab("Severity")

CV_by_quantile = sapply(qt,function(q){
  sd(pmin(sev, q))/mean(pmin(sev, q))})

plotdat2 = data.frame(qs, qt, CV_by_quantile)
data.frame(quantile = round(qt), round(CV_by_quantile,3))

#visualize the above
ggplot(plotdat2, aes(x = qs, y = CV_by_quantile)) +
  geom_line() +
  geom_text(aes(label=qs), hjust=0, vjust=0) +
  ggtitle("CV at Various Quantiles") +
  xlab("Quantile") +
  ylab("CV")

qs = c(0.9, 0.95, 0.97, 0.98, 0.985, 0.99, 0.995, 0.998, 0.999)
qt = quantile(sev, qs)

LER_by_quantile = sapply(qt,function(q){
  1 - sum(pmin(sev, q))/sum(sev)})

plotdat4 = data.frame(qs, qt, LER_by_quantile)
data.frame(quantile = round(qt), round(LER_by_quantile,3))

#visualize the above
ggplot(plotdat4, aes(x = qs, y = LER_by_quantile)) +
  geom_line() +
  geom_text(aes(label=qs), hjust=0, vjust=0) +
  ggtitle("LER at Various Quantiles") +
  xlab("Quantile") +
  ylab("LER")

qs <- c(0.9, 0.95, 0.97, 0.98, 0.985, 0.99, 0.995, 0.998, 0.999, 1)
qt <- quantile(coll_dataset_claims$severity, qs)

# MEL Function
MEL_by_quantile <- sapply(qt,function(q){
  mean(coll_dataset_claims[which(coll_dataset_claims$severity > q), ]$severity) - q})

plotdatMEL <- data.frame(qs, qt, MEL_by_quantile)
data.frame(quantile = round(qt), round(MEL_by_quantile,3))

# Plotting
ggplot(plotdatMEL, aes(x = qs, y = MEL_by_quantile)) +
  geom_line() +
  geom_text(aes(label=qs), hjust=0, vjust=0) +
  ggtitle("MEL at Various Quantiles") +
  xlab("Quantile") +
  ylab("Mean Excess Loss")

#cap at 58106 for now. 
coll_dataset_claims = coll_dataset_claims %>%
  mutate(severity = ifelse(severity<59319, severity, 59319))

#Deductabes
#gross claim amount
coll_dataset_claims$gross_claim_amount=
  coll_dataset_claims$Collision_incurred_amount+coll_dataset_claims$Collision_deductible

total_loss= sum(coll_dataset_claims$gross_claim_amount)
total_claims=sum(coll_dataset_claims$Collision_claim_count)
total_exposure = sum(coll)
avg_sev<-total_loss/total_claims

avg_sev

#for 500:
coll_dataset_claims$deductible_500=
  coll_dataset_claims$gross_claim_amount-500
total_500<-sum(coll_dataset_claims$deductible_500)
avg_sev_500<-total_500/total_claims

#for 1000
coll_dataset_claims$deductible_1000=
  coll_dataset_claims$gross_claim_amount-1000
total_1000<-sum(coll_dataset_claims$deductible_1000)
avg_sev_1000<-total_1000/total_claims

ratio_500<-avg_sev_500/avg_sev
ratio_1000<-avg_sev_1000/avg_sev

#Adding to dataset:
coll_dataset_claims=coll_dataset_claims %>% 
  mutate(deductible=ifelse(Collision_deductible==500, log(ratio_500), log(ratio_1000)))

#creating the base severity model 
base_glm1<-glm(severity ~ AY_factor, 
              family=Gamma(link="log"), 
              data = coll_dataset_claims, 
              weights=Collision_claim_count,
              subset=(partition=="Training"), 
              na.action="na.pass",
              x = TRUE)
summary(base_glm1)
coll_dataset$sevPrediction <- predict(base_glm1,
                                      newdata = coll_dataset,
                                      type = "response")

#CV for base_glm1
train<-filter(coll_dataset_claims, partition=="Training")
setDT(train)
# Cross-validated random folds (good to set a seed prior to creating folds for reproducibility)
set.seed(123)
foldID = sample(1:5, size = nrow(train), replace = T)

# Create storage for the Gini coefficients, CV models and CV predictions
res = rep(0, 5)
models = vector(mode = "list", 5)
predictions = vector(mode = "list", 5)

# Loop over folds, fit a model, predict and compute Gini 
for (i in 1:5){
  models[[i]] = glm(severity ~ AY_factor,
                    family=Gamma(link="log"), 
                    data = train[foldID != i,], 
                    weights=Collision_claim_count,
                    na.action="na.pass",
                    x = TRUE)
  predictions[[i]] = predict(models[[i]], newdata = train[foldID == i,], type = "response")
  res[i] = NormalizedWeightedGini(train[foldID == i, severity], 
                                  weights = train[foldID == i, Collision_claim_count], 
                                  submission = predictions[[i]])
}

mean_gini_base<-mean(res)
mean_gini_base

# Here's how to add back the predictions to the dataset
for (i in 1:5){
  train[foldID == i, prediction := predictions[[i]]]
}

# A CV Lift Chart
binnedPlot(response = train[, .(severity, prediction)], x = train$prediction, 
           weight = train$Collision_claim_count, type = "quantile",
           title="Double Lift Chart for base_glm1")


#adding new_policy
coll_dataset_claims= coll_dataset_claims %>%
  mutate(new_policy= case_when(
    Inception_date == Term_effective_date ~ "Y",
    TRUE ~ "N"
  ))

coll_dataset= coll_dataset %>%
  mutate(new_policy= case_when(
    Inception_date == Term_effective_date ~ "Y",
    TRUE ~ "N"
  ))

coll_dataset_Test= coll_dataset_Test %>%
  mutate(new_policy= case_when(
    Inception_date == Term_effective_date ~ "Y",
    TRUE ~ "N"
  ))

#one-way Analysis
dataToPlot = coll_dataset_claims %>%
  filter(partition == "Training")

binnedPlot(data.frame(dataToPlot$severity), 
           x = factor(dataToPlot$new_policy), 
           weight = dataToPlot$Collision_claim_count, 
           xlab = "New Policy", 
           ylab = "Claim Severity", 
           title = "Average claim Severity by New Policy?")

glm2<-glm(severity ~ AY_factor + new_policy,
          family=Gamma(link="log"), 
          data = coll_dataset_claims, 
          weights=Collision_claim_count,
          subset=(partition=="Training"), 
          na.action="na.pass",
          x = TRUE)
summary(glm2) #all significant

#Time consistency
glm2_TC<-glm(severity ~ AY_factor*new_policy,
             family=Gamma(link="log"), 
             data = coll_dataset_claims, 
             weights=Collision_claim_count, 
             subset=(partition=="Training"), 
             na.action="na.pass",
             x = TRUE)
plot(predictorEffects(glm2_TC, "AY_factor"), multiline=T) #failed

#F-test
anova(glm2, base_glm1, test="F") #passed

#Parismony 
c(AIC(base_glm1), BIC(base_glm1))
c(AIC(glm2), BIC(glm2)) #slight decrease in both

#CV for glm2
train<-filter(coll_dataset_claims, partition=="Training")
setDT(train)
# Cross-validated random folds (good to set a seed prior to creating folds for reproducibility)
set.seed(123)
foldID = sample(1:5, size = nrow(train), replace = T)

# Create storage for the Gini coefficients, CV models and CV predictions
res = rep(0, 5)
models = vector(mode = "list", 5)
predictions = vector(mode = "list", 5)

# Loop over folds, fit a model, predict and compute Gini 
for (i in 1:5){
  models[[i]] = glm(severity ~ AY_factor + new_policy,
                    family=Gamma(link="log"), 
                    data = train[foldID != i,], 
                    weights=Collision_claim_count,
                    na.action="na.pass",
                    x = TRUE)
  predictions[[i]] = predict(models[[i]], newdata = train[foldID == i,], type = "response")
  res[i] = NormalizedWeightedGini(train[foldID == i, severity], 
                                  weights = train[foldID == i, Collision_claim_count], 
                                  submission = predictions[[i]])
}

mean_gini_glm2<-mean(res)
mean_gini_glm2

# Here's how to add back the predictions to the dataset
for (i in 1:5){
  train[foldID == i, prediction := predictions[[i]]]
}

# A CV Lift Chart
binnedPlot(response = train[, .(severity, prediction)], x = train$prediction, 
           weight = train$Collision_claim_count, type = "quantile",
           title="Double Lift Chart for glm2")

#adding vehicle_age
coll_dataset_claims= coll_dataset_claims %>%
  mutate(vehicle_age=as.numeric(difftime(as.Date(Term_effective_date),as.Date(ISOdate(Vehicle_model_year, 1, 1)),
                                         units = "weeks"))/52.25)
summary(coll_dataset_claims$vehicle_age)

coll_dataset= coll_dataset %>%
  mutate(vehicle_age=as.numeric(difftime(as.Date(Term_effective_date),as.Date(ISOdate(Vehicle_model_year, 1, 1)),
                                         units = "weeks"))/52.25)

coll_dataset_Test= coll_dataset_Test %>%
  mutate(vehicle_age=as.numeric(difftime(as.Date(Term_effective_date),as.Date(ISOdate(Vehicle_model_year, 1, 1)),
                                         units = "weeks"))/52.25)
#capping
quantile(coll_dataset_claims$vehicle_age, c(0.8,0.85,0.9,0.95,0.96,0.97,0.98,0.99,1)) ## 99% below 23 years

coll_dataset = coll_dataset %>%
  mutate(vehicle_age = ifelse(vehicle_age<23, vehicle_age, 23))

coll_dataset_claims = coll_dataset_claims %>%
  mutate(vehicle_age = ifelse(vehicle_age<23, vehicle_age, 23))

coll_dataset_Test = coll_dataset_Test %>%
  mutate(vehicle_age = ifelse(vehicle_age<23, vehicle_age, 23))

nrow(coll_dataset_claims[coll_dataset_claims$vehicle_age<4,])
         
#one-way Analysis
dataToPlot = coll_dataset_claims %>%
  filter(partition == "Training")

binnedPlot(response = dataToPlot$severity,
           x = dataToPlot$vehicle_age, 
           weight = dataToPlot$Collision_incurred_amount,
           type= "equal", 
           g = 6, 
           xlab = "Vehicle Age", 
           ylab = "Claim Severity", 
           title = "Observed and fitted claim Severity by Vehicle Age")

#let's turn this into an ordinal variate:
coll_dataset_claims = coll_dataset_claims %>%
  mutate(vehicle_age_cat = ifelse(vehicle_age<=5, 1, ifelse(vehicle_age<=10, 2,
                                  ifelse(vehicle_age<=15, 3, ifelse(vehicle_age<=20, 4, 5)))))

dataToPlot = coll_dataset_claims %>%
  filter(partition == "Training")

binnedPlot(data.frame(dataToPlot$severity), 
           x = factor(dataToPlot$vehicle_age_cat), 
           weight = dataToPlot$Collision_claim_count, 
           xlab = "Vehicle Age Cat", 
           ylab = "Claim Severity", 
           title = "Average claim Severity by Vehicle Age Cat")                                                                    

glm3<-glm(severity ~ AY_factor + new_policy + vehicle_age,
          family=Gamma(link="log"), 
          data = coll_dataset_claims, 
          weights=Collision_claim_count,
          subset=(partition=="Training"), 
          na.action="na.pass",
          x = TRUE)
summary(glm3) #all significant

#Time consistency
glm3_TC<-glm(severity ~ AY_factor*vehicle_age,
             family=Gamma(link="log"), 
             data = coll_dataset_claims, 
             weights=Collision_claim_count, 
             subset=(partition=="Training"), 
             na.action="na.pass",
             x = TRUE)
plot(predictorEffects(glm3_TC, "AY_factor"), multiline=T) #passed

#Parsimony
c(AIC(glm2), BIC(glm2))
c(AIC(glm3), BIC(glm3)) #decrease in both

#F-Test
anova(glm2, glm3, test="F") #passed

#CV for glm3
train<-filter(coll_dataset_claims, partition=="Training")
setDT(train)
# Cross-validated random folds (good to set a seed prior to creating folds for reproducibility)
set.seed(123)
foldID = sample(1:5, size = nrow(train), replace = T)

# Create storage for the Gini coefficients, CV models and CV predictions
res = rep(0, 5)
models = vector(mode = "list", 5)
predictions = vector(mode = "list", 5)

# Loop over folds, fit a model, predict and compute Gini 
for (i in 1:5){
  models[[i]] = glm(severity ~ AY_factor +vehicle_age,
                    family=Gamma(link="log"), 
                    data = train[foldID != i,], 
                    weights=Collision_claim_count,
                    na.action="na.pass",
                    x = TRUE)
  predictions[[i]] = predict(models[[i]], newdata = train[foldID == i,], type = "response")
  res[i] = NormalizedWeightedGini(train[foldID == i, severity], 
                                  weights = train[foldID == i, Collision_claim_count], 
                                  submission = predictions[[i]])
}

mean_gini_glm3<-mean(res)
mean_gini_glm3

# Here's how to add back the predictions to the dataset
for (i in 1:5){
  train[foldID == i, prediction := predictions[[i]]]
}

# A CV Lift Chart
binnedPlot(response = train[, .(severity, prediction)], x = train$prediction, 
           weight = train$Collision_claim_count, type = "quantile",
           title="Double Lift Chart for glm3")


#adding age
coll_dataset_claims= coll_dataset_claims %>%
  mutate(age=as.numeric(difftime(as.Date("2021-12-31"),as.Date(Insured_birth_date), units = "weeks"))/52.25)

coll_dataset= coll_dataset %>%
  mutate(age=as.numeric(difftime(as.Date("2021-12-31"),as.Date(Insured_birth_date), units = "weeks"))/52.25)

coll_dataset_Test= coll_dataset_Test %>%
  mutate(age=as.numeric(difftime(as.Date("2021-12-31"),as.Date(Insured_birth_date), units = "weeks"))/52.25)

##replace NA with mean
coll_dataset_claims= coll_dataset_claims %>%
  mutate(age=ifelse(is.na(age), 48.93, age))

coll_dataset= coll_dataset %>%
  mutate(age=ifelse(is.na(age), 48.93, age))

coll_dataset_Test= coll_dataset_Test %>%
  mutate(age=ifelse(is.na(age), 48.93, age))

#one-way analysis
dataToPlot = coll_dataset_claims %>%
  filter(partition == "Training")

binnedPlot(response = dataToPlot$severity,
           x = dataToPlot$age, 
           weight = dataToPlot$Collision_incurred_amount,
           type= "equal", 
           g = 12, 
           xlab = "Age", 
           ylab = "Claim Severity", 
           title = "Observed and fitted claim Severity by Age") #weird trend

#let's try grouping
coll_dataset_claims= coll_dataset_claims %>%
  mutate(agecat=ifelse(age<25, 1, ifelse(age<36, 2, ifelse(age<51, 3, ifelse(age<66, 4, ifelse(age<81, 5, 6))))))

coll_dataset= coll_dataset%>%
  mutate(agecat=ifelse(age<25, 1, ifelse(age<36, 2, ifelse(age<51, 3, ifelse(age<66, 4, ifelse(age<81, 5, 6))))))

coll_dataset_Test= coll_dataset_Test%>%
  mutate(agecat=ifelse(age<25, 1, ifelse(age<36, 2, ifelse(age<51, 3, ifelse(age<66, 4, ifelse(age<81, 5, 6))))))

#one-way analysis
dataToPlot = coll_dataset_claims %>%
  filter(partition == "Training")

binnedPlot(response = dataToPlot$severity,
           x = dataToPlot$agecat, 
           weight = dataToPlot$Collision_incurred_amount,
           type= "equal", 
           g = 6, 
           xlab = "Agecat", 
           ylab = "Claim Severity", 
           title = "Observed and fitted claim Severity by AgeCat") #nice decreasing trend

#adding agecat to the model
glm4<-glm(severity ~ AY_factor + vehicle_age +agecat + new_policy,
          family=Gamma(link="log"), 
          data = coll_dataset_claims, 
          weights=Collision_claim_count, 
          subset=(partition=="Training"), 
          na.action="na.pass",
          x = TRUE)
summary(glm4) # new policy is no longer significant

#removing new_policy
glm4<-glm(severity ~ AY_factor + vehicle_age +agecat,
          family=Gamma(link="log"), 
          data = coll_dataset_claims, 
          weights=Collision_claim_count, 
          subset=(partition=="Training"), 
          na.action="na.pass",
          x = TRUE)
summary(glm4) 

# Time Consistency
glm4_TC<-glm(severity ~ AY_factor*agecat,
             family=Gamma(link="log"), 
             data = coll_dataset_claims, 
             weights=Collision_claim_count, 
             subset=(partition=="Training"), 
             na.action="na.pass",
             x = TRUE)
plot(predictorEffects(glm4_TC, "AY_factor"), multiline=T) #passed

#Parsimony
c(AIC(glm3), BIC(glm3))
c(AIC(glm4), BIC(glm4)) #slight decrease in both

#F-Test
anova(glm3, glm4, test="F") #passed

#CV for glm4
train<-filter(coll_dataset_claims, partition=="Training")
setDT(train)
# Cross-validated random folds (good to set a seed prior to creating folds for reproducibility)
set.seed(123)
foldID = sample(1:5, size = nrow(train), replace = T)

# Create storage for the Gini coefficients, CV models and CV predictions
res = rep(0, 5)
models = vector(mode = "list", 5)
predictions = vector(mode = "list", 5)

# Loop over folds, fit a model, predict and compute Gini 
for (i in 1:5){
  models[[i]] = glm(severity ~ AY_factor + vehicle_age +agecat,
                    family=Gamma(link="log"), 
                    data = train[foldID != i,], 
                    weights=Collision_claim_count,
                    na.action="na.pass",
                    x = TRUE)
  predictions[[i]] = predict(models[[i]], newdata = train[foldID == i,], type = "response")
  res[i] = NormalizedWeightedGini(train[foldID == i, severity], 
                                  weights = train[foldID == i, Collision_claim_count], 
                                  submission = predictions[[i]])
}

mean_gini_glm4<-mean(res)
mean_gini_glm4

# Here's how to add back the predictions to the dataset
for (i in 1:5){
  train[foldID == i, prediction := predictions[[i]]]
}

# A CV Lift Chart
binnedPlot(response = train[, .(severity, prediction)], x = train$prediction, 
           weight = train$Collision_claim_count, type = "quantile",
           title="Double Lift Chart for glm4")

#adding Liab_driving_record
#as factor
coll_dataset_claims = coll_dataset_claims %>%
  mutate(Liab_driving_record_f = as.factor(Liab_driving_record))

coll_dataset_claims = coll_dataset_claims %>%
  mutate(Liab_driving_record_f= ifelse(is.na(Liab_driving_record_f),"NA", Liab_driving_record_f))

coll_dataset_claims = coll_dataset_claims %>%
  mutate(Liab_driving_record_f = as.factor(Liab_driving_record))

coll_dataset = coll_dataset %>%
  mutate(Liab_driving_record_f = as.factor(Liab_driving_record))

coll_dataset= coll_dataset %>%
  mutate(Liab_driving_record_f= ifelse(is.na(Liab_driving_record_f),"NA", Liab_driving_record_f))

coll_dataset= coll_dataset%>%
  mutate(Liab_driving_record_f = as.factor(Liab_driving_record))

coll_dataset_Test = coll_dataset_Test %>%
  mutate(Liab_driving_record_f = as.factor(Liab_driving_record))

coll_dataset_Test= coll_dataset_Test %>%
  mutate(Liab_driving_record_f= ifelse(is.na(Liab_driving_record_f),"NA", Liab_driving_record_f))

coll_dataset_Test= coll_dataset_Test%>%
  mutate(Liab_driving_record_f = as.factor(Liab_driving_record))

#one-way Analysis
dataToPlot = coll_dataset_claims %>%
  filter(partition == "Training")

binnedPlot(response = dataToPlot$severity,
           x = dataToPlot$Liab_driving_record_f, 
           weight = dataToPlot$Collision_incurred_amount,
           type= "equal", 
           g = 6, 
           xlab = "Liab_driving_record", 
           ylab = "Claim Severity", 
           title = "Observed and fitted claim Severity by Driving Record") #not a great trend

#Indicator variate
coll_dataset_claims = coll_dataset_claims %>%
  mutate(Liab_driving_record_Ind= ifelse(Liab_driving_record_f == 6, "group6", "not6"))

coll_dataset_claims = coll_dataset_claims %>%
  mutate(Liab_driving_record_Ind= ifelse(is.na(Liab_driving_record_Ind), "not6", Liab_driving_record_Ind))

coll_dataset= coll_dataset %>%
  mutate(Liab_driving_record_Ind= ifelse(Liab_driving_record_f == 6, "group6", "not6"))

coll_dataset = coll_dataset %>%
  mutate(Liab_driving_record_Ind= ifelse(is.na(Liab_driving_record_Ind), "not6", Liab_driving_record_Ind))

coll_dataset_Test= coll_dataset_Test %>%
  mutate(Liab_driving_record_Ind= ifelse(Liab_driving_record_f == 6, "group6", "not6"))

coll_dataset_Test = coll_dataset_Test %>%
  mutate(Liab_driving_record_Ind= ifelse(is.na(Liab_driving_record_Ind), "not6", Liab_driving_record_Ind))


#one-way analysis
dataToPlot = coll_dataset_claims %>%
  filter(partition == "Training")

binnedPlot(response = dataToPlot$severity,
           x = dataToPlot$Liab_driving_record_Ind, 
           weight = dataToPlot$Collision_incurred_amount,
           type= "equal", 
           g = 6, 
           xlab = "Liab_driving_record_Ind", 
           ylab = "Claim Severity", 
           title = "Observed and fitted claim Severity by Driving Record Ind") #makes sense

#adding to model
glm5<-glm(severity ~ AY_factor + vehicle_age +agecat + Liab_driving_record_Ind,
          family=Gamma(link="log"), 
          data = coll_dataset_claims, 
          weights=Collision_claim_count,
          subset=(partition=="Training"), 
          na.action="na.pass",
          x = TRUE)
summary(glm5) #all significant

#Parsimony
c(AIC(glm4), BIC(glm4))
c(AIC(glm5), BIC(glm5)) #slight decrease in both

#Time Consistency
glm5_TC<-glm(formula = severity ~ AY_factor * Liab_driving_record_Ind, 
             family = Gamma(link = "log"), data = coll_dataset_claims, 
             weights = Collision_claim_count, subset = (partition == "Training"), 
             na.action = "na.pass", x = TRUE)
plot(predictorEffects(glm5_TC, "AY_factor"), multiline=T) #passed

#F-Test
anova(glm4,glm5,test="F") #passed

#CV for glm5
train<-filter(coll_dataset_claims, partition=="Training")
setDT(train)
# Cross-validated random folds (good to set a seed prior to creating folds for reproducibility)
set.seed(123)
foldID = sample(1:5, size = nrow(train), replace = T)

# Create storage for the Gini coefficients, CV models and CV predictions
res = rep(0, 5)
models = vector(mode = "list", 5)
predictions = vector(mode = "list", 5)

# Loop over folds, fit a model, predict and compute Gini 
for (i in 1:5){
  models[[i]] = glm(severity ~ AY_factor + vehicle_age +agecat + 
                      Liab_driving_record_Ind,
                    family=Gamma(link="log"), 
                    data = train[foldID != i,], 
                    weights=Collision_claim_count,
                    na.action="na.pass",
                    x = TRUE)
  predictions[[i]] = predict(models[[i]], newdata = train[foldID == i,], type = "response")
  res[i] = NormalizedWeightedGini(train[foldID == i, severity], 
                                  weights = train[foldID == i, Collision_claim_count], 
                                  submission = predictions[[i]])
}

mean_gini_glm5<-mean(res)
mean_gini_glm5

# Here's how to add back the predictions to the dataset
for (i in 1:5){
  train[foldID == i, prediction := predictions[[i]]]
}

# A CV Lift Chart
binnedPlot(response = train[, .(severity, prediction)], x = train$prediction, 
           weight = train$Collision_claim_count, type = "quantile",
           title="Double Lift Chart for glm5")

#Adding marital_status
coll_dataset_claims = coll_dataset_claims%>%
  mutate(Marital_status_Ind = ifelse(Marital_status == "Common Law", "Married", 
                                     ifelse(Marital_status=="Same sex", "Married", 
                                            ifelse(Marital_status=="Married", "Married", "Single"))))

coll_dataset = coll_dataset%>%
  mutate(Marital_status_Ind = ifelse(Marital_status == "Common Law", "Married", 
                                     ifelse(Marital_status=="Same sex", "Married", 
                                            ifelse(Marital_status=="Married", "Married", "Single"))))

coll_dataset_Test = coll_dataset_Test%>%
  mutate(Marital_status_Ind = ifelse(Marital_status == "Common Law", "Married", 
                                     ifelse(Marital_status=="Same sex", "Married", 
                                            ifelse(Marital_status=="Married", "Married", "Single"))))

#one-way analysis
dataToPlot = coll_dataset_claims %>%
  filter(partition == "Training")

binnedPlot(response = dataToPlot$severity,
           x = dataToPlot$Marital_status_Ind, 
           weight = dataToPlot$Collision_incurred_amount,
           type= "equal", 
           g = 6, 
           xlab = "Marital Status", 
           ylab = "Claim Severity", 
           title = "Observed and fitted claim Severity by Marital Status") #makes sense

#adding marital status to model
glm6<-glm(severity ~ AY_factor + vehicle_age +agecat+ 
            Liab_driving_record_Ind + Marital_status_Ind,
          family=Gamma(link="log"), 
          data = coll_dataset_claims, 
          weights=Collision_claim_count, 
          subset=(partition=="Training"), 
          na.action="na.pass",
          x = TRUE)
summary(glm6) #borderline significant, let's see...

#Parsimony
c(AIC(glm5), BIC(glm5))
c(AIC(glm6), BIC(glm6)) #BIC slightly increases

#Time consistency
glm6_TC<-glm(severity ~ AY_factor*Marital_status_Ind,
             family=Gamma(link="log"), 
             data = coll_dataset_claims, 
             weights=Collision_claim_count,
             subset=(partition=="Training"), 
             na.action="na.pass",
             x = TRUE)
plot(predictorEffects(glm6_TC, "AY_factor"), multiline=T) #passed

#F-Test
anova(glm5,glm6, test="F") #passed, barely

#CV for glm6
train<-filter(coll_dataset_claims, partition=="Training")
setDT(train)
# Cross-validated random folds (good to set a seed prior to creating folds for reproducibility)
set.seed(123)
foldID = sample(1:5, size = nrow(train), replace = T)

# Create storage for the Gini coefficients, CV models and CV predictions
res = rep(0, 5)
models = vector(mode = "list", 5)
predictions = vector(mode = "list", 5)

# Loop over folds, fit a model, predict and compute Gini 
for (i in 1:5){
  models[[i]] = glm(severity ~ AY_factor + vehicle_age +agecat + 
                      Liab_driving_record_Ind + Marital_status_Ind,
                    family=Gamma(link="log"), 
                    data = train[foldID != i,], 
                    weights=Collision_claim_count,
                    na.action="na.pass",
                    x = TRUE)
  predictions[[i]] = predict(models[[i]], newdata = train[foldID == i,], type = "response")
  res[i] = NormalizedWeightedGini(train[foldID == i, severity], 
                                  weights = train[foldID == i, Collision_claim_count], 
                                  submission = predictions[[i]])
}

mean_gini_glm6<-mean(res)
mean_gini_glm6

# Here's how to add back the predictions to the dataset
for (i in 1:5){
  train[foldID == i, prediction := predictions[[i]]]
}

# A CV Lift Chart
binnedPlot(response = train[, .(severity, prediction)], x = train$prediction, 
           weight = train$Collision_claim_count, type = "quantile",
           title="Double Lift Chart for glm6")

#adding Num_at_fault
coll_dataset_claims = coll_dataset_claims %>%
  mutate(Num_at_fault_claims_past_1_yr_Ind= ifelse(Num_at_fault_claims_past_1_yr!=0, 1, 0))

coll_dataset_claims = coll_dataset_claims %>%
  mutate(Num_at_fault_claims_past_1_yr_Ind= as.factor(Num_at_fault_claims_past_1_yr_Ind))

coll_dataset= coll_dataset %>%
  mutate(Num_at_fault_claims_past_1_yr_Ind= ifelse(Num_at_fault_claims_past_1_yr!=0, 1, 0))

coll_dataset = coll_dataset %>%
  mutate(Num_at_fault_claims_past_1_yr_Ind= as.factor(Num_at_fault_claims_past_1_yr_Ind))

coll_dataset_Test= coll_dataset_Test %>%
  mutate(Num_at_fault_claims_past_1_yr_Ind= ifelse(Num_at_fault_claims_past_1_yr!=0, 1, 0))

coll_dataset_Test = coll_dataset_Test %>%
  mutate(Num_at_fault_claims_past_1_yr_Ind= as.factor(Num_at_fault_claims_past_1_yr_Ind))

#Adding to the model
glm7<-glm(severity ~ AY_factor + vehicle_age +agecat + 
            + Marital_status_Ind + Liab_driving_record_Ind +
            Num_at_fault_claims_past_1_yr_Ind,
          family=Gamma(link="log"), 
          data = coll_dataset_claims, 
          weights=Collision_claim_count, 
          subset=(partition=="Training"), 
          na.action="na.pass",
          x = TRUE)
summary(glm7) #all signifincant

#Parsimony
c(AIC(glm6), BIC(glm6)) 
c(AIC(glm7), BIC(glm7)) #both decrease

#Time consistency
glm7_TC<-glm(severity ~ AY_factor*Num_at_fault_claims_past_1_yr_Ind,
             family=Gamma(link="log"), 
             data = coll_dataset_claims, 
             weights=Collision_claim_count, 
             subset=(partition=="Training"), 
             na.action="na.pass",
             x = TRUE)
plot(predictorEffects(glm7_TC, "AY_factor"), multiline=T) #passed

#F-Test
anova(glm6,glm7,test="F") #passed

#CV for glm7
train<-filter(coll_dataset_claims, partition=="Training")
setDT(train)
# Cross-validated random folds (good to set a seed prior to creating folds for reproducibility)
set.seed(123)
foldID = sample(1:5, size = nrow(train), replace = T)

# Create storage for the Gini coefficients, CV models and CV predictions
res = rep(0, 5)
models = vector(mode = "list", 5)
predictions = vector(mode = "list", 5)

# Loop over folds, fit a model, predict and compute Gini 
for (i in 1:5){
  models[[i]] = glm(severity ~ AY_factor + vehicle_age +agecat + 
                      Liab_driving_record_Ind + Marital_status_Ind +
                      Num_at_fault_claims_past_1_yr_Ind,
                    family=Gamma(link="log"), 
                    data = train[foldID != i,], 
                    weights=Collision_claim_count,
                    na.action="na.pass",
                    x = TRUE)
  predictions[[i]] = predict(models[[i]], newdata = train[foldID == i,], type = "response")
  res[i] = NormalizedWeightedGini(train[foldID == i, severity], 
                                  weights = train[foldID == i, Collision_claim_count], 
                                  submission = predictions[[i]])
}

mean_gini_glm7<-mean(res)
mean_gini_glm7

# Here's how to add back the predictions to the dataset
for (i in 1:5){
  train[foldID == i, prediction := predictions[[i]]]
}

# A CV Lift Chart
binnedPlot(response = train[, .(severity, prediction)], x = train$prediction, 
           weight = train$Collision_claim_count, type = "quantile",
           title="Double Lift Chart for glm7")

#correlation check!

coll_dataset_claims = coll_dataset_claims %>%
  mutate(Vehicle_retail_price= ifelse(is.na(Vehicle_retail_price),34618, Vehicle_retail_price))

coll_dataset_claims = coll_dataset_claims %>%
  mutate(Vehicle_horsepower= ifelse(is.na(Vehicle_horsepower),213.1, Vehicle_horsepower))

coll_dataset_claims = coll_dataset_claims %>%
  mutate(Vehicle_wheelbase= ifelse(is.na(Vehicle_wheelbase),2944, Vehicle_wheelbase))

coll_dataset = coll_dataset%>%
  mutate(Vehicle_retail_price= ifelse(is.na(Vehicle_retail_price),34618, Vehicle_retail_price))

coll_dataset = coll_dataset %>%
  mutate(Vehicle_horsepower= ifelse(is.na(Vehicle_horsepower),213.1, Vehicle_horsepower))

coll_dataset = coll_dataset %>%
  mutate(Vehicle_wheelbase= ifelse(is.na(Vehicle_wheelbase),2944, Vehicle_wheelbase))

coll_dataset_Test = coll_dataset_Test %>%
  mutate(Vehicle_retail_price= ifelse(is.na(Vehicle_retail_price),34618, Vehicle_retail_price))

coll_dataset_claims %>% 
  filter(partition == "Training") %>%
  select(Vehicle_horsepower, Vehicle_retail_price, Vehicle_wheelbase) %>%
  cor(method = "spearman")

#Demerit points
coll_dataset_claims = coll_dataset_claims %>%
  mutate(Num_demerit_points=ifelse(is.na(Num_demerit_points), 0, Num_demerit_points))
## too empty

coll_dataset_claims = coll_dataset_claims %>%
  mutate(Num_demerit_points_Ind=ifelse(Num_demerit_points==0, 0, 1))

coll_dataset_claims = coll_dataset_claims %>%
  mutate(driving_record=ifelse(Liab_driving_record_Ind=="group6", 0, 1))

#Num minor convinctions
#one way analysis
dataToPlot = coll_dataset_claims %>%
  filter(partition == "Training")

binnedPlot(response = dataToPlot$severity,
           x = dataToPlot$Num_minor_convictions, 
           weight = dataToPlot$Collision_incurred_amount,
           type= "equal", 
           g = 12, 
           xlab = "Number of Minor Convictions", 
           ylab = "Claim Severity", 
           title = "Observed and fitted claim Severity by Number of Minor Convictions")

quantile(coll_dataset_claims$Num_minor_convictions, c(0.8,0.85,0.90,0.95,0.96,0.97,0.98,0.985,0.99,0.995,1)) #cap at 11

coll_dataset_claims = coll_dataset_claims %>%
  mutate(Num_minor_convictions=ifelse(Num_minor_convictions>8,8,Num_minor_convictions))
#don't add

#Num_yrs_since_last_at_fault_claim
#turn into indicator
coll_dataset_claims = coll_dataset_claims %>%
  mutate(Num_yrs_since_fault=ifelse(Num_yrs_since_last_at_fault_claim<=5,"last_5","over_5"))

coll_dataset_claims = coll_dataset_claims %>%
  mutate(Num_yrs_since_fault= ifelse(is.na(Num_yrs_since_fault), "never", Num_yrs_since_fault))

coll_dataset_claims = coll_dataset_claims %>%
  mutate(Num_yrs_since_fault= factor(Num_yrs_since_fault, levels=c("never", "over_5", "last_5")))

#one-way analysis
dataToPlot = coll_dataset_claims %>%
  filter(partition == "Training")

binnedPlot(data.frame(dataToPlot$severity), 
           x = factor(dataToPlot$Num_yrs_since_fault), 
           weight = dataToPlot$Collision_claim_count, 
           xlab = "Num_yrs_since_fault", 
           ylab = "Claim Severity", 
           title = "Average claim Severity by Num_yrs_since_fault")

#Add to model
glm8<-glm(severity ~ AY_factor + vehicle_age +agecat + 
            Liab_driving_record_Ind + Marital_status_Ind +
            Num_at_fault_claims_past_1_yr_Ind + Num_yrs_since_fault,
          family=Gamma(link="log"), 
          data = coll_dataset_claims, 
          weights=Collision_claim_count, 
          subset=(partition=="Training"), 
          na.action="na.pass",
          x = TRUE)
summary(glm8) #not significant

#Vehicle Retail Price
#one way analysis
dataToPlot = coll_dataset_claims %>%
  filter(partition == "Training")

binnedPlot(response = dataToPlot$severity,
           x = dataToPlot$Vehicle_retail_price, 
           weight = dataToPlot$Collision_incurred_amount,
           type= "equal", 
           g = 8, 
           xlab = "Retail Price", 
           ylab = "Claim Severity", 
           title = "Observed and fitted claim Severity by Retail Price") #makes sense

quantile(coll_dataset_claims$Vehicle_retail_price, c(0.8,0.85,0.90,0.95,0.96,0.97,0.98,0.985,0.99,0.995,1)) #cap at 90300

coll_dataset_claims = coll_dataset_claims %>%
  mutate(Vehicle_retail_price=ifelse(Vehicle_retail_price>90300, 90300, Vehicle_retail_price))

coll_dataset = coll_dataset %>%
  mutate(Vehicle_retail_price=ifelse(Vehicle_retail_price>90300, 90300, Vehicle_retail_price))

coll_dataset_Test = coll_dataset_Test %>%
  mutate(Vehicle_retail_price=ifelse(Vehicle_retail_price>90300, 90300, Vehicle_retail_price))


#Add to model
glm9<-glm(severity ~ AY_factor + vehicle_age +agecat + 
            Liab_driving_record_Ind + Marital_status_Ind +
            Num_at_fault_claims_past_1_yr_Ind + Vehicle_retail_price,
          family=Gamma(link="log"), 
          data = coll_dataset_claims, 
          weights=Collision_claim_count, 
          subset=(partition=="Training"), 
          na.action="na.pass",
          x = TRUE)
summary(glm9) #all significant

#Parsimony
c(AIC(glm7), BIC(glm7))
c(AIC(glm9), BIC(glm9)) #both decrease

#Time consistency
glm9_TC<-glm(severity ~ AY_factor*Vehicle_retail_price,
             family=Gamma(link="log"), 
             data = coll_dataset_claims, 
             weights=Collision_claim_count,
             subset=(partition=="Training"), 
             na.action="na.pass",
             x = TRUE)
plot(predictorEffects(glm9_TC, "AY_factor"), multiline=T) #passed

#F-Test
anova(glm7,glm9,test="F") #passed

#CV for glm9
train<-filter(coll_dataset_claims, partition=="Training")
setDT(train)
# Cross-validated random folds (good to set a seed prior to creating folds for reproducibility)
set.seed(123)
foldID = sample(1:5, size = nrow(train), replace = T)

# Create storage for the Gini coefficients, CV models and CV predictions
res = rep(0, 5)
models = vector(mode = "list", 5)
predictions = vector(mode = "list", 5)

# Loop over folds, fit a model, predict and compute Gini 
for (i in 1:5){
  models[[i]] = glm(severity ~ AY_factor + vehicle_age +agecat + 
                      Liab_driving_record_Ind + Marital_status_Ind +
                      Num_at_fault_claims_past_1_yr_Ind + Vehicle_retail_price,
                    family=Gamma(link="log"), 
                    data = train[foldID != i,], 
                    weights=Collision_claim_count,
                    na.action="na.pass",
                    x = TRUE)
  predictions[[i]] = predict(models[[i]], newdata = train[foldID == i,], type = "response")
  res[i] = NormalizedWeightedGini(train[foldID == i, severity], 
                                  weights = train[foldID == i, Collision_claim_count], 
                                  submission = predictions[[i]])
}

mean_gini_glm9<-mean(res)
mean_gini_glm9

# Here's how to add back the predictions to the dataset
for (i in 1:5){
  train[foldID == i, prediction := predictions[[i]]]
}

# A CV Lift Chart
binnedPlot(response = train[, .(severity, prediction)], x = train$prediction, 
           weight = train$Collision_claim_count, type = "quantile",
           title="Double Lift Chart for glm9")

#Vehicle Horsepower
dataToPlot = coll_dataset_claims %>%
  filter(partition == "Training")

binnedPlot(response = dataToPlot$severity,
           x = dataToPlot$Vehicle_horsepower, 
           weight = dataToPlot$Collision_incurred_amount,
           type= "equal", 
           g = 8, 
           xlab = "Horsepower", 
           ylab = "Claim Severity", 
           title = "Observed and fitted claim Severity by Horsepower") #makes sense

quantile(coll_dataset_claims$Vehicle_horsepower, c(0.8,0.85,0.90,0.95,0.96,0.97,0.98,0.985,0.99,0.995,1)) #cap at 403

coll_dataset_claims = coll_dataset_claims %>%
  mutate(Vehicle_horsepower=ifelse(Vehicle_horsepower>403, 403, Vehicle_horsepower))

#Add to model
glm10<-glm(severity ~ AY_factor + vehicle_age +agecat + 
            Liab_driving_record_Ind + Marital_status_Ind +
            Num_at_fault_claims_past_1_yr_Ind + Vehicle_horsepower,
          family=Gamma(link="log"), 
          data = coll_dataset_claims, 
          weights=Collision_claim_count, 
          subset=(partition=="Training"), 
          na.action="na.pass",
          x = TRUE)
summary(glm10) #all significant

#Parsimony
c(AIC(glm7), BIC(glm7))
c(AIC(glm10), BIC(glm10)) #both decrease

#Time consistency
glm10_TC<-glm(severity ~ AY_factor*Vehicle_horsepower,
             family=Gamma(link="log"), 
             data = coll_dataset_claims, 
             weights=Collision_claim_count, 
             subset=(partition=="Training"), 
             na.action="na.pass",
             x = TRUE)
plot(predictorEffects(glm10_TC, "AY_factor"), multiline=T) #passed

#F-Test
anova(glm7,glm10,test="F") #passed

#CV for glm10
train<-filter(coll_dataset_claims, partition=="Training")
setDT(train)
# Cross-validated random folds (good to set a seed prior to creating folds for reproducibility)
set.seed(123)
foldID = sample(1:5, size = nrow(train), replace = T)

# Create storage for the Gini coefficients, CV models and CV predictions
res = rep(0, 5)
models = vector(mode = "list", 5)
predictions = vector(mode = "list", 5)

# Loop over folds, fit a model, predict and compute Gini 
for (i in 1:5){
  models[[i]] = glm(severity ~ AY_factor + vehicle_age +agecat + 
                      Liab_driving_record_Ind + Marital_status_Ind +
                      Num_at_fault_claims_past_1_yr_Ind + Vehicle_horsepower,
                    family=Gamma(link="log"), 
                    data = train[foldID != i,], 
                    weights=Collision_claim_count,
                    na.action="na.pass",
                    x = TRUE)
  predictions[[i]] = predict(models[[i]], newdata = train[foldID == i,], type = "response")
  res[i] = NormalizedWeightedGini(train[foldID == i, severity], 
                                  weights = train[foldID == i, Collision_claim_count], 
                                  submission = predictions[[i]])
}

mean_gini_glm10<-mean(res)
mean_gini_glm10

# Here's how to add back the predictions to the dataset
for (i in 1:5){
  train[foldID == i, prediction := predictions[[i]]]
}

# A CV Lift Chart
binnedPlot(response = train[, .(severity, prediction)], x = train$prediction, 
           weight = train$Collision_claim_count, type = "quantile",
           title="Double Lift Chart for glm10")

#adding vehicle_wheelbase
dataToPlot = coll_dataset_claims %>%
  filter(partition == "Training")

binnedPlot(response = dataToPlot$severity,
           x = dataToPlot$Vehicle_wheelbase, 
           weight = dataToPlot$Collision_incurred_amount,
           type= "equal", 
           g = 8, 
           xlab = "Horsepower", 
           ylab = "Claim Severity", 
           title = "Observed and fitted claim Severity by Horsepower") #makes sense

quantile(coll_dataset_claims$Vehicle_wheelbase, c(0.8,0.85,0.90,0.95,0.96,0.97,0.98,0.985,0.99,0.995,1)) #no capping

quantile(coll_dataset_claims$Vehicle_wheelbase, c(0.005, 0.01,0.02,0.03,0.04,0.05,0.07,0.1)) #floor at 2300

coll_dataset_claims = coll_dataset_claims %>%
  mutate(Vehicle_wheelbase=ifelse(Vehicle_wheelbase<2300, 2300, Vehicle_wheelbase))

#add to model 
glm11<-glm(severity ~ AY_factor + vehicle_age +agecat + 
             Liab_driving_record_Ind + Marital_status_Ind +
             Num_at_fault_claims_past_1_yr_Ind + Vehicle_wheelbase,
           family=Gamma(link="log"), 
           data = coll_dataset_claims, 
           weights=Collision_claim_count, 
           subset=(partition=="Training"), 
           na.action="na.pass",
           x = TRUE)
summary(glm11) #all significant

#Parsimony
c(AIC(glm7), BIC(glm7))
c(AIC(glm11), BIC(glm11)) #both decrease

#Time consistency
glm11_TC<-glm(severity ~ AY_factor*Vehicle_wheelbase,
              family=Gamma(link="log"), 
              data = coll_dataset_claims, 
              weights=Collision_claim_count, 
              subset=(partition=="Training"), 
              na.action="na.pass",
              x = TRUE)
plot(predictorEffects(glm11_TC, "AY_factor"), multiline=T) #passed, but converging

#F-Test
anova(glm7,glm11,test="F") #passed, barely

#CV for glm11
train<-filter(coll_dataset_claims, partition=="Training")
setDT(train)
# Cross-validated random folds (good to set a seed prior to creating folds for reproducibility)
set.seed(123)
foldID = sample(1:5, size = nrow(train), replace = T)

# Create storage for the Gini coefficients, CV models and CV predictions
res = rep(0, 5)
models = vector(mode = "list", 5)
predictions = vector(mode = "list", 5)

# Loop over folds, fit a model, predict and compute Gini 
for (i in 1:5){
  models[[i]] = glm(severity ~ AY_factor + vehicle_age +agecat + 
                      Liab_driving_record_Ind + Marital_status_Ind +
                      Num_at_fault_claims_past_1_yr_Ind + Vehicle_wheelbase,
                    family=Gamma(link="log"), 
                    data = train[foldID != i,], 
                    weights=Collision_claim_count,
                    na.action="na.pass",
                    x = TRUE)
  predictions[[i]] = predict(models[[i]], newdata = train[foldID == i,], type = "response")
  res[i] = NormalizedWeightedGini(train[foldID == i, severity], 
                                  weights = train[foldID == i, Collision_claim_count], 
                                  submission = predictions[[i]])
}

mean_gini_glm11<-mean(res)
mean_gini_glm11

# Here's how to add back the predictions to the dataset
for (i in 1:5){
  train[foldID == i, prediction := predictions[[i]]]
}

# A CV Lift Chart
binnedPlot(response = train[, .(severity, prediction)], x = train$prediction, 
           weight = train$Collision_claim_count, type = "quantile",
           title="Double Lift Chart for glm11")

#CV Gini DF
models<-c(1,2,3,4,5,6,7,9,10,11)
mean_ginis<-c(mean_gini_base, mean_gini_glm2, mean_gini_glm3, mean_gini_glm4, mean_gini_glm5,
              mean_gini_glm6, mean_gini_glm7, mean_gini_glm9, mean_gini_glm10, mean_gini_glm11)
cbind(models,mean_ginis)

#According to the train data, model 9 is the best!
#We will test models 9 and 10 on the holdout data. 


##Testing for glm9
#deviance residuals
dev_resid_glm9 <- resid(glm9, type="deviance")
plot(glm9$fitted.values, dev_resid_glm9, xlab="Deviance Residuals",
     ylab="Fitted Values", main="Deviance Residuals for Retail Price Model")

hist(dev_resid_glm9, prob=TRUE, breaks=30, main="Retail Price Model")
curve(dnorm(x, mean=mean(dev_resid_glm9), sd=sd(dev_resid_glm9)), add=TRUE)

qqnorm(dev_resid_glm9, main="Retail Price Model Q-Q Plot")
qqline(dev_resid_glm9)

#Testing for glm10
dev_resid_glm10 <- resid(glm10, type="deviance")
plot(glm10$fitted.values, dev_resid_glm10, xlab="Deviance Residuals",
     ylab="Fitted Values", main="Deviance Residuals for Horsepower Model")

hist(dev_resid_glm10, prob=TRUE, breaks=30, main="Horsepower Model")
curve(dnorm(x, mean=mean(dev_resid_glm10), sd=sd(dev_resid_glm10)), add=TRUE)

qqnorm(dev_resid_glm10, main="Horsepower Q-Q Plot")
qqline(dev_resid_glm10)
#error assumptions for both are valid

##Holdout Testing comparing glm9 and glm10
holdout<-filter(coll_dataset, partition=="Holdout")
holdout<-coll_dataset[coll_dataset$Collision_incurred_amount!=0,]
glm_9_reduced<- glm(severity ~ AY_factor + vehicle_age +agecat + 
                      Marital_status_Ind +
                      Num_at_fault_claims_past_1_yr_Ind + Vehicle_retail_price,
                    family=Gamma(link="log"), 
                    data = holdout)
glm_9_hold<-glm(severity ~ AY_factor + vehicle_age +agecat + 
                  Liab_driving_record_Ind + Marital_status_Ind +
                  Num_at_fault_claims_past_1_yr_Ind + Vehicle_retail_price,
                family=Gamma(link="log"), 
                data = holdout)
c(AIC(glm_9_hold), BIC(glm_9_hold))
c(AIC(glm_9_reduced),BIC(glm_9_reduced)) #9_hold is lower for both.  

train<- coll_dataset_claims

adj_factor <- (mean(holdout$Collision_incurred_amount)/mean(holdout$Collision_claim_count))/
  (mean(train$Collision_incurred_amount)/mean(train$Collision_claim_count))
adj_factor 

holdout$predictionsglm9_hold <- predict(glm_9_hold, holdout,  type="response",na.action="na.pass")*adj_factor 
holdout$predictionsglm9_reduced <- predict(glm_9_reduced, holdout,
                                               type="response",na.action="na.pass")*adj_factor

#Gini Coefficients 
holdout_gini_glm9<-NormalizedWeightedGini(holdout$Collision_incurred_amount, holdout$Collision_claim_count, 
                       holdout$predictionsglm9*holdout$Collision_claim_count) 

holdout_gini_glm10<-NormalizedWeightedGini(holdout$Collision_incurred_amount, holdout$Collision_claim_count, 
                                          holdout$predictionsglm10*holdout$Collision_claim_count)

#Lift for glm9
holdout$predictionsglm9_hold_decile = pmin(floor(rank(holdout$predictionsglm9_hold)/nrow(holdout)*10)+1, 10)
dataToPlot1 = data.frame("Observed Severity" = holdout$severity, "Predicted Severity - Full Model" = holdout
                        $predictionsglm9_hold)
binnedPlot(dataToPlot1, x = factor(holdout$predictionsglm9_hold_decile), weight = holdout$Collision_claim_count, xlab = 
             "Predicted Decile - Full Model", ylab = "Claim Severity",
           title = "Observed and Predicted Claim Severity - Full Model by Decile")

#Lift for glm10
holdout$predictionsglm9_reduced_decile = pmin(floor(rank(holdout$predictionsglm9_reduced)/nrow(holdout)*10)+1, 10)
dataToPlot2 = data.frame("Observed Severity" = holdout$severity, "Predicted Severity - Reduced Model" = holdout
                        $predictionsglm9_reduced)
binnedPlot(dataToPlot2, x = factor(holdout$predictionsglm9_reduced_decile), weight = holdout$Collision_claim_count, xlab = 
             "Predicted Decile - Reduced Model", ylab = "Claim Severity", 
           title = "Observed and Predicted Claim Severity - Reduced by Decile")

#Double Lift for glm9 vs glm10
holdout$sort_ratio <- holdout$predictionsglm9/holdout$predictionsglm10
holdout$sort_ratio_decile <- pmin(floor(rank(holdout$sort_ratio)/nrow(holdout)*12)+1, 12)
dataToPlot = data.frame("Observed Severity" = holdout$severity, "Predicted Severity - Model glm9" = holdout
                        $predictionsglm9, "Predicted Severity - Model glm10" = holdout$predictionsglm10)
#dev.new()
binnedPlot(dataToPlot, x = factor(holdout$sort_ratio_decile), weight = holdout$Collision_claim_count, xlab = "Sort Ratio 
(Retail Price/Horsepower) Decile", ylab = "Claim Severity", title = "Double Lift Chart - Retail Price vs. Horsepower")

#We go with GLM9 for severity!!!!

#Final glm9:
coll_dataset_claims=coll_dataset_claims%>%
  mutate(Collision_deductible=factor(Collision_deductible))

glm9_final<-glm(severity ~ AY_factor + vehicle_age +agecat + 
                  Liab_driving_record_Ind + Marital_status_Ind +
                  Num_at_fault_claims_past_1_yr_Ind + Vehicle_retail_price, 
                family=Gamma(link="log"), 
                data = coll_dataset_claims, 
                weights=Collision_claim_count, 
#                offset=Collision_deductible,
                offset=(ifelse(Collision_deductible==1000, -6.807e-05, 0)),
                na.action="na.pass",
                x = TRUE)
summary(glm9_final)

#Adding the predictions to the full dataset
coll_dataset_claims$sev_predictions<-predict(glm9_final, coll_dataset_claims,
                                             type="response",na.action="na.pass")
head(coll_dataset_claims$sev_predictions)

#Loading in frequency data 
freq_data<-read.csv("~/Documents/Winter 2023/ACTSC 489/freq_holdout_2.csv")

freq_model<-glm(Collision_claim_count ~ Accident_year + Age_imputed + Age_imputed_squared + 
                                           Vehicle_age_cap_30 +
                                           Num_minor_convictions_cap_5+
                                           New_business_imputed +
                                           Has_partner,
                                         family = quasipoisson(link = "log"),
                                         data = freq_data,
                                         offset = log(Collision_earned_count),
                                         x = TRUE)

#Adding predictions to dataset
adj_freq=0.9728966
freq_data$freq_predictions<-predict(freq_model, freq_data,
                                             type="response",na.action="na.pass")/freq_data$Collision_earned_count


freq_data$freq_predictions<-freq_data$freq_predictions*adj_freq

holdout_full<-filter(coll_dataset, partition=="Holdout")

adj_sev <- (mean(holdout_full$Collision_incurred_amount)/mean(holdout_full$Collision_claim_count))/
  (mean(train$Collision_incurred_amount)/mean(train$Collision_claim_count))

holdout_full$sev_predictions<-predict(glm9, holdout_full, type='response')*adj_factor

holdout_merge<-merge(x = freq_data, y = holdout_full, by = "id", all.x = FALSE, all.y = TRUE)

#making frequency X severity
holdout_merge$freq_x_sev=holdout_merge$freq_predictions*holdout_merge$sev_predictions
holdout_merge$freq_x_sev_predictions = holdout_merge$freq_predictions*holdout_merge$sev_predictions

#making loss cost
holdout_merge$loss_cost=holdout_merge$Collision_incurred_amount.x/holdout_merge$Collision_earned_count.x

head(cbind(full_data$loss_cost, full_data$freq_x_sev))

non_zero<-full_data[full_data$loss_cost!=0,]

NormalizedWeightedGini(holdout_merge$loss_cost, weights= holdout_merge$Collision_earned_count.x, submission = holdout_merge$freq_x_sev)

NormalizedWeightedGini(freq_data$Collision_claim_count/freq_data$Collision_earned_count,
                       weights= freq_data$Collision_earned_count.x, submission = freq_data$freq_predictions)

results<-cbind(full_data$freq_predictions,
           full_data$sev_predictions, full_data$freq_x_sev_predictions)

colnames(results)<-c("freq_predictions", "sev_predictions","freq_x_sev_predictions")

head(results)

write.csv(holdout_merge, "/Users/vanisingh/Documents/Winter 2023/ACTSC 489/vani_holdout.csv")

#Kaggle
coll_dataset_Test$sev_predictions<-predict(glm9, coll_dataset_Test, type='response')
freq_kaggle<-read.csv("~/Documents/Winter 2023/ACTSC 489/dat2022_forFrequency.csv")
kaggle_sub1<-merge(x=coll_dataset_Test, y=freq_kaggle, by="id", all.x=FALSE)
kaggle_sub1$fs<-kaggle_sub1$predictions*kaggle_sub1$sev_predictions
kaggle_sub1<-cbind(kaggle_sub1$id, kaggle_sub1$fs)
colnames(kaggle_sub1)<-c("id", "predictions")
write.csv(kaggle_sub1, "/Users/vanisingh/Documents/Winter 2023/ACTSC 489/kaggle_sub1.csv")



#Spatial Analysis 
datLC_merge<- read.csv("~/Documents/Winter 2023/ACTSC 489/datLC_merge.csv")

datLC_merge = datLC_merge %>%
  mutate(per_2_LH= ifelse(per_2<0.3, "low", "high"))

datLC_merge = datLC_merge %>%
  mutate(per_5_LH= ifelse(per_5<0.09, "low", "high"))

geo_1 <- glm(Loss_cost ~ Accident_year + Vehicle_age_cap_30_floor_6 + 
               + Years_driving + Years_driving_squared + Num_minor_convictions_cap_5 +
               Num_yrs_since_last_at_fault_claim_num_cap_13 + New_business_imputed + Has_partner +
               + age_15,
             family=tweedie(var.power = 1.5, link.power = 0), 
             data = datLC_merge,
             subset = (partition == "Training"), 
             weights = Collision_earned_count)
summary(geo_1)

datLC_merge=datLC_merge%>%
  mutate(Collision_deductible=factor(Collision_deductible))

full_tweedie<-glm(Loss_cost ~ Accident_year + Vehicle_age_cap_30_floor_6 + 
                                 + Years_driving + Years_driving_squared + Num_minor_convictions_cap_5 +
                                 Num_yrs_since_last_at_fault_claim_num_cap_13 + New_business_imputed + Has_partner,
                               family=tweedie(var.power = 1.5, link.power = 0), 
                               data = datLC_merge,
                  offset=ifelse(Collision_deductible==1000, -0.090767, 0),
                  weights = Collision_earned_count)

## Territorial Modelling
# Load required packages
easypackages::packages("dplyr", "modelr", "purrr", "broom", "tidyr", "data.table",
                       "ggplot2", "shiny", "Hmisc", "feather", "fst", "statmod",
                       "tweedie", "insuranceData", "shinyjs", "sp", "rgdal",
                       "maptools", "ggmap", "gridExtra", "geosphere", "caret",
                       "scales", "zoo", "xts", "effects", "rgeos", "leaflet", "tmap")


tmap_options(check.and.fix = TRUE)
require(rgdal)
require(rgeos)
require(maptools)
ON = readOGR(dsn = "~/Documents/Winter 2023/ACTSC 489/ON-Subset/ON-subset.shp", stringsAsFactors = F)
# ZCTA5CE10 represents the zip code within this shape file. We convert it to numeric. WA@data$ZCTA5CE10 = as.numeric(WA@data$ZCTA5CE10)
# For simplicity, we keep only those zip codes which are in our housing dataset.
plot(ON)

# Compute multiplicative residual
train<-filter(datLC_merge, partition=="Training")
datLC_merge$residual = datLC_merge$loss_cost/predict(full_tweedie, newdata = datLC_merge, type = "response")

train$residual = train$loss_cost/predict(full_tweedie, newdata = train, type = "response")

# Aggregate residual by FSA
train_meanResidualByFSA = train %>% dplyr::select(residual, Collision_earned_count, FSA)%>% group_by(FSA) %>% 
  summarise(meanResidual = weighted.mean(residual, Collision_earned_count))

# First ensure that no previous residuals have been merged onto the shape file as yet
ON@data$residual = NULL 

# Now, merge the residuals onto the shape file
ON@data = merge(ON@data, meanResidualByFSA, by.x = "CFSAUID", by.y = "FSA", all.x = T)

# Set the mapping mode
tmap_mode("view") # Change to plot for non-interactive plots

tm_shape(ON) + tm_polygons(col = "meanResidual", n = 3, style = "quantile")

###############
# Steps for fitting territorial cluster
###############

# You would have defined folds before using the createFolds function. 
# We need to repeat the creation of the folds but this time, we make it a list
set.seed(123) # You should set the same seed you used before to create the folds.
foldList = createFolds(1:nrow(train), k = 5, list = T)

# Perform territorial clustering
cvGini = territorialSmoothing(data = train, dataRegionVar = "FSA", shapeFile = ON, shapeFileRegionVar = "CFSAUID",
                              glmMod = full_tweedie, responseVar = "loss_cost", weightVar = "Collision_earned_count",
                              folds = foldList, smoothingFactor = 50, clusters = 4)

# You can ignore any warnings about adding new columns and removing them

# The above function is mainly used for computing the CV Gini and finding the optimal smoothing parameter and cluster count
# To get the clusters, we do the following:
clusters = territorialSmoothingClusters(data = train, dataRegionVar = "FSA", shapeFile = ON, shapeFileRegionVar = "CFSAUID", 
                                        glmMod = full_tweedie, responseVar = "loss_cost", weightVar = "Collision_earned_count",
                                        smoothingFactor = 50, clusters = 4)

# Making a categorical version of the clusters
clusters$terr_cluster_factor = as.factor(clusters$terr_cluster)

# You can merge the clusters back onto the dataset as follows
train = merge(train, clusters, by.x = "FSA", by.y = "id", all.x = T)

train$terr_cluster_factor <- as.factor(train$terr_cluster_factor) %>% relevel(ref = "0.92")

# Sample model with clusters
train_tweedie_terr<-glm(Loss_cost ~ Accident_year + Vehicle_age_cap_30_floor_6 + 
                         + Years_driving + Years_driving_squared + Num_minor_convictions_cap_5 +
                         Num_yrs_since_last_at_fault_claim_num_cap_13 + New_business_imputed + Has_partner+
                         terr_cluster_factor,
                       family=tweedie(var.power = 1.5, link.power = 0), 
                       data = train, 
                       weights = Collision_earned_count)

summary(train_tweedie_terr)

###############
# Visualize clusters
###############

# First ensure that no previous clusters have been merged onto the shape file as yet
ON@data$terr_cluster = NULL 
ON@data$terr_cluster_factor = NULL

# Set the mapping mode
tmap_mode("view") # Change to plot for non-interactive plots

# Now, merge the clusters onto the shape file
ON@data = merge(ON@data, clusters, by.x = "CFSAUID", by.y = "id", all.x = T)

# Create map now
tm_shape(ON) + tm_polygons(col = "terr_cluster_factor", popup.vars = c("CFSAUID", "terr_cluster_factor"))

ON_fortified<- tidy(ON, region="CFSAUID")
setDT(ON_fortified)
unique_zip_codes = ON_fortified[, list("lat" = mean(lat), "long" = mean(long)), by = "id"] # Plot now


```{r}
J = 0
sigma = 100
numClusters = 3
unique_zip_codes_with_residuals = merge(unique_zip_codes,
                                        train_meanResidualByFSA, 
                                        by.x = "id",
                                        by.y = "FSA")

##Adj-Based Smoothing
adjacency_matrix = gTouches(ON, byid=TRUE)
diag(adjacency_matrix) = T

all.equal(unique_zip_codes_with_residuals[, id], ON@data)

unique_zip_codes_with_residuals = unique_zip_codes_with_residuals[, adjacency_weighted_mean_res :=
                                                                    unlist(lapply(1:nrow(ON), function(x){
                                                                      currentAdjacencies = adjacency_matrix[x,] 
                                                                      unique_zip_codes_with_residuals[currentAdjacencies == T, weighted.mean(mean_residual, count }))]
  # Compute sj's
  unique_zip_codes_with_residuals[, credibility_smoothing_factor :=
                                    unique_zip_codes[, count/(count + J)]] # Compute adjacency-based smoothed residual
  unique_zip_codes_with_residuals[, adjacency_based_smoothed_residual :=
                                    credibility_smoothing_factor*mean_residual +
                                    (1 - credibility_smoothing_factor)*adjacency_weighted_mean_residual]
  # Re-do the cluster creation process for adjacency-based smoothing
  set.seed(100) # Seed for reproducibility
  kmeans_clustering = kmeans(unique_zip_codes_with_residuals[, adjacency_based_smoothed_residual]
                             centers = numClusters)
  unique_zip_codes_with_residuals = unique_zip_codes_with_residuals[, adjacency_smoothed_cluster_
                                                                    round(kmeans_clustering$centers[kmeans_clustering$cluster], 2)] WA_fortified_with_cluster = merge(WA_fortified_with_cluster,
                                                                                                                                                                      unique_zip_codes_with_residuals[, c("adjacency_smoothed_cluster_mean", "id"), with = F],
                                                                                                                                                                      by = "id", sort = F) 
  # Merge clusters onto map data for plotting WA_fortified_with_cluster[, adjacency_smoothed_cluster_mean :=
  factor(adjacency_smoothed_cluster_mean)] # Convert cluster to factor for plotting purposes
idual := )]


### Distance-based
# Compute distance matrix
distance_matrix = apply(unique_zip_codes_with_residuals[, c("long", "lat"), with = F], 1, 
                        function(x) distHaversine(x, unique_zip_codes_with_residuals[, c("long", "lat"), with = F]))

# Compute Rj's
unique_zip_codes_with_residuals$distance_weighted_mean_residual =
  unlist(lapply(1:nrow(distance_matrix), 
                function(x){weights = unique_zip_codes_with_residuals[, Sum_collision_earned_count]*
                  exp(-distance_matrix[, x]/sigma^2)
                weighted.mean(unique_zip_codes_with_residuals[, meanResidual], weights)
                
                }))

# Compute sj's
unique_zip_codes_with_residuals$credibility_smoothing_factor =
  unique_zip_codes_with_residuals[, Sum_collision_earned_count/(Sum_collision_earned_count + J)]

# Compute distance-based smoothed residual
unique_zip_codes_with_residuals[, distance_based_smoothed_residual :=
                                  credibility_smoothing_factor*meanResidual +
                                  (1 - credibility_smoothing_factor)*distance_weighted_mean_residual]


# Re-do the cluster creation process for distance-based smoothing
set.seed(100) # Seed for reproducibility
kmeans_clustering = kmeans(unique_zip_codes_with_residuals[, distance_based_smoothed_residual],
                           centers = numClusters)

unique_zip_codes_with_residuals = unique_zip_codes_with_residuals[, distance_smoothed_cluster_mean:=
                                                                    round(kmeans_clustering$centers[kmeans_clustering$cluster], 2)]

ON_fortified_with_cluster = merge(ON_fortified_with_cluster,
                                  unique_zip_codes_with_residuals[, c("distance_smoothed_cluster_mean", "id"), with = F],
                                  by = "id", sort = F) # Merge clusters onto map data for plotting

ON_fortified_with_cluster[, distance_smoothed_cluster_mean :=
                            factor(distance_smoothed_cluster_mean)] # Convert cluster to factor for plotting purposes
```

### Adjacency-based
# Compute adjacency matrix
# Defining adjacent to include given postal code (i.e. a postal code is considered to be adjacent to itself 
adjacency_matrix = gTouches(ON, byid=TRUE)
diag(adjacency_matrix) = T

# Verify that zipcodes are in same order for WA and unique_zip_codes_with_residuals before proceeding
all.equal(unique_zip_codes_with_residuals[, id], ON@data$CFSAUID)

# Compute Rj's
unique_zip_codes_with_residuals = unique_zip_codes_with_residuals[, adjacency_weighted_mean_residual :=
                                                                    unlist(lapply(1:nrow(ON), function(x){
                                                                      currentAdjacencies = adjacency_matrix[x,] 
                                                                      unique_zip_codes_with_residuals[currentAdjacencies == T, weighted.mean(meanResidual, Sum_collision_earned_count)]
                                                                    }))]
# Compute sj's
unique_zip_codes_with_residuals[, credibility_smoothing_factor :=
                                  unique_zip_codes[, Sum_collision_earned_count/(Sum_collision_earned_count + J)]] 

# Compute adjacency-based smoothed residual
unique_zip_codes_with_residuals[, adjacency_based_smoothed_residual := credibility_smoothing_factor*meanResidual +
                                  (1 - credibility_smoothing_factor)*adjacency_weighted_mean_residual]

# Re-do the cluster creation process for adjacency-based smoothing
set.seed(100) # Seed for reproducibility
kmeans_clustering = kmeans(unique_zip_codes_with_residuals[, adjacency_based_smoothed_residual],
                           centers = numClusters)

unique_zip_codes_with_residuals = unique_zip_codes_with_residuals[, adjacency_smoothed_cluster_mean :=
                                                                    round(kmeans_clustering$centers[kmeans_clustering$cluster], 2)]

ON_fortified_with_cluster = merge(ON_fortified_with_cluster,
                                  unique_zip_codes_with_residuals[, c("adjacency_smoothed_cluster_mean", "id"), with = F],
                                  by = "id", sort = F) # Merge clusters onto map data for plotting 

ON_fortified_with_cluster[, adjacency_smoothed_cluster_mean :=
                            factor(adjacency_smoothed_cluster_mean)] # Convert cluster to factor for plotting purposes
```

### Combining
#### Distance-based
datLC_merge = merge(datLC_merge,
                    unique_zip_codes_with_residuals[,c("distance_smoothed_cluster_mean", "id"), with = F], 
                    by.x = "FSA",
                    by.y = "id", 
                    sort = F)
setnames(datLC_merge, "distance_smoothed_cluster_mean", "territorialClusters_distance_smoothed")
datLC_merge$territorialClusters_distance_smoothed <- 
  as.factor(datLC_merge$territorialClusters_distance_smoothed) %>% relevel(ref = "0.87")
datLC_merge <-datLC_merge[order(datLC_merge$id),]
row.names(datLC_merge) <- NULL
datLC_merge_train <- datLC_merge %>% filter(partition == "Training")
datLC_merge_holdout <- datLC_merge %>% filter(partition == "Holdout")


# datLC_merge %>% filter(partition == "Training") %>% group_by(territorialClusters) %>% summarise(exp = sum(Collision_earned_count))
# Highest exposure = 0.87
```

#### Adjacency-based
datLC_merge = merge(datLC_merge,
                    unique_zip_codes_with_residuals[,c("adjacency_smoothed_cluster_mean", "id"), with = F], 
                    by.x = "FSA",
                    by.y = "id", 
                    sort = F)
setnames(datLC_merge, "adjacency_smoothed_cluster_mean", "territorialClusters_adjacency_smoothed")
datLC_merge$territorialClusters_adjacency_smoothed <- 
  as.factor(datLC_merge$territorialClusters_adjacency_smoothed) %>% relevel(ref = "1.02")
datLC_merge <-datLC_merge[order(datLC_merge$id),]
row.names(datLC_merge) <- NULL
datLC_merge_train <- datLC_merge %>% filter(partition == "Training")
datLC_merge_holdout <- datLC_merge %>% filter(partition == "Holdout")


# datLC_merge %>% filter(partition == "Training") %>% group_by(territorialClusters) %>% summarise(exp = sum(Collision_earned_count))
# Highest exposure = 1.02


dat2022 = dat2022 %>% 
  mutate(FSA=ifelse(FSA=="M4R", "LOC", FSA))