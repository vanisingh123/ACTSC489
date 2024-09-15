# ACTSC489
A GLM Severity Model Made in R

## Report Description
This process discusses the methodology and results of building a loss cost model that will then be used for pricing auto 
insurance policies. To achieve this, firstly, this report will build and compare a Tweedie model, which models loss cost 
directly, and a Frequency X Severity model, which models collision frequency and severity independently and then 
multiplies them together to achieve loss cost. Both models are built using the training data. After comparing both of these 
models’ performance on a holdout dataset, this report finds that the Tweedie model has a better performance and is 
therefore chosen to model loss cost using non-geographical predictors from the dataset. 
 
Afterwards, the report attempts to improve the model by introducing geographic predictors and territoria/residual 
modelling. The geographic predictors were derived from the 2011 Canadian census. However, the findings of this analysis 
revealed that the inclusion of these predictors resulted in overfitting of the model to the training dataset. Therefore, it was 
concluded that the Tweedie model that did not include any territorial or geographic predictors yielded the most optimal 
performance and was subsequently recommended for utilization in modeling collision loss cost. 
 
Finally, the loss cost model was transformed into an auto insurance pricing model by incorporating the deductibles. 
Additionally, the report deliberated on the methods to extrapolate this data for upcoming years, specifically for the 
accident year 2022.

## Repository Description
This repository includes the code for the model building of the severity model. 
