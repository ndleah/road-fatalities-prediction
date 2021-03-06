# Road Fatalities Prediction <img src="https://media0.giphy.com/media/3o6ozgHi0Fv82zA12M/giphy.gif" align="right" width="120" />
 > Group project for "Statistical Thinking for Data Science" @ UTS, Spring 2021

> 👉 Access the full analysis report [**here**](/Part%20A%20-%20proposal/Retail_Proposal_5th_September.pdf)

## Overview
Modeling the severity of accidents based on the most effective variables accounts for developing a high-precision model presenting the possibility of occurrence of each category of future accidents, and
it could be utilized to prioritize the corrective measures for authorities.

The purpose of this study is to identify the most influential variables contributing to fatal accidents, given an accident occurs. To do so we collected road accident data for Victoria, Australia, between the
years 2006 to 2020. In this regard, multiple logistic regression is used to recognize the most influential variables on fatal accidents and the approach for accident prediction.

## Methodology
Then it was prepared to handle its outliers, missing values and skewness. This was followed with further transformation of the data using one hot encoding and label encoding. And then we fixed the
data imbalance by using two different techniques, undersampling and SMOTE, to create two different datasets. Several iterations of these three steps were taken to conduct this regression analysis:

> * **Step 1:** Select features that go into the model
> * **Step 2:** Develop and adjust model
> * **Step 3:** Assess and validate model

Our final model uses the undersampling data set. Training set is from the years 2006 to 2017, and the testing set is from the years 2017 to 2020. With a prediction threshold set as 0.50.

## Result
Results show that the logistic regression in the forward stepwise method has an accuracy prediction power of 72.4%. The most important result of the logit model accentuates the role of variables for
speed zone, vulnerability of pedestrians, number of people involved (more distractions), alongside unfavorable weather and the dominant role of unsafe and poor quality of vehicles on increasing the severity of accidents.

## Limitations
Our limitations of this work were the limited availability of complete data on which to conduct the analysis. So, while the analysis produced non-significance, it is anticipated that as more data becomes
available, the models will yield more concrete findings.

Regardless, understanding the relationships among incident causal factors and outcomes may shed light on those causal factors which have the potential to lead to catastrophic events and those which may
lead to less severe events.
