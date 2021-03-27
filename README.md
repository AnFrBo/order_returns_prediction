# Prediction of Order Returns of an Online Clothing Retailer

Customers send back a substantial part of the products that they purchase online. Return shipping is expensive for online platforms and return orders are said to reach 50% for certain industries and products. Nevertheless, free or inexpensive return shipping has become a customer expectation and de-facto standard in the fierce online competition on clothing, but shops have indirect ways to influence customer purchase behaviour. For purchases where return seems likely, a shop could, for example, restrict payment options or display additional marketing communication. 

In order to access this information, I build a targeting model to return the likelihood of a customer returning an order and thus optimizing the shop revenue. The (real-world) data was provided by an online clothing retailer. However, the data was artificially balanced (1:1 ratio between returns and non-returns).

With my final model which was an ensemble of differently tuned xgboosts and random forests, the AUC of my prediction was 0.73725 and within the top 20% of 178 participants.

The full description of the kaggle challenge as well as the data and leaderboard can be accessed [here](https://www.kaggle.com/c/bads1920/leaderboard).

All code was written in RStudio.

## Organization

__Author:__ Anna Franziska Bothe <br>
__Institute:__ Humboldt University Berlin, Chair of Information Systems <br>
__Course__: Business Analytics and Data Science <br>
__Semester:__ WS 2019/20 <br>


## Content

```
.
├── data                  # folder with original data sets and cleaned, engineered data that are outputted by the data_preparation file
├── code                  # folder with file containing data cleaning, prep and feature engineering (= data_preparation.R) plus file containing model tuning, train, selection and the code for the final prediction (= model&prediction.R)
├── final_prediction.csv  # final prediction that was handed in for the evaluationa at the [kaggle challenge](https://www.kaggle.com/c/bads1920/overview)
├── README.md             # this readme file
├── requirements.txt      # contains all used libraries
├── setup.txt             # describes execution of pipeline in detail

```






