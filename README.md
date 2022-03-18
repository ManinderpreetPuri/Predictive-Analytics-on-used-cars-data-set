# Predictive-Analytics-on-used-cars-data-set
A used car online selling company in the USA is in the process of updating their car price assessment method where they want to apply a data driven technique. The trial dataset consists of 25 variables describing 23531 car sales from 2019 to 2020. The management is very keen to apply predictive modelling for this task where the trail data set is to be used to build and evaluate predictive models to ascertain the feasibility of such an approach. The company outsourced the task to me.

Part A– Problem Formulation 
The objective of this section (Part A) is ‘domain understanding and familiarisation’ phase data analysts go through prior to the actual analytics. Since I have to 
carry out analytics projects in different domains in the future, where I may not have domain knowledge, it was an important skill to develop. I answered the folowing questions:

1. Carried out an exploratory study to identify the background and relevant features of used cars which influence their value in USA and methods used for price evaluation and assessment of used cars.
2. Identifed the data sources that would contain information useful for value assessment of used cars. What is the possible format of such information? Will I face any problems in accessing this data?
3. What variables would be useful to build a predictive model to assess the used cars? How do I identify such variables?


Part B– Data Exploration and Cleaning 
I used the provided dataset to answer this section. I had 24 variables that were directly related to used carsales from the above-mentioned dataset. Most of these variables were similar to the type of information that an assessor will use to evaluate and assess the price of a used car (e.g. when was it made? What is the length and width of the car? What type of wheel system? Number of seats?). 

I answered the following questions with evidence and justifications.
1. a. Which variables are continuous/numerical? Which are ordinal? Which are nominal?
b. What are the methods for transforming categorical variables? 
c. Carried out and demonstrated data transformation where necessary. 
2. a. Calculated following summary statistics: mean, median, max and standard deviation for each of the continuous variables, and count for each categorical variable. 
b. Is there any evidence of extreme values? Briefly discussed.
3. Plotted histograms for each of the continuous variables and createed summary statistics. Based on 
the histogram and summary statistics answered the following and provided brief explanations:
a. Which variables have the largest variability?
b. Which variables seems skewed?
c. Are there any values that seem extreme? 
4. a. Which, if any, of the variables have missing values? 
b. What are the methods of handling missing values?
c. Applied the 3 methods of missing value and demonstrate the output (summary statistics and 
transformation plot) for each method in (4-a). Which method of handling missing values is most suitable for this data set? Discussed 
briefly referring to the data set.
5. a. Evaluated the correlations between the variables. 
b. Which variables should be used for dimension reduction and why? Carried out dimensionality 
reduction.
c. Explored the distribution of selected variables (from step 5-a) against the target variable. Explained.


Part C– Building predictive models
1. Regression Modelling 
a. Built a regression model with the selected variables. 
b. Evaluated the regression model and carried out feature selection to build a better regression model. I tried out at least 3 regression models to identify the 
optimal model.
c. Compared these regression models based on evaluation metrics and provided the 
formula for each regression model.
2. Decision Tree Modelling
a. Build a decision tree with the selected variables.
b. Evaluated the decision tree model and carried out pruning to build a better decision tree model. I tried out at least 3 decision trees to obtain the 
optimaltree.
c. Compared these decision treemodels based on evaluation metrics and provided the tree plot for each model and explained the outputs.
3. Model Comparison
a. Why do we need to build several models in both regression and decision trees (as requested in question 1 and 2)?
b. Compared the accuracy of the selected (optimal) regression model and (optimal) decision tree and discussed and justified the most suitable predictive model for the 
business case.
4. Modelling and Comparison with a Filtered Data Set
a. Filter and select rows with only one specific make_name (i.e., BMW) from the data set with selected variables.
b. Redo the question 1 and 2 based on the filtered data (only need to build one model each based on the best model setting from Q1 and Q2)
c. Compared the performance of regression model before filtering and after filtering, and the decision tree model before filtering and after filtering. What do you find? Discussed findings.
