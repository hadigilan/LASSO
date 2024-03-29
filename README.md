# LASSO
This repository provides materials on the LASSO theory and its application in finance --- has been used in the course Advances of Machine Learning in Finance (ACCFIN5229), at ASBS, University of Glasgow, 2022-23.

# The importance of the LASSO
Gelman, A., & Vehtari, A. (2021). What are the most important statistical ideas of the past 50 years?. Journal of the American Statistical Association, 116(536), 2087-2097.

# R code
We use the glmnet function from glmnet package to run LASSO regression in R. We have two examples to show the results of this function and interpretation:
1) An artificial data analysis to illustrate variable selection results and draw the solution (or regularization) path 
2) A real data analysis to show the applicability of the LASSO in finance

# The PwerPoint file
There is a short review on LASSO theory and R programming.

# Google Sheet
The LASSO created a new path in the world of variable selection, and model fitting. In this [Google Sheet](https://docs.google.com/spreadsheets/d/1MSrK9J_LY4USCosxjmCCPW6U2sEnZw0LuaeUOV_FumE/edit?usp=sharing), we introduce those methods which have close connection with LASSO. 

There exist five sheets: 
1) penalty function: introduces lasso-related penalties
2) loss function: introduces references which use an alternative loss function insead of the sum of squares
3) computational algorightm: introduces studies which propose an algorithm to solve the objective functions in penalized regression 
4) theoretical properties
5) applications of lasso and its different variants in non-regression models


