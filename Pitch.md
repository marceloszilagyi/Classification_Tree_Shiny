Interactive Sandbox for a Classification Tree Model
========================================================
author: Marcelo Szilagyi
date: November, 11, 2016
autosize: true

Motivation and Approach
========================================================

This shiny app is a *prototype* for an app that helps data analysts to perform an initial data analysis and run a [classification tree](https://en.wikipedia.org/wiki/Decision_tree_learning) to test hypothesis for a (binary) classification problem

Users can:

- Navigate between the tabs (one for each type of variable)
- Understand the data using contingency tables and visualizations
- Selecting the variables to be part of the classification tree
- See the models results and refine the model variable selection, by adding or removing variables

Data 
========================================================

The data used for the example is the [German Credit Data](https://archive.ics.uci.edu/ml/datasets/Statlog+(German+Credit+Data)), extracted from the [caret package](http://topepo.github.io/caret/datasets.html). A script adds more meaningful names for the user and based on the number of unique values, defines if a variable is binary, categorical or continuous. The code below starts the process

```r
require(caret); data("GermanCredit"); credit = GermanCredit
fac = which((as.data.frame(lapply(sapply(credit,unique),length))<10)==T)
credit2 = cbind((as.data.frame(lapply(credit[,fac],as.factor))),credit[,-fac])
credit2 = credit2[c(names(credit))]
factors = which(lapply (credit2, class) == "factor")
head(data.frame(name = names(credit2),
                          class=sapply(credit2,class),
                          count=sapply(credit2,function (x)length(unique(x)))))
```

```
                                               name   class count
Duration                                   Duration integer    33
Amount                                       Amount integer   921
InstallmentRatePercentage InstallmentRatePercentage  factor     4
ResidenceDuration                 ResidenceDuration  factor     4
Age                                             Age integer    53
NumberExistingCredits         NumberExistingCredits  factor     4
```

Results
========================================================
After the proper selection of variables, users can see the decision tree and the confusion matrix: 
![App results](myimage.PNG)


App and Code hyperlinks, future developments
========================================================

The app can be found at <https://meszilag.shinyapps.io/Week4/> and the code can be found at <

Future developments may include
- User upload of data files
- Automated creation of visualizations
- Selection of models


