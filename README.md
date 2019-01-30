## Shiny App for the Classification of Binary Response Data ##

This project initially created as part of my thesis for my MSc. in Statistics. 
I aim to make it more of a generalised shiny application, that can take any .csv file given and assess selected machine learning methods on their attempt at classifying a dataset (as it was written with the objective of performaing on two datasets, and hence may not be that robust).

### Prerequisites ###

Currenlty, the version of `R` I'm running is `R version 3.3.2`. Various libraries are required to run this application, the majority are statistical modelling packages, listed in the `server.R` file, but two packages not listed which must be loaded prior to running are; 
* `shiny`
* `plotly`

### Sample Data ###

The sample data used in the begining to run tests on the app are listed below with a brief explaination listed on both.

* __Luminex Data__ - This dataset contains data relating to patients proteomic information, the dependent variable (which we are looking to classify) details the type of arthritis that the patient has, either Rhemuthoid (RA) or Psoriatic (PsA). 
* __Prostate Data__ - This dataset also contains the relating to the patients protemic make-up. The dependant variable in this case details whether the patient's prostate cancer is malignant or benign. 

One of the objectives of updating this project is to make the application more robust to the data that is uploaded, however, the datasets must follow some basic criteria, namely;

* The dependant variable is binomially distributed
* The independant varibles are continuous, numeric variables
* The data uploaded is in either a `.csv` or `.xlsx`format.

### Machine Learning/ Statistical Models ###

Currently, the shiny application looks at the performance of the following classification methods:

* Lasso Regression (L<sub>1</sub> Regularization)
* Support Vector Machine 
* Adaptive Boosting
* Random Forest 
* Oblique Random Forest 

