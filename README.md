## Shiny App for the Classification of Binary Response Data ##

This project initially created as part of my thesis for my MSc. in Statistics. I've uploaded it to github in an effort to make myself improve it, and make it more of a generalised app (as it was written with the objective of performaing on two datasets, and hence may not be that robust).

With that in mind, I'm looking to firstly improve it to a quality I'm happy with, then possibly publish it ( _make it public on github rather_ ).

### Prerequisites ###

Currenlty, the version of `R` I'm running is `R version 3.3.2`. Various libraries are required to run this application, the majority are statistical modelling packages, listed in the `server.R` file, but two packages not listed which must be loaded prior to running are; 
* `shiny`
* `plotly`

## Sample Data ##

The sample data used in the begining to run tests on the app are listed below with a brief explaination listed on both.

* __Luminex Data__ - This dataset contains data relating to patients proteomic information, the dependent variable (which we are looking to classify) details the type of arthritis that the patient has, either Rhemuthoid (RA) or Psoriatic (PsA). 
* __Prostate Data__ - This dataset also contains the relating to the patients protemic make-up. The dependant variable in this case details whether the patient's prostate cancer is malignant or benign. 

One of the objectives of updating this project is to make the application more robust to the data that is uploaded, however, the datasets must follow some basic criteria, namely;

* The dependant variable is binomially distributed
* The independant varibles are continuous, numeric variables
* The data uploaded is in either a `.csv` or `.xlsx`format.

