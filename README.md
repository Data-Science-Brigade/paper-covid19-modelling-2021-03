# paper-covid19-modelling-2021-03
Repository for the paper: Weekly Bayesian modelling strategy to predict deaths by COVID-19: a model and case study for the state of Santa Catarina, Brazil

# Licenses

All code, in this repository, authored by the paper's authors, available inside a subfolder `modelo-epidemiologico-sc` is under the license defined in the original repository: https://github.com/Data-Science-Brigade/modelo-epidemiologico-sc/

All other code, in this repository, authored by the paper's authors, is under a MIT license with additional terms, present below.

All code not authored by the paper's authors should have their original authors consulted to check under which license it was made available.

## LICENSE

Copyright (c) 2021 Jonathan Cardoso-Silva, Pedro Henrique da Costa Avelar

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Any person using this software must agree to make careful and thoughtful use of the software, not use it to cause harm to anyone, and to be especially careful when using it to drive decisions.

If any person uses the software present here in any way, they agree to follow the requirements under the "Citation" section of this file and cite the authors for their work.

# Required software

R (version 3.6.3), preferrably in RStudio or with devtools installed. The required packages should be downloaded from CRAN as needed when building the project. To generate the plots you will also need to install the `lemon` package in your environment, as it is required by some plots.

Python (version 3.7.7), with pandas (version 1.1.3), numpy (version 1.19.2), scipy (version 1.5.2) and tqdm (version 4.46.0) installed to run the test-batches.

If you want to generate the eps files from the pdf files (using the `pdf2eps` script), you will also need to install Inkscape (version 0.92.5).

# How to prepare the data

Data availabilty is still to be defined. When and if we can make it available, a link will be here to the data and giving instructions of where to put each file.

Most of the data used, however, was taken from one of the following sources (or a closed-acess alternative by the same provider):

* SES: http://dados.sc.gov.br/organization/ses
  * http://dados.sc.gov.br/dataset/covid-19-dados-anonimizados-de-casos-confirmados 
* Google: https://www.google.com/covid19/mobility/
  * https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv

# How to run the tests

For each folder `{test-name}` in `tests`, enter its subfolder `tests/{test-name}/modelo-epidemiologico-sc/epiCata/` and build and install the project using RStudio or with `R -x "library(devtools); build(); install()"`. Then access the subfolder `tests/{test-name}/test/` and run `python test.py` -- this will generate two subfolders `tests/{test-name}/figures/` and `tests/{test-name}/results/` where the diagnostic figures and the results for each model are located.

To produce the result tables and figures, copy the all subfolders, except for the first, in the `results` folder of each tested model to the `plots/results` folder and run the `comp.R` file either in RStudio or by running `R -f comp.R`. The file might need to be altered point to the last `base-reported` model produced by you.

# Citation

Please cite our paper if you use any of the models or results of our authorship in this repository:

(To be defined)

If you use the `base` and `base-nature` stan files, please cite the original papers:

Flaxman, S. et al. Estimating the number of infections and the impact of non-pharmaceutical interventions on COVID-19 in European countries: technical description update. arXiv 1–6 (2020). 2004.11342.

Flaxman, S. et al. Estimating the effects of non-pharmaceutical interventions on COVID-19 in Europe. Nature 584, 257–261, DOI: 10.1038/s41586-020-2405-7 (2020).

If you use any other code which is not of our authorship, but is somehow present in this repository, please contact the original author for instructions.
