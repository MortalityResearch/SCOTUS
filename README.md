# SCOTUS Readme file
This repo contains the input data (SCOTUS.csv and smr.txt) and the programs needed for reproducing the analysis of Supreme Court Justice mortality, as published in:  
> *Reynolds RJ, Kush SJ, Day SM, Vachon P. Comparative mortality and risk factors for death among Justices of the Supreme Court of the United States, 1789-2013: experience from an occupational cohort with over two centuries of follow-up.  Journal of Insurance Medicine. 2015;45:9–16.*  

In order to use these files correctly, run the program SCOTUS_cohort.R first, followed by SCOTUS_lifeintervals.R. After that the SCOTUS_models.R program will have the data it needs to fit models. Be sure to change the location in the cohort program to find the data source (SCOTUS.csv) in your working directory.  

The program SCOTUS_SMR_plot.R reads the smr.txt file and then creates a chart of the SMRs by decade, with confidence bands, as was presented in the original paper. Again, make sure to change the directory information to point to the location of smr.txt in your working directory. The second set of commands towards the bottom of the page creates an updated figure with an extra SMR in red.  

The data in *smr.txt* was created separately through a tedious process. Should you want the tools and instructions to do these computations, please contact the authors.
