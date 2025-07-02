This software estimates the concurrency between harmful and fragile conditions within an ecosystem. It can be applied to every type of ecosystem, although it was originally conceived for marine ecosystems.

The present repository contains examples to execute different models across different years, which generate annual risk assessment based on annually-aggregated data.

A data input example is given in [dataset_mediterranean_sea_2017_2018_2019_2020_2021.csv](https://github.com/cybprojects65/UnsupervisedMarineRiskAssessment/blob/main/dataset_mediterranean_sea_2017_2018_2019_2020_2021.csv)

This file was standardised (mean=0 and sd=1) for each column to produce an input dataset for the models, visible in [Dataset_Risk_Assessment_Mediterranean_Sea.csv](https://github.com/cybprojects65/UnsupervisedMarineRiskAssessment/blob/main/Dataset_Risk_Assessment_Mediterranean_Sea.csv)

The supported models and related examples are:

 - Multi K-means using the UNIF function to select the optimal K:  [Workflow_MultiKmeans_2017.R](https://github.com/cybprojects65/UnsupervisedMarineRiskAssessment/blob/main/Workflow_MultiKmeans_2017.R "Workflow_MultiKmeans_2017.R")
 - Fuzzy  C-means: [Workflow_Fuzzy_C_means_2017.R](https://github.com/cybprojects65/UnsupervisedMarineRiskAssessment/blob/main/Workflow_Fuzzy_C_means_2017.R "Workflow_Fuzzy_C_means_2017.R")
 - X-means:  [Workflow_X_means_2017_2021.R](https://github.com/cybprojects65/UnsupervisedMarineRiskAssessment/blob/main/Workflow_X_means_2017_2021.R "Workflow_X_means_2017_2021.R")
 - DBSCAN: [Workflow_DBSCAN_2017.R](https://github.com/cybprojects65/UnsupervisedMarineRiskAssessment/blob/main/Workflow_DBSCAN_means_2017.R)
 - Artificial Neural Networks (trained on the Multi K-means output): [Workflow_ANN_2017.R](https://github.com/cybprojects65/UnsupervisedMarineRiskAssessment/blob/main/Workflow_ANN_2017.R)
 - Variational Autoencoder: [Workflow_VAE_2017.R](https://github.com/cybprojects65/UnsupervisedMarineRiskAssessment/blob/main/Workflow_VAE_2017.R "Workflow_VAE_2017.R")

The corresponding outputs are reported in the Cluster_ALGORITHM_YEAR.csv files.
