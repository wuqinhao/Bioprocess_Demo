# Bioprocess_Demo
Smart Manufacturing for Bioprocess Mode Decision
Project member: Alex Mey, Qinhao Wu, Linmei Shang, Hua Ke
The demo can also be found in: https://apriko.shinyapps.io/Demo_bioprocess/

1. Background
According to Merck's requirements, the problem happens when a lab-developed bioprocess can be scaled to the manufacturing condition. There are several methods to tackle the scalability problem and support the decision-making between batch, fed-batch, or continuous mode operations. However, with the smart manufacturing trend, more and more data is being collected during manufacturing. Also, more manufacturing modes have been introduced to accelerate the manufacturing process. It becomes tricky to model the bioprocess following the traditional manner. Therefore, we proposed a data-driven way to tackle these problems and cooperate flexibly with the traditional models. This framework can also illustrate the Key Performance Indicators (KPIs) and compare manufacturing modes.

2. Method
The general idea is to set up a data-driven machine-learning framework based on the historical records of bioprocess for each mode. This framework is explainable, scalable and well-performed. To construct the framework based on the historical records, we assume that the predictive and historical events depend on each other. Therefore, we propose to use k Nearest Neighbour (k-NN) to construct the framework. Based on the framework, the KPIs are given.
	2.1. k-NN regression framework
	We propose using the k-NN regression to organise the training data containing process parameters, e.g., basal (kg), bioreactor volume (L), etc. The historical data can be used as the training data. At the same time, the process mode categorises the historical data so that the k-NN regression can offer the nearest neighbours in each processing mode. Therefore, the comparison between different modes can be achieved. 
Compared to other data-driven models, the k-NN regression only requires one hyperparameter, the neighbour number, k. Moreover, this k-NN framework can be further extended and enhanced by kernelisation, distance measurement, etc. The initial guess of the hyperparameter k is k=(sample size)^(1/2), while with sufficient data, k can be further validated with methods such as cross-validation.

	2.2. k-NN framework cooperation with the traditional models
	We use logistic regression to model the upstream growth to demonstrate the flexibility of the proposed k-NN framework. To simplify the description of cell/mAb growth, it can be separated into two stages: exponential and stationary. The logistic regression can be used to present these two stages. As we separated the historical data into each process mode, the historical data can be used to estimate the parameters of the selected model. Therefore, the fitted logistic model can predict the expected mAb outcome for each mode. Specifically, we use a linear model to predict the upstream cost for each mode. We assume the cost only contains average facility, average basal, and labour costs. 

For the downstream modelling, we use the support vector machine (SVM) regression to fit the downstream data. For this model, we assume that the product outcome will be based on the mAb output from the upstream. Therefore, by fitting the SVM regression model to our simulated data, we can offer the prediction of the downstream process.

Once we get the upstream and downstream prediction outcomes, we can apply these predictions as the input to the k-NN model. Therefore, we can get the most similar cases from the historical records of each mode. For further KPI calculations, the prediction and its most similar cases are used to calculate the KPIs, such as cost per mAb, mAb per day (throughput), total carbon dioxide (CO2), etc. Moreover, the details of the history profile are given along with the KPIs.

	2.3. KPIs calculation
	The details of KPIs calculation are given as follows:
	- Cost per mAb (dollar/g) := (total cost)/(predicted upstream mAb)
	- mAb per day (g/day, throughput) := (predicted upstream mAb)/(upstream process days)
	- Total CO2 := (facility footprint) + (predicted upstream mAb)*(constant CO2 per mAb, 14.15)
From the given information, we found that it focuses on the upstream process. Therefore, we heavily rely on our upstream prediction in our summarisation and the KPI calculation. With further input of information and expert knowledge on the downstream process, we can also cooperate with the downstream process in our k-NN framework.

3. Implementation
We use R and R Shiny to implement the method and demonstrate the demo. The requirement for R packages is listed below:
| Package  	| Version 	|
| ------------- | ------------- |
| R		|4.0.5		|
|shiny		|1.7.4		|
|shinydashboard	|0.7.2		|
|shinyWidgets	|0.7.6		|
|shinyjs 	|2.1.0		|
|dplyr		|1.1.2		|
|ggplot2	|3.4.2		|
|FNN		|1.1.3.2	|
|scales		|1.2.1		|
|gridExtra	|2.3		|
|grid		|4.0.5		|
|reactable	|0.4.4		|
|e1071		|1.7-13		|
