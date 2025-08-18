PEPP_traj
===============================================

trajectories of depressive and negative symptoms in PEPP

Created by Alyssa Qian alyssa.qian@camh.ca

Contact People:
	Principle Investigator: Aristotle Voineskos
	Primary: Alyssa Qian Alyssa.Qian@camh.ca
     Please contact alyssa.qian@mail.utoronto.ca if the email above does not work
	Others:
         Sierra Vaillancourt Sierra.Vaillancourt@camh.ca
         Lindsay Oliver Lindsay.Oliver@camh.ca
         Julia Gallucci Julia.Gallucci@camh.ca

Date Processed: 06-02-2025 to 08-15-2025

Quality Control:
	Primary: 
 		Alyssa Qian Alyssa.Qian@camh.ca 
		Sierra Vaillancourt Sierra.Vaillancourt@camh.ca
QC directory: ./reports/others/quality_control


Project Organization
-----------------------------------

    .
    ├── README.md          <- The top-level README
    ├── .gitignore         <- Files to not upload to github - by default includes /data
    ├── LICENSE            <- usage license if applicable
    ├── data
    │   ├── processed      <- The final dataset (can include subfolders etc)
            ├── 
            ├── 
    │   └── raw            <- The original dataset, contains the raw PEPP data
    │
    ├── notebooks          <- Empty
    │
    ├── docs/references    <- Data dictionaries, abstract and poster for 2025 SURP
    │
    ├── reports            <- Generated analysis
	│   ├── GMM2_18mo_outputs      <- Created on July 10th. GMM2 models for up to 18month data
        ├── GMM2_18mo_outputs      <- Used between June 23rd to July 9th. GMM2 models for up to 24month data
        ├── GMM_class_membership   <- Contains information class memberships for each trajectory of SANS, LV1, and CDSS
		├── demographics_explore   <- Organized and explored age, gender, and other demographics between trajectories
 		├── figures 			   <- All visualization of corresponding statistics
			├── PEPP_distributions 		<- Histograms of every quantitative variable in the PEPP dataset
   			├── age_comparison 		    <- Boxplot of age distribution of each trajectory
	     	├── age_distribution 		<- Histogram of age of each trajectory
	  		├── chord_diagrams 			<- Chord diagrams of class membership overlaps among 24mo trajectory classes
			├── correlation_matrix 		<- Correlation matrix of clinical variables in PEPP dataset
   			├── histograms 				<- Histogram of clinical measurements at all timepoints
	  		├── long_data_histograms	<- Histogram of trajectories at all timepoints
	 	├── others 			   		<- Contains meeting minutes, date backup, and exploration of SD cutoffs
    │   └── quality_control         <- Quality control records for smriprep and freesurfer
    │
    ├── requirements.txt   <- The requirements file for reproducing the analysis environment (if applicable)
    │
    ├── code           <- RStudio code for use in this project
		├── GMM2 			   			<- Growth Mixture Model, divided into 18 months data and different variables for 24 months
 		├── demographics_exploration   	<- Explored how age, gender, and other demographics differ in 18mo trajectories
   		├── logistic_regression   		<- Linear regression and multinomial logistic regression
	 	├── model_selection   			<- Fit statistics, including average posterior probabilities, boostrap likelihood ratio, scaled entropy, etc.
   		├── tstats   					<- T-tests compairing cortical thickness and surface area of brain regions

