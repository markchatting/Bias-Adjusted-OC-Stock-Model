# **Bias-Adjusted Predictors and Random Forest Models for Organic Carbon Stock Estimation**  

This repository contains scripts for processing, modeling, and bias-adjusting environmental predictors to estimate organic carbon (OC) stock in the seafloor sediments of the Irish Sea. The workflow involves preprocessing point data, training Random Forest (RF) models, and applying bias correction to improve prediction accuracy.  

### **Workflow Overview:**  
1. **Organic Carbon Stock Estimation (`oc_stock_model.R`)**  
   - Preprocesses OC point data and predictor variables.  
   - Stacks predictor layers for spatial modeling.  
   - Trains an RF model to predict OC content.  
   - Uses a dry bulk density (DBD) model to calculate OC stock.  

2. **Dry Bulk Density (DBD) Modeling (`dbd_model.R`)**  
   - Trains an RF model to spatially predict DBD.  
   - Uses environmental variables to refine DBD estimates.  
   - Outputs a high-resolution DBD dataset for OC stock calculations.  

3. **Bias Adjustment of Predictors (`bias_adjustment.R`)**  
   - Corrects biases in global environmental datasets using in situ measurements.  
   - Applies **quantile-quantile (QQ) mapping** for more accurate regional predictors.  
   - Produces bias-adjusted predictor datasets for the OC content model.  


### **Usage:**  
Run the scripts to generate bias-adjusted predictors, train models, and estimate OC stock.  

### **Citation:**  
If you use this repository, please cite:  
Chatting, Mark, "Bias-Adjusted Predictors and Random Forest Models for Organic Carbon Stock Estimation" 2025.  

