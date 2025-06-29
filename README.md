# Business vs. Climate Priority Prediction

**Author:** Cora Fagan  

---

## Overview

This project builds a predictive model to infer whether UK survey respondents prioritize **economic growth** or **climate protection**, based on political and demographic features. The analysis uses the **British Election Study (BES) 2017 Wave**, applying machine learning techniques to evaluate how well environmental preferences can be predicted from existing survey data.

---

## Data

- **Dataset:** British Election Study (BES) 2017 face-to-face survey
- **Target Variable:** Attitude toward government priority:
  - *“Government should prioritize economic growth, even if the environment suffers”*  
  - Responses on a 0–10 scale grouped into:
    - **Growth Priority (0–3)**
    - **Neutral (4–6)**
    - **Environment Priority (7–10)**
- **Preprocessing:**
  - Responses grouped to reduce noise
  - *"Don't Know"* responses excluded due to model instability
  - Categorical variables encoded; missing data imputed or preserved using SMOTE/missingness categories

---

## Modeling Approach

- **Model:** XGBoost multi-class classifier
- **Balancing Strategy:** SMOTE (Synthetic Minority Oversampling Technique)
- **Evaluation:**
  - Metric: **F1 Score (macro-averaged)**
  - Baseline: No Information Rate
  - Error concentrated in the **Neutral vs. Environment** boundary

---

## Feature Insights

- **Top predictors:**
  - Green Party support
  - Views on income equality
  - Age and education
- **SHAP-style plots** reveal directional importance, but overlap between segments limits precise inference

---

## Results Summary

- Despite optimization, the model performs only marginally better than random guessing
- Attitudes toward climate vs. business **cannot be reliably inferred** from available covariates

---

## Files
- business-vs-climate.R # the code where the XGBoost model was built
- business-vs-climate.Rmd # the Markdown file with slides displaying the processes and results
- business-vs-climate.html # the knitted .html file
- other plots and figures

