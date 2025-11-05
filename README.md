# Breast Cancer Recurrence Risk Assessment

## Project Overview

This project develops an alternative, **data-driven model** to assess recurrence risk for treated breast cancer patients. It addresses limitations of current genomic tests, such as restricted eligibility and focus, by leveraging a broader set of **clinical and molecular factors**. The primary goal was to create an interactive, accessible tool for patient education and clinical decision support.

The model was successfully created via an **R Shiny web application** to visualize patient-specific recurrence probabilities.

## Methodology & Statistical Approach

### Data and Outcome Definition
* **Dataset:** Original clinical features dataset included 922 patients with 98 variables. The final subset used **818 patients** who underwent surgery, encompassing treatments (chemotherapy, radiation, endocrine), **molecular subtype**, and tumor grades (T, M, N).
* **Outcome:** Recurrence was defined as a **binary outcome** within a **3-year follow-up** window.

### Modeling
* **Algorithm:** The **eXtreme Gradient Boosting (XGBoost)** classifier was employed. XGBoost was chosen for its strong performance in prediction tasks, handling complex interactions, and providing robust feature importance scores essential for clinical interpretability.
* **Clinical Goal:** The model explores how to **more effectively assess recurrence risk** using a combination of clinical and molecular data for personalized treatment planning, addressing limitations like the cost and narrow scope of the Oncotype Score.

## R Shiny Application & Clinical Utility

An interactive **R Shiny application** was developed to bridge the gap between complex model predictions and practical clinical use.

### Features:
* **Visualization:** Allows clinicians and patients to input a patient's characteristics and instantly view the predicted **recurrence probability**.
* **Accessibility:** Designed as a simple, accessible tool to address genomic test limitations (restricted eligibility, cost, chemotherapy focus).

## Limitations and Future Biostatistical Work

The project identified several limitations that offer clear directions for future research, highly relevant to Biostatistics:

| Limitation (Current Status) | Future Work (Biostatistics Focus) |
| :--- | :--- |
| **Binary Outcome:** Model only predicts "yes/no" recurrence, missing **time-to-event** information. | **Time-to-Event Prediction:** Adapt the model to predict **specific time-to-recurrence probabilities** using Survival Analysis methods (e.g., Cox Proportional Hazards or Survival Forests). |
| **Limited Follow-Up:** Predictions are limited to a **3-year timeline** (69.4% of patients followed up). | **Data Expansion:** Obtain longer-term follow-up data to capture recurrence events past 3 years, improving model generalizability. |
| **Missing Predictors:** Model is limited to clinical/molecular data in the original set. | **Enrichment:** Incorporate other valuable predictors such as **sociodemographic** and **comorbidity** data to improve clinical relevance and accuracy. |

## Technologies Used

* **Statistical Software:** R
* **Machine Learning:** XGBoost (via the `xgboost` library)
* **Web Deployment:** R Shiny
* **Data Analysis:** [Insert other libraries used, e.g., `tidyverse`, `survival`]
