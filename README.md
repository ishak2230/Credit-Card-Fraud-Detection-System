# Credit Card Fraud Detection using SVM

This project detects fraudulent credit card transactions using Support Vector Machines (SVM) in R. The dataset is highly imbalanced, with only 0.17% fraud cases. The model focuses on identifying subtle fraud patterns, such as one-dollar scams, while minimizing false positives.

## Overview

- Performed exploratory data analysis on transaction behavior
- Handled class imbalance using KMeans clustering on non-fraud data
- Trained an SVM model using RBF kernel with `ksvm` from `kernlab`
- Evaluated using accuracy, recall, specificity, F1 score, and AUC

## Final Results

- Accuracy: **97.79%**
- Recall (Sensitivity): **96.94%**
- AUC: **~0.975**
- Successfully detected high-risk low-amount frauds
