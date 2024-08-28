# Density Estimation in Kernel Exponential Families: Methods and Their Sensitivities

This repository contains my dissertation for the Ph.D. degress in Statistics at the Ohio State University. 

The title of my dissertation is **Density Estimation in Kernel Exponential Families: Methods and Their Sensitivities**. My main contributions are the following: 

1. We propose a new density estimator called the early stopping SM density estimator, which is obtained by applying the gradient descent algorithm to minimizing the SM loss functional and terminating the algorithm early to regularize. We investigate various statistical properties of this density estimator and compare this early stopping score matching density estimator with the penalized score matching density estimator that has been studied in the literature and address their similarities and differences.

2. We propose an algorithm to compute the penalized maximum likelihood density estimator that is obtained by minimizing the penalized negative log-likelihood loss functional. We empirically compare the penalized and early stopping score matching density estimators with the penalized maximum likelihood density estimator and highlighted their similarities and differences. 

3. The differences in the score matching and maximum likelihood density estimators motivate us to study the sensitivities of different density estimators to the presence of an isolated observation. We extend the definition of the influence function by allowing its input to be function-valued statistical functionals. We study various properties of this extended influence functions of maximum likelihood and score matching density projections in finite-dimensional and kernel exponential families, and empirically demonstrate that regularized score matching density estimators in a kernel exponential family are more sensitive to the presence of an additional observation than the penalized ML density estimator when the amount of regularization is small. 


![Plot](plots/waiting-ML-vs-SM-density-estimates-baseden=Gamma-26-3-gridpoint=1-181-1.pdf)
