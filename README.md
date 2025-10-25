# Economic-Freedom-Map-Algorithms
[![R](https://img.shields.io/badge/R-4.4-blue)]() [![License](https://img.shields.io/badge/license-MIT-green)]() [![Fraser-Institute](https://img.shields.io/badge/Institute-Fraser-orange)]()

## Author
**Henry Risley** — (Student Contributor, Fraser Institute)

---

### Purpose
A reproducible, auditable R-based algorithm that recalculates the **Economic Freedom Score (EFS)** directly from the *EFW Panel Dataset* and predicts future Economic Freedom Scores using the Markov Chain Monte Carlo, Bayersian Inference, and a seperate Economic Freedom Score prediction equation.  
By normalizing, weighting, and change-tracking every country-year, the algorithm powers a **modernized interactive map** suitable for future Fraser Institute publications and academic citation.

---

### Core Formulas
\[
EFS_{c,t} = \frac{1}{5}\sum_{i=1}^{5} A_{i,c,t}
\quad , \quad
EFS_{c,t} = \sum_{i=1}^{5} w_i A_{i,c,t} \ ,\  \sum w_i = 1
\]
\[
\Delta EFS_{c,t} = EFS_{c,t} - EFS_{c,t-1}
\]
\text{EFS}{c,t} \sim \mathcal{N}(\alpha_c + \phi\,\text{EFS}{c,t-1} + \beta^\top X_{c,t}, \sigma)


Bootstrapped CIs and PCA weighting are implemented in `3_bootstrap_ci.R`.

---

### Quickstart
```bash
git clone https://github.com/henryrisley09-creator22/Economic-Freedom-Map-Algorithms
cd efw-panel-analysis-R
Rscript run_pipeline.R
