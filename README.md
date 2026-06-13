# Choice Probabilities Review: Analysis Replication

This repository contains the core statistical analysis scripts and data underlying our manuscript submitted to *eLife*.

---

## Repository Structure

* **`Data for CP meta-analysis - data.csv`**: The compiled, curated dataset tracking choice probability ($CP$) metrics and parameters from the literature.
* **`scripts/main.R`**: The primary executable pipeline that runs the core meta-analyses and generates the manuscript results.
* **`scripts/functions.R`**: Utility functions, model helpers, and statistical subroutines imported during execution.

*Note: Any other intermediate processing scripts used during the drafting process served strictly to pre-calculate or format specific individual cells within the final CSV matrix and are not required for replication.*

---

## How to Reproduce the Analysis

To replicate the statistical models and findings presented in the paper, execute the main pipeline file using R:

```bash
Rscript scripts/main.R
