# Statistical Testing using R for Continuous Variable for more than two groups.

This R program/script can be a standalone tool for this specific terms.
It provides an interactive workflow to perform **hypothesis testing** and **post hoc analyses** on 
continuous variables across more than **two independent groups**.  
It guides the user through data loading, variable setup, assumptions checking,
and statistical test selection.

---

## Features
- Load Excel datasets (.xlsx)
- Define groups and continuous variable
- Labeling of groups
- Generate violin + boxplots for normality checks
- Perform assumption tests:
  - Shapiro–Wilk (normality)
  - Levene’s test (homoscedasticity)
- Run appropriate tests based on assumptions:
  - One-way ANOVA (+ Tukey & Bonferroni post hoc)
  - Welch’s ANOVA (+ Games–Howell)
  - Kruskal–Wallis (+ Dunn & Wilcoxon post hoc)
- Output summary statistics and decisions

---

## Requirements
The script will install/load required libraries automatically:  
`readxl`, `ggplot2`, `tidyverse`, `rstatix`, `dlookr`, `dplyr`, `ggpol`, `rstudioapi`, `ggpubr`

---

## Usage
1. Save your Excel dataset in the same folder as the script.  
2. Run the script in RStudio and execute the script:

```r
main()
```
3. Follow the prompts in the terminal to:
 - Enter dataset name
 - Locate/Select the groups & the continuous variable
 - Give group labels and input the plot title

4. Review summary statistics, plots, and test results. (Do your own tests as well!)

