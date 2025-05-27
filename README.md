# 📊 TSSN Behavioral Intervention Analysis (R Scripts)

This repository contains R scripts used for data analysis in the study:

**"Effective Nudging with Heterogeneous Interventions: Evidence from Medical Debt Collection"**

We propose a Three-Stage Smart Nudging (TSSN) framework and apply causal forest models to explore treatment effect heterogeneity and to identify subgroups that respond differently to behavioral interventions.

---

## 📁 File Descriptions

| File                     | Description                                                                 |
|--------------------------|-----------------------------------------------------------------------------|
| `cate_analysis.R`        | Estimates and visualizes the Conditional Average Treatment Effects (CATEs) distribution across experimental groups using causal forest. |
| `heterogeneity_age.R`    | Estimates treatment effect heterogeneity by **age** subgroup.               |
| `heterogeneity_gender.R` | Estimates treatment effect heterogeneity by **gender** subgroup.            |
| `heterogeneity_amount.R` | Estimates treatment effect heterogeneity by **amount owed** (debt size).    |
| `negative_effect_group.R`| Identifies individuals who experienced **negative treatment effects** from the intervention. |

---

## 🔧 Dependencies

These scripts are written in **R**, and rely on the following packages:

- [`grf`](https://cran.r-project.org/web/packages/grf/index.html)
- `ggplot2`
- `dplyr`

You can install them using:

```r
install.packages("grf")
install.packages("ggplot2")
install.packages("dplyr")
```

---

## 📌 Notes

- These scripts assume the dataset has already been cleaned and preprocessed.
- Due to privacy and institutional restrictions, the original patient-level data are **not publicly available**.
- Anonymized data may be shared upon reasonable request and subject to approval by the partner institution.
- The TSSN framework and analytical approach can be adapted to other domains such as tax compliance, public health adherence, and environmental behavior.

---

## 📫 Contact

For questions or data access requests, please contact:

**Ji Wu**  
Email: *[wuji3@mail.sysu.edu.cn]*  

**Wenjing Zhu**  
Email: *[zhuwj58@mail2.sysu.edu.cn]*  
