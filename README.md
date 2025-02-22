# Regression-model-for-physician-with-predictor-factors
Use the regression model to analyze the number of active physicians in a given area, based on our choice of predictor variables from the dataset


---
author: "Afnan Dzaharudin, Linfeng Wang"
date: "2022-10-10"
output:
  pdf_document:
    toc: yes
  html_document:
    toc: yes
    toc_float: yes
---

# Setup & Functions

We import the libraries used for this case study. `readr` is simply to import the dataset as a tibble, `tidyr` and `dplyr` is for ease of dataset manipulation and `ggplot2` is for plotting graphs. `lmtest` is for diagnostics.

```{r init, echo = FALSE}
  knitr::opts_chunk$set(echo = FALSE, message = FALSE)
```


```{r setup, message = FALSE}
  library(readr)
  library(MASS)  
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  library(lmtest)
  library(faraway)
  library(gridExtra)
  library(grid)
  library(gridtext)

  # Returns a heatmap of all columns in the input dataset
  plot_heatmap = function(dat) {
    cors = dat %>%
    cor() %>%
    round(2)

    tnames = dat %>%
        names()
    
    pivoted_corrtable = cors %>%
      round(2) %>%
      as_tibble() %>%
      cbind(tnames) %>%
      select(tnames, everything()) %>%
      pivot_longer(!tnames)
    
    ggplot(pivoted_corrtable, aes(x = tnames, y = name, fill = value)) +
      geom_tile() + 
      geom_text(aes(label = value), color = "black", size = 3) +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      scale_fill_gradient2(
        low = "blue",
        high = "orange",
        mid = "white", 
        midpoint = 0,
        limit = c(-1,1),
        space = "Lab", 
        name="Correlation\nValue"
      ) +
      labs(
        x = "Variable",
        y = "Variable",
        title = "Correlation heatmap of predictor variables"
      )
  }
  
  # Plots distributions of variables in the input dataset
  plot_distributions = function(dat) {
    
    n_row = (dim(dat)[2] %% 3) + 1
    
    par(mfrow=c(n_row, 3))
    for (n in names(dat)) {
      hist(
        dat %>%
          pull(n),
        main = paste("Distribution of ", n),
        xlab = "Value",
        ylab = "Frequency"
      )
    }
  }
  
  # Gives residual and diagnostic plots
  all_summary = function(model) {
    
    # My favorite color, hehe
    vuc_blue = "#478EAC"
    
    # Parameters
    n = 440 # Sample size
    p = length(model$coefficients)
    
    resid_plot = ggplot() +
      geom_point(aes(x = model$fitted.values, y = model$residuals)) +
      geom_hline(aes(yintercept = mean(model$residuals)), color = vuc_blue, linetype = "dashed") +
      labs(
        title = "Distribution of Residuals",
        y = "Residual value",
        x = "Fitted value"
      )
    
    residqq_plot = ggplot(mapping = aes(sample = model$residuals)) +
      geom_qq() +
      geom_qq_line(color = vuc_blue) +
      labs(
        title = "QQ-Plot of Model Residuals"
      )
    
    residhist_plot = ggplot() +
      geom_histogram(aes(x = model$residuals), color = vuc_blue) +
      labs(
        title = "Histogram of Model Residuals",
        y = "Frequency",
        x = "Value"
      )
    
    # Basic model summary
    cat("MODEL SUMMARY:")
    print(summary(model))
    
    # KS Test for Normality
    cat("TEST FOR NORMALITY:")
    print(ks.test(model$residuals, "pnorm"))
    
    # BP Test for Constant Variance
    cat("TEST FOR CONSTANT VARIANCE:")
    print(bptest(model))
    
    # High influential points
    cooks = cooks.distance(model)
    cooks_plot = ggplot() +
      geom_histogram(aes(x = cooks, color = (cooks <=1)), show.legend = FALSE) +
      geom_vline(aes(xintercept = 1.0), color = "red") +
      scale_color_brewer(direction = -1) +
      labs(
        titles = "Plot of Cook's Distances",
        subtitle = "points past line are high influential",
        y = "Frequency",
        x = "Cook's Distance Values"
      )
  
    # Leverages
    leverages = lm.influence(model)$hat
    
    leverage_plot = ggplot() + 
        geom_dotplot(aes(x = lm.influence(model)$hat, color = lm.influence(model)$hat <= 2*p/n), show.legend = FALSE) +
        geom_vline(aes(xintercept = 2*p/n), color = "red") +
        labs(
          titles = "Plot of Leverage Points",
          subtitle = "points past line are high leverage",
          x = "Leverage Values"
        )
    
    # Outliers
    resid_studentized = rstudent(model); 
    
    bonferroni_cv = qt(.05/(2*n), n-p-1) 
    
    outlier_plot = ggplot() +
      geom_histogram(aes(x = resid_studentized, color = (abs(resid_studentized) <= abs(bonferroni_cv))), show.legend = FALSE) +
      geom_vline(xintercept = c(-1, 1) * bonferroni_cv, color = "red") +
      scale_color_brewer(direction = -1) +
      labs(
        title = "Distribution of Studentized Residuals",
        subtitle = "points past line are outliers"
      )
    
    grid.arrange(resid_plot, cooks_plot, residqq_plot, leverage_plot, residhist_plot, outlier_plot, ncol = 2, nrow = 3, top = textbox_grob(paste("Diagnostic Plots for: ", paste(model$call)[2])))
  }
  
  
```

```{r import_data, message = FALSE}
  data_physicians = read_table("CDI.txt", col_names = FALSE)
  
  colnames(data_physicians) = c(
    "id", "county", "state",
    "land_area", "pop_total",
    "pct_18_24", "pct_65_older",
    "n_physicians", "beds", "crime",
    "pct_highschool", "pct_bachelor",
    "pct_below_poverty", "pct_unemployed",
    "per_capita_income", "total_personal_income",
    "region"
  )
  
  data_physicians = data_physicians %>%
    mutate(pct_beds = beds / pop_total) %>%
    mutate(pct_crime = crime / pop_total) %>%
    mutate(pop_density = pop_total / land_area) %>%
    mutate(log_pop_density = log(pop_density))

```

```{r dataset_variations, message = FALSE}
  # Used in Introduction to show heatmap of predictors
  data_predictors_all_initial = data_physicians %>%
    select(pop_total, land_area, pct_18_24, pct_65_older, beds, crime, pct_highschool, pct_bachelor, pct_below_poverty, pct_unemployed, per_capita_income, total_personal_income, region)

  # Used in Introduction to show these points are highly skewed
  data_predictors_problematic = data_physicians %>%
    select(pop_total, beds, crime, land_area)
  
  # Used to show points are no longer skewed except pop density (part 1)
  data_predictors_fixed_1 = data_physicians %>%
    select(pct_beds, pct_crime, pop_density)
  
  
  # Used to show points are no longer skewed (part 2)
  data_predictors_fixed_2 = data_physicians %>%
    select(pct_beds, pct_crime, log_pop_density)
  
  # Used in Model Selection for the beginning model
  data_cleaned_all = data_physicians %>%
    select(per_capita_income, log_pop_density, pct_beds, pct_crime, region, pct_18_24, pct_65_older, pct_highschool, pct_bachelor, pct_below_poverty, pct_unemployed)
  
  # Used in the reduced model right after the Permutation Test
  data_cleaned_reduced_1 = data_physicians %>%
    select(pct_below_poverty, per_capita_income, log_pop_density, region)
```

```{r model_variations}
  # First model that fits all cleaned predictors
  model_full = data_cleaned_all %>%
    bind_cols(n_physicians = data_physicians$n_physicians) %>%
    lm(formula = "n_physicians ~ per_capita_income + log_pop_density + pct_beds + pct_crime + region + pct_18_24 + pct_65_older + pct_highschool + pct_bachelor + pct_below_poverty + pct_unemployed")
  
  # Second model that fits the null model after the Permutation Test
  model_partial = data_cleaned_reduced_1 %>%
    bind_cols(n_physicians = data_physicians$n_physicians) %>%
    lm(formula = "n_physicians ~ pct_below_poverty + per_capita_income + log_pop_density + region")
  

  # Third model that boxcox-transforms the Y by log(y)
  model_final = data_cleaned_reduced_1 %>%
    bind_cols(n_physicians = data_physicians$n_physicians) %>%
    lm(formula = "log(n_physicians) ~ 0 + pct_below_poverty + per_capita_income + log_pop_density + region")
```



\newpage

# Introduction

We have removed some columns from the dataset in the beginning, because we will not use them for our model selection. The columns in question and our reasoning for their removal are:

- `id`, because it is an indexing value and has no meaning for regression.
- `county`, `state`, and `region`, because they are _categorical variables_, which we have not learned to handle in class. Additionally, all three of these variables are somewhat the same,  as they are all location indicators of varying coverage.

We will also place `n_physicians`, our to-be response variable, in the front of the dataset for readability.

```{r shrink_dataset}
  # Remove the unused predictor variables; seat n_physicians in the front
  data_physicians = data_physicians %>%
    select(n_physicians, everything(), -id, -county, -state)
  
  # Show the dataset
  head(data_physicians)
```

# Variable Transformations



# Model Selection

## Full Model

### Analysis of the Full Model

At this stage we do not yet know much about _any_ of our predictor variables, and which ones are significant for our regression model and which ones are not. Therefore, we will begin with a full model that uses all predictor variables, and work our way down.

We create this model with all variables, named `model_allvars` and print the summary for it.

```{r fullmodel_print}
  summary(model_full)
```

Here we notice: not many predictors pass a **single-predictor test** on
$$H_0: \hat\beta_i = 0, \space H_a:\hat\beta_i\ne 0$$
at alpha = 0.05. Only `per_capita_income`, `log(pop_density)`, `region`, `pct_18_24` and `pct_below_poverty` pass these as they have a `Pr(>|t|)` value less than `0.05`. The **overall F-test** _does_ pass at alpha = 0.001, meaning $H_a$: at least one predictor is nonzero.

Overall, this tells us that it is likely that _many_ of the coefficients are not statistically significant in our regression model, but at least _some_ are. Our next step is to work towards the model of best fit by reducing this model.


### Reduction by Permutation Test

Because the current model's residuals are likely not normally distributed, We perform a **Permutation Test** in place of the partial F-test with a chosen full model and null model.

If we fail to reject the Null hypothesis, we can conclude that the null model (which is a smaller subset of the full model) is sufficient.

**Our chosen variables for the null model is based on the previous section. We will keep the five mentioned coefficients that were statistically significant according to a single predictor test and test the remainder**

More specifically, now, our test will be, at alpha = 0.001:

$$ \begin{aligned}
H_0&: \hat\beta_\text{land\_area} = \hat\beta_\text{pct\_18\_24} = \hat\beta_\text{pct\_65\_older} = \hat\beta_\text{crime} = \hat\beta_\text{pct\_highschool}
\\& = \hat\beta_\text{pct\_below\_poverty} = \hat\beta_\text{pct\_unemployed} = \hat\beta_\text{per\_capita\_income} = 0 \\
H_a&: \text{At least one of these } \hat\beta \text{ are nonzero}
\end{aligned} $$

Which leads to the two models:

$$ \begin{aligned}
H_0: \hat y_0 &= \hat\beta_\text{pop\_total}x_\text{pop\_total} + \hat\beta_\text{beds}x_\text{beds} + \hat\beta_\text{pct\_bachelor}x_\text{pct\_bachelor} \\& + \hat\beta_\text{per\_capita\_income}x_\text{per\_capita\_income} + \hat\beta_\text{total\_personal\_income}x_\text{total\_personal\_income} \\
H_a: \hat y_1 &= \hat y_0 +  \hat\beta_\text{land\_area}x_\text{land\_area} + \hat\beta_\text{pct\_18\_24}x_\text{pct\_18\_24} + \hat\beta_\text{pct\_65\_older}x_\text{pct\_65\_older} + \hat\beta_\text{crime}x_\text{crime} \\& + \hat\beta_\text{pct\_highschool}x_\text{pct\_highschool} + \hat\beta_\text{pct\_below\_poverty}x_\text{pct\_below\_poverty} + \hat\beta_\text{pct\_unemployed}x_\text{pct\_unemployed}
\end{aligned} $$

```{r fullmodel_partial_f}
  null_model = lm("n_physicians ~ pop_total + beds + pct_bachelor + per_capita_income + total_personal_income", data_physicians)
  full_model = lm("n_physicians ~ . ", data_physicians)

  anova(null_model, full_model)
```

Calculating for the partial $F$-test gives us a statistic of $F = 1.6796$. However, the critical value for this test is $F_{0.95}(7, 427) \approx 2.031$. $F \ngtr F_{0.95}(7, 427) \approx 2.031$ therefore **we fail to reject the null hypothesis**. This means the null model is sufficient, and we can move forward using the following reduced model:

$$ \begin{aligned}
\hat y &= \hat\beta_\text{pop\_total}x_\text{pop\_total} + \hat\beta_\text{beds}x_\text{beds} + \hat\beta_\text{pct\_bachelor}x_\text{pct\_bachelor} \\& + \hat\beta_\text{per\_capita\_income}x_\text{per\_capita\_income} + \hat\beta_\text{total\_personal\_income}x_\text{total\_personal\_income}
\end{aligned} $$

## First Reduced Model

```{r reduced_creation}
  model_reduced_1 = lm("n_physicians ~ pop_total + beds + pct_bachelor + per_capita_income + total_personal_income", data_physicians)
```


We now have a smaller multiple linear regression model for the number of active physicians (`n_physicians`):

$$ \begin{aligned}
\hat y &= \hat\beta_\text{pop\_total}x_\text{pop\_total} + \hat\beta_\text{beds}x_\text{beds} + \hat\beta_\text{pct\_bachelor}x_\text{pct\_bachelor} \\& + \hat\beta_\text{per\_capita\_income}x_\text{per\_capita\_income} + \hat\beta_\text{total\_personal\_income}x_\text{total\_personal\_income}
\end{aligned} $$

A quick summary shows us:

```{r reduced_printsummary}
  summary(model_reduced_1)
```

Now, all the coefficients left would pass single predictor tests at $\alpha=0.05$ or much lower. The $R^2$ also remains high, though this is also cause for suspicion about the nature of the predictors. Overall though, observations give us some confidence. We proceed to run diagnostics on this model.


```{r}
  full_fstat = summary(model_full)$fstat[["value"]]
  n_iter = 5000

  relevant_data = data_cleaned_all %>%
    bind_cols(n_physicians = data_physicians$n_physicians) %>%
    select(n_physicians, everything())

  simulated_fstats = numeric(5000)

  for (i in 1:5000) {
    data = relevant_data %>%
      mutate(across(.cols = 7:12, .fns = sample  ))

    m = lm("n_physicians ~ .", data)

    simulated_fstats[i] = summary(m)$fstat[["value"]]
  }

  hist(simulated_fstats)

  sum(simulated_fstats > full_fstat) / 2000
```

```{r}
# library(faraway)
# #Find good or bad high leverage point
# # Calculate the IQR for the dependent variable
# IQR_y = IQR(data_physicians$n_physicians)
# leverages = lm.influence(model_final)$hat
# 
# #Define a range with its lower limit being (Q1 - IQR) and upper limit being (Q3 + IQR)
# QT1_y = quantile(data_physicians$n_physicians,0.25)
# QT3_y = quantile(data_physicians$n_physicians,0.75)
# 
# lower_lim_y = QT1_y - IQR_y
# upper_lim_y = QT3_y + IQR_y
# 
# vector_lim_y = c(lower_lim_y,upper_lim_y)
# 
# # Range for y variable
# vector_lim_y
# 
# # Extract observations with high leverage points from the original data frame
# highlev = data_physicians %>% slice(which(leverages>2*p/n))
# 
# # Select only the observations with leverage points outside the range
# highlev_lower = highlev[highlev$n_physicians < vector_lim_y[1], ]
# highlev_upper = highlev[highlev$n_physicians > vector_lim_y[2], ]
# highlev_new = rbind(highlev_lower,highlev_upper)
# View(highlev_new)
# View(highlev)

  
```


\newpage

# Appendix

## Model Summary: Full Model

`n_physicians ~ per_capita_income + log_pop_density + pct_beds + pct_crime + region + pct_18_24 + pct_65_older + pct_highschool + pct_bachelor + pct_below_poverty + pct_unemployed`

```{r model_summary_full, fig.height=9}
  all_summary(model_full)
```

\newpage

## Model Summary: Reduced Model (Reduced via Permutation Test in place of Partial F-Test)

`n_physicians ~ pct_below_poverty + per_capita_income + log_pop_density + region`

```{r model_summary_partial, fig.height=9}
  all_summary(model_partial)
```

\newpage

## Model Summary: Final Model (Applied Boxcox Transformation)

`log(n_physicians) ~ pct_below_poverty + per_capita_income + log_pop_density + region`

```{r model_summary_final, fig.height=9}
  all_summary(model_final)
```

```{r}
##Checking the linearity:

data_cleaned_reduced_1
test_final_df = data_cleaned_reduced_1 %>%
    bind_cols(n_physicians = data_physicians$n_physicians)


View(test_final_df)

final_mlr1 = lm(log(n_physicians) ~  0 + pct_below_poverty + per_capita_income + log_pop_density + region, data= test_final_df)
y_= update(, .~. -Gestation)$res

```
