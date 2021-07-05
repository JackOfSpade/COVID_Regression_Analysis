library(tidyverse)
library(interactions)
library(rgl)
library(sjPlot)

get_df = function()
{
  # GROUP BY MONTH AND TAKE LAST CULMULATIVE DOSES COUNT
  df = read.csv(file="dataset.csv", sep=",")
  df=rename(df, "cumulative_vaccine_doses_administered_johnsonandjohnson"="cumulative_vaccine_doses_administered_janssen")
  US_population = 332466570
  
  df["daily_new_confirmed_percentage"] = df["new_confirmed"] / US_population
 
  return(df)
}


get_model = function(formula, df)
{
  model = lm(formula=formula, data = df)
  
  return(model)
}


model_summary_wrapper = function(model)
{
  summary(model)
}

plot_linear_regression = function(model)
{
  print(summary(model))
  plot3d(model)
}

plot_interaction_term = function(df, explanatory_variable1, explanatory_variable2)
{
  # uses the mean value of the moderator as well as one standard deviation below and above mean value to plot the effect of the moderator on the independent variable (following the convention suggested by Cohen and Cohen and popularized by Aiken and West (1991), i.e. using the mean, the value one standard deviation above, and the value one standard deviation below the mean as values of the moderator
  print(plot_model(model, type = "pred", terms = c(explanatory_variable1, explanatory_variable2), mdrt.values = "meansd"))
}

df = get_df()

explanatory_variables <- c("cumulative_vaccine_doses_administered_pfizer", "cumulative_vaccine_doses_administered_moderna", "cumulative_vaccine_doses_administered_johnsonandjohnson")
formula <- as.formula(
  paste("daily_new_confirmed_percentage",
        paste(explanatory_variables, collapse = " + "),
        sep = " ~ "))

model = get_model(formula=formula, df=df)

# model_summary_wrapper(model=model)
# plot_interaction_term(df=df, explanatory_variable1="cumulative_vaccine_doses_administered_pfizer", explanatory_variable2="cumulative_vaccine_doses_administered_moderna")
