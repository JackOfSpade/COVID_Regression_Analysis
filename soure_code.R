library(tidyverse)
library(interactions)
library(rgl)
library(sjPlot)

get_df = function()
{
  # GROUP BY MONTH AND TAKE LAST CULMULATIVE DOSES COUNT
  df = read.csv(file="dataset.csv", sep=",")
  df=rename(df, "cumulative_vaccine_doses_administered_johnsonandjohnson"="cumulative_vaccine_doses_administered_janssen")
  US_population_in_millions = 332466570 / 1000000
  
  df["daily_new_confirmed_percentage"] = df["new_confirmed"] / US_population_in_millions
  df["cumulative_vaccine_doses_administered_pfizer_percentage"] = df["cumulative_vaccine_doses_administered_pfizer"] / US_population_in_millions
  df["cumulative_vaccine_doses_administered_moderna_percentage"] = df["cumulative_vaccine_doses_administered_moderna"] / US_population_in_millions
  df["cumulative_vaccine_doses_administered_johnsonandjohnson_percentage"] = df["cumulative_vaccine_doses_administered_johnsonandjohnson"] / US_population_in_millions
  
  return(df)
}


get_model = function(df)
{
  model = lm(new_confirmed_percentage ~ cumulative_vaccine_doses_administered_pfizer_percentage + cumulative_vaccine_doses_administered_moderna_percentage + cumulative_vaccine_doses_administered_johnsonandjohnson_percentage, data = df)
  
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
model = get_model(df)

plot_interaction_term(, "cumulative_vaccine_doses_administered_pfizer_percentage", "cumulative_vaccine_doses_administered_moderna_percentage")