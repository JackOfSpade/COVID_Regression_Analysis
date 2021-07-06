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

plot_interaction_term = function(df, explanatory_variables)
{
  plot_number = 0
  
  for(x in 1:(length(explanatory_variables) - 1))
  {
    moderating_variable_starting_index = x + 1
    for(y in moderating_variable_starting_index:length(explanatory_variables))
    {
      plot_number = plot_number + 1
      
      formula <- as.formula(
        paste("daily_new_confirmed_percentage",
              paste(c(explanatory_variables, paste(explanatory_variables[x], " * ", explanatory_variables[y])), collapse = " + "),
              sep = " ~ "))
      
      print(paste("formula for plot ", plot_number, ": "))
      print(formula)
      
      model = get_model(formula=formula, df=df)
      
      # uses the mean value of the moderator as well as one standard deviation below and above mean value to plot the effect of the moderator on the independent variable (following the convention suggested by Cohen and Cohen and popularized by Aiken and West (1991), i.e. using the mean, the value one standard deviation above, and the value one standard deviation below the mean as values of the moderator
      print(plot_model(model, type = "pred", terms = c(explanatory_variables[x], explanatory_variables[y]), mdrt.values = "meansd"))
    }
  }
}

df = get_df()

explanatory_variables <- c("cumulative_vaccine_doses_administered_pfizer", "cumulative_vaccine_doses_administered_moderna", "cumulative_vaccine_doses_administered_johnsonandjohnson")
formula <- as.formula(
  paste("daily_new_confirmed_percentage",
        paste(c(explanatory_variables), collapse = " + "),
        sep = " ~ "))

formula2 <- as.formula(
  paste("daily_new_confirmed_percentage",
        paste(c(explanatory_variables, paste(explanatory_variables[1], " * ", explanatory_variables[3]), paste(explanatory_variables[2], " * ", explanatory_variables[3])), collapse = " + "),
        sep = " ~ "))

model = get_model(formula=formula, df=df)
model2 = get_model(formula=formula2, df=df)

# model_summary_wrapper(model=model2)
plot_interaction_term(df=df, explanatory_variables=explanatory_variables)

# TO DO: Write-up on interaction terms
# DO NOT USE R^2
