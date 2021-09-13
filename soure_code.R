library(tidyverse)
library(interactions)
library(rgl)
library(sjPlot)
library(lmtest)
library(orcutt)
library(dplyr)

get_df = function()
{
  # GROUP BY MONTH AND TAKE LAST CULMULATIVE DOSES COUNT
  df = read.csv(file="dataset.csv", sep=",")
  df=rename(df, "X_1"="cumulative_vaccine_doses_administered_pfizer")
  df=rename(df, "X_2"="cumulative_vaccine_doses_administered_moderna")
  df=rename(df, "X_3"="cumulative_vaccine_doses_administered_janssen")
  US_population = 333253254
  
  df["Y"] = df["new_confirmed"] / US_population
  
  df["Y_star"] = NA
  
  # Sort by Date
  df=df[with(df, order(date)),]
  
  return(df)
}


get_model = function(formula, df)
{
  model = lm(formula=formula, data = df)
  
  return(model)
}


summary_wrapper = function(model)
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
        paste("Y",
              paste(c(explanatory_variables, paste(explanatory_variables[x], " * ", explanatory_variables[y])), collapse = " + "),
              sep = " ~ "))
      
      print(paste("formula for plot ", plot_number, ": "))
      print(formula)
      
      model = get_model(formula=formula, df=df)
      
      # uses the mean value of the moderator as well as one standard deviation below and above mean value to plot the effect of the moderator on the independent variable (following the convention suggested by Cohenand popularized by Aiken and West (1991), i.e. using the mean, the value one standard deviation above, and the value one standard deviation below the mean as values of the moderator
      print(plot_model(model, type = "pred", terms = c(explanatory_variables[x], explanatory_variables[y]), mdrt.values = "meansd"))
    }
  }
}

confidence_level_wrapper = function(model, level)
{
  confint(object=model, level=level)
}

SS_R = function(model, df)
{
  return(sum((fitted(model) - mean(df$Y))^2))
}

SS_Res = function(model)
{
  return(sum(residuals(model)^2))
}

SS_Total = function(model)
{
  return(SS_R(model)+SS_Res(model))
}

MS_R= function(model, df, p)
{
  return(SS_R(model, df) / p)
}

MS_Res = function(model, n, k)
{
  return(SS_Res(model) / (n - k))
}

residual_plot = function(model, x_axis, x_axis_label)
{
  res = residuals(model)
  plot(x_axis, res, xlab=x_axis_label, ylab="Residual",)
  abline(0,0)
}

durbin_watson_test = function(df, model, alternative)
{
  if (alternative == "positive_autocorrelation")
  {
    alternative_arguement = c("greater")
  }
  else if(alternative == "negative_autocorrelation")
  {
    alternative_arguement = c("less")
  }
  else if(alternative == "any_autocorrelation")
  {
    alternative_arguement = c("two.sided")
  }
  else
  {
    stop('Invalid "alternative" parameter input')
  }
  
  return(dwtest(model, alternative = alternative_arguement, data = df))
}


Cochrane_Orcutt = function(df, pre_cochrane_orcutt_model)
{
  rho = cochrane.orcutt(pre_cochrane_orcutt_model)$rho
  df = mutate(df, Y_star = Y - rho*lag(Y, default = (sqrt(1-rho^2)*first(Y)) / -rho - first(Y)))
  df = mutate(df, X_1_star = X_1 - rho*lag(X_1, default = (sqrt(1-rho^2)*first(X_1)) / -rho - first(X_1)))
  df = mutate(df, X_2_star = X_2 - rho*lag(X_2, default = (sqrt(1-rho^2)*first(X_2)) / -rho - first(X_2)))
  df = mutate(df, X_3_star = X_3 - rho*lag(X_3, default = (sqrt(1-rho^2)*first(X_3)) / -rho - first(X_3)))
  
  return(df)
}


df = get_df()
n = nrow(df)

explanatory_variables = c("X_1", "X_2", "X_3")

formula5 <- as.formula(
  paste("log(Y)",
        paste(c(explanatory_variables, paste(explanatory_variables[1], " : ", explanatory_variables[3])), collapse = " + "),
        sep = " ~ "))

model5 = get_model(formula=formula5, df=df)

df = Cochrane_Orcutt(df=df, pre_cochrane_orcutt_model=model5)


model6 = get_model(formula="log(Y_star)~X_1_star + X_2_star + X_3_star + X_1_star:X_3_star", df=df)

model7 = get_model(formula="log(Y_star)~X_2_star + X_3_star + X_1_star:X_3_star", df=df)

plot(model7)