library(tidyverse)
library(interactions)
library(rgl)
library(sjPlot)

get_df = function()
{
  # GROUP BY MONTH AND TAKE LAST CULMULATIVE DOSES COUNT
  df = read.csv(file="dataset.csv", sep=",")
  df=rename(df, "X_1"="cumulative_vaccine_doses_administered_pfizer")
  df=rename(df, "X_2"="cumulative_vaccine_doses_administered_moderna")
  df=rename(df, "X_3"="cumulative_vaccine_doses_administered_janssen")
  US_population = 333253254
  #df["X_1"] = df["X_1"] / 1000000
  #df["X_2"] = df["X_2"] / 1000000
  #df["X_3"] = df["X_3"] / 1000000
  df["Y"] = df["new_confirmed"] / US_population
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

residual_plot = function(model, x_value, x_axis_label)
{
  res = residuals(model)
  plot(x_value, res, xlab=x_axis_label, ylab="Residual",)
  abline(0,0)
}

df = get_df()
n = nrow(df)

explanatory_variables = c("X_1", "X_2", "X_3")


formula1 <- as.formula(
  paste("Y",
        paste(c(explanatory_variables), collapse = " + "),
        sep = " ~ "))

formula2 <- as.formula(
  paste("Y",
        paste(c(explanatory_variables, paste(explanatory_variables[1], " * ", explanatory_variables[3])), collapse = " + "),
        sep = " ~ "))

formula3 <- as.formula(
  paste("Y",
        paste(c(explanatory_variables, paste(explanatory_variables[2], " * ", explanatory_variables[3])), collapse = " + "),
        sep = " ~ "))

formula4 <- as.formula(
  paste("Y",
        paste(c(explanatory_variables, paste(explanatory_variables[1], " * ", explanatory_variables[3]), paste(explanatory_variables[2], " * ", explanatory_variables[3])), collapse = " + "),
        sep = " ~ "))

formula5 <- as.formula(
  paste("log(Y)",
        paste(c(explanatory_variables, paste(explanatory_variables[1], " * ", explanatory_variables[3])), collapse = " + "),
        sep = " ~ "))


model1 = get_model(formula=formula1, df=df)
model2 = get_model(formula=formula2, df=df)
model3 = get_model(formula=formula3, df=df)
model4 = get_model(formula=formula4, df=df)
model5 = get_model(formula=formula5, df=df)


print(summary_wrapper(model1))
print(summary_wrapper(model2))
print(summary_wrapper(model3))
print(summary_wrapper(model4))

#plot_interaction_term(df=df, explanatory_variables=explanatory_variables)

# residual_plot(model=model5, x_value=fitted(model5), x_axis_label="Y_hat")
# residual_plot(model=model5, x_value=seq(from = 1, to = n, by = 1), x_axis_label="Observation Order")

acf(fitted(model5))

#print(paste("SS_Res(R): ", SS_Res(model2)))
#print(paste("SS_Res(C): ", SS_Res(model4)))
#print(paste("SS_Res(R) - SS_Res(C): ", SS_Res(model2) - SS_Res(model4)))
#print(paste((SS_Res(model2) - SS_Res(model4)) / SS_Res(model2)))

#print(summary_wrapper(model2))
#print(summary_wrapper(model5))

#residual_plot(model=model1, x_value=df$X_1X_3_in_millions, x_axis_label="X_1*X_2 (millions)")
#residual_plot(model=model2, x_value=fitted(model2), x_axis_label="Y_hat")

#residual_plot(model=model5, x_value=fitted(model5), x_axis_label="Y_hat")