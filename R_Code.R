library(tidyverse)
library(interactions)
library(rgl)

get_df = function()
{
  df = read.csv(file="dataset.csv", sep=",")
  df=rename(df, "cumulative_vaccine_doses_administered_johnsonandjohnson"="cumulative_vaccine_doses_administered_janssen")
  return(df)
}

plot_linear_regression = function(model)
{
  print(summary(model))
  plot3d(model)
}

plot_interaction_term = function(df, explanatory_variable1, explanatory_variable2)
{
  print(ggplot(data=df, mapping=aes(y=new_confirmed, x=explanatory_variable1, color=cut(explanatory_variable2, breaks=c(-Inf, Inf)))) 
        + geom_point()
        + geom_smooth(method = "lm", se=FALSE))
}

df = get_df()

model = lm(new_confirmed ~ cumulative_vaccine_doses_administered_pfizer + cumulative_vaccine_doses_administered_moderna + cumulative_vaccine_doses_administered_johnsonandjohnson, data = df)

print(summary(model))


