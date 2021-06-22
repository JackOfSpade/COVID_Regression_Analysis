library(tidyverse)
library(interactions)
library(rgl)

get_df = function()
{
  df = read.csv(file="dataset2.csv", sep=",")
  return(df)
}

plot_linear_regression = function(model)
{
  print(summary(model))
  plot3d(model)
}

plot_interaction_term = function(df)
{
  print(ggplot(data=df, mapping=aes(y=new_confirmed, x=cumulative_vaccine_doses_administered_pfizer, color=cut(cumulative_vaccine_doses_administered_moderna, breaks=c(-Inf, Inf)))) 
        + geom_point()
        + geom_smooth(method = "lm", se=FALSE))
}

df = get_df()
df["perentage_of_population"] = df["new_confirmed"] / df["population"]

#school_closing + workplace_closing + cancel_public_events + restrictions_on_gatherings + public_transport_closing + stay_at_home_requirements + restrictions_on_internal_movement + international_travel_controls + public_information_campaigns

model = lm(new_confirmed ~ cumulative_vaccine_doses_administered_pfizer + cumulative_vaccine_doses_administered_moderna, data = df)

print(summary(model))

res <- resid(model)
plot(fitted(model), res)
abline(0,0)
# plot_linear_regression(model)
plot_interaction_term(df)
#


