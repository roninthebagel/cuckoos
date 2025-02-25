# reading csv file
cuckoo <- read_csv("data/cuckoo.csv")

#______________________________________#

# looking at column names
head(cuckoo)
# column names are Mass, Beg, and Species

# viewing data as point plot
ggplot(cuckoo, aes(x=Mass, y=Beg, colour=Species)) + geom_point()

#______________________________________#
  
# displaying the model diagnostics plots for this linear model.

# fit model
# there is an interaction term here, it is reasonable to think that how calling rates change with size might be different between the two species.
cuckoo_ls1 <- lm(Beg ~ Mass+Species+Mass:Species, data=cuckoo) 

performance::check_model(cuckoo_ls1, 
                         residual_type = "normal",
                         detrend = FALSE)

#______________________________________#
  
# general linear model - poisson regression
cuckoo_glm1 <- glm(Beg ~ Mass + Species + Mass:Species, data=cuckoo, family=poisson(link="log"))

summary(cuckoo_glm1)

# using the emmeans function to generate mean beg rates for different mass + species values
means <- emmeans::emmeans(cuckoo_glm1, 
                          specs = ~ Mass + Species + Mass:Species,
                          at = list(Mass = seq(0,40, by = 5))) |> 
  as_tibble()


ggplot(data = means, aes(x=Mass, y= emmean, colour=Species)) + 
  geom_point() +
  geom_line()+
  scale_colour_manual(values=c("green3","turquoise3"))+
  theme_minimal()

# plotting the mean regression for each species on the response scale
means_response <- emmeans::emmeans(cuckoo_glm1, 
                                   specs = ~ Mass + Species + Mass:Species,
                                   at = list(Mass = seq(0,40, by = 5)),
                                   type = "response") |> 
  as_tibble()

ggplot(data = means_response, aes(x=Mass, y= rate, colour=Species)) + 
  geom_point(data = cuckoo,
             aes(y = Beg,
                 x = Mass,
                 colour = Species),
             alpha = .4) +
  geom_point() +
  geom_line()+
  scale_colour_manual(values=c("green3","turquoise3"))+
  theme_minimal()

#______________________________________#

# model checks
  
performance::check_model(cuckoo_glm1, 
                          residual_type = "normal",
                          detrend = FALSE)

#______________________________________#

# model summary

summary(cuckoo_glm1)

#______________________________________#

# estimates and intervals

### Intercept - Incidence rate at mean mass, and Species = Cuckoo
exp(coef(cuckoo_glm1)[1])

### Change in the average incidence rate with Mass 
exp(coef(cuckoo_glm1)[2]) 

### Change in the incidence rate intercept when Species = Warbler and Mass = 0
exp(coef(cuckoo_glm1)[3]) 

### The extra change in incidence rate for each unit increase in Mass when Species = Warbler (the interaction effect)
exp(coef(cuckoo_glm1)[4])

# tidying models with the broom package
broom::tidy(cuckoo_glm1, 
            exponentiate = T, 
            conf.int = T)

#______________________________________#

# interpretation

# For a fixed  mean-variance model we use a Chisquare distribution
drop1(cuckoo_glm1, test = "Chisq")

# emmeans can be another handy function - if you specify response then here it provideds the average call rate for each species, at the average value for any continuous measures - so here the average call rate for both species at an average body mass of 20.3
emmeans::emmeans(cuckoo_glm1, specs = ~ Species:Mass, type = "response")

#______________________________________#

# overdispersion

# Formal test of overdispersion
check_overdispersion(cuckoo_glm1)
# overdispersion is detected

# dealing with overdispersion
cuckoo_glm2 <- glm(Beg ~ Mass+Species+Mass:Species, data=cuckoo, family=quasipoisson(link="log"))

summary(cuckoo_glm2)

