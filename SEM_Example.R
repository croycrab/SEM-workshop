#Install packages
install.packages("piecewiseSEM")
install.packages("openxlsx")
install.packages("lme4")
install.packages("glmmTMB")
install.packages("DHARMa")
install.packages("MASS")

#Libraries
library(piecewiseSEM)
library(openxlsx)
library(lme4)
library(glmmTMB)
library(DHARMa)
library(MASS)

## Read in the data
# For SEM analysis
sem_data <- read.xlsx("example data.xlsx")

## specify regression models and inspect (test model assumptions) -- there should be a model for each response variable, let's take a simple example:

#natural enemy models
mod.enemies<- glmmTMB(nat_enemies ~ soil_P_ppm, data = sem_data, family = nbinom2())
#glm.nb(nat_enemies ~ soil_P_ppm, data = sem_data)
testDispersion(mod.enemies) #view dispersion of Poisson and negative binomial, nbinom models have greater flexibility in mean-variance relationships
summary(mod.enemies)

#herbivore model
mod.herb <- glmmTMB(herbivores ~ nat_enemies + soil_P_ppm, data = sem_data, family = nbinom2())
#glm.nb(herbivores ~ nat_enemies + soil_P_ppm, data = sem_data)
testDispersion(mod.herb)
summary(mod.herb)

#yield model
mod.yield  <- glmmTMB(yield_kg ~herbivores + soil_P_ppm, data = sem_data, family = gaussian())
#lm(yield_kg ~herbivores + soil_P_ppm, data = sem_data)
hist(resid(mod.yield),breaks = 10)
shapiro.test(resid(mod.yield))
summary(mod.yield)

## SEM analysis
# stitch models together
SEM <- psem(glm.nb(nat_enemies ~ soil_P_ppm, sem_data),
            glm.nb(herbivores ~ nat_enemies + soil_P_ppm, sem_data),
            lm(yield_kg ~herbivores + soil_P_ppm, sem_data)
)#you'll notice that we are no longer using glmmTMB to specify models--this is simply because we can't seem to get psem to recognize those models as easily.

#view SEM (not the best presentation, but it works)
plot(SEM, alpha = 0.05, ns_dashed = T) 
#view basis set (independence claims)
basisSet(modelList = SEM)

# Get summary of results of model
summary(SEM, conserve = TRUE) #what is conserve = T doing here? There is an independence claim for yield and natural enemies, this can be tested as yield ~ natural enemies or natural enemies ~ yield, but because yield is a Gaussian model and natural enemies is a negative binomial models, there will be asymmetry in the results. Conserve says, "take the most conservative P-value for this test. You can use the "direction" option to specify the direction of the relationship that should be tested (or no direction)

# Model selection – select model with lowest possible AIC value that maintains biological meaning
# First run model with all possible pathways and then slowly remove variables to optimize AIC	
SEM_testing <- psem(
  glm.nb(nat_enemies ~ soil_P_ppm, sem_data),
  glm.nb(herbivores ~ nat_enemies + soil_P_ppm, sem_data),
  lm(yield_kg ~herbivores + soil_P_ppm + nat_enemies, sem_data))

# Get summary of results of model
summary(SEM_testing, conserve = TRUE)

#compare two models
AIC_psem(SEM,AIC.type = "loglik", aicc = T)
AIC_psem(SEM_testing)


#just to dispell any of the perceived magic that this approach is doing compared to regression, let's look at the coefficients resulting from the SEM vs a linear regression
#we can focus on the direct effect of soil P on herbivores; based on our dag, we need to condition this relationship on natural enemies. Our model defined in mod.herb already does this. So let's compare:
coefs(mod.herb) #estimate for soil effects on herbivores  = 0.0119
coefs(SEM) #estimate for soil effects on herbivores  = 0.011927
#they are equal! WOW!

