library(brms)
set.seed(1234)
dat <- data.frame(
  y = c(rnorm(200), rnorm(100, 6)), 
  x = rnorm(300),
  z = sample(0:1, 300, TRUE)
)

## fit a simple normal mixture model
mix <- mixture(gaussian, gaussian)
prior <- c(
  prior(normal(0, 7), Intercept, dpar = mu1),
  prior(normal(5, 7), Intercept, dpar = mu2)
)
fit1 <- brm(bf(y ~ x + z), dat, family = mix,
            prior = prior, chains = 2) 
summary(fit1)
pp_check(fit1)

## use different predictors for the components
fit2 <- brm(bf(y ~ 1, mu1 ~ x, mu2 ~ z), dat, family = mix,
            prior = prior, chains = 2) 
summary(fit2)

## fix the mixing proportions
fit3 <- brm(bf(y ~ x + z, theta1 = 1, theta2 = 2), 
            dat, family = mix, prior = prior, 
            inits = 0, chains = 2)
summary(fit3)
pp_check(fit3)    

## predict the mixing proportions
fit4 <- brm(bf(y ~ x + z, theta2 ~ x), 
            dat, family = mix, prior = prior, 
            inits = 0, chains = 2)
summary(fit4)
pp_check(fit4)           

## compare model fit
LOO(fit1, fit2, fit3, fit4)  