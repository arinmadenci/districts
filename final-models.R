# This script fits random intercept models, as described below
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, reshape2, tidylog, splines, pbapply, here, brms)
load(file=here("data","districts.Rda"))
states <- d$state %>% unique

# Final models for each outcome, stratified by state
m.anemia <- pblapply(setNames(states, states),
                     function(x) brm(anemia ~ 1 + (1|district), 
                                     data = d %>% filter(state==x), family = bernoulli,
                                     prior = c(prior(normal(0, 2), class = Intercept),
                                               prior(normal(0, 2), class = sd)),
                                     chains=1, iter=30000, warmup=5000,
                                     control=list(adapt_delta = 0.99999)))
save(m.anemia, file=here("models", "anemia-models.Rda"))

m.underweight <- pblapply(setNames(states, states),
                          function(x) brm(underweight ~ 1 + (1|district), 
                                          data = d %>% filter(state==x), family = bernoulli,
                                          prior = c(prior(normal(0, 2), class = Intercept),
                                                    prior(normal(0, 2), class = sd)),
                                          chains=1, iter=30000, warmup=5000,
                                          control=list(adapt_delta = 0.999)))
save(m.underweight, file=here("models", "underweight-models.Rda"))

m.stunting <- pblapply(setNames(states, states),
                       function(x) brm(stunting ~ 1 + (1|district), 
                                       data = d %>% filter(state==x), family = bernoulli,
                                       prior = c(prior(normal(0, 2), class = Intercept),
                                                 prior(normal(0, 2), class = sd)),
                                       chains=1, iter=30000, warmup=5000,
                                       control=list(adapt_delta = 0.999)))
save(m.stunting, file=here("models", "stunting-models.Rda"))

m.wasting <- pblapply(setNames(states, states),
                       function(x) brm(wasting ~ 1 + (1|district), 
                                       data = d %>% filter(state==x), family = bernoulli,
                                       prior = c(prior(normal(0, 2), class = Intercept),
                                                 prior(normal(0, 2), class = sd)),
                                       chains=1, iter=30000, warmup=5000,
                                       control=list(adapt_delta = 0.999)))
save(m.wasting, file=here("models", "wasting-models.Rda"))