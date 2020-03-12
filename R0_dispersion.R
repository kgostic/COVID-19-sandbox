## Superspreading analysis
nsim = 10000

## Write a function to simulate offspring distributions
get_offspring_dist = function(R0, k){
  data.frame(dispersion = k,
             indivR0 = rnbinom(n = nsim, size = k, mu = R0))
}

## Simulate offspring distributions when dispersion parameter is 1, .5 and .1
lapply(c(1, .5, .1), function(dd) get_offspring_dist(2.5, dd)) %>%
  bind_rows() -> sims


## ------------ IMPLEMENT CONTROL -------------- ##
## Define superspreader as anyone who causes 7 or more infections. See what happens to R0 if we move 50% of superspreaders into the non-superspreader category.
## Do this by sampling from a truncated negative binomial distribution with paramters R0 and k, and max value (threshold - 1)
get_reduced_r0 = function(origR0s, k, R0, superThreshold = 7){
   isSuperspreader = origR0s >= superThreshold
   toReplace = isSuperspreader & (runif(length(isSuperspreader)) > .5) ## Replace half of superspreaders
   newR0s = origR0s
   newR0s[toReplace] = sample(0:(superThreshold-1), size = sum(toReplace), replace = TRUE, prob = dnbinom(0:(superThreshold-1), size = k, mu = R0)/sum(dnbinom(0:(superThreshold-1), size = k, mu = R0)))
   newR0s
}
get_reduced_r0(origR0s = 1:100, k = .5, R0 = 2.5)

sims %>% ungroup() %>%
  group_by(dispersion) %>%
  mutate(reducedR0 = get_reduced_r0(indivR0, k = unique(dispersion), R0 = 2.5, superThreshold = 7)) -> simWControl



## Get the expected number of cases caused by each infectious person with and without control
## Should be R0 without control
simWControl %>%
  group_by(dispersion) %>% 
  summarise(mean = mean(indivR0),
            reducedMean = mean(reducedR0))

## Get the fraction of infected people who cause 80% and 90% of new cases
## These numbers seem wacky. Am I doing this right?
simWControl %>%
  group_by(dispersion) %>% 
  arrange(indivR0) %>%
  mutate(fTns0 = cumsum(indivR0)/sum(indivR0),
            fTnsC = cumsum(reducedR0)/sum(reducedR0)) %>%
  summarise(fTns0_80 = sum(fTns0 >= .8),
            fTns0_90 = sum(fTns0 >= .9))



simWControl %>%
  ungroup() %>%
  ggplot() +
  geom_histogram(aes(indivR0, fill = dispersion, color = dispersion), binwidth = 1)
