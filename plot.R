library("tidyverse")

expected_yield_susceptible = 55
expected_yield_resistant   = 53
expected_price = 9.13

yield_decrease = 0.127

seed_cost = 0.89
spray_cost = 1.42-0.89

probability_aphids <- 0.47


pm <- seq(0,1,by=0.01)

d <- data.frame(pm=pm) %>%
  mutate(resistant = expected_yield_resistant*expected_price-seed_cost,
         susceptible = 
           expected_yield_susceptible * (1-probability_aphids +
                                           probability_aphids*(1-pm) +
                                           probability_aphids*pm*(1-yield_decrease)) * 
           expected_price - seed_cost - spray_cost) %>%
  gather(variety, expected_return, resistant, susceptible)

ggplot(d, aes(pm, expected_return, linetype = variety, color=variety)) + 
  geom_line() + 
  theme_bw()
