library(EpiEstim)



## create fake data
library(incidence)
# data <- c(0,1,1,2,1,3,4,5,5,5,5,4,4,26,6,7,9)
n = 60
x = 1:n
incid = rpois(n=n, lambda = x*(40-x))
incid = incid[!is.na(incid)]
# location[1:length(data)] <- "imported"
# location[1] <- "imported" # forcing the first case to be imported

## get incidence per group (location)
# incid <- incidence(data, groups = location)

plot(incid)

## Estimate R with assumptions on serial interval
res <- estimate_R(incid, method = "parametric_si",
                  config = make_config(list(
                    mean_si = 3, std_si = 1)))
plot(res)

foo = res$R
print(select(foo, t_start, t_end, `Mean(R)`))
