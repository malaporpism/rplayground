library(pacman)
p_load(tidyverse, magrittr, here)
p_load(forecast)

system('open -a "/System/Volumes/Data/Applications/Utilities/XQuartz.app"')
p_load(mappoly)

curl_download(url = "https://raw.githubusercontent.com/mmollina/MAPpoly_vignettes/master/data/tetra_solcap.csv",
              destfile = "~/Desktop/geno.csv", 
              quiet = F)

here::i_am(fileName)

data("AirPassengers")
AP <- AirPassengers

orig %>% ts(frequency = 12, start = c(2016,1)) -> orig_ts
smo %>% ts(frequency = 52, start = c(2016,4)) -> smo_ts


origd <-decompose(log(orig_ts))
smod <- decompose(log(smo_ts))

smod$seasonal %>%
  as_tibble() -> smod_seasonal

plot(origd$figure, type = 'b',
      xlab = 'Week', ylab = 'Seasonality Index',
      col = 'blue', las = 2)
plot(origd)
plot(smod)






x = (0:181)


f %>%
  as_tibble()
real(f)







strfourier(mort_ts, K = 4) -> fo
plot(fo[,1], col = 1, type = 'l')
lines(fo[,2], col = 1)
lines(fo[,3], col = 2)
lines(fo[,4], col = 2)

mort.model <-  auto.arima(mort_ts, xreg = fo, seasonal = T)
mort.fcast <- forecast(mort.model, xreg = fo)
autoplot(mort.fcast) + xlab("Year")


 plot(mortd$seasonal)
abline(a=0,b=0, col ='red')

mortd$seasonal %>%
  as_tibble() -> seasonal






g <- read_geno_csv(
  file.in = "/Users/jackliu/Downloads/DataForAssignment2023.csv",
  ploidy  = 2,
  filter.non.conforming = F,
  elim.redundant = F,
  verbose = TRUE
)
print(g, detailed = T)
plot(g)


