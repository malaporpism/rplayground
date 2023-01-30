library(pacman)
p_load(tidyverse, magrittr, here,
       lubridate)

here::i_am(fileName)

# playground ----
# Fit notes issued by GP practices, England, Apr 2018 to Sep 2022
# CSV (Episodes with diagnosis and duration)
fitnotes <- read_csv('https://files.digital.nhs.uk/AF/AC228F/gp-fit-note-eng-csv-ep-dur-sep-22.csv')

fitnotes %>%
  mutate(Date = dmy(paste0("01", Date))) %>%
  mutate(Resp = grepl("respiratory", `Diagnosis (ICD10 chapter)`), 
         .before = 3, .keep = "unused") %>%
  group_by(Date, Resp, Duration) %>%
  summarise(mean = round(mean(`Count of episodes`),0)) -> fitnotes_sum

# test viz
ggplot(fitnotes_sum, aes(Date, mean)) +
  geom_bar(aes(fill = Resp), position = "dodge", stat = "identity")

# get cumulative num of ppl taking sick leave
# by modelling using normal (or χ2) distribution
sick <- tibble(date = unique(fitnotes_sum$Date))


hist(rchisq(200,6))


X=rchisq(1:1000, df=3)
mean(X)


