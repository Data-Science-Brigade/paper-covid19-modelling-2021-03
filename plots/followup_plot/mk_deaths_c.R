require(tidyverse)
require(dplyr)
require(lubridate)

# Load latest model
deaths <- read_csv("deaths.csv")
deaths <- deaths %>% select(c("data_ocorrencia", "obitos"))
deaths <- deaths %>% mutate(data_ocorrencia = ymd(case_when(
    data_ocorrencia=="IGNORADO" ~ max(data_ocorrencia,na.rm = TRUE),
    T ~ data_ocorrencia)))
deaths <- deaths %>% group_by(data_ocorrencia)
deaths <- deaths %>% summarise(obitos=sum(obitos))
deaths <- deaths %>% arrange(data_ocorrencia)
deaths <- deaths %>% mutate(obitos=cumsum(obitos))
deaths <- deaths %>% rename(deaths_c = obitos, date = data_ocorrencia)
deaths <- deaths %>% remove_missing()

print(deaths)
print(unique(deaths$date))
write_csv(deaths, "deaths_c.csv")
