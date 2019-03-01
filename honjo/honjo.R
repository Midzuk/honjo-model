library(tidyverse)
library(magrittr)

# 2015年時点メッシュ人口およびメッシュの緯度経度
population_2015 <- read_csv("population_2015.csv")

mesh <- read_csv("mesh.csv") %>%
  inner_join(population_2015, by = "mesh_code") %>%
  select("mesh_code", "longitude", "latitude")

population_2015 %<>%
  gather(key = age, value = population, -mesh_code)

mesh_codes <- mesh[,1]


# 施設利用頻度
use_frequency <- read_csv("use_frequency.csv", locale = locale(encoding = "cp932")) %>%
  select("age",
         "公民館",
         "市民活動施設",
         "文化施設",
         "スポーツ施設",
         "産業振興施設",
         "図書館",
         "資料館等",
         "児童施設",
         "病院",
         "スーパー") %>%
  gather(key = "type", value = "use_frequency", -age)

facility_type <- use_frequency %>%
  nest(-type)
facility_type <- facility_type[,1]



# # バス停
# bus_stop <- read_csv("bus_stop.csv", locale = locale(encoding = "cp932")) %>%
#   select(-name)

# bus_stop %<>%
#   gather(key = "lon/lat", value = "degree", -route, -num)



# 公共施設
public_facility <- read_csv("public_facility.csv", locale = locale(encoding = "cp932")) %>%
  mutate(use_rate = as.numeric(sub("%", "", use_rate)) / 100)

# 医療施設
medical_facility <- read_csv("medical_facility.csv", locale = locale(encoding = "cp932")) %>%
  mutate(use_rate = as.numeric(sub("%", "", use_rate)) / 100)
# 市外または病院以外利用率
other_medical_facility_rate <- 0.576

# 商業施設
commercial_facility <- read_csv("commercial_facility.csv", locale = locale(encoding = "cp932")) %>%
  mutate(use_rate = as.numeric(sub("%", "", use_rate)) / 100)
# 市外利用率
other_medical_facility_rate <- 0.225



# 大円距離
great_circuler_distance <- function(lon1, lat1, lon2, lat2){
  f <- function(x){
    x * pi / 180
  }
  
  6378137 * acos(sin(f(lat1)) * sin(f(lat2)) + cos(f(lat1)) * cos(f(lat2)) * cos(f(lon1) - f(lon2)))
}



