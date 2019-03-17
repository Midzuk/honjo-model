library(tidyverse)
library(magrittr)

# 2015年時点メッシュ人口およびメッシュの緯度経度
population_2015 <- read_csv("old/population_2015.csv")

mesh <- read_csv("old/mesh_new.csv") %>%
  inner_join(population_2015, by = "mesh_code") %>%
  select("mesh_code", "longitude", "latitude")

population_2015 %<>%
  gather(key = age, value = population, -mesh_code) %>%
  split(.$mesh_code)

mesh_codes <- mesh$mesh_code

# 施設利用頻度
use_frequency <- read_csv("old/use_frequency.csv") %>%
  select("age",
         "kominkan",
         "shiminkatsudo",
         "bunka",
         "sports",
         "sangyoshinko",
         "toshokan",
         "shiryokanto",
         "jido",
         "byoin",
         "super") %>%
  gather(key = "type", value = "use_frequency", -age) %>%
  split(.$type)

facility_type <- names(use_frequency)



# # バス停
# bus_stop <- read_csv("bus_stop.csv", locale = locale(encoding = "cp932")) %>%
#   select(-name)

# bus_stop %<>%
#   gather(key = "lon/lat", value = "degree", -route, -num)



# 公共施設
public_facility <- read_csv("old/public_facility.csv") %>%
  mutate(use_rate = as.numeric(sub("%", "", use_rate)) / 100) %>%
  select(-name)

public_facility_type <- public_facility %>%
  nest(-type) %>%
  pull(type)

# 医療施設
medical_facility <- read_csv("old/medical_facility.csv") %>%
  mutate(use_rate = as.numeric(sub("%", "", use_rate)) / 100) %>%
  select(-name)
# 市外または病院以外利用率
other_medical_facility_rate <- 0.576

# 商業施設
commercial_facility <- read_csv("old/commercial_facility.csv") %>%
  mutate(use_rate = as.numeric(sub("%", "", use_rate)) / 100) %>%
  select(-name)
# 市外利用率
other_medical_facility_rate <- 0.225



# 大円距離
# great_circuler_distance <- function(lon1, lat1, lon2, lat2){
#   f <- function(x){
#     x * pi / 180
#   }
#  
#   6378137 * acos(sin(f(lat1)) * sin(f(lat2)) + cos(f(lat1)) * cos(f(lat2)) * cos(f(lon1) - f(lon2)))
# }



# 施設緯度経度
public_facility_lonlat <- public_facility %>%
  select(type, num, longitude, latitude)
medical_facility_lonlat <- medical_facility %>%
  select(type, num, longitude, latitude)
commercial_facility <- commercial_facility %>%
  select(type, num, longitude, latitude)

facility_lonlat <- bind_rows(public_facility_lonlat,
                             medical_facility_lonlat,
                             commercial_facility
                             )


# Haskellで算出したものを利用
distance <- read_csv("old/distance.csv") %>%
  split(.$type) %>%
  map(~ split(.x, .$mesh_code) %>%
        map(~ filter(., distance == min(distance))) %>%
        map(~ .[1,])
      )


#　test 重複確認 => 問題なし
distance1 <- read_csv("old/distance.csv") %>%
  split(.$type) %>%
  map(~ split(.x, .$mesh_code) %>%
        map(~ filter(., distance == min(distance)))
  ) 

for (i in 1 : length(distance1)) {
  for (j in 1 : length(distance1[[i]])) {
    if (nrow(distance1[[i]][[j]]) > 1) {
      print(distance1[[i]][[j]]$type)
    }
  }
}

# for (t in facility_type) {
#   for (m in mesh_codes) {
#     
#   }
# }

facility_user <- facility_lonlat %>%
  select(type, num) %>%
  mutate(user = 0, distance = 0, share = 0) %>%
  split(.$type)

facility_user1 <- facility_lonlat %>%
  select(type, num) %>%
  mutate(user = 0, distance = 0, share = 0)

# test => OK
total_user <- tribble(~type, ~user,
                      "bunka", 0,
                      "byoin", 0,
                      "jido", 0,
                      "kominkan", 0,
                      "sangyoshinko", 0,
                      "shiminkatsudo", 0,
                      "shiryokanto", 0,
                      "sports", 0,
                      "super", 0,
                      "toshokan",0
)


# for文でよい
1 : length(facility_user) %>%
  map(function(i) { # i : 施設
    total_user[i,]$user <<-
      1 : length(population_2015) %>%
      map_dbl(function(j) { # j : メッシュ
        1 : nrow(population_2015[[j]]) %>%
          map_dbl(function(k) { # k : 年齢
            population_2015[[j]][k,]$population * use_frequency[[i]][k,]$use_frequency
          }) %>%
          sum()
      }) %>%
      sum() + total_user[i,]$user
  })



# list(facility_user, distance, use_frequency) %>%
#   pmap(~ 1 : nrow(..1) %>%
#          map(function(x) {
#            ..1$user[x] <- 1
#          }))

# 副作用あり (2度回さないこと)
for (i in 1 : length(distance)) {
  for (j in 1 : length(distance[[i]])) {
    num <- distance[[i]][[j]]$num + 1
    facility_user[[i]][num,]$user <-
      1 : nrow(population_2015[[j]]) %>%
      map_dbl(function(k) { # k : 年齢
        population_2015[[j]][k,]$population * use_frequency[[i]][k,]$use_frequency
      }) %>%
      sum() + facility_user[[i]][num,]$user
    facility_user[[i]][num,]$distance <-
      1 : nrow(population_2015[[j]]) %>%
      map_dbl(function(k) { # k : 年齢
        (population_2015[[j]][k,]$population * use_frequency[[i]][k,]$use_frequency) * distance[[i]][[j]]$distance
      }) %>%
      sum() + facility_user[[i]][num,]$distance
  }
}



# シェア算出
for (i in 1 : length(facility_user)) {
  for (num in 1 : nrow(facility_user[[i]])) {
    facility_user[[i]][num,]$share <- facility_user[[i]][num,]$user / total_user[i,]$user
  }
}



# 出力用
facility_user_out <- 1 : length(facility_user) %>%
  map_dfr(~ facility_user[[.]]) %>%
  left_join(public_facility, by = c("type", "num")) %>%
  left_join(medical_facility, by = c("type", "num")) %>%
  left_join(commercial_facility, by = c("type", "num"))




# Haskellで算出したものを利用
distance1 <- read_csv("old/distance.csv") %>%
  split(.$type) %>%
  map(~ split(., .$mesh_code) %>%
        map(~ split(., .$num))
  )

public_facility1 <- public_facility %>%
  split(.$type) %>%
  map(~ split(., .$num))

public_facility1_name <- names(public_facility1)

f <- function(i, x) {
  # public : 1-8
  if (1 <= i && i <= 8) {
    for (j in length(distance1[[1]])) {
      # ここから
      
      
    }
  }
  
  # commercial : 9
  if (i == 9) {
    # ここから
  }
  
  # medical : 
  if (i == 10) {
    # ここから
  }
}
