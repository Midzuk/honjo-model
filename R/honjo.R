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

commercial_facility1 <- commercial_facility %>%
  split(.$num)

medical_facility1 <- medical_facility %>%
  split(.$num)

# 2015年時点メッシュ人口およびメッシュの緯度経度
population_2015_1 <- read_csv("old/population_2015.csv")

population_2015_1 %<>%
  gather(key = age, value = population, -mesh_code) %>%
  split(.$mesh_code) %>%
  map(~ split(., .$age))



# 施設利用頻度
use_frequency1 <- read_csv("old/use_frequency.csv") %>%
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
  split(.$type) %>%
  map(~ split(., .$age))


# xは正のみ
calc_use_rate <- function(i, x) {
  users <- rep(0, length(distance1[[i]][[1]])) # 汚い
  use_rates <- rep(0, length(distance1[[i]][[1]])) # 汚い
  
  for (j in 1 : length(distance1[[i]])) { # j : メッシュ
    sum = 0
    us <- rep(0, length(distance1[[i]][[j]]))
    
    distance_min <- distance1[[i]][[j]][[1]]$distance
    
    for (k in 2 : length(distance1[[i]][[j]])) { # k : 施設番号
      if (distance1[[i]][[j]][[k]]$distance < distance_min) {
        distance_min <- distance1[[i]][[j]][[k]]$distance
      }
    }
    
    for (k in 1 : length(distance1[[i]][[j]])) { # k : 施設番号
      s <- 0
      
      # 汚い
      if (i == 2) { # 病院
        s <- medical_facility1[[k]]$hospital_bed
      } else if (i == 9) { # スーパー
        s <- commercial_facility1[[k]]$area_m2
      } else {
        if (i >= 3 && i <= 8) {
          i2 <- i - 1
        } else if (i == 10) {
          i2 <- 8
        } else {
          i2 <- i
        }
        
        s <- public_facility1[[i2]][[k]]$area_m2
      }
      
      us[k] <- s * exp(- x * (distance1[[i]][[j]][[k]]$distance - distance_min))
    }
    
    # メッシュ上の利用者数合計
    user <- 1 : length(population_2015_1[[j]]) %>%
      map_dbl(function(k) { # k : 年齢
        population_2015_1[[j]][[k]]$population * use_frequency1[[i]][[k]]$use_frequency
      }) %>%
      sum()
    
    for (k in 1 : length(distance1[[i]][[j]])) {
      p <- us[k] / sum(us) # あるメッシュある施設の施設選択率

      # 年齢ごとの利用回数合計
      users[k] <- users[k] + p * user
    }
  }
  
  for (k in 1 : length(distance1[[i]][[1]])) {
    use_rates[k] <- users[k] / sum(users)
  }

  return(use_rates)
}



# xは正のみ
# ハフモデル(対数じゃない版)
calc_use_rate1 <- function(i, x) {
  users <- rep(0, length(distance1[[i]][[1]])) # 汚い
  use_rates <- rep(0, length(distance1[[i]][[1]])) # 汚い
  
  for (j in 1 : length(distance1[[i]])) { # j : メッシュ
    sum = 0
    us <- rep(0, length(distance1[[i]][[j]]))
  
    distance_max <- 0
    
    for (k in 2 : length(distance1[[i]][[j]])) { # k : 施設番号
      if (distance1[[i]][[j]][[k]]$distance > distance_max) {
        distance_max <- distance1[[i]][[j]][[k]]$distance
      }
    }
    
    for (k in 1 : length(distance1[[i]][[j]])) { # k : 施設番号
      s <- 0
      
      if (i == 2) { # 病院
        s <- medical_facility1[[k]]$hospital_bed
      } else if (i == 9) { # スーパー
        s <- commercial_facility1[[k]]$area_m2
      } else {
        if (i >= 3 && i <= 8) {
          i2 <- i - 1
        } else if (i == 10) {
          i2 <- 8
        } else {
          i2 <- i
        }
        
        s <- public_facility1[[i2]][[k]]$area_m2
      }
      
      us[k] <- s / (distance1[[i]][[j]][[k]]$distance / distance_max) ^ x
    }
    
    # メッシュ上の利用者数合計
    user <- 1 : length(population_2015_1[[j]]) %>%
      map_dbl(function(k) { # k : 年齢
        population_2015_1[[j]][[k]]$population * use_frequency1[[i]][[k]]$use_frequency
      }) %>%
      sum()
    
    for (k in 1 : length(distance1[[i]][[j]])) {
      p <- us[k] / sum(us) # あるメッシュある施設の施設選択率
      
      # 年齢ごとの利用回数合計
      users[k] <- users[k] + p * user
    }
  }
  
  for (k in 1 : length(distance1[[i]][[1]])) {
    use_rates[k] <- users[k] / sum(users)
  }
  
  return(use_rates)
}



use_rate_error <- function(i, x) {
  es <- rep(0, length(distance1[[i]][[1]]))
  
  use_rates_est <- calc_use_rate1(i, x)
  
  for (k in 1 : length(distance1[[i]][[1]])) { # k : 施設番号
    if (i == 2) { # 病院
      use_rate <- medical_facility1[[k]]$use_rate
    } else if (i == 9) { # スーパー
      use_rate <- commercial_facility1[[k]]$use_rate
    } else {
      if (i >= 3 && i <= 8) {
        i2 <- i - 1
      } else if (i == 10) {
        i2 <- 8
      } else {
        i2 <- i
      }
      use_rate <- public_facility1[[i2]][[k]]$use_rate
    }
    
    es[k] <- (use_rates_est[k] - use_rate) ^ 2
  }
  
  return(sum(es))
}

#           x          error
# 文化 i == 1 => 0.6398631, 1.010923e-12
# 病院 i == 2 => 2.010507, 0.00343115
# 児童 i == 3 => 0.853016, 5.795824e-13
# 公民 i == 4 => 1.209198, 0.004022097
# 産業 i == 5 => 0       , - # 直線距離に依存しない
# 市民 i == 6 => 1.738493, 0.01302915
# 資料 i == 7 => 0       , - # 直線距離に依存しない
# スポ i == 8 => 0.8526133, 0.002382758
# スパ i == 9 => 0       , - # 直線距離に依存しない
# 図書 i == 10 => ∞      , - # 完全に直線距離に依存

x <- c(0.6398631, 2.010507, 0.853016, 1.209198, 0, 1.738493, 0, 0.8526133, 0) # 変数

distance2 <- distance1

# 利用率
for (i in 1 : length(distance1)) { # i: 施設種別
  for (j in 1 : length(distance1[[i]])) { # j: メッシュ
    for (k in 1 : length(distance1[[i]][[j]])) { # k: 施設
      distance2[[i]][[j]][[k]] %<>%
        mutate(huff = NA, use_rate = NA, user = NA, userXdistance = NA) %>%
        select(huff, use_rate, user, userXdistance)
    }
  }
}

# 利用率
for (i in 1 : (length(distance1) - 1)) { # i: 施設種別 (図書館除く)
  for (j in 1 : length(distance1[[i]])) { # j: メッシュ
    for (k in 1 : length(distance1[[i]][[j]])) { # k: 施設
      if (i == 2) { # 病院
        s <- medical_facility1[[k]]$hospital_bed
      } else if (i == 9) { # スーパー
        s <- commercial_facility1[[k]]$area_m2
      } else {
        if (i >= 3 && i <= 8) {
          i2 <- i - 1
        } else if (i == 10) {
          i2 <- 8
        } else {
          i2 <- i
        }
        
        s <- public_facility1[[i2]][[k]]$area_m2
      }
    
      distance2[[i]][[j]][[k]]$huff <- s / distance1[[i]][[j]][[k]]$distance ^ x[i]
    }
    
    sum <- 0
    for (k in 1 : length(distance1[[i]][[j]])) { # k: 施設
      sum <- sum + distance2[[i]][[j]][[k]]$huff
    }
    
    for (k in 1 : length(distance1[[i]][[j]])) { # k: 施設
      distance2[[i]][[j]][[k]]$use_rate <- distance2[[i]][[j]][[k]]$huff / sum
    }
    
    # メッシュ上の利用者数合計
    user <- 1 : length(population_2015_1[[j]]) %>%
      map_dbl(function(k) { # k : 年齢
        population_2015_1[[j]][[k]]$population * use_frequency1[[i]][[k]]$use_frequency
      }) %>%
      sum()
    
    for (k in 1 : length(distance1[[i]][[j]])) { # k: 施設
      distance2[[i]][[j]][[k]]$user <- user * distance2[[i]][[j]][[k]]$use_rate
      
      distance2[[i]][[j]][[k]]$userXdistance <- distance2[[i]][[j]][[k]]$user * distance1[[i]][[j]][[k]]$distance
    }
  }
}

# 図書館
for (j in 1 : length(distance1[[10]])) { # j: メッシュ
  distance_min <- distance1[[10]][[j]][[1]]$distance
  
  for (k in 2 : length(distance1[[10]][[j]])) { # k: 施設
    if (distance1[[10]][[j]][[k]]$distance < distance_min) {
      distance_min <- distance1[[10]][[j]][[k]]$distance
    }
  }
  
  for (k in 1 : length(distance1[[10]][[j]])) {
    if (distance1[[10]][[j]][[k]]$distance == distance_min) {
      distance2[[10]][[j]][[k]]$use_rate <- 1
    } else {
      distance2[[10]][[j]][[k]]$use_rate <- 0
    }
  }
  
  # メッシュ上の利用者数合計
  user <- 1 : length(population_2015_1[[j]]) %>%
    map_dbl(function(k) { # k : 年齢
      population_2015_1[[j]][[k]]$population * use_frequency1[[10]][[k]]$use_frequency
    }) %>%
    sum()
  
  for (k in 1 : length(distance1[[10]][[j]])) { # k: 施設
    distance2[[10]][[j]][[k]]$user <- user * distance2[[10]][[j]][[k]]$use_rate
    
    distance2[[10]][[j]][[k]]$userXdistance <- distance2[[10]][[j]][[k]]$user * distance1[[10]][[j]][[k]]$distance
  }
}

use_rate_output <- read_csv("old/distance.csv") %>%
  select(-distance) %>%
  mutate(use_rate = NA, user = NA, userXdistance = NA) %>%
  arrange(mesh_code, type, num) %>%
  nest(-type, -mesh_code) %>%
  nest(-type)

for (i in 1 : length(distance1)) { # i: 施設種別
  for (j in 1 : length(distance1[[i]])) { # j: メッシュ
    for (k in 1 : length(distance1[[i]][[j]])) { # k: 施設
      use_rate_output[i,]$data[[1]][j,]$data[[1]][k,]$use_rate <- distance2[[i]][[j]][[k]]$use_rate
      use_rate_output[i,]$data[[1]][j,]$data[[1]][k,]$user <- distance2[[i]][[j]][[k]]$user
      use_rate_output[i,]$data[[1]][j,]$data[[1]][k,]$userXdistance <- distance2[[i]][[j]][[k]]$userXdistance
    }
  }
}

use_rate_output1 <- use_rate_output %>%
  unnest(data) %>%
  unnest(data)

use_rate_disp <- use_rate_output1 %>%
  nest(-type, -mesh_code) %>%
  nest(-type)

for (i in 1 : length(distance1)) { # i: 施設種別
  use_rate_disp[i,]$data[[1]] %<>%
    mutate(disp = NA)
  
  for (j in 1 : length(distance1[[i]])) { # j: メッシュ
    use_rate_disp[i,]$data[[1]][j,]$disp <- use_rate_disp[i,]$data[[1]][j,]$data[[1]]$use_rate %>% var()
  }
}

use_rate_disp1 <- use_rate_disp %>%
  unnest(data) %>%
  select(-data)

