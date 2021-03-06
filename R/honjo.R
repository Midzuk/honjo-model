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
        select(huff, use_rate, user, userXdistance, distance)
    }
  }
}

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
  mutate(use_rate = NA, user = NA, userXdistance = NA) %>%
  arrange(mesh_code, type, num) %>%
  nest(-type, -mesh_code) %>%
  nest(-type)

for (i in 1 : length(distance1)) { # i: 施設種別
  for (j in 1 : length(distance1[[i]])) { # j: メッシュ
    for (k in 1 : length(distance1[[i]][[j]])) { # k: 施設
      use_rate_output$data[[i]]$data[[j]]$use_rate[[k]] <- distance2[[i]][[j]][[k]]$use_rate
      use_rate_output$data[[i]]$data[[j]]$user[[k]] <- distance2[[i]][[j]][[k]]$user
      use_rate_output$data[[i]]$data[[j]]$userXdistance[[k]] <- distance2[[i]][[j]][[k]]$userXdistance
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
  use_rate_disp$data[[i]] %<>%
    mutate(disp = NA)
  
  for (j in 1 : length(distance1[[i]])) { # j: メッシュ
    use_rate_disp$data[[i]]$disp[[j]] <- use_rate_disp$data[[i]]$data[[j]]$use_rate %>% var()
  }
}

use_rate_disp1 <- use_rate_disp %>%
  unnest(data) %>%
  select(-data)

# cost <- tibble(mesh = mesh_codes, type = list(facility_type)) %>% unnest(type)

type <- tibble(type = facility_type, num = list(0:1, 0:5, 0:1, 0:12, 0:2, 0:3, 0:2, 0:5, 0:4, 0:1)) %>%
  unnest(num) %>%
  mutate(cost = NA) %>%
  nest(cost) %>%
  nest(-type)

# cost %<>%
#   left_join(type, by = "type") %>%
#   unnest(num)

use_rate_temp <- use_rate_output1 %>%
  nest(distance, use_rate, user, userXdistance) %>%
  nest(-num, -type) %>%
  nest(-type)

for (i in 1 : nrow(use_rate_temp)) { # i: 施設種別
  for (j in 1 : nrow(use_rate_temp$data[[i]])) { # j: 施設
    sum <- 0
    
    for (k in 1 : nrow(use_rate_temp$data[[i]])) { # k: 施設
      for (l in 1 : nrow(use_rate_temp$data[[i]]$data[[k]])) { # l: メッシュ
        
        if (j == k) {
          userXdistance <- 0
        } else if (i == 10 && use_rate_temp$data[[i]]$data[[k]]$data[[l]]$user == 0) {
          user <- use_rate_temp$data[[i]]$data[[j]]$data[[l]]$user
          userXdistance <- user * use_rate_temp$data[[i]]$data[[k]]$data[[l]]$distance
        } else {
          user <-
            use_rate_temp$data[[i]]$data[[k]]$data[[l]]$user / (1 - use_rate_temp$data[[i]]$data[[j]]$data[[l]]$use_rate)
          userXdistance <- user * use_rate_temp$data[[i]]$data[[k]]$data[[l]]$distance
        }
        
        sum <- sum + userXdistance
      }
    }
    
    type$data[[i]]$data[[j]]$cost <- sum
  }
}

type1 <- type %>%
  unnest(data) %>%
  unnest(data)

cost <- tibble(type = facility_type, cost = NA)
use_rate_output2 <- use_rate_output1 %>%
  nest(-type)

for (i in 1 : nrow(use_rate_temp)) {
  cost$cost[[i]] <- use_rate_output2$data[[i]]$userXdistance %>%
    sum()
}

user <- tibble(type = facility_type, user = NA)
for (i in 1 : nrow(use_rate_temp)) {
  user$user[[i]] <- use_rate_output2$data[[i]]$user %>%
    sum()
}


cost %<>%
  mutate(cost_before = cost) %>%
  select(-cost)

type1 %<>%
  mutate(cost_after = cost) %>%
  select(-cost)

type2 <- type1 %>%
  left_join(cost, by = "type") %>%
  mutate(cost_delta = cost_after - cost_before) %>%
  left_join(user, by = "type") %>%
  mutate(cost_delta_per_capita = cost_delta / user)


# 総移動距離最小化
# 最小化対象となる関数

facility <- tibble(type = facility_type, num = list(0:1, 0:5, 0:1, 0:12, 0:2, 0:3, 0:2, 0:5, 0:4, 0:1))
area_total <- tibble(type = facility_type, area = NA)



for (i in 1:length(facility_type)) { # i: 施設種別
  sum <- 0
  
  for (j in facility$num[[i]] + 1) { # j: 施設番号
    if (i == 2) { # 病院
      sum <- sum + medical_facility1[[j]]$hospital_bed
    } else if (i == 9) { # スーパー
      sum <- sum + commercial_facility1[[j]]$area_m2
    } else {
      if (i >= 3 && i <= 8) {
        i2 <- i - 1
      } else if (i == 10) {
        i2 <- 8
      } else {
        i2 <- i
      }
      
      sum <- sum + public_facility1[[i2]][[j]]$area_m2
    }
  }
  
  area_total$area[[i]] <- sum
}

par <- c(0.6398631, 2.010507, 0.853016, 1.209198, 0, 1.738493, 0, 0.8526133, 0) # 変数

calc_total_distance <- function(i, x) { # i: 施設種別, x: 施設規模(面積, 病床数)
  res <- 0
  
  for (j in 1:length(mesh_codes)) { # j: メッシュ
    huff_sum <- 0
    
    for (k in facility$num[[i]] + 1) { # k: 施設番号
      if (k == length(facility$num[[i]])) {
        huff_sum <- huff_sum + (area_total$area[i] - sum(x)) / (distance2[[i]][[j]][[k]]$distance ^ par[i])
      } else {
        huff_sum <- huff_sum + x[k] / (distance2[[i]][[j]][[k]]$distance ^ par[i])
      }
    }
    
    user <- 1 : length(population_2015_1[[j]]) %>%
      map_dbl(function(l) { # l : 年齢
        population_2015_1[[j]][[l]]$population * use_frequency1[[i]][[l]]$use_frequency
      }) %>%
      sum()
    
    for (k in facility$num[[i]] + 1) { # k: 施設番号
      if (k == length(facility$num[[i]])) {
        huff <- (area_total$area[i] - sum(x)) / (distance2[[i]][[j]][[k]]$distance ^ par[i]) 
      } else {
        huff <- x[k] / (distance2[[i]][[j]][[k]]$distance ^ par[i])
      }
      
      res <- res + user * huff / huff_sum * distance2[[i]][[j]][[k]]$distance
    }
  }
  
  return(res)
}

g <- function(i,x) {
  if (area_total$area[i] - sum(x) < 0) {
    10 ^ 100
  } else if (length(x[x<0]) > 0) {
    10 ^ 100
  } else {
    0
  }
}

calc_total_distance1 <- function(i,x) {calc_total_distance(i,x) + g(i,x)}



optim(c(0), function (x) {calc_total_distance1(1,x)}, method = "Brent", lower = 0, upper = 8874)
# (7789.981, 1084.019)
# value: 574,785,264 m

optim(c(0,0,0,0,0), function (x) {calc_total_distance1(2,x)})
# (52.77539, 53.59706, 92.21750, 154.22759, 133.27675, 94.90571)
# value: 3,472,072,461 m

# i == 3
# (498.868, 434.132)
# value: 189,916,223 m
# (865.8884, ---)
# value: 182,429,391 m


optim(rep(0, length(facility$num[[3]]) - 1), function (x) {calc_total_distance1(3,x)}, method = "SANN")

for (i in 3:9) {
  print(paste("--------", i, "--------"))
  print(optim(rep(0, length(facility$num[[i]]) - 1), function (x) {calc_total_distance1(i,x)}, method = "SANN"))
}

for (i in 2:9) {
  print(paste("--------", i, "--------"))
  print(optim(rep(area_total$area[i] / length(facility$num[[i]]) - 1, length(facility$num[[i]]) - 1), function (x) {calc_total_distance1(i,x)}, method = "SANN"))
}

public_facility2 <- public_facility %>%
  nest(-type) %>%
  arrange(type)

for (i in 3:9) {
  print(paste("--------", i, "--------"))
  
  if (i == 2) { # 病院
    s <- medical_facility$hospital_bed[1:(length(facility$num[[i]]) - 1)]
  } else if (i == 9) { # スーパー
    s <- commercial_facility$area_m2[1:(length(facility$num[[i]]) - 1)]
  } else {
    if (i >= 3 && i <= 8) {
      i2 <- i - 1
    } else if (i == 10) {
      i2 <- 8
    } else {
      i2 <- i
    }
    
    s <- public_facility2$data[[i2]]$area_m2[1:(length(facility$num[[i]]) - 1)]
  }
  
  print(optim(s, function (x) {calc_total_distance1(i,x)}, method = "SANN"))
}

# [1] "-------- 2 --------"
# $par
# [1]  52.74168  53.58270  92.24684 154.22377 133.31243
# $value
# [1] 3472072474
# 収束 & ◎

# [1] "-------- 3 --------"
# $par
# [1] 865.8886
# $value
# [1] 182429391
# 収束 & ◎

# [1] "-------- 4 --------"
# $par
# [1] 1.188787e+03 4.153997e+02 5.782893e+02 1.087967e+03 1.936100e+03 3.902561e+02 1.077406e+02 1.557834e-04
# [9] 1.777444e-04 1.421468e+01 1.564071e+03 1.306565e+02
# $value
# [1] 807693921
# 収束 & ◎



# [1] "-------- 5 --------"
# $par
# [1] 5.889035e-06 5.391469e-06
# $value
# [1] 165786110
# 収束 & ◎


# [1] "-------- 6 --------"
# $par
# [1] 1034.0077  426.9638 1195.7450
# $value
# [1] 237117969
# 収束 & ◎


# [1] "-------- 7 --------"
# $par
# [1] 7.100000e+02 1.541293e-05
# $value
# [1] 113022067
# 収束 & ○

# [1] "-------- 8 --------"
# $par
# [1] 1.067792e+04 6.727142e-04 4.377684e-03 1.371831e+03 1.652721e+03
# $value
# [1] 1628641464
# 収束 & ○


# [1] "-------- 9 --------"
# $par
# [1] 1.823405e+04 2.006490e-04 1.674895e+04 4.958497e-04
# $value
# [1] 32339745132
# 収束 & △

for (i in 2:9) {
  print(paste("--------", i, "--------"))
  
  print(optim(rep(area_total$area[i] / length(facility$num[[i]]) - 1, length(facility$num[[i]]) - 1), function (x) {calc_total_distance1(i,x)}, method = "SANN"))
}

par1 <- list(c(52.80104,53.59158,92.18381,154.19881,133.28055),
            c(865.8886),
            c(1.188895e+03,4.098005e+02,5.790303e+02,1.085774e+03,1.937060e+03,3.900983e+02,1.106556e+02,1.512622e-03,1.878734e-03,1.321827e+01,1.571621e+03,1.291475e+02),
            c(0.0016889395,0.0003616966),
            c(1034.0077,426.9638,1195.7450),
            c(643.34234,66.64962),
            c(2827.4528,480.6773,1473.0287,1210.4129,7524.9970),
            c(11422.685,5267.759,13213.767,1592.885))

for (i in 9:3) {
  j <- 0
  k <- TRUE
  
  ans <- optim(par1[[i-1]], function (x) {calc_total_distance1(i,x)}, method = "Nelder-Mead")
  
  while ((j < 30) && k) {
    if (ans$convergence == 0) {
      k <- FALSE
    }
    
    ans <- optim(ans$par, function (x) {calc_total_distance1(i,x)}, method = "Nelder-Mead")
    
    j <- j + 1
  }
  
  print(paste("--------", i, "--------"))
  print(ans)
}

for (i in 9:4) {
  j <- 0
  k <- TRUE
  
  ans <- optim(rep(0, length(par1[[i-1]])), function (x) {calc_total_distance1(i,x)}, method = "Nelder-Mead")
  
  while ((j < 30) && k) {
    if (ans$convergence == 0) {
      k <- FALSE
    }
    
    ans <- optim(ans$par, function (x) {calc_total_distance1(i,x)}, method = "Nelder-Mead")
    
    j <- j + 1
  }
  
  print(paste("--------", i, "--------"))
  print(ans)
}

par <- c(0.6398631, 2.010507, 0.853016, 1.209198, 0, 1.738493, 0, 0.8526133, 0) # 変数

par11 <- list(c(7789.981, 1084.019),
              c(52.74168, 53.58270, 92.24684, 154.22377, 133.31243, 104.8926),
              c(865.8886, 67.1114),
              c(1.188787e+03, 4.153997e+02, 5.782893e+02, 1.087967e+03, 1.936100e+03, 3.902561e+02, 1.077406e+02, 1.557834e-04, 1.777444e-04, 1.421468e+01, 1.564071e+03, 1.306565e+02, 129.5178),
              c(5.889035e-06, 5.391469e-06, 815),
              c(1034.0077,426.9638,1195.7450, 51.2835),
              c(7.100000e+02,1.541293e-05, 1.541293e-05),
              c(1.067792e+04,6.727142e-04,4.377684e-03,1.371831e+03,1.652721e+03,3377.523),
              c(1.823405e+04,2.006490e-04,1.674895e+04,4.958497e-04,0))

use_rate_output3 <- use_rate_output

for (i in 1:(length(use_rate_output3$type)-1)) {
  for (j in 1:length(use_rate_output3$data[[i]]$mesh_code)) {
    use_rates <- rep(0, nrow(use_rate_output3$data[[i]]$data[[j]]))
    us <- rep(0, nrow(use_rate_output3$data[[i]]$data[[j]]))
    
    for (k in 1:nrow(use_rate_output3$data[[i]]$data[[j]])) {
      us[k] <- par11[[i]][k] / distance1[[i]][[j]][[k]]$distance ^ par[i]
    }
    
    use_rates <- us / sum(us)
    
    use_rate_output3$data[[i]]$data[[j]]$use_rate <- use_rates
  }
}
# xは正のみ
# ハフモデル(対数じゃない版)
calc_use_rate2 <- function(i) {
  users <- rep(0, length(distance1[[i]][[1]])) # 汚い
  use_rates <- rep(0, length(distance1[[i]][[1]])) # 汚い
  
  for (j in 1 : length(distance1[[i]])) { # j : メッシュ
    sum = 0
    us <- rep(0, length(distance1[[i]][[j]]))
    
    for (k in 1 : length(distance1[[i]][[j]])) { # k : 施設番号
      s <- par11[[i]][k]
      
      us[k] <- s / distance1[[i]][[j]][[k]]$distance ^ par[i]
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


use_rate_output4 <- use_rate_output3 %>%
  unnest(data) %>%
  unnest(data) %>%
  select(-distance, -user, -userXdistance)

use_rate_disp2 <- use_rate_output4 %>%
  nest(-type, -mesh_code) %>%
  nest(-type)

for (i in 1 : 9) { # i: 施設種別
  use_rate_disp2$data[[i]] %<>%
    mutate(disp = NA)
  
  for (j in 1 : length(distance1[[i]])) { # j: メッシュ
    use_rate_disp2$data[[i]]$disp[[j]] <- use_rate_disp2$data[[i]]$data[[j]]$use_rate %>% var()
  }
}

use_rate_disp3 <- use_rate_disp2 %>%
  unnest(data) %>%
  select(-data)

use_rate_disp3 %<>%
  mutate(sd = sqrt(disp))

# このあたり注意
use_rate_disp3 %<>% bind_rows(use_rate_disp11)

use_rate_disp3 %<>%
  nest(-mesh_code)

use_rate_disp3 %<>%
  mutate(sd_average = NA)

for (i in 1:nrow(use_rate_disp3)) {
  use_rate_disp3$sd_average[[i]] <- use_rate_disp3$data[[i]]$sd %>%
    mean()
}

use_rate_disp3 %<>%
  select(-data)

facility_area <- facility %>%
  unnest(num) %>%
  mutate(area_before = NA) %>%
  nest(-type)


for (i in 1:10) {
  if (i == 2) { # 病院
    facility_area$data[[i]] <- medical_facility$hospital_bed[1:length(facility$num[[i]])]
  } else if (i == 9) { # スーパー
    facility_area$data[[i]] <- commercial_facility$area_m2[1:length(facility$num[[i]])]
  } else {
    if (i >= 3 && i <= 8) {
      i2 <- i - 1
    } else if (i == 10) {
      i2 <- 8
    } else {
      i2 <- i
    }
    
    facility_area$data[[i]] <- public_facility2$data[[i2]]$area_m2[1:length(facility$num[[i]])]
  }
}

facility_area %<>%
  left_join(facility, by = "type") %>%
  unnest(data,num)

facility_area %<>%
  mutate(area_after = NA)

facility_area1 <- facility_area %>%
  nest(-type)

for (i in 1:9) {
  facility_area1$data[[i]]$area_after <- par11[[i]]
}

facility_area2 <- facility_area1 %>%
  unnest(data)

facility_area2 %<>%
  mutate(rat = area_after / data)
