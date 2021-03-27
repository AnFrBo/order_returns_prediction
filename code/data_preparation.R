setwd("/Users/Bisa/Documents/Studium/Masterstudium/3. Semester/BADS/Challenge")

##################################################################################################################
######################################### DATA CLEANING & PREPARATION ############################################
##################################################################################################################

###data cleaning & feature engineering (known data set)

#read data
known <- read.csv("BADS_WS1920_known.csv", stringsAsFactors = F)

known$order_date <- as.Date(known$order_date)
known$user_reg_date <- as.Date(known$user_reg_date)

#-2 (NA) immer return = 0 --> delivery data = NA  (100%)
#item_color = ? (92%)

'Engineered features'
known$childrensize <- ifelse(known$item_size == "104" | known$item_size == "110" |
                               known$item_size == "116" | known$item_size == "122" |
                               known$item_size == "128" | known$item_size == "134" |
                               known$item_size == "140" | known$item_size == "146" |
                               known$item_size == "152" | known$item_size == "158" |
                               known$item_size == "164" | known$item_size == "170" |
                               known$item_size == "176", 1, 0)
#79% return == 0

known$nodelivery <- ifelse(is.na(known$delivery_date), 1, 0)
#100% return == 0


known$present <- ifelse(known$item_price == 0, 1, 0)
#88% return == 0

known$waitingtime <- ifelse(as.Date(known$delivery_date) - as.Date(known$order_date) < 0, 1, 0)
#62% return == 0

known$lowprice <- ifelse(known$item_price <= 23.9, 1, 0)
#<= 14.45 80% return == 0; 2.500 obs.
#<= 16.9 75% return == 0; 5.200 obs.
#<= 19 74% return == 0; 5.500 obs.
#<= 23.9 70% return == 0; 11.500 obs. (included)

#top brands (all with more than 4000 obs.)
'sort(table(known$brand_id), decreasing = T)
known$topbrands <- ifelse(known$brand_id == 3 | known$brand_id == 1 | known$brand_id == 5 |
                            known$brand_id == 37 | known$brand_id == 11, 1, 0)
#56% return == 0
known <- known[,-22]'

#old people send back less
known$user_dob <- as.Date(as.character(known$user_dob), '%Y-%m-%d')
known$age <- round(((as.Date(Sys.Date())-known$user_dob)/365), digits = 0)

known$oldppl <- ifelse(known$age > 80 & known$age < 100, 1, 0)
table(known$oldppl, known$return)
#63% return == 0

known$agegroups <- ifelse(known$age < 30, "below 30", 
                          ifelse(known$age >= 30 & known$age < 40, "30s",
                                 ifelse(known$age >= 40 & known$age < 50, "40s",
                                        ifelse(known$age >= 50 & known$age < 60, "50s",
                                               ifelse(known$age >= 60 & known$age < 70, "60s",
                                                      ifelse(known$age >= 70, "above 69", 0))))))

#number of items per order
known$ordid <- paste(as.character(known$order_date), as.character(known$user_id),
                     sep = " - ")
noitems <- table(known$ordid)
known$noitems <- noitems[match(known$ordid, names(noitems))]

#number of orders per user
noorders <-plyr::count(known, vars = c("user_id", "order_date"))
noorders <- table(noorders$user_id)
known$noorders <- noorders[match(known$user_id, names(noorders))]

#orders close to birthday
known$delivery_date <- as.Date(known$delivery_date)

known$mdorder <- strftime(known$order_date,"%m%d")
known$mdorder <- as.numeric(known$mdorder)

known$mddelivery <- strftime(known$delivery_date,"%m%d")
known$mddelivery <- as.numeric(known$mddelivery)

known$mdbirthd <- strftime(known$user_dob,"%m%d")
known$mdbirthd <- as.numeric(known$mdbirthd)

known$preorders <- known$mdbirthd-known$mddelivery

known$prebd <- ifelse((known$preorders < 0 & known$preorders > -30) |
                        (known$mdbirthd >= 1215 & known$mddelivery <= 115) , 1, 0)

#christmas order (FEHLER)
known$xmas <- ifelse(known$order_date >= as.Date("2016-12-01"), 1,0)

#new years eve order (FEHLER)
known$newyears <- ifelse(known$order_date >=1200 & known$delivery_date <= 102, 1,0)

#gender
#female
known$female <- ifelse(known$user_title == "Mrs", 1, 0)

#male
known$male <- ifelse(known$user_title == "Mr", 1, 0)

#time diff between orders
known <- known[order(known$user_id, known$order_date) , ]
known$difforder <- 0
for(r in 2:nrow(known)) {
  if(known[r-1, "user_id"] == known[r, "user_id"]) {
    known[r, "difforder"] <- known[r, "order_date"] - known[r-1, "order_date"]
  }
}

known$difforder <- ifelse(known$difforder !=0 & known$difforder < 30 &
                            known$difforder > 7, 1, 0)

'Feature Selection'

known$user_reg_date <- as.Date(known$user_reg_date)
known$age <- as.numeric(known$age)
known$noitems <- as.numeric(known$noitems)
known$noorders <- as.numeric(known$noorders)

known_selected <- known[, c("order_item_id", "item_id", "item_price",
                            "user_reg_date", "return", "childrensize", "nodelivery",
                            "present", "waitingtime", "lowprice", "xmas", "newyears",
                            "age", "oldppl", "noitems", "noorders",
                            "prebd", "female", "male",
                            "difforder", "preorders")]


'Missing values'
table(is.na(known_selected$waitingtime))
table(is.na(known_selected$agegroups))
table(is.na(known_selected$oldppl))
table(is.na(known_selected$prebd))

known_selected$age[is.na(known_selected$age)] <- -99
known_selected$oldppl[is.na(known_selected$oldppl)] <- -99
known_selected$waitingtime[is.na(known_selected$waitingtime)] <- -99
known_selected$prebd[is.na(known_selected$prebd)] <- -99
known_selected$xmas[is.na(known_selected$xmas)] <- -99
known_selected$newyears[is.na(known_selected$newyears)] <- -99
known_selected$preorders[is.na(known_selected$preorders)] <- -99

known_clean <- known_selected

write.csv(known_clean, file = "known_clean.csv")

###preparation of unknown dataset (do the same with it as with known dataset, exclude return in feature selection)

unknown <- read.csv("BADS_WS1920_unknown.csv", stringsAsFactors = F)

unknown$order_date <- as.Date(unknown$order_date)
unknown$user_reg_date <- as.Date(unknown$user_reg_date)

#-2 (NA) immer return = 0 --> delivery data = NA  (100%)
#item_color = ? (92%)

'Engineered features'
unknown$childrensize <- ifelse(unknown$item_size == "104" | unknown$item_size == "110" |
                                 unknown$item_size == "116" | unknown$item_size == "122" |
                                 unknown$item_size == "128" | unknown$item_size == "134" |
                                 unknown$item_size == "140" | unknown$item_size == "146" |
                                 unknown$item_size == "152" | unknown$item_size == "158" |
                                 unknown$item_size == "164" | unknown$item_size == "170" |
                                 unknown$item_size == "176", 1, 0)
#79% return == 0

unknown$nodelivery <- ifelse(is.na(unknown$delivery_date), 1, 0)
#100% return == 0


unknown$present <- ifelse(unknown$item_price == 0, 1, 0)
#88% return == 0

unknown$waitingtime <- ifelse(as.Date(unknown$delivery_date) - as.Date(unknown$order_date) < 0, 1, 0)
#62% return == 0

unknown$lowprice <- ifelse(unknown$item_price <= 23.9, 1, 0)
#<= 14.45 80% return == 0; 2.500 obs.
#<= 16.9 75% return == 0; 5.200 obs.
#<= 19 74% return == 0; 5.500 obs.
#<= 23.9 70% return == 0; 11.500 obs. (included)

#old people send back less
unknown$user_dob <- as.Date(as.character(unknown$user_dob), '%Y-%m-%d')
unknown$age <- round(((as.Date(Sys.Date())-unknown$user_dob)/365), digits = 0)

unknown$oldppl <- ifelse(unknown$age > 80 & unknown$age < 100, 1, 0)
#63% return == 0

unknown$agegroups <- ifelse(unknown$age < 30, "below 30", 
                            ifelse(unknown$age >= 30 & unknown$age < 40, "30s",
                                   ifelse(unknown$age >= 40 & unknown$age < 50, "40s",
                                          ifelse(unknown$age >= 50 & unknown$age < 60, "50s",
                                                 ifelse(unknown$age >= 60 & unknown$age < 70, "60s",
                                                        ifelse(unknown$age >= 70, "above 69", 0))))))

#number of items per order
unknown$ordid <- paste(as.character(unknown$order_date), as.character(unknown$user_id),
                       sep = " - ")
noitems <- table(unknown$ordid)
unknown$noitems <- noitems[match(unknown$ordid, names(noitems))]

#number of orders per user
noorders <-plyr::count(unknown, vars = c("user_id", "order_date"))
noorders <- table(noorders$user_id)
unknown$noorders <- noorders[match(unknown$user_id, names(noorders))]

#orders close to birthday
unknown$delivery_date <- as.Date(unknown$delivery_date)

unknown$mdorder <- strftime(unknown$order_date,"%m%d")
unknown$mdorder <- as.numeric(unknown$mdorder)

unknown$mddelivery <- strftime(unknown$delivery_date,"%m%d")
unknown$mddelivery <- as.numeric(unknown$mddelivery)

unknown$mdbirthd <- strftime(unknown$user_dob,"%m%d")
unknown$mdbirthd <- as.numeric(unknown$mdbirthd)

unknown$preorders <- unknown$mdbirthd-unknown$mddelivery

unknown$prebd <- ifelse((unknown$preorders < 0 & unknown$preorders > -30) |
                          (unknown$mdbirthd >= 1215 & unknown$mddelivery <= 115) , 1, 0)

#christmas order (FEHLER)
unknown$xmas <- ifelse(unknown$order_date >= as.Date("2016-12-01"), 1,0)

#new years eve order (FEHLER)
unknown$newyears <- ifelse(unknown$order_date >=1200 & unknown$delivery_date <= 102, 1,0)

#gender
#female
unknown$female <- ifelse(unknown$user_title == "Mrs", 1, 0)

#male
unknown$male <- ifelse(unknown$user_title == "Mr", 1, 0)

#time diff between orders
unknown <- unknown[order(unknown$user_id, unknown$order_date) , ]
unknown$difforder <- 0
for(r in 2:nrow(unknown)) {
  if(unknown[r-1, "user_id"] == unknown[r, "user_id"]) {
    unknown[r, "difforder"] <- unknown[r, "order_date"] - unknown[r-1, "order_date"]
  }
}

unknown$difforder <- ifelse(unknown$difforder !=0 & unknown$difforder < 30 &
                              unknown$difforder > 7, 1, 0)

'Feature Selection'

unknown$user_reg_date <- as.Date(unknown$user_reg_date)
unknown$age<- as.numeric(unknown$age)
unknown$noitems <- as.numeric(unknown$noitems)
unknown$noorders <- as.numeric(unknown$noorders)

unknown_selected <- unknown[, c("order_item_id", "item_id", "item_price",
                                "user_reg_date", "childrensize", "nodelivery",
                                "present", "waitingtime", "lowprice", "xmas", "newyears",
                                "age", "oldppl", "noitems", "noorders",
                                "prebd", "female", "male",
                                "difforder", "preorders")]


'Missing values'
table(is.na(unknown_selected$waitingtime))
table(is.na(unknown_selected$agegroups))
table(is.na(unknown_selected$oldppl))
table(is.na(unknown_selected$prebd))

unknown_selected$age[is.na(unknown_selected$age)] <- -99
unknown_selected$oldppl[is.na(unknown_selected$oldppl)] <- -99
unknown_selected$waitingtime[is.na(unknown_selected$waitingtime)] <- -99
unknown_selected$prebd[is.na(unknown_selected$prebd)] <- -99
unknown_selected$xmas[is.na(unknown_selected$xmas)] <- -99
unknown_selected$newyears[is.na(unknown_selected$newyears)] <- -99
unknown_selected$preorders[is.na(unknown_selected$preorders)] <- -99

unknown_clean <- unknown_selected

write.csv(unknown_clean, file = "unknown_clean.csv")
