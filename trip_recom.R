library(stringr)
library(dplyr)
library(readr)

# trip rec

# install.packages('rsconnect')
s_t = read.csv("/students/dvchuprina/proect_kutezh/sh_trip_text.csv")
sh_tr = read.csv("/students/dvchuprina/proect_kutezh/sh_tr.csv")
sh_tr = select(sh_tr, link, query_n, address, lng,lat,price_min)
sh_tr = unique(sh_tr)
sh = left_join(s_t, sh_tr, by = "query_n")

sh = select(sh,link, query_n, address, lng, lat, price_min, rate, review_auth_id)
sh$city = "Shanghai"

p_t = read.csv("/students/dvchuprina/proect_kutezh/p_tr.csv")
pe = select(p_t,link, query_n, address, lng, lat, price_min, rate, review_auth_id)
pe$city = "Beijing"

# pekin <- read_csv("~/proect_kutezh/pekin.csv")
trip = rbind(sh, pe)
# write.csv(trip, "trip_full.csv", row.names = F)
trip <- read_csv("~/proect_kutezh/trip_full.csv")

t = trip %>% group_by(review_auth_id) %>% dplyr::summarise(count = n()) %>% filter(count >5)
t3 = left_join(t, trip, by = "review_auth_id")
t3 = select(t3, -count)

t3 = unique(t3)
t4 = t3[!is.na(t3$lng),]
t4 = t4[!is.na(t4$review_auth_id),]
# write.csv(t4, "trip_test.csv", row.names = F)



eu_g = read.csv("/students/dvchuprina/proect_kutezh/eu_guys.csv")
eu_g = select(eu_g, -Spaniard)
names(eu_g) = c("restId", "ital", "rus","amer","swed","ger")

# adding num - new restID
t4 = read.csv("/students/dvchuprina/proect_kutezh/trip_test.csv")
t4$price_min[is.na(t4$price_min)] = 1000
h = t4 %>% select(num, review_auth_id, rate)
h = unique(h)

h1 = h %>% group_by(num, review_auth_id) %>% dplyr::summarise(rate = mean(rate))

names(h1) = c("restId", "revId","stars")
h3 = h1 %>%
  ungroup() %>%
  select(restId)


h2 = inner_join(h3, eu_g, by = "restId")
h2 = unique(h2)



library(tidyr)
h11 = h1 %>% ungroup()
rates_t = spread(h11, key = restId, value = stars)

rownames(rates_t) = rates_t$revId
rates_t = select(rates_t, -revId)


# chinese dudes
# eu_guys = read.csv("/students/dvchuprina/proect_kutezh/eu_guys.csv")
eu_guys = h2
# names(eu_guys) = c("restId", "ital", "rus","amer","swed","ger", "span")

rownames(eu_guys) = eu_guys$restId
eu_guys = select(eu_guys, -restId)
eu_guys = as.data.frame(t(eu_guys))


eu = plyr::rbind.fill(rates_t, eu_guys)



train_t = eu[-c(313,314,315,316,317),]
test_t = eu[c(313,314,315,316,317),]


library(recommenderlab)
train_t = as.matrix(train_t)
tr_t = as(train_t, "realRatingMatrix")

test_t = as.matrix(test_t)
te_t = as(test_t, "realRatingMatrix")


recc_model_t <- Recommender(data = tr_t, method = "UBCF")
recc_model_t

# recommending

recc_predicted_t <- predict(object = recc_model_t, newdata = te_t, n = 10)
recc_predicted_t
str(recc_predicted_t)

# prediction for italian
recc_user_1 <- recc_predicted_t@items
# finding rests' names
recc_user_1 <- recc_predicted_t@items[[1]]
rest_user1 <- recc_predicted_t@itemLabels[recc_user_1]

recc_user_2 <- recc_predicted_t@items[[2]]
rest_user2 <- recc_predicted_t@itemLabels[recc_user_2]

recc_user_3 <- recc_predicted_t@items[[3]]
rest_user3 <- recc_predicted_t@itemLabels[recc_user_3]

recc_user_4 <- recc_predicted_t@items[[4]]
rest_user4 <- recc_predicted_t@itemLabels[recc_user_4]

recc_user_5 <- recc_predicted_t@items[[5]]
rest_user5 <- recc_predicted_t@itemLabels[recc_user_5]

a = as.data.frame(cbind(rest_user1,rest_user2,rest_user3,rest_user4,rest_user5))
names(a) = c("ital", "rus","amer","swed","ger")
aa = as.data.frame(t(as.matrix(a)))
aa <- data.frame(names = row.names(aa), aa)
rownames(aa) <- NULL
a1 = aa
a1 = gather(a1, names1, num, 2:11)
a1 =select(a1, -names1)

t4$num = as.character(t4$num)

dudes_rec_full = left_join(a1, t4, by = "num")
dudes_rec_full = dudes_rec_full[!is.na(dudes_rec_full$lng),]


write.csv(dudes_rec_full, "trip_b_dudes.csv", row.names = F)
