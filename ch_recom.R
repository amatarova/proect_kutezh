library(stringr)
library(dplyr)
library(readr)

# chinese recommendation

user_rest = read.csv("/students/dvchuprina/proect_kutezh/user_rest.csv")

user_rest$rate = as.numeric(user_rest$rate)

u = user_rest %>% filter(rate > 0) %>% select(restId, userId, rate)

s = select(dp_bs1, redtId, name, score, address, lng,lat,cost,tag)
names(s) = c("restId", "name", "score", "address", "lng","lat","cost","tag")
s$restId = as.character(s$restId)

u = left_join(u, s, by = "restId")
u1 = select(u, userId, restId, rate)
# write.csv(u3, "ch_test.csv", row.names = F)

u2 = u1 %>% group_by(userId) %>% dplyr::summarise(count = n()) %>% filter(count >150)
u3 = left_join(u2, u1, by = "userId")
u3 = select(u3, -count)


library(tidyr)
rates = spread(u3, key = restId, value = rate)
rownames(rates) = rates$userId
rates = select(rates, -userId)

# chinese dudes
kit_guys = read.csv("/students/dvchuprina/proect_kutezh/kit_guys.csv")
names(kit_guys) = c("restId", "bei", "tai","fuj","georg","rus")

rownames(kit_guys) = kit_guys$restId
kit_guys = select(kit_guys, -restId)
kit_guys = as.data.frame(t(kit_guys))

kit = plyr::rbind.fill(rates, kit_guys)

# kit = rates
# train = kit[-c(1),]
# test = kit[c(1),]

train = kit[-c(242,243,244,245,246),]
test = kit[c(242,243,244,245,246),]



library(recommenderlab)
train = as.matrix(train)
tr = as(train, "realRatingMatrix")

test = as.matrix(test)
te = as(test, "realRatingMatrix")



recc_model <- Recommender(data = tr, method = "UBCF")
recc_model

# recommending

recc_predicted <- predict(object = recc_model, newdata = te, n = 5)
recc_predicted
str(recc_predicted)

recc_user_1 <- recc_predicted@items
recc_user_1
