library(stringr)
library(dplyr)
library(readr)

# chinese recommendation

user_rest = read.csv("/students/dvchuprina/proect_kutezh/user_rest.csv")

user_rest$rate = as.numeric(user_rest$rate)

u = user_rest %>% filter(rate > 0) %>% select(restId, userId, rate)

dp_bs1 = read.csv("/students/dvchuprina/proect_kutezh/location.csv")
s = select(dp_bs1, redtId, name, score, address, lng,lat,cost,tag)
names(s) = c("restId", "name", "score", "address", "lng","lat","cost","tag")
s$restId = as.character(s$restId)
u$restId = as.character(u$restId)

u = left_join(u, s, by = "restId")
u1 = select(u, userId, restId, rate)


u2 = u1 %>% group_by(userId) %>% dplyr::summarise(count = n()) %>% filter(count >50)
u3 = left_join(u2, u1, by = "userId")
u3 = select(u3, -count)

u3 = unique(u3)
# write.csv(u3, "ch_test.csv", row.names = F)


library(tidyr)
rates = spread(u3, key = restId, value = rate)
rownames(rates) = rates$userId
rates = select(rates, -userId)

# chinese dudes
kit_guys = read.csv("/students/dvchuprina/proect_kutezh/kit_guys.csv")
names(kit_guys) = c("restId", "bei", "tai","fuj","georg","mad")

rownames(kit_guys) = kit_guys$restId
kit_guys = select(kit_guys, -restId)
kit_guys = as.data.frame(t(kit_guys))

kit = plyr::rbind.fill(rates, kit_guys)

# kit = rates
# train = kit[-c(1),]
# test = kit[c(1),]

train = kit[-c(3118,3119,3120,3121,3122),]
test = kit[c(3118,3119,3120,3121,3122),]




library(recommenderlab)
train = as.matrix(train)
tr = as(train, "realRatingMatrix")

test = as.matrix(test)
te = as(test, "realRatingMatrix")



recc_model <- Recommender(data = tr, method = "UBCF")
recc_model

# recommending

recc_predicted <- predict(object = recc_model, newdata = te, n = 10)
recc_predicted
str(recc_predicted)

# finding rests' names
recc_user_1 <- recc_predicted@items[[1]]
rest_user1 <- recc_predicted@itemLabels[recc_user_1]

recc_user_2 <- recc_predicted@items[[2]]
rest_user2 <- recc_predicted@itemLabels[recc_user_2]

recc_user_3 <- recc_predicted@items[[3]]
rest_user3 <- recc_predicted@itemLabels[recc_user_3]

recc_user_4 <- recc_predicted@items[[4]]
rest_user4 <- recc_predicted@itemLabels[recc_user_4]

recc_user_5 <- recc_predicted@items[[5]]
rest_user5 <- recc_predicted@itemLabels[recc_user_5]

a = as.data.frame(cbind(rest_user1,rest_user2,rest_user3,rest_user4,rest_user5))
names(a) = c("bei", "tai","fuj","georg","mad")

aa = as.data.frame(t(as.matrix(a)))
aa <- data.frame(names = row.names(aa), aa)
rownames(aa) <- NULL
a1 = aa
a1 = gather(a1, names1, restId, 2:11)
a1 =select(a1, -names1)

s1 = select(s, -score)
s1$tag = as.character(s1$tag)
ch_rec_shiny = left_join(a1, s1, by = "restId")

# dudes_rec_full = dudes_rec_full[!is.na(dudes_rec_full$lng),]


# write.csv(ch_rec_shiny, "ch_rec_shiny.csv", row.names = F)
