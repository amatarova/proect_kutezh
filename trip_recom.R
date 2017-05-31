library(stringr)
library(dplyr)
library(readr)

# trip rec

install.packages('rsconnect')
p_t = read.csv("/students/dvchuprina/proect_kutezh/pekin_text_1.csv")
name = select(p_t, number, title)
names(name) = c("query_n", "title")
name = unique(name)

pekin <- read_csv("~/proect_kutezh/pekin.csv")
pekin = select(pekin, query_n, lng,lat, address, price_min)
pek = left_join(name, pekin, by = "query_n")


eu_g = read.csv("/students/dvchuprina/proect_kutezh/eu_guys.csv")
eu_g = select(eu_g, -Spaniard)
names(eu_g) = c("restId", "ital", "rus","amer","swed","ger")

h = p_t %>% select(number, review_auth_id, stars)
h = unique(h)

h1 = h %>% group_by(number, review_auth_id) %>% dplyr::summarise(rate = mean(stars))
names(h) = c("restId", "revId","stars")
h3 = select(h, restId)


h2 = inner_join(h3, eu_g, by = "restId")
h2 = unique(h2)



library(tidyr)
rates_t = spread(h1, key = number, value = rate)

rownames(rates_t) = rates_t$review_auth_id
rates_t = select(rates_t, -review_auth_id)


# chinese dudes
eu_guys = read.csv("/students/dvchuprina/proect_kutezh/eu_guys.csv")
eu_guys = h2
# names(eu_guys) = c("restId", "ital", "rus","amer","swed","ger", "span")

rownames(eu_guys) = eu_guys$restId
eu_guys = select(eu_guys, -restId)
eu_guys = as.data.frame(t(eu_guys))


eu = plyr::rbind.fill(rates_t, eu_guys)



train_t = eu[-c(6649,6650,6651,6652,6653),]
test_t = eu[c(6649,6650,6651,6652,6653),]


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
recc_user_1 <- recc_predicted_t@items[[1]]
a = as.data.frame(recc_user_1)
names(a) = "query_n"
it = left_join(a, pek, by = "query_n")
