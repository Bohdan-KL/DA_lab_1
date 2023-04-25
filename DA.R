#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("jsonlite")
#install.packages("ggplot2")
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)

df = read.csv('C:\\Users\\User\\Desktop\\train.csv')


#df = read.csv(
#  'C:\\Users\\maksn\\Desktop\\Study\\6 sem\\Аналіз даних\\Лаб1\\train.csv')
init_shape <- dim(df)

# Remove unnecessary columns.
df <- df[,!names(df) %in% c("thumbnail_url")]

# Count na values.
init_na_count <- sum(is.na(df))
init_na_count_df <- data.frame(
  sapply(df, function(y) sum(length(which(is.na(y)))))
)

# Replace na values in integer columns with mean.
df <- 
  df %>% mutate_if(is.numeric, ~replace_na(.,round(mean(., na.rm = TRUE), 0)))

na_count <- sum(is.na(df))
na_count_df <- data.frame(
  sapply(df, function(y) sum(length(which(is.na(y)))))
)

# Count cells with empty values.
init_empty_count <- sum(df == "")
init_empty_count_df <- data.frame(
  sapply(df, function(y) sum(length(which(y == ""))))
)

# Empty values can be replaced with
# frequent occurences in such columns:
df <- df %>% mutate(host_has_profile_pic = 
                      if_else(host_has_profile_pic == "", 
                              max(host_has_profile_pic), 
                              host_has_profile_pic)
)
df <- df %>% mutate(host_identity_verified = 
                      if_else(host_identity_verified == "", 
                              max(host_identity_verified), 
                              host_identity_verified)
)
df <- df %>% mutate(host_response_rate = 
                      if_else(host_response_rate == "", 
                              max(host_response_rate), 
                              host_response_rate)
)

df <- df %>% mutate(neighbourhood = 
                      if_else(neighbourhood == "", 
                              max(neighbourhood), 
                              neighbourhood)
)

df <- df %>% mutate(zipcode = 
                      if_else(zipcode == "", 
                              max(zipcode), 
                              zipcode)
)
# Empty values can be replaced with
# median in such columns:
df <- df %>% mutate(first_review = 
                      if_else(first_review == "", 
                              median(first_review), 
                              first_review)
)

df <- df %>% mutate(last_review = 
                      if_else(last_review == "", 
                              median(last_review), 
                              last_review)
)

df <- df %>% mutate(host_since = 
                      if_else(host_since == "", 
                              median(host_since), 
                              host_since)
)
# Re count cells with empty values.
empty_count <- sum(df == "")
empty_count_df <- data.frame(
  sapply(df, function(y) sum(length(which(y == ""))))
)

new_shape <- dim(df)

#clear end

#editing data types
df$id <- as.character(df$id)
#be careful with next line, if you run again - there will be a repeated division by 100
df$host_response_rate <- as.numeric(sub("%", "", df$host_response_rate))/100
df$first_review <-  as.Date(df$first_review, "%Y-%m-%d")
df$host_since <-  as.Date(df$host_since, "%Y-%m-%d")
df$last_review <-  as.Date(df$last_review, "%Y-%m-%d")
df$property_type <- factor(unlist(df$property_type))
df$room_type <- factor(unlist(df$room_type))
df$bathrooms <- factor(unlist(df$bathrooms))
df$bed_type <- factor(unlist(df$bed_type))
df$cancellation_policy <- factor(unlist(df$cancellation_policy))
df$city <- factor(unlist(df$city))
df$accommodates <- factor(unlist(df$accommodates))
df$bedrooms <- factor(unlist(df$bedrooms))
df$beds <- factor(unlist(df$beds))
df$cleaning_fee <- as.logical(factor(unlist(df$cleaning_fee), levels = c("False", "True")))

df$host_has_profile_pic <- gsub("t", "True", df$host_has_profile_pic)
df$host_has_profile_pic <- gsub("f", "False", df$host_has_profile_pic)
df$host_has_profile_pic <- as.logical(factor(unlist(df$host_has_profile_pic), levels = c("False", "True")))

df$host_identity_verified <- gsub("t", "True", df$host_identity_verified)
df$host_identity_verified <- gsub("f", "False", df$host_identity_verified)
df$host_identity_verified <- as.logical(factor(unlist(df$host_identity_verified), levels = c("False", "True")))

df$instant_bookable <- gsub("t", "True", df$instant_bookable)
df$instant_bookable <- gsub("f", "False", df$instant_bookable)
df$instant_bookable <- as.logical(factor(unlist(df$instant_bookable), levels = c("False", "True")))


ggplot(df, aes(x = c('Присутня', 'Відсутня'), y = c(94.279, 93.583))) +
  geom_bar(stat = "identity") +
  labs(title = "Залежність рейтингу від верифікації", x = "Верифіковано", y = "Рейтинг")

ggplot(df, aes(x = host_identity_verified, y = review_scores_rating)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Залежність рейтингу від верифікації", x = "Верифіковано", y = "Рейтинг")+
  geom_text(stat = "summary", fun = "mean", aes(label = round(..y.., 3)), vjust = -1)

corr_review_scores_ratings_host_identity_verified <- cor(df$review_scores_rating, df$host_identity_verified)
ggplot(df,aes(x=host_identity_verified, y=review_scores_rating)
) + geom_col() + xlab('Верифікація') + ylab('Рейтинг') +
  ggtitle(
    sprintf(
      'Залежність рейтингу від верифікації. R = %.3f',
      corr_review_scores_ratings_host_identity_verified
    )
  )


ggplot(cleaned_df, aes("", y = log_price))+
  geom_boxplot()+
  ggtitle("Боксплот ціни після очищення")+
  ylab("Ціна")

ggplot(cleaned_2_df, aes("", y = review_scores_rating))+
  geom_boxplot()+
  ggtitle("Боксплот рейтинг після очищення")+
  ylab("Рейтинг")

q1 <- quantile(df$log_price, 0.25)
q3 <- quantile(df$log_price, 0.75)
upper <- q3 + 1.5 * (q3 - q1)
lower <- q1 - 1.5 * (q3 - q1)
cleaned_df <- df[df$log_price >= lower & df$log_price <= upper,]

q1_2 <- quantile(df$review_scores_rating, 0.25)
q3_2 <- quantile(df$review_scores_rating, 0.75)
upper <- q3_2 + 1.5 * (q3_2 - q1_2)
lower <- q1_2 - 1.5 * (q3_2 - q1_2)
cleaned_2_df <- df[df$review_scores_rating >= lower & df$review_scores_rating <= upper,]

#descriptive statistics
str(cleaned_2_df)
summary(cleaned_2_df)


data_1 <- gsub(",\"", ":", df$amenities, fixed = TRUE)
data_1 <- gsub(",\"", ":", data_1, fixed = TRUE)
data_1 <- gsub("\"", ":", data_1, fixed = TRUE)
data_1 <- gsub(":,", ":", data_1, fixed = TRUE)
data_1 <- gsub("::", ":", data_1, fixed = TRUE)
data_1 <- gsub(": ", ":", data_1, fixed = TRUE)
data_1 <- gsub(":}", "}", data_1, fixed = TRUE)
data_1 <- gsub("{", "", data_1, fixed = TRUE)
data_1 <- gsub("}", "", data_1, fixed = TRUE)

unique_values <- unique(unlist(strsplit(data_1, ":")))

communication <- c("Wireless Internet", "TV", "Internet", "Cable TV", "TV,Internet", "Ethernet connection")
security <- c("Smoke detector", "Carbon monoxide detector", "Fire extinguisher", "First aid kit", "Safety card", 
              "Lock on bedroom door", "Elevator in building", "Lockbox", "Smartlock", "Smart lock", "Window guards")
Air_conditioning <- c("Air conditioning")
friendly_workspace <- c("Laptop friendly workspace", "Keypad")
translation_missing <- c("translation missing")
bathroom_amenities <- c("Hair dryer", "Iron", "Essentials,Shampoo", "Washer,Dryer", "Essentials,Shampoo,Hangers", "Essentials",
                        "Heating,Washer,Dryer", "Bathtub", "Shampoo", "Essentials,Shampoo,Hangers,Iron", "Essentials,Hangers,Iron",
                        "Shampoo,Hangers", "Washer,Dryer,Essentials,Shampoo", "Hot tub")
family_children <- c("Family/kid friendly", "Long term stays allowed", "Children’s books and toys", "Babysitter recommendations",
                     "Children’s dinnerware", )
kitchen_amenities <- c("Kitchen", "Kitchen,Heating", "Pool,Kitchen", "Dishes and silverware", "Cooking basics", "Oven,Stove",
                       "Coffee maker", "Microwave", "Kitchen,Heating,Washer,Dryer", "Kitchen,Elevator", "Breakfast", 
                       "Refrigerator,Dishwasher", "Refrigerator", "Breakfast,Heating", "Kitchen,Breakfast", "Kitchen,Elevator,Heating",
                       "Kitchen,Breakfast,Heating", "BBQ grill", "Children’s dinnerware", "Kitchen,Elevator,Heating,Washer,Dryer",
                       "Kitchen,Doorman,Elevator")
heating_heat <- c("Heating", "Kitchen,Heating", "Heating,Washer,Dryer", "Hot water", "Kitchen,Heating,Washer,Dryer", "Breakfast,Heating",
                  "Dog(s),Heating", "Kitchen,Elevator,Heating", "Kitchen,Breakfast,Heating", "Cat(s),Heating", "Indoor fireplace")
comfort_to_move_in <- c("en.hosting_amenity_50", "en.hosting_amenity_49", "24-hour check-in", "Self Check-In", "Elevator in building",
                        "Wheelchair accessible", "Suitable for events", "Smoking allowed", "Step-free access", "Gym", " Luggage dropoff allowed",
                        "Doorman","Elevator", "Well-lit path to entrance")
living_room_amenties <- c("Hangers", "Bed linens", "Extra pillows and blankets", "Room-darkening shades", "Patio or balcony", "High chair")
pets_allowed <- c("Pets allowed", "Pets live on this property", "Dog(s)", "Dog(s),Heating", "Cat(s)", "Cat(s),Heating")

df_2 <- subset(df, select = c("id", "log_price", "amenitiesd", "host_responce_rate"))











sort(table(unlist(strsplit(data_1, ":"))), decreasing = TRUE)[1:100]
sum(table(unlist(strsplit(data_1, ":")))[1:100])
sum(table(unlist(strsplit(data_1, ":"))))
#float_ <- df$bathrooms[grepl("\\d+\\.\\d+", df$bathrooms)]



df_2 <- df
df <- cleaned_2_df


stats_verified_and_profile_pic <- df %>%
  group_by(host_identity_verified, host_has_profile_pic) %>%
  summarise(mean_value = mean(review_scores_rating), median_value = median(review_scores_rating))
colnames(stats_verified_and_profile_pic) <- c("identity_verified", "profile_pic", "mean", "median")

stats_price_verified_and_profile_pic <- df %>%
  group_by(host_identity_verified, host_has_profile_pic) %>%
  summarise(mean_value = mean(log_price), median_value = median(log_price))
colnames(stats_price_verified_and_profile_pic) <- c("identity_verified", "profile_pic", "mean", "median")

ggplot (cleaned_2_df, aes(x = room_type, y = review_scores_rating)) +
  geom_boxplot() +
  labs (title = "Боксплоти рейтингу від типу кімнати", x = "Тип кімнати", y = "Рейтинг")


ggplot(cleaned_2_df, aes(x = review_scores_rating, y = log_price)) +
  geom_line() +
  labs(title = "Залежність рейтингу від ціни з логарифмічною шкалою", x = "Рейтинг", y = "Ціна (логарифмована)")

ggplot(cleaned_2_df, aes(x = review_scores_rating)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Гістограма рейтингу", x = "Рейтинг", y = "Частота")

ggplot(cleaned_2_df, aes(x = log_price)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Гістограма ціни", x = "Ціна", y = "Частота")


ggplot(stats_verified_and_profile_pic, aes(x = interaction(identity_verified, profile_pic), fill = interaction(identity_verified, profile_pic))) +
  geom_col(aes(y = mean), position = position_dodge(width = 0.9), width = 0.4) +
  geom_col(aes(y = median),alpha = 0.2, position = position_dodge(width = 1.1), width = 0.4, fill = "black") +
  geom_text(aes(y = mean, label = round(mean, 3)), position = position_dodge(width = 0.9), vjust = -1.5) +
  geom_text(aes(y = median, label = round(median, 3)), position = position_dodge(width = 1.1), vjust = 1.5)+
  labs(title = "Залежність рейтингу від верифікації та наявності фото",
       x = "Верифіковано.Фото", y = "Рейтинг", fill = "Темний колір - медіана, світлий - середнє\nЧи верифіковано.Чи є фото")

ggplot(stats_price_verified_and_profile_pic, aes(x = interaction(identity_verified, profile_pic), fill = interaction(identity_verified, profile_pic))) +
  geom_col(aes(y = mean), position = position_dodge(width = 0.9), width = 0.4) +
  geom_col(aes(y = median),alpha = 0.2, position = position_dodge(width = 1.1), width = 0.4, fill = "black") +
  geom_text(aes(y = mean, label = round(mean, 3)), position = position_dodge(width = 0.9), vjust = -1.5) +
  geom_text(aes(y = median, label = round(median, 3)), position = position_dodge(width = 1.1), vjust = 1.5)+
  labs(title = "Залежність ціни від верифікації та наявності фото",
       x = "Верифіковано.Фото", y = "Логарифмована ціна", fill = "Темний колір - медіана, світлий - середнє\nЧи верифіковано.Чи є фото")







cor(df$review_scores_rating, df$host_identity_verified, method = "cramer")

corr_log_price_property_type <- cor(df$log_price, property_type_factored)
corr_log_price_review_scores_rating <- cor(df$log_price, df$review_scores_rating)
corr_review_scores_ratings_room_type <- cor(df$review_scores_rating, room_type_factored)
corr_review_scores_ratings_host_has_profile_pic <- cor(df$review_scores_rating, host_has_profile_pic_factored)
corr_review_scores_ratings_host_identity_verified <- cor(df$review_scores_rating, host_identity_verified_factored)


t.test(df$review_scores_rating[df$host_identity_verified == "TRUE"],
       df$review_scores_rating[df$host_identity_verified == "FALSE"],
        alternative = "greater")
t.test(df$review_scores_rating[df$host_has_profile_pic == "TRUE"],
       df$review_scores_rating[df$host_has_profile_pic == "FALSE"],
       alternative = "greater")

t.test(df$log_price[df$host_identity_verified == "TRUE"],
       df$log_price[df$host_identity_verified == "FALSE"],
       alternative = "greater")
t.test(df$log_price[df$host_has_profile_pic == "TRUE"],
       df$log_price[df$host_has_profile_pic == "FALSE"],
       alternative = "less")


df$host_has_profile_pic <- factor(unlist(df$host_has_profile_pic))
df$host_identity_verified <- factor(unlist(df$host_identity_verified))

