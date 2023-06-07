
packages <- c(
  'dplyr',
  'tidyr',
  'readr',
  'rstudioapi',
  'mice',
  'lmtest',
  'car',
  'stargazer'
)

for(package in packages){
  if(!require(package, character.only = TRUE)){
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}

data_file_name <- 'train.csv'
csv_path <- paste(dirname(getSourceEditorContext()$path), data_file_name, sep = '/')

csv_path

# Get probabilities of encoding
guess_encoding(csv_path, n_max = 1000)
# Mostly UTF-8

# Read dataframe
df = read.csv(csv_path)

# Get size of dataframe
init_shape <- dim(df)

# Remove unnecessary columns.
df <- df[
  ,!names(df) %in% c(
    "thumbnail_url", "description",
    "name", 'first_review', 'host_since',
    'last_review', 'neighbourhood', 'amenities',
    'description', 'zipcode'
  )
]

# Replace empty cells with NA
df[df == ''] <- NA

df$host_response_rate <- as.numeric(sub("%", "", df$host_response_rate)) / 100

df$host_has_profile_pic <- gsub("t", "True", df$host_has_profile_pic)
df$host_has_profile_pic <- gsub("f", "False", df$host_has_profile_pic)
df$host_has_profile_pic <- as.logical(factor(
  unlist(df$host_has_profile_pic),
  levels = c("False", "True")
)
)

df$host_identity_verified <- gsub("t", "True", df$host_identity_verified)
df$host_identity_verified <- gsub("f", "False", df$host_identity_verified)
df$host_identity_verified <- as.logical(factor(
  unlist(df$host_identity_verified),
  levels = c("False", "True")
)
)

# Editing data types
df$id <- as.character(df$id)

# Count na values.
init_na_count <- sum(is.na(df))
init_na_count_df <- data.frame(
  sapply(df, function(y) sum(length(which(is.na(y)))))
)

# Count cells with empty values.
init_empty_count <- sum(df == "")
init_empty_count_df <- data.frame(
  sapply(df, function(y) sum(length(which(y == ""))))
)

# Applying MICE package for filling missed data
df <- mice(df, printFlag = FALSE, seed=100)
df <- complete(df)
sum(df == "")
sum(is.na(df))

na_count <- sum(is.na(df))
na_count_df <- data.frame(
  sapply(df, function(y) sum(length(which(is.na(y)))))
)

# Recount cells with empty values.
empty_count <- sum(df == "")
empty_count_df <- data.frame(
  sapply(df, function(y) sum(length(which(y == ""))))
)

new_shape <- dim(df)

# Density plots

# densityplot(df$bathrooms)
# densityplot(df$host_has_profile_pic)
# densityplot(df$host_identity_verified)
# densityplot(df$host_response_rate)
# densityplot(log(df$review_scores_rating_log))
# densityplot(df$bedrooms)
# densityplot(df$beds)


df$property_type <- factor(unlist(df$property_type))
df$room_type <- factor(unlist(df$room_type))
df$city <- factor(unlist(df$city))

# Створення даммі змінних для міст
city_dummies <- model.matrix(~ city - 1, data = df)

# Додавання даммі змінних міст до даних
df <- cbind(df, city_dummies)

# Створення даммі змінних типу кімнат
room_type_dummies <- model.matrix(~ room_type - 1, data = df)

# Додавання даммі змінних типу кімнат до даних
df <- cbind(df, room_type_dummies)

# Створення даммі змінних типу апартаментів

property_type_dummies <- model.matrix(~ property_type - 1, data = df)
property_type_dummies <- property_type_dummies[, -1]

# Додавання даммі змінних типу апартаментів до даних
df <- cbind(df, property_type_dummies)



df$review_scores_rating_log <- log10(df$review_scores_rating)


# Побудова моделі
base_model <- lm(log_price ~ city_dummies + room_type_dummies + property_type_dummies + bathrooms + beds + bedrooms + review_scores_rating_log, data = df)

ggplot(df, aes(x = property_type, y = log_price)) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 20, fill = "white") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Property type") +
  ylab("Price")
# title("Довірчі інтервали за середнім значенням ціни по типам апартаментів.")

# Identify duplicate column names
duplicate_columns <- duplicated(names(df))
df <- df[, !duplicate_columns]

# Plot the data using ggplot
ggplot(df, aes(x = review_scores_rating_log, y = log_price)) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 20, fill = "white") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Rating") +
  ylab("Price")



df$cleaning_fee <- ifelse(df$cleaning_fee == "True", 1, 0)
df$instant_bookable <- ifelse(df$instant_bookable == "t", 1, 0)

#ускладнена модель
more_factors_model <- lm(log_price ~ city_dummies + room_type_dummies + property_type_dummies + interaction(bathrooms, bedrooms) + review_scores_rating_log + cleaning_fee + interaction(host_has_profile_pic, host_identity_verified) + instant_bookable, data = df)
summary(more_factors_model)




base_model_hc2 <- coeftest(base_model, vcov. = hccm(base_model, type = "hc1"))
ci <- coefci(base_model, vcov. = hccm(base_model, type = "hc1"))

stargazer( base_model, 
           type = "latex",
           dep.var.caption = "",
           se = list(base_model_hc2[, 2]),
           omit.stat = c("rsq", "f", "ser"),
           ci = TRUE, ci.custom = list(ci),
           no.space = TRUE,
           font.size = "tiny", 
           out="Базова_модель_із_стандартною_похибкою.html")



more_factors_model_hc2 <- coeftest(more_factors_model, vcov. = hccm(more_factors_model, type = "hc0"))
ci <- coefci(more_factors_model, vcov. = hccm(more_factors_model, type = "hc0"))

stargazer( more_factors_model, 
           type = "latex",
           dep.var.caption = "",
           se = list(more_factors_model_hc2[, 2]),
           omit.stat = c("rsq", "f", "ser"),
           ci = TRUE, ci.custom = list(ci),
           no.space = TRUE,
           font.size = "tiny", 
           out="Ускладнена_модель_із_стандартною_похибкою.html")


stargazer(base_model, more_factors_model, type = "latex",
          title = "Порівняння", label = "table:evals-reg-mult",
          dep.var.labels = c("Середня оцінка"),
          dep.var.caption = "",
          se = list(base_model_hc2[, 2], more_factors_model_hc2[, 2]),
          omit.stat = c("rsq", "f", "ser"),
          no.space = TRUE,
          font.size = "tiny", 
          out="Порівняння.html")



# Виконати аналіз дисперсії
anova_result <- aov(log_price ~ property_type, data = df)

# Вивести результати аналізу
summary(anova_result)



rows_to_remove <- c(3064, 17571, 25563, 28964, 42505, 64970)
df <- df[-rows_to_remove, ]
rows_to_remove_2 <- c(6224, 17479, 18145, 20099, 27808, 29977, 31194, 31456, 36551, 43560)
df <- df[-rows_to_remove_2, ]

df$property_type <- gsub("Bed & Breakfast", "Bed_and_Breakfast", df$property_type)




summary(base_model)
