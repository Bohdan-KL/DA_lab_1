packages <- c(
  'dplyr',
  'tidyr',
  'readr',
  'rstudioapi'
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
init_df = read.csv(csv_path)

# Get size of dataframe
init_shape <- dim(df)

# Remove unnecessary columns.
df <- init_df[,!names(df) %in% c("thumbnail_url", "id", "description", "name")]

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

# Recount cells with empty values.
empty_count <- sum(df == "")
empty_count_df <- data.frame(
  sapply(df, function(y) sum(length(which(y == ""))))
)

new_shape <- dim(df)

# Editing data types
df$id <- as.character(df$id)

# Be careful with line 102,
# if you run again - there will be a repeated division by 100.
df$host_response_rate <- as.numeric(sub("%", "", df$host_response_rate))/100
df$first_review <- as.Date(df$first_review, "%Y-%m-%d")
df$host_since <- as.Date(df$host_since, "%Y-%m-%d")
df$last_review <- as.Date(df$last_review, "%Y-%m-%d")
df$property_type <- factor(unlist(df$property_type))
df$room_type <- factor(unlist(df$room_type))
df$bathrooms <- factor(unlist(df$bathrooms))
df$bed_type <- factor(unlist(df$bed_type))
df$cancellation_policy <- factor(unlist(df$cancellation_policy))
df$city <- factor(unlist(df$city))
df$accommodates <- factor(unlist(df$accommodates))
df$bedrooms <- factor(unlist(df$bedrooms))
df$beds <- factor(unlist(df$beds))
df$cleaning_fee <- as.logical(factor(
    unlist(df$cleaning_fee),
    levels = c("False", "True")
  )
)

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

df$instant_bookable <- gsub("t", "True", df$instant_bookable)
df$instant_bookable <- gsub("f", "False", df$instant_bookable)
df$instant_bookable <- as.logical(factor(
    unlist(df$instant_bookable),
    levels = c("False", "True")
  )
)

property_type_factored <- as.numeric(factor(df$property_type))
room_type_factored <- as.numeric(factor(df$room_type))
host_has_profile_pic_factored <- as.numeric(factor(df$host_has_profile_pic))
host_identity_verified_factored <- as.numeric(factor(df$host_identity_verified))