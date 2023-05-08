packages <- c(
  'dplyr',
  'tidyr',
  'readr',
  'rstudioapi',
  'mice'
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
    "thumbnail_url", "id", "description",
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

densityplot(df$bathrooms)
densityplot(df$host_has_profile_pic)
densityplot(df$host_identity_verified)
densityplot(df$host_response_rate)
densityplot(df$review_scores_rating)
densityplot(df$bedrooms)
densityplot(df$beds)