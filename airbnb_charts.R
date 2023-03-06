packages <- c(
  'tools',
  'sf',
  'maps',
  'rnaturalearth',
  'rnaturalearthdata',
  'ggplot2',
  'ggforce',
  'fmsb'
)

for(package in packages){
  if(!require(package, character.only = TRUE)){
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}

# Залежності:
#  ціна(log_price) - тип оселі(property_type)
#  ціна(log_price) - рейтинг(review_scores_rating)
#  рейтинг(review_scores_ratings) - тип кімнат(room_type)
#  рейтинг(review_scores_ratings) - наявність фото(host_has_profile_pic)/
#                                  верифік. апартаментів(host_identity_verified)
# Вплив вмістимість апартаментів(accommodates, bathrooms, bedrooms, beds)
#    на кількість відгуків(number_of_reviews)

# Correlation
corr_log_price_property_type <- cor(df$log_price, property_type_factored)
corr_log_price_review_scores_rating <- cor(df$log_price, df$review_scores_rating)
corr_review_scores_ratings_room_type <- cor(df$review_scores_rating, room_type_factored)
corr_review_scores_ratings_host_has_profile_pic <- cor(df$review_scores_rating, host_has_profile_pic_factored)
corr_review_scores_ratings_host_identity_verified <- cor(df$review_scores_rating, host_identity_verified_factored)

# Density plots
density_log_price <- ggplot(
  df,
  aes(x=log_price)
) + geom_density(fill='lightblue') +
  geom_vline(
    aes(xintercept=mean(log_price)),
    color='Blue', linetype='dashed', size = 1
  ) + xlab('Ціна') + ylab('Щільність ціни') + 
  ggtitle('Щільність ціни')

density_review_scores_rating <- ggplot(
  df,
  aes(x=review_scores_rating)
) + geom_density(fill='lightblue') +
  geom_vline(
    aes(xintercept=mean(review_scores_rating)),
    color='Blue', linetype='dashed', size = 1
  ) + xlab('Рейтинг') + ylab('Щільність рейтингу') + 
  ggtitle('Щільність рейтингу')

density_room_type <- ggplot(
  df,
  aes(x=room_type)
) + geom_density(fill='lightblue') +
  xlab('Тип кімнат') + ylab('Щільність типу кімнат') + 
  ggtitle('Щільність типу кімнат')

# QQ Plots(for outliers)
qq_log_price <- ggplot(
  df,
  aes(sample=log_price)
) + stat_qq() + stat_qq_line() + xlab('Теоретичні квантилі') + ylab('Ціна') + 
  ggtitle('QQ Plot розподілу ціни')

qq_property_type <- ggplot( # ???
  df,
  aes(sample=property_type_factored)
) + stat_qq() + stat_qq_line() + xlab('Теоретичні квантилі') +
  ylab('Факторизований тип апартаментів') + 
  ggtitle('QQ Plot розподілу типу апартаментів')

qq_review_scores_rating <- ggplot( # ???
  df,
  aes(sample=review_scores_rating)
) + stat_qq() + stat_qq_line() + xlab('Теоретичні квантилі') +
  ylab('Рейтинг') + 
  ggtitle('QQ Plot розподілу рейтингу')

# Scatter plots.
scatter_log_price_property_type <- ggplot(
  df,
  aes(x=log_price, y=property_type)
  ) + geom_point(size=1, shape=10) + xlab('Ціна') + ylab('Тип оселі') +
  ggtitle(
    sprintf(
      'Залежність ціни від типу оселі. R = %.3f',
      corr_log_price_property_type
      )
  )

scatter_log_price_review_scores_rating <- ggplot(
  df,
  aes(x=review_scores_rating, y=log_price)
  ) + geom_point(size=1, shape=10) + xlab('Ціна') + ylab('Рейтинг') +
  ggtitle(
    sprintf(
      'Залежність ціни від рейтингу. R = %.3f',
      corr_log_price_review_scores_rating
    )
  )

scatter_review_scores_rating_room_type <- ggplot(
  df,
  aes(x=review_scores_rating, y=room_type)
  ) + geom_point(size=1, shape=10) + xlab('Рейтинг') + ylab('Тип кімнати') +
  ggtitle(
    sprintf(
      'Залежність рейтингу від типу кімнат. R = %.3f',
      corr_review_scores_ratings_room_type
    )
  )

scatter_review_scores_rating_host_has_profile_pic <- ggplot(
  df,
  aes(
    x=review_scores_rating, 
    y=host_has_profile_pic
  )
) + geom_point(
      size=1,
      shape=10,
    ) + xlab('Рейтинг') + ylab('Наявність фото') +
  ggtitle(
    sprintf(
      'Залежність рейтингу від наявності фото. R = %.3f',
      corr_review_scores_ratings_host_has_profile_pic
    )
  )

scatter_review_scores_rating_host_identity_verified <- ggplot(
  df,
  aes(x=review_scores_rating, y=host_identity_verified)
) + geom_point(size=1, shape=10) + xlab('Рейтинг') + ylab('Наявність верифікації') +
  ggtitle(
    sprintf(
      'Залежність рейтингу від наявності верифікації. R = %.3f',
      corr_review_scores_ratings_host_identity_verified
    )
  )

# Map plots

world <- ne_countries(scale = "medium", returnclass = "sf")

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
states <- cbind(states, st_coordinates(st_point_on_surface(states)))
states$ID <- toTitleCase(states$ID)

sites <- df[c('longitude', 'latitude')]

# Clustering
set.seed(55)
cluster_sites <- kmeans(df[c('longitude', 'latitude', 'log_price')], 6, nstart = 20)
cluster_sites$cluster <- as.factor(cluster_sites$cluster)
cluster_centers_df <- data.frame(cluster_sites$centers)

log_price_cluster_labels <- sapply(
  cluster_sites$centers[, 3],
  function(x) paste(c('bold("', round(x, 2), '")'), collapse = '')
)
states_labels <- sapply(
  states$ID,
  function(x) paste(c('bold("', x, '")'), collapse = '')
)

full_map <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) +
  geom_text(
    data = states,
    aes(X, Y, label = states_labels),
    size = 2.5,
    parse = TRUE
  ) +
  geom_point(
    data = sites,
    aes(x = longitude, y = latitude),
    size = 3,
    shape = 21,
    fill = cluster_sites$cluster
  ) +
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50), expand = FALSE) +
  xlab('Довгота') + ylab('Ширина') + ggtitle('Карта США з апартаментами') + 
  geom_circle(
    data = cluster_centers_df,
    aes(x0 = longitude, y0 = latitude, r = 2),
    fill = 'red',
    alpha = 0.1
  ) + 
  annotate(
    geom = 'text',
    x = cluster_sites$centers[, 1] - 1,
    y = cluster_sites$centers[, 2] + 2.5,
    label = log_price_cluster_labels,
    size = 3,
    color = 'red',
    parse = TRUE
  )

right_detailed_map <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) +
  coord_sf(xlim = c(-80, -70), ylim = c(35, 45), expand = FALSE) +
  geom_point(
    data = sites,
    aes(x = longitude, y = latitude),
    size = 3,
    shape = 21,
    fill = cluster_sites$cluster
  ) +
  geom_text(
    data = states,
    aes(X, Y, label = states_labels),
    size = 2.5,
    parse = TRUE,
    angle = 5
  ) +
  xlab('Довгота') + ylab('Ширина') + ggtitle('Карта східної частини США з апартаментами') +
  geom_circle(
    data = cluster_centers_df,
    aes(x0 = longitude, y0 = latitude, r = 1),
    fill = 'red',
    alpha = 0.1
  ) + 
  annotate(
    geom = 'text',
    x = cluster_sites$centers[, 1] - 1,
    y = cluster_sites$centers[, 2] + 1,
    label = log_price_cluster_labels,
    size = 3,
    color = 'red',
    parse = TRUE
  )

# Radarchart
property_type_grouped_by_avg <- aggregate(
  df$log_price,
  list(df$property_type),
  FUN = function(x) cbind(max(df$log_price), min(df$log_price), mean(x))
)

property_type_grouped_by_avg <- data.frame(
  property_type_grouped_by_avg$x,
  row.names = property_type_grouped_by_avg$Group.1
)

property_type_grouped_by_avg <- setNames(property_type_grouped_by_avg, c('max', 'min', 'mean'))

property_type_grouped_by_avg$min <- min(property_type_grouped_by_avg$mean)
property_type_grouped_by_avg$max <- max(property_type_grouped_by_avg$mean)

property_type_grouped_by_avg <- property_type_grouped_by_avg[
  order(
    property_type_grouped_by_avg$mean,
    decreasing = TRUE
  ),
  ]

t_property_type_grouped_by_avg <- data.frame(t(property_type_grouped_by_avg))

radarchart(
  t_property_type_grouped_by_avg,
  axistype = 4,
  caxislabels = quantile(round(property_type_grouped_by_avg$mean, 1)),
  pcol = '#00AFBB',
  pfcol = scales::alpha('#00AFBB', 0.1),
  plwd = 2,
  plty = 1,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.6,
  vlabels = paste(
    names(t_property_type_grouped_by_avg),
    '\n',
    round(property_type_grouped_by_avg$mean, 2)
  ),
  title = 'Середня ціна на тип оселі'
)

show(density_log_price)
show(density_review_scores_rating)
show(density_room_type)
show(qq_log_price)
show(qq_property_type)
show(qq_review_scores_rating)
show(scatter_log_price_property_type)
show(scatter_log_price_review_scores_rating)
show(scatter_review_scores_rating_room_type)
show(scatter_review_scores_rating_host_has_profile_pic)
show(scatter_review_scores_rating_host_identity_verified)

show(full_map)
show(right_detailed_map)

# list.files(tempdir(), pattern="rs-graphics", full.names = TRUE)