packages <- c(
  'ggplot2',
  'DescTools',
  'pracma'
)

for(package in packages){
  if(!require(package, character.only = TRUE)){
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}

# --- Довірчі інтервали ---
# Дослідницькі питання:
#   Чи залежить ціна від типу оселі?
#   Чи залежить рейтинг від наявності верифікації/фото?
#   Чи залежить ціна апартаментів від географічного положення?
#   Чи залежить рейтинг від типу орендованих кімнат?
#   Чи залежить ціна від рейтингу?
# 
# Довірчі інтервали:
#   1) Для середнього і дисперсії:
#      ціна, рейтинг
#   2) 1 квартиль, медіана, 3 квартиль, коефіцієнти кореляції Пірсона і Спірмена

# Розмір генеральної сукупності(датасету)
n <- nrow(df)
# Рівень значущості
p <- 0.05

# Графік густини ціни
densityplot(df$log_price)
# Графік густини рейтингу
densityplot(df$review_scores_rating)

# Оскільки датасет великий, то оберемо метод Харке-Бера
# для дослідження на нормальність розподілу:
# Формуємо гіпотези:
# H0(нульова гіпотеза): розподіл є нормальним
# H1(альтернативна гіпотеза): розподіл не є нормальним

# Нормальність розподілу ціни:
JarqueBeraTest(df$log_price)
# Р-значення тесту становить 2.2e-16.
# Оскільки це p-значення менше 0,05,
# ми можемо відкинути нульову гіпотезу.
# Тобто розподіл даних ціни не є нормальним.

# Нормальність розподілу рейтингу:
JarqueBeraTest(df$review_scores_rating)
# Р-значення тесту становить 2.2e-16.
# Оскільки це p-значення менше 0,05,
# ми можемо відкинути нульову гіпотезу.
# Тобто розподіл даних рейтингу не є нормальним.

# Довірчий рівень
confidence_level = 0.95

# Довірчий інтервал для середнього значення ціни
MeanCI(df$log_price, conf.level = confidence_level)

# Дослідження довірчого інтервалу ціни в залежності від
# типу апартаментів:

ggplot(df, aes(property_type, log_price))+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, size = 1) +
  stat_summary(fun = mean, geom = "point", size = 6, shape = 22, fill = "white") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Property type") +
  ylab("Price")

# Довірчий інтервал для дисперсії ціни
VarCI(df$log_price, conf.level = confidence_level)

# Довірчий інтервал для середнього значення рейтингу
MeanCI(df$review_scores_rating, conf.level = confidence_level)

# Довірчий інтервал для дисперсії рейтингу
VarCI(df$review_scores_rating, conf.level = confidence_level)

# --- Доведення гіпотез ---
# Гіпотези:
