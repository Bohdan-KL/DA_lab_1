packages <- c(
  'ggplot2',
  'DescTools',
  'coin',
  'boot',
  'lmtest',
  'aod'
)

for(package in packages){
  if(!require(package, character.only = TRUE)){
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}

# --- --- --- Довірчі інтервали --- --- ---
# Дослідницькі питання:
#   Чи залежить ціна від типу оселі?
#   Чи залежить рейтинг від наявності верифікації/фото?
#   Чи залежить ціна апартаментів від географічного положення?
#   Чи залежить рейтинг від типу орендованих кімнат?
#   Чи залежить ціна від рейтингу?
# 
# Довірчі інтервали:
#   1) Для середнього і дисперсії: ціна, рейтинг
#   2) Медіана: ціна
#   3) Коефіцієнти кореляції Пірсона і Спірмена

# Розмір генеральної сукупності(датасету)
n <- nrow(df)
# Рівень значущості
p <- 0.05
# Довірчий рівень
confidence_level = 0.95

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

# --- --- Ціна --- ---

# --- Довірчі інтервали для середнього значення ---

# Довірчий інтервал для середнього значення ціни
MeanCI(df$log_price, conf.level = confidence_level)

# Дослідження довірчого інтервалу ціни в залежності від
# типу апартаментів:
ggplot(df, aes(x = property_type, y = log_price)) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 20, fill = "white") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Property type") +
  ylab("Price")
  # title("Довірчі інтервали за середнім значенням ціни по типам апартаментів.")

# --- Для дисперсії ---

# Довірчий інтервал для дисперсії ціни
VarCI(df$log_price, conf.level = confidence_level)

# Дослідження довірчого інтервалу дисперсії ціни в залежності від
# типу апартаментів:

# Вилучено такі типи апартаментів як: Cave, Earth House, Tipi, Treehouse.
# Оскільки верхня межа довірчого інтервалу дисперсії цих апартаметів була значно вищою
# за інші типи. Можлива причина: мало апартаметів з цими типами, що може
# псувати статистику.
ggplot(
    df[!df$property_type %in% c("Cave", "Earth House", "Tipi", "Treehouse"), ],
    aes(x = property_type, y = log_price)
  ) +
  stat_summary(fun = var, geom = "point", size = 2, shape = 20, fill = "white") +
  stat_summary(fun.data = function(y) {
    data.frame(y = var(y),
               ymin = ((length(y) - 1) * var(y)) / qchisq(p,length(y) - 1),
               ymax = ((length(y) - 1) * var(y)) / qchisq(1 - p,length(y) - 1))
  }, geom = "errorbar") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Property type") +
  ylab("Price")
  # title("Довірчі інтервали за дисперсією ціни по типам апартаментів.")

# Довірчі інтервали дисперсії окремо для для Cave, Earth House, Tipi, Treehouse
VarCI(df[df$property_type == "Cave", ]$log_price, conf.level = confidence_level)
VarCI(df[df$property_type == "Earth House", ]$log_price, conf.level = confidence_level)
VarCI(df[df$property_type == "Tipi", ]$log_price, conf.level = confidence_level)
VarCI(df[df$property_type == "Treehouse", ]$log_price, conf.level = confidence_level)

# --- --- Рейтинг --- ---

# --- Довірчі інтервали для середнього значення ---

# Довірчий інтервал для середнього значення рейтингу
MeanCI(df$review_scores_rating, conf.level = confidence_level)

# Дослідження довірчого інтервалу середнього значення ціни в залежності від рейтингу:
ggplot(df, aes(x = review_scores_rating, y = log_price)) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 20, fill = "white") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Rating") +
  ylab("Price")
# title("Довірчі інтервали за середнім значенням ціни по рейтингу.")

# Закономірність полягає у тому, що чим більше рейтинг тим більш
# ціна стає фіксованішою.

# --- Для дисперсії ---

# Довірчий інтервал для дисперсії рейтингу
VarCI(df$review_scores_rating, conf.level = confidence_level)

# Дослідження довірчого інтервалу дисперсія ціни в залежності від рейтингу:
# Вилучено апартаменти з рейтингом 56, оскільки верхня межа занадто висока.
ggplot(df[df$review_scores_rating != 56,], aes(x = review_scores_rating, y = log_price)) +
  stat_summary(fun = var, geom = "point", size = 2, shape = 20, fill = "white") +
  stat_summary(fun.data = function(y) {
    data.frame(y = var(y),
               ymin = ((length(y) - 1) * var(y)) / qchisq(p,length(y) - 1),
               ymax = ((length(y) - 1) * var(y)) / qchisq(1 - p,length(y) - 1))
  }, geom = "errorbar") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Rating") +
  ylab("Price")

# Обрахунок довірчого інтервалу для рейтингу 56:
# df[df$review_scores_rating == 56,]

VarCI(df[df$review_scores_rating == 56,]$log_price, conf.level = confidence_level)
# Ймовірна причина даного явлення - маленька кількість апартаментів з
# рейтингом 56

# --- --- Медіана --- ---

# Застосуємо алгоритм бутстреп для визначення довірчого інтервалу медіани ціни

# Кількість бутстреп вибірок
n_bootstrap = 1000

# Формування вибірок
bootstrap <- function(data, n) {
  indices <- sample(1:length(data), n, replace = TRUE)
  return(data[indices])
}

# Вектор для зберігання медіан
bootstrap_medians <- numeric(n_bootstrap)

# Виконуємо бутстреп і розраховуємо медіани.
for (i in 1:n_bootstrap) {
  bootstrap_sample <- bootstrap(df$log_price, n)
  bootstrap_medians[i] <- median(bootstrap_sample)
}

# Межі довірчого інтервалу медіани, а саме перший і третій квартилі:
lower_quantile <- quantile(bootstrap_medians, p)
upper_quantile <- quantile(bootstrap_medians, 1 - p)

lower_quantile
upper_quantile
# Отже маємо такий інтервал: [4.70048, 4.736198]

# --- --- Коефіцієнт кореляції Спірмена --- ---

# Дослідження кореляції між ціною і рейтингом:
# df$log_price
# df$review_scores_rating

# Функція обрахунку кореляції Спірмена
cor_spearman <- function(data, indices) {
  x <- data[indices, 1]
  y <- data[indices, 2]
  return(cor(x, y, method = "spearman"))
}

# Функція обрахунку кореляції Пірсона
cor_pearson <- function(data, indices) {
  x <- data[indices, 1]
  y <- data[indices, 2]
  return(cor(x, y, method = "pearson"))
}

# Застосування бутстрепу
bootstrap_results <- boot(cbind(df$log_price, df$review_scores_rating), cor_spearman, R = n_bootstrap)

# Межі довірчого інтервалу
lower_quantile <- quantile(bootstrap_results$t, p)
upper_quantile<- quantile(bootstrap_results$t, 1 - p)

lower_quantile
upper_quantile

# Отже маємо такий інтервал: [0.09218075, 0.1037385]

# --- --- Коефіцієнт кореляції Пірсона --- ---

# Дослідження кореляції між ціною і типом кімнат:
# df$log_price
# df$property_type

# Застосування бутстрепу
bootstrap_results <- boot(cbind(df$log_price, factor(unlist(df$property_type))), cor_spearman, R = n_bootstrap)

# Межі довірчого інтервалу
lower_quantile <- quantile(bootstrap_results$t, p)
upper_quantile<- quantile(bootstrap_results$t, 1 - p)

lower_quantile
upper_quantile

# Отже маємо такий інтервал: [0.04217857, 0.05577007]

#  --- --- --- Доведення гіпотез  --- --- ---

# Гіпотези:
# - Чи рівні середні значення ціни вибірок апартаметів таких типів "Condominium" та "Loft"?

# --- Рівність середніх значень ціни вибірок типів Condominium і Loft. ---

# Графіки щільностей вибірок.
densityplot(df[df$property_type == 'Condominium',]$log_price)
densityplot(df[df$property_type == 'Loft',]$log_price)

# Візуально: вибірки мають нормальний розподіл.

# Перевірка однорідностей дисперсії:
# H0: дисперсії вибірок рівні.
# H1: дисперсії вибірок різні.
# p = 0.05
result <- var.test(
  df[df$property_type == 'Condominium',]$log_price,
  df[df$property_type == 'Loft',]$log_price,
  alternative = 'two.sided'
)
# Обрано двусторонню альтернативу, оскільки нас цікавить лише однорідність,
# а не різницю у конкретних напрямках середніх значень.

result
result$p.value
# P-значення більше за заданий нами рівень(0.05), тому існує
# статистична значимість рівності у дисперсіях вибірок, тобто немає
# підстав відхиляти нульову гіпотезу про рівність дисперсії вибірок.

# Вибірки мають нормальний розподіл і дисперсії однорідні, тому проводимо
# t-тест.

# H0: середні значення вибірок рівні.
# H1: середні значення вибірок різняться.
result <- t.test(
  df[df$property_type == 'Condominium',]$log_price,
  df[df$property_type == 'Loft',]$log_price,
  alternative = 'two.sided',
  paired = FALSE,
  var.equal = FALSE
)
# Обрано двусторонню альтернативу, оскільки нас цікавить лише однорідність,
# а не різницю у конкретних напрямках середніх значень.

result
result$p.value
# P-значення більше за заданий нами рівень(0.05), тому існує
# статистична значимість рівності у середніх значень вибірок, тобто немає
# підстав відхиляти нульову гіпотезу про рівність середнії значень вибірок.

# --- Тест Вальда ---

# Маємо таку модель: ціна = w1 * тип кімнати + w2 * кількість ванних кімнат +
#                           + w3 * кількість спальних кімнат + кількість ліжок
model <- lm(log_price ~ property_type + bathrooms + bedrooms + beds, data = df)
summary(model)

# H0: деякі або всі предиктори рівні нулю.
# H1: жоден або не всі змінні предиктори рівні нулю

for (i in 1:4) {
  print(wald.test(Sigma = vcov(model), b = coef(model), Terms = i))
}

# Із тесту Вальда маємо p-value << 0 для всіх змінних предикторів,
# оскільки воно менше за рівень значимості(0.05), то ми можемо відхилити
# нульову гіпотезу про рівність регресорів нулю.
