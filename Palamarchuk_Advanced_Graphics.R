# 1. Встановіть пакет vcd. Завантажте таблицю Arthritis, яка є частиною інстальованого 
# пакету.Таблиця  містить  дані клінічного  випробування,  що  досліджувало  нове 
# лікування ревматоїдного артриту.

install.packages("vcd")
library(vcd)

data("Arthritis")
head(Arthritis)

summary(Arthritis)

# 2. Побудуйте  прості  стовпчикові  вертикальну  та  горизонтальну  діаграми  
# частоти різної реакції (None,  Some,  Marked) досліджуваних пацієнтів на ліки 
# або плацебо. Для цього потрібно використат истовпець Improved таблиці Arthritis. 
# Для визначення частоти зустрічі окремих значень у векторі або матриці можна 
# використати функцію table(). Для побудови стовпчикових діаграм можна використати 
# функції plot або barplot.

freq <- table(Arthritis$Improved)
par(mfrow = c(1, 2))
barplot(freq, main = "Вертикальна стовпчикова діаграма", 
        xlab = "Покращення", ylab = "Частота", col = "lightblue")

barplot(freq, main = "Горизонтальна стовпчикова діаграма", 
        xlab = "Частота", ylab = "Покращення", horiz = TRUE, col = "lightgreen")


# 3.Побудуйте стовпчикові діаграми з вертикальним та горизонтальним розподілом 
# на підгрупи. Побудова діаграм з групуванням відбувається, якщо в якості 
# аргументу функції barplotвикористовувати не вектор, а матрицю (таблицю). 
# Наприклад,  нашому випадку для дослідження реакції пацієнта на різні методи 
# лікування можна використати таке формування матриці.

counts <- table(Arthritis$Improved, Arthritis$Treatment)
par(mfrow = c(1, 1))

barplot(counts, main = "Стовпчикова діаграма з вертикальним розбиттям", 
        xlab = "Лікування", ylab = "Частота", col = c("skyblue", "lightgreen", "pink"), 
        legend = rownames(counts))

barplot(counts, main = "Горизонтальна діаграма з горизонтальним розбиттям", 
        xlab = "Частота", ylab = "Лікування", col = c("skyblue", "lightgreen", "pink"), 
        legend = rownames(counts), beside = TRUE)

# 4. Побудуйте спінограми – особливий варіант стовпчикових діаграм, у яких висота 
# кожного результуючого стовпця дорівнює 1, а висоти його складових відображають 
# пропорції. Спінограми створюються за допомогою команди spine() із пакету vcd()
# Реалізуйте в R приклад, який задано наступним скриптом...
# Чи можливо розфарбувати частини спінограми в різні кольори?Якщо так, то зробіть це.

# spine(Improved ~ Age, data = Arthritis, breaks = quantile(Arthritis$Age))


spine(counts, main = "Приклад спінограми")

# spine(counts, main = "Приклад спінограми", col = c("red", "green", "blue"))
# Зробити спінограму різнокольоровою - неможливо.


# 5. Використовуючи  R  команди  для  базової  візуалізації,  
# відтворіть  такий  графік:  (Датасет, який був використаний 
#    для цього зображення InsectSprays, який доступний в базовій Rінсталяції) 

data("InsectSprays")

head(InsectSprays)
str(InsectSprays)

par(mfrow = c(1, 2))

boxplot(InsectSprays$count, 
        main = "Effectivenes of Sprays", col = "orange",
        xlab = "Spray Type", ylab = "Insect Count")

boxplot(count ~ spray, data = InsectSprays, col = "lightblue",
        main = "Boxplot of Insect Counts by Spray Type",
        xlab = "Spray Type", ylab = "Insect Count")


# 6. Для  датасету mtcars побудуйте  вдосконалені діаграми  розмахів
# (boxplot), порівнявши витрати пального для автомобілів з чотирма, 
# шістьма та вісьмома циліндрами.  При  побудові  використати  
# додаткові  параметри notch=TRUE, varwidth=TRUE,  які  дозволяють  
# зобразити  ширину  фігур,  пропорційну  обсягу вибірок. Зафарбуйте 
# фігури на графіку довільним кольором.

mtcars
par(mfrow = c(1, 1))
boxplot(mpg~cyl, data = mtcars, notch=TRUE, varwidth=TRUE,
        col = "green", main = "Інформація про авто",
        xlab = "Циліндри", ylab = "Витрати пального")

# 7.Функція boxplot також дозволяє будувати на одному рисунку діаграми 
# розмахів для декількох факторів. Поставимо, наприклад, задачу 
# дослідити витрати пального не тільки від кількості циліндрів, 
# а й від типу трансмісії автомобіля (механічна чи автоматична). 
# Реалізуйте в R наступний програмний код...

mtcars$cyl.f <-factor(mtcars$cyl,
                      levels=c(4,6,8),
                      labels=c("4ц","6ц","8ц"))

mtcars$am.f <-factor(mtcars$am,levels=c(0,1),
                     labels=c("АТ", "МТ"))

boxplot(mpg ~ am.f * cyl.f,
        data = mtcars, varwidth=TRUE,
        col = c("lightgreen", "mediumpurple1"),
        main = "Витрати пального за типом трансмісії та кількістю циліндрів",
        xlab = "Тип трансмісії", ylab = "Витрати пального (mpg)")


# 8. Для датасету mtcars побудуйте ще одну модифікацію діаграмвитрат 
# пального для автомобілів з чотирма, шістьма та вісьмома циліндрами –
# скрипковідіаграми. Таку діаграму можна створити за допомогою функції 
# vioplot() із пакету з такою ж назвою vioplot. Формат застосування функції
# vioplot() такий...

install.packages("vioplot")
library(vioplot)

x1 <- mtcars$mpg[mtcars$cyl == 4]
x2 <- mtcars$mpg[mtcars$cyl == 6]
x3 <- mtcars$mpg[mtcars$cyl == 8]

vioplot(x1, x2, x3, names = c("4 циліндри", "6 циліндрів", "8 циліндрів"),
        col = c("darkgreen", "green", "lightgreen"))
title("Скрипкові діаграми витрат пального")
