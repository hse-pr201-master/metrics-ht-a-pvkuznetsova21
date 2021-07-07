install.packages("tidyverse") # коллекция пакетов от Hadley Wickham
install.packages("lmtest") # тесты в линейных моделях
install.packages("sandwich") # оценки ковариационной матрицы робастные к гетероскедастичности
install.packages("erer") # подборка пакетов для эмпирических исследований
install.packages("estimatr") # модели с робастными стандартными ошибками
install.packages("GGally") # матрица диаграмм рассеяния
install.packages("lattice") # конкурент ggplot2
install.packages("vcd") # мозаичный график
install.packages("hexbin") # график из шестиугольников
install.packages("sjPlot") # визуализация результатов МНК
install.packages("factoextra") # визуализация для метода главных компонент и не только
install.packages("reshape2") # длинные <-> широкие таблицы
install.packages("psych") # описательные статистики
install.packages("skimr") # описательные статистики
install.packages("HSAUR")
install.packages("sgof")
install.packages("car") # для тестирования линейных гипотез, подсчёта vif
install.packages("spikeslab") # байесовская регрессия пик-плато
install.packages("quantreg") # квантильная регрессия
install.packages("MCMCpack") # набор моделей с байесовским подходом
install.packages("devtools") # разработка пакетов
install.packages("caret") # подбор параметров с помощью кросс-валидации
install.packages("AER")
install.packages("ivpack") # интсрументальные переменные
install.packages("zoo") # нерегулярные временные ряды
install.packages("xts") # еще ряды
install.packages("forecast") # ARMA, экспоненциальное сглаживание
install.packages("rugarch") # не используется в курсе, хорош для GARCH
install.packages("quantmod") # загрузка с finance.google.com
install.packages("Quandl") # загрузка с Quandl
install.packages("sophisthse") # read data from sophist.hse.ru

library(tidyverse) 
library(mfx) 
library(rio) 
library(texreg) 

set.seed(0)
#Лесные пожары
data = import("C:/Users/pvkuz/Desktop/forestfires.csv")

# X - ось абсцисс (от 1 до 9)
# Y - ось ординат (от 1до 9)
# month - месяц года (от января до декабря)
# day - день недели (от пн до вс)
# FFMC - лёгкость воспламенения топлива = вероятность возгорания
# DMC - норма влажности угля = вероятность возгорания в результате удара молнии
# DC - норма засухи = устойчивость огня к тушению
# ISI - индекс первоначального распространения = скорость распространения огня сразу после возгорания
# temp - температура воздуха (◦C) 
# RH - относительная влажность воздуха (%)
# wind - скорость ветра (км/ч)
# rain - дождь (мм/м^2)
# area - общая площадь пожаров (га)

# Задание 1:
# Для начала посмотрим на исходные данные
summary(data)
# Отфильтруем нулевые значения целевой функции
data_upd <- data[data$area > 0, ]
# Посмотрим на преобразованные данные
summary(data_upd)
# Для визуализации сделанных преобразований построим гистограмму и посмотрим, как распределены значения переменной area после удаления нулевых значений
qplot(data = data_upd, area, xlab="area", ylab = "number of observations", main = "area distribution", binwidth = 1)
# Изучив гистограмму, Можно заметить, что даже после избавления от нулей значения переменной "area" смещены в сторону нуля. В связи с этим будет целесообразно попробовать взять логарифм это1 переменной и посмотреть, как изменится график.
# Создадим новую переменную lnarea и построим график
lnarea = log(data_upd$area)
data_upd <- data.frame(lnarea, data_upd)
qplot(data = data_upd, lnarea, xlab="lnarea", ylab = "Number of observations", main = "lnarea distribution", binwidth = 1)
# Распределение переменной стало более однородным, поэтому будем использовать именно логарифм объясняемой переменной area
#Прежде чем приступать к отбору необходимых факторов, сделаем ещё несколько преобразований, которые немного упростят анализ: запишем месяцы и дни цифрами: месяцы от 1(январь)до 12(декабрь), а дни недели от 1(пн) до 7(вт)

data_upd$month[data_upd$month == "jan"] <- 1
data_upd$month[data_upd$month == "feb"] <- 2
data_upd$month[data_upd$month == "mar"] <- 3
data_upd$month[data_upd$month == "apr"] <- 4
data_upd$month[data_upd$month == "may"] <- 5
data_upd$month[data_upd$month == "jun"] <- 6
data_upd$month[data_upd$month == "jul"] <- 7
data_upd$month[data_upd$month == "aug"] <- 8
data_upd$month[data_upd$month == "sep"] <- 9
data_upd$month[data_upd$month == "oct"] <- 10
data_upd$month[data_upd$month == "nov"] <- 11
data_upd$month[data_upd$month == "dec"] <- 12

# Задание 2:
# Теперь отберём факторы, которые будем включать в модель, для этого посмотрим на корреляции между различными переменными и построим корреляционную матрицу
library("corrplot")
library("caret")

cor(data_upd$X,data_upd$area)
cor(data_upd$Y, data_upd$area)
cor(data_upd$FFMC, data_upd$area)
cor(data_upd$DMC, data_upd$area)
cor(data_upd$DC, data_upd$area)
cor(data_upd$ISI, data_upd$area)
cor(data_upd$temp, data_upd$area)
cor(data_upd$RH, data_upd$area)
cor(data_upd$wind, data_upd$area)
cor(data_upd$rain, data_upd$area)
# Сделаем преобразованием категориальных переменных
bin <- dummyVars(" ~ .", data = data_upd)
bindata <- data.frame(predict(bin, newdata = data_upd))

# Выведем корреляционную матрицу
cormatrix = cor(bindata)
corrplot(cormatrix, method="ellipse")
#Как и ожидалось, мы видим достаточно высокую зависимость между различными индексами, использующимися для оценки степени пожароопасности погоды, поскольку многие из них рассчитываются на основе аналогичных показателей ( в каких-то индексах этот набор меньше в других больше, однако есть пересечения в формулах расчёта данных индексов - подробнее можно прочитать в статье "A Data Mining Approach to Predict Forest Fires using Meteorological Data" by Paulo Cortez and Anibal Morais)
#Мы также можем отметить, что общая площаль пожаров наиболее коррелирована с температурой воздуха и влажностью воздуха, а также с днём недели "суббота". Если первые высокая корреляция с природными показателями вполне интуитивна, то зависимость пожаров от субботы может объясняться тем, что именно в выходные дни людипредпочитают отдыхать на природе, устраивать пикники и разводить костры, что может привести к пожарам при неаккуратном поведении.
#Помимо этого, все индексы сильно коррелируют с температурой (лишь RH - относительная слажность воздуха - имеет чуть меньшую связь с температурой), поскольку данные показатели рассчитываются на основе нескольких составляющих и включают вв себя температуру в том числе.
#С температурой воздуха также коррелирует месяц года, что вполне очевидно и связано с особенностями климата
#Прелагаю включить в модель следующие признаки: летние месяцы года (июнь-август), выходные дни (суббота-воскресенье), DC(норму засухи), температуру и ветер. Теперь коротко поясню свой выбор.
#Температура:высокая температура воздуха увеличивает шансы возникновения пожаров, а также их более быстрому распространению и услилению процесса горения => ожидаемый эффект: положительный
#Летние месяцы: летние месяцы наиболее благоприятны для активного отдыха на природе, когда из-за неаккуратности людей что-нибудь может загореться и произойдёт пожар => ожидаемый эффект: положительный
#Выходные: во время выходных люди стараются выбираться на дачу или проводят время на природе, при этом часто безответствеено себя ведут, разжигают костры, оставляют мусор, забывая о том, что при контакте огня и бмуаги или травы может возникнуть пожар => ожидаемый эффект: положительный
#Ветер:в более вытреных районах риск пожаров выше ввиду климатических и физических особенностей, огонь легко распространяется по поверхности => ожидаемый эффект: положительный

#Описательные статистики
summary(bindata$temp)
var(bindata$temp)
summary(bindata$month6)
var(bindata$month6)
summary(bindata$month7)
var(bindata$month7)
summary(bindata$month8)
var(bindata$month8)
summary(bindata$daysat)
var(bindata$daysat)
summary(bindata$daysun)
var(bindata$daysun)
summary(bindata$wind)
var(bindata$wind)

#Тест Шапиро-Уилка о нормальности распределения
shapiro.test(bindata$temp)
shapiro.test(bindata$wind)
shapiro.test(bindata$area)
shapiro.test(log(bindata$temp))
#Во всех случаях H0 отвергается => распределение не является нормальным

#Визуализируем выбранные признаки:
qplot(data = bindata, temp, xlab="temp", ylab = "area", main = "area distribution according to temperature", binwidth = 1)
qplot(data = bindata, month6, xlab="june", ylab = "area", main = "area distribution according to month (june)")
qplot(data = bindata, month7, xlab="july", ylab = "area", main = "area distribution according to month (july)")
qplot(data = bindata, month8, xlab="august", ylab = "area", main = "area distribution according to month (august)")
qplot(data = bindata, daysat, xlab="daysat", ylab = "area", main = "saturday's area distribution")
qplot(data = bindata, daysun, xlab="daysun", ylab = "area", main = "sunday's area distribution")
qplot(data = bindata, wind, xlab="wind", ylab = "area", main = "area distribution according to wind", binwidth = 1)
#В данных по температуре есть выбросы: в обоих случаях в меньшую сторону

#Ящики с усами
boxplot(bindata$temp, main = "Temperature's probability distribution", xlab = "degrees Celsius", col = "red",  border = "black", horizontal = TRUE, notch = FALSE)
boxplot(bindata$wind, main = "Winds's probability distribution", xlab = "degrees Celsius", col = "red",  border = "black", horizontal = TRUE, notch = FALSE)

#Избавляемся от выбросов
bindata$temp[bindata$temp < 5] <- NA
na.omit("temp")

#Строим модель наконец :))
m1 <- lm(data = bindata, log(area) ~ temp + month6 + month7 + month8 + daysat + daysun + wind)
summary(m1)

# Задание 3:
#Проверка на мультиколинеарность
library("car")
vif(m1)

# CN
install.packages("olsrr")
library(olsrr)
ols_coll_diag(m1)

# В нашей модели нет мультиколинеарности: VIF везде в районе 1 (то есть значительно меньше 10), CN = 12.77< 30

# Задание 4:
# Посмотрим на модель ещё раз
summary(m1)
# Как мы и предполагали, температура, ветер и  выходной день недели влияют на пожары положительно, а вот летние месяцы оказывают отрицательный эффект согласно построенной модели.Значимыми оказались коэффициенты при переменных месяц август и день недели суббота на 0,1% и 1% уровне значимости.

library("glmnet")
library("tseries")
coeftest(m1)
confint(m1)

# Проверка остатков регрессии на нормальность
jarque.bera.test(residuals(m1))
# Остатки модели нормально распределены: p-value = 0.076 => на 1%, 2,5% И 5% уровне значимости. Ура!

# Задание 5:
# Точечный интервал
predict(m1, newdata = bindata)
# Доверительный интервал для среднего
predict(m1, newdata = bindata, interval = "confidence")
# Предиктивный интервал индивидуального значения
predict(m1, newdata = bindata, interval = "prediction")

# Задание 6:
# Возможная гетероскедастичность
# Первое, что приходит в голову, это гетеровскедастичность переменной temp, поскольку природа и погодные условия крайне непредстказуемы: резкие перепады температуры будут оказывать такое же резкое воздействие на изменение вероятности возникновения пожара, поэтому sigma будет волатильна.
# Второй переменной, попадающей под подозрение о наличии гетероскедастичности, является ветер, так как неожиданное изменение направления ветра или его усиление может оказывать серьёзное влияние на масштабы распространения пожара.

# Попробуем выявить гетероскедастичность
# Вначале воспользуемся графическим способом
qplot(data = bindata, temp, log(area), main = "Is temperature hetero?")
qplot(data = bindata, wind, log(area), main = "Is wind hetero?")
# Построенные графики говорят нам о том, что ветер всё-таки скорее гомоскедастичен, а температура гетероскедастична
# Посмотрим на оценку ковариационной матрицы
library("sandwich")
vcov(m1)

# Тест Голдфельда-Квандта
install.packages("prettyR")
install.packages("devtools")
install.packages("broom")

# Поменяем порядок строк в табличке bindata, отсортируем по temp
bindata_order1 <- bindata[order(bindata$temp), ] 
m2 <- lm(data = bindata_order1, log(area) ~ temp + month6 + month7 + month8 + daysat + daysun + wind) 
# Проведем GQ тест, убрав посередине 20% наблюдений
gqtest(m2, fraction = 0.2) 
# H0 отвергается при 1% и 5% уровне значимости => есть гетероскедастичность для температуры

# Поменяем порядок строк в табличке bindata, отсортируем по wind
bindata_order2 <- bindata[order(bindata$wind), ]
m3 <- lm(data = bindata_order2, log(area) ~ temp + month6 + month7 + month8 + daysat + daysun + wind) 
# Проведем GQ тест, убрав посередине 20% наблюдений
gqtest(m3, fraction = 0.2)
#H0 не отвергается при всех уровнях значимости => нет гетероскедастичности для ветра
#Согласно тесту Голдфельда-Квандта температура оказалась гетероскедастична, а для ветра, напротив, она не наблюдается
#Результаты графического и тестового методов совпали, ура, мы мололцы!

# Задание 8:
#Взвешенный МНК(не получилось, поэтому не буду ничего писать)

# Задание 9:
#При гетероскедастичности проблема в стандартных ошибках β с крышкой, они несостоятельны. Из-за этого мы не можем строить доверительные интервалы. 
#Чтобы справиться с этой проблемой, используем другие стандартные ошибки – в форме Уайта. 
#Уайт предложил корректировку: стандартные ошибки β с крышкой = X'X в минус 1 на X' на оценку матрицы Ω на X'X в минус 1, где Ω — это диагональная матрица, по диагонали квадраты остатков из обычной МНК регрессии. Вначале строится обычная регрессия при помощи МНК, по старой формуле считается β с крышкой. 
#После этого аналогично считаются ошибки ε с крышкой. А после этого каждая из ошибок из остатков ε с крышкой возводится в квадрат. 
#Получается диагональная матрица, на диагонали квадраты остатков, вне диагонали – нули. Эта матрица подставляется в формулу для Var с крышкой β с крышкой, и получаются другие оценки дисперсии β с крышкой, которые состоятельный в условиях гетероскедастичности.
#Вычислим робастные ошибки в форме Уайта
vcovHC(m1, type = "HC0")
#Найдём p-value для всех переменных
coeftest(m1, vcov. = vcovHC(m1))
#Сравнение результатов значимости коэффициентов с линейной моделью
coeftest(m1)
#Критические значения (p-value) стали немного ниже после применения робастных ошибок, значимыми так остались коэффициенты при month8 и daysat, однако коэффициент при переменной суббота повысил свою значимость до 5% уровня 
#Повторим то же самое для HC3 (p-value также снизились в сравнении с изначальной моделью)
vcovHC(m1, type = "HC3")
coeftest(m1, vcov. = vcovHC(m1, type="HC3"))

# Задание 10:
#Преобразование независимых переменных  с помощью PCA
install.packages("factoextra")
library(factoextra)

d <- data.frame(DSat = bindata$daysat, DSun = bindata$daysun, M6 = bindata$month6, M7 = bindata$month7, M8 = bindata$month8, T = bindata$temp, W = bindata$wind, NewFFMC = bindata$FFMC)
d <- na.omit(d)
#Сделаем наши переменные стандартизированными
pcad <- prcomp(d, scale = TRUE)
#Извлечём первую главную компоненту
c1 <- pcad$x[, 1]
head(c1)
#Извлечём веса, с которыми переменные входят в первую главную компоненту
v1 <- pcad$rotation[, 1]
v1
summary(pcad)
#Первая главная компонента объясняет четверть дисперсии, вторая - 16%


#Построим линейную регрессию на первые две гк
m4 <- lm(data = bindata, log(area) ~ c1)
summary(m4)
#Возникает ошибка, не смогла разобраться, что не так, поэтому на этом мы и закончим)
#Спасибо! Было сложно и бессонно, но этопомгло мне узнать много нового и познакомиться с R :)



