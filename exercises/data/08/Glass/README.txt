Glass
=====

Задача заключается в прогнозировании типа стекла на основе данных химического анализа. Исследование вызвано потребностями криминалистики: необходимо по оставленному на месте преступления осколку стекла определить его тип (оконное, бутылочное, автомобильное и т. п.).

Переменные 1-9 -- результаты хим. анализа, 10 -- метка класса. Всего меток семь, метка 4 в наборе данных отсутствует.

Размерность данных: 214 наблюдений, 10 признаков. Есть повторяющиеся строки.

Признаки:

1. RI: refractive index
2. Na: Sodium (unit measurement: weight percent in corresponding oxide, as 
               are attributes 4-10)
3. Mg: Magnesium
4. Al: Aluminum
5. Si: Silicon
6. K: Potassium
7. Ca: Calcium
8. Ba: Barium
9. Fe: Iron
10. Type of glass: (class attribute)
    -- 1 building_windows_float_processed
    -- 2 building_windows_non_float_processed
    -- 3 vehicle_windows_float_processed
    -- 4 vehicle_windows_non_float_processed (none in this database)
    -- 5 containers
    -- 6 tableware
    -- 7 headlamps

Источник данных: [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Glass+Identification).

Информация о достигнутой точности классификации: [Published accuracy results](http://fizyka.umk.pl/kis-old/projects/datasets.html).

В R данные Glass находятся в пакете mlbench. Загрузка данных:

```
install.packages("mlbench")  # установка mlbench
library(mlbench)
data("Glass")
```
