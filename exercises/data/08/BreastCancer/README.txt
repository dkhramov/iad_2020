BreastCancer
=============

Задача: спрогнозировать по результатам биопсии, является опухоль доброкачественной (benign) или злокачественной (malignant).

Переменная 1 -- идентификатор анализа. Переменные 2-10 -- результаты биопсии, преобразованные в числа, лежащие в диапазоне от 0 до 10.

Размерность данных: 699 наблюдений, 11 переменных (признаков). Имеется 16 пропущенных значений.

Признаки:

1. Sample code number: id number
2. Clump Thickness: 1 - 10
3. Uniformity of Cell Size: 1 - 10
4. Uniformity of Cell Shape: 1 - 10
5. Marginal Adhesion: 1 - 10
6. Single Epithelial Cell Size: 1 - 10
7. Bare Nuclei: 1 - 10
8. Bland Chromatin: 1 - 10
9. Normal Nucleoli: 1 - 10
10. Mitoses: 1 - 10
11. Class: (2 for benign, 4 for malignant)

Источник данных: [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Original))

Информация о достигнутой точности классификации: [Published accuracy results](https://towardsdatascience.com/building-a-simple-machine-learning-model-on-breast-cancer-data-eca4b3b99fa3)

В R данные BreastCancer находятся в пакете mlbench. Загрузка данных:

```
install.packages("mlbench")  # установка mlbench
library(mlbench)
data("BreastCancer")
```
