Sonar, Mines vs. Rocks
=======================

Задача: определить по сигналу сонара (гидролокатора), обнаружен металл (M) или камень (R).

Наблюдение представляет собой набор из 60 чисел в диапазоне от 0.0 до 1.0. Каждое число -- это доля энергии, возвращенная сонару в определенном диапазоне частот. Переменная 61 -- метка класса: металл ("M", mine) или камень ("R", rock).

Размерность данных: 208 наблюдений, 61 переменная (признак).

Источник данных: [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Connectionist+Bench+(Sonar,+Mines+vs.+Rocks))

Информация о достигнутой точности классификации: [Published accuracy results](https://www.simonwenkel.com/2018/08/23/revisiting_ml_sonar_mines_vs_rocks.html)

Набор данных использовался Горманом и Сейновским при изучении классификации сигналов гидролокатора с помощью нейронной сети [1]. Задача состояла в том, чтобы обучить сеть различать сигналы гидролокатора, отраженные от металлических и каменных цилиндров.

1. Gorman, R. P., and Sejnowski, T. J. (1988).  "Analysis of Hidden Units
in a Layered Network Trained to Classify Sonar Targets" in Neural Networks,
Vol. 1, pp. 75-89.


В R данные Sonar находятся в пакете mlbench. Загрузка данных:

```
install.packages("mlbench")  # установка mlbench
library(mlbench)
data("Sonar")
```

