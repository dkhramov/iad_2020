Satellite
==========

Набор данных состоит из значений пикселей в окрестностях 3х3 на мультиспектральном спутниковом снимке. Число признаков -- 36 (= 4 спектральных канала x 9 пикселей в окрестности). Каждому образцу сопоставлен класс земной поверхности, наблюдаемый в центральном пикселе окрестности.

Цель состоит в том, чтобы построить классификацию земной поверхности по новым спутниковым снимкам.

Классы земной поверхности:

	Number			Class

	1			red soil
	2			cotton crop
	3			grey soil
	4			damp grey soil
	5			soil with vegetation stubble
	6			mixture class (all types present) - удалены из набора данных.
	7			very damp grey soil 

Файлы данных:

* sat.trn -- обучающая выборка;
* sat.tst -- тестовая выборка.

Источник данных: [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Statlog+(Landsat+Satellite)).

В R данные Satellite находятся в пакете mlbench. Загрузка данных:

```
install.packages("mlbench")  # установка mlbench
library(mlbench)
data("Satellite")
```
