Abalone (морское ушко)
======================

Описание данных
---------------

Морское ушко (Abalone) – деликатесный моллюск, который выращивается в Австралии, Америке и Восточной Азии. Является отличным источником железа и пантотеновой кислоты: 100 г. морского ушка дают более 20% рекомендуемой суточной нормы потребления этих веществ. Чем старше моллюск, тем он ценнее. Однако современные технологии определения возраста моллюсков затратны и неэффективны. По размерам определить возраст ушка нельзя, поскольку размер зависит не только от возраста, но и от питания моллюска. Поэтому фермеры режут ракушки и с помощью микроскопа подсчитывают кольца – чем больше колец, тем старше моллюск. Наша цель: спрогнозировать число колец (возраст моллюска) с помощью методов машинного обучения.

Число наблюдений: 4177, число переменных: 8. 

Описание переменных: 

Name			Data Type	Meas.	Description
----			---------	-----	-----------
Sex				nominal				M, F, and I (infant)
Length			continuous	mm		Longest shell measurement
Diameter		continuous	mm		perpendicular to length
Height			continuous	mm		with meat in shell
Whole weight	continuous	grams	whole abalone
Shucked weight	continuous	grams	weight of meat
Viscera weight	continuous	grams	gut weight (after bleeding)
Shell weight	continuous	grams	after being dried
Rings			integer				+1.5 gives the age in years
 
**Задача.** Спрогнозировать число колец (Rings).

Источник данных: http://archive.ics.uci.edu/ml/datasets/Abalone
