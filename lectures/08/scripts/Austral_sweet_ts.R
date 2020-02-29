wine <- read.table("../data/wine_Austral.dat", sep="\t", header = T)
# Преобразуем год и месяц в дату
x <- paste(wine$year_,wine$month_,sep="-")
date <- as.Date(paste(x,"-01",sep=""))

plot(date, wine$sweet, type="b", ylab="sweet")
