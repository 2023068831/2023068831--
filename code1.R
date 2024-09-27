#Imput dataset
mof = read.csv("04. mof.CSV",fileEncoding = 'cp949')

# column names
names(mof)

# dim
dim(mof)

# summary
summary(mof)

# boxplot

boxplot(mof[,6:9])
