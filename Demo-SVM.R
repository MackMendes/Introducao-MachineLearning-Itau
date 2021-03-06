## Exemplo, Aprendizado de M�quina n�o supervisinado
# Carregamento em mem�rio a biblioteca com os algoritmos de SVM
library("e1071")

# Obtendo o dataset com esp�cies da flor Iris
datasetIris <- iris

names(datasetIris) <- c("Sepala.Tamanho", "Sepala.Largura", "Petala.Tamanho", "Petala.Largura", "Especies")

# Vamos verificar como esta a distribui��o deste dataset
summary(datasetIris)


datasetIrisPart  <- datasetIris[,c(3,4,5)]
summary(datasetIrisPart)


# Desta vez, preciso mostrar um pouco do Dataset para o meu algoritmo e ele tem que aprender.
# Depois, tenho que testa-lo com o resto do dataset

indexes = sample(1:nrow(datasetIrisPart), size=(0.3*nrow(datasetIrisPart)))
#################################
# Cortar o dataset aleatoriamente
dataset_treino = datasetIrisPart[-indexes,]
dataset_teste = datasetIrisPart[indexes,]

# Mostrar dataset de teste
summary(dataset_teste)

# Mostrar dataset de treino
summary(dataset_treino)

attach(dataset_treino)

####
# Vou treinar o meu modelo SVM!
svm_model <- svm(Especies ~ ., data = dataset_treino)
####


#svm_model <- svm(Especies ~ ., data=dataset_treino, kernel="radial", cost=1, gamma=1)

#Plot the results
plot(svm_model , dataset_treino)




# Pegando apenas as colunas com os tamanhos e larguras (da S�pala e da P�tala) - chamado de X
tamanhos_Larguras_teste = subset(dataset_teste, select = -Especies)

summary(tamanhos_Larguras_teste)

####
# Agora, vou pegar o Dataset de Teste, sem os r�tulos das Esp�cies
svm_predicao <- predict(svm_model, newdata = tamanhos_Larguras_teste)
####


# Agora, vamos verificar os acertos da Predi��o feita acima, passando apenas os r�tulos com as Esp�cies
table(dataset_teste[,3], svm_predicao)


# Mostrar dataset de teste, novamente
summary(dataset_teste)

# Calculando a acur�cia
sum(diag(table(dataset_teste[,3], svm_predicao))) / length(dataset_teste[,3])

plot(svm_model, dataset_teste)


