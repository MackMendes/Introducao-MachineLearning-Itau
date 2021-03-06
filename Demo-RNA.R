## Exemplo, Aprendizado de M�quina n�o supervisinado
# Carregamento em mem�rio a biblioteca da Rede Neural
library(nnet)

# Obtendo o dataset com esp�cies da flor Iris
datasetIris <- iris

names(datasetIris) <- c("Sepala.Tamanho", "Sepala.Largura", "Petala.Tamanho", "Petala.Largura", "Especies")

# Vamos verificar como esta a distribui��o deste dataset
summary(datasetIris)


# Desta vez, preciso mostrar um pouco do Dataset para o meu algoritmo e ele tem que aprender.
# Depois, tenho que testa-lo com o resto do dataset

xEntrada <- data.frame(datasetIris$Sepala.Tamanho, datasetIris$Sepala.Largura, datasetIris$Petala.Tamanho,
                       datasetIris$Petala.Largura)

xSaida <- data.frame(setosa = ifelse(datasetIris$Especies == "setosa",1,0), 
                     versicolor = ifelse(datasetIris$Especies == "versicolor",1,0), 
                     virginica = ifelse(datasetIris$Especies == "virginica",1,0))

summary(xEntrada)

summary(xSaida)


indexes = sample(1:nrow(datasetIris), size=(0.3*nrow(datasetIris)))


#################################
# Cortar o dataset aleatoriamente
dataset_treino_Entrada = xEntrada[-indexes,]
dataset_teste_Entrada = xEntrada[indexes,]

# Cortar o dataset aleatoriamente
dataset_treino_Saida = xSaida[-indexes,]
dataset_teste_Saida = xSaida[indexes,]


####
# Vou treinar a minha rede neural
# Size = Tamanho da camada escondida;
# rang = Pesos aleat�rios iniciais em [-rang, rang];
# Decay = Par�metro para a deteriora��o de peso;
# maxit = M�ximo n�mero de intera��es at� convergi;
rna_model <- nnet(dataset_treino_Entrada, dataset_treino_Saida, size = 2, rang = 0.1,
                  decay = 5e-4, maxit = 1000)

####

library(NeuralNetTools)

plotnet(rna_model, alpha=0.6)

####
# Agora, vou pegar o Dataset de Teste, sem os r�tulos das Esp�cies 
rna_predicao <- round(predict(rna_model, dataset_teste_Entrada))
####


Table1<-abs(rna_predicao - dataset_teste_Saida)


Error <- (sum(Table1)/2) / nrow(dataset_teste_Saida)

# Calculando acur�cia
1-Error





#Plot the results
#plot(dataset_treino_Entrada, rna_model$fitted.values)


