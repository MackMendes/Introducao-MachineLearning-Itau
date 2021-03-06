## Exemplo, Aprendizado de M�quina n�o supervisinado

# Obtendo o dataset com esp�cies da flor Iris
datasetIris <- iris

names(datasetIris) <- c("Sepala.Tamanho", "Sepala.Largura", "Petala.Tamanho", "Petala.Largura", "Especies")

# Vamos verificar como esta a distribui��o deste dataset
summary(datasetIris)

# Pegando apenas as colunas com os tamanhos e larguras (da S�pala e da P�tala)
tamanhos_Larguras = datasetIris[,-5]

# Executar o algoritmo k-means com o dataset filtrado acima (x) e pedindo para encontrar 3 grupos (centers).
resultado <- kmeans(x = tamanhos_Larguras, centers = 3)

# monstrar o resultado
resultado

# Unindo os resultados com os nomes das esp�cies 
table(datasetIris$Especies, resultado$cluster)

# Plotar o resultado, levando em considera��o os atributos da S�pala
plot(tamanhos_Larguras[c("Sepala.Tamanho", "Sepala.Largura")], col=resultado$cluster)
points(resultado$centers[,c("Sepala.Tamanho", "Sepala.Largura")], col=1:3, pch="o", cex=3)
