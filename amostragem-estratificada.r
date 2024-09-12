############# 1 opcao #############

TE = c(873, 386, 246, 186, 112)  # Tamanhos dos estratos inseridos manualmente
N = 1803  # Tamanho total da população
n = 100   # Tamanho total da amostra

# Calcular o tamanho da amostra para cada estrato
ni = n / N * TE  # Calcula o tamanho da amostra proporcional para cada estrato
ni2 = round(ni, 0)  # Arredonda os tamanhos da amostra para números inteiros

# Realiza a amostragem casual simples nas populações homogêneas
sample(TE[1], ni2[1])
sample(TE[2], ni2[2])
sample(TE[3], ni2[3])
sample(TE[4], ni2[4])
sample(TE[5], ni2[5])

############# 2 opcao #############

# recolhendo os dados
dados = nome-do-conjunto-de-dados$nome-da-variavel

# tamanho da população
N <- length(dados)

# tamanho da amostra
n = 18

# verifica o numero de elementos em cada estrato
NE <- summary(nome-do-conjunto-de-dados$nome-da-variavel)

# calcular o tamanho da amostra em cada estrato
ne <- (NE / N) * n

# Arredonda os valores manualmente para inteiros (inserir manualmente)
ne = c(12, 5, 1)

# Verifica se o total corresponde ao tamanho da amostra
sum(ne)

# Coletando os elementos de cada estrato
# repita esse processo para cada estrato, modificando o valor
vet.aux.est1 = c()  # Inicializa um vetor auxiliar para o estrato 1
j <- 1
for(i in 1:N) {
    if(Nome_Dos_Dados$Titulacao[i] == "E") {
        vet.aux.est1[j] <- i
        j <- j + 1
    }
}

vet.aux.est2 = c()  # Inicializa um vetor auxiliar para o estrato 2
j <- 1
for(i in 1:N) {
    if(Nome_Dos_Dados$Titulacao[i] == "G") {
        vet.aux.est2[j] <- i
        j <- j + 1
    }
}

# Seleciona as amostras de cada estrato
# repita esse processo para cada estrato, modificando o valor
PE1 <- c()  # Inicializa vetor para os elementos do estrato 1
for(i in 1:NE[1]) {
    PE1[i] <- dados[vet.aux.est1[i]]  # Coleta os dados de renda do estrato 1
}

PE2 <- c()  # Inicializa vetor para os elementos do estrato 2
for(i in 1:NE[2]) {
    PE2[i] <- dados[vet.aux.est2[i]]  # Coleta os dados de renda do estrato 2
}

# Sorteio dentro de cada estrato
# repita esse processo para os estratos restantes, modificando o valor
vet.aux.e1 <- sample(NE[1], ne[1])  # Sorteio de amostras do estrato 1
amostra.e1 <- c()
for(i in 1:ne[1]) {
    amostra.e1[i] <- dados[vet.aux.e1[i]]  # Coleta os dados sorteados do estrato 1
}
amostra.e1  # Apresenta os dados sorteados do estrato 1

vet.aux.e2 <- sample(NE[2], ne[2])  # Sorteio de amostras do estrato 2
amostra.e2 <- c()
for(i in 1:ne[2]) {
    amostra.e2[i] <- dados[vet.aux.e2[i]]  # Coleta os dados sorteados do estrato 2
}
amostra.e2  # Apresenta os dados sorteados do estrato 2

# Cálculo da média
amostra.est <- c(amostra.e1, amostra.e2, amostra.e3)  # Junta as amostras dos três estratos
media.est <- mean(amostra.est)  # Calcula a média da amostra estratificada