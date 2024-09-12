# recolhendo os dados
dados = nome-do-conjunto-de-dados$nome-da-variavel

# tamanho da população
N <- length(dados)

# tamanho da amostra
n = 10

# realizando o sorteio e guardando-os em um vetor auxiliar
vet.aux <- sample(N, n)
vet.aux

# coletando os dados
amostra.cs <- c()

for (i in 1:n)
    amostra.cs[i] <- dados[vet.aux[i]]

# calcular média dos dados
media.cs <- mean(amostra.cs)
media.cs