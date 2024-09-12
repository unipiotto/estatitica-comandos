############# 1 opcao #############

# tamanho da população
N=1000

# tamanho da amostra
n=50

# calcula os passos de amostragem
R=round(N/n,0)

# Selecionando o primeiro elemento aleatoriamente
sort_elem1=sample(R,1)

# Definindo as posições dos elementos amostrados
posicao=seq(sort_elem1,N,R);

############# 2 opcao #############

# recolhendo os dados
dados=nome-do-conjunto-de-dados$nome-da-variavel

# tamanho da população
N<-length(dados)

# tamanho da amostra
n=18

# calcula os passos de amostragem
R=round(N/n,0)

# Selecionando o primeiro elemento aleatoriamente
sort_elem1=sample(R,1)

# Definindo as posições dos elementos amostrados
posicao=seq(sort_elem1,N,R)

#Conferindo o número de elementos da amostra
length(posicao)

# SE FALTAR ALGUM ELEMENTO
posicao[17]+R-N

posicao<-c(posicao,8) #Acrescentando a oitava observação na amostra
posicao

# coletando os dados da amostra
amostra.sis<-c()

for(i in 1:n) {
    amostra.sis[i] <- dados[posicao[i]]
}
amostra.sis