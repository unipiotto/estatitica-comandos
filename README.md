### 1. Amostragem Simples ao Acaso

#### Definição:
A **amostragem simples ao acaso** envolve a seleção de elementos de uma população de maneira completamente aleatória, onde cada elemento tem a mesma probabilidade de ser selecionado.

#### Quando Utilizar:
- **Populações homogêneas**: Quando não há grandes diferenças entre os elementos da população.
- **Situações sem categorias ou grupos claramente definidos**: Se todos os indivíduos ou itens da população são relativamente similares ou não podem ser divididos em subgrupos significativos.
- **Exemplo Prático**: Escolher uma amostra aleatória de pessoas para uma pesquisa em uma pequena cidade, onde as características da população são bastante homogêneas.

#### Aplicação:
No código, você utiliza a função `sample()` para realizar a amostragem simples. Os dados sorteados são utilizados para construir uma tabela de frequência e histograma, como mostrado:

```r
amostra.cs <- c()
for(i in 1:n) {
    amostra.cs[i] <- dados[vet.aux[i]]
}
```

### 2. Amostragem Estratificada

#### Definição:
A **amostragem estratificada** divide a população em subgrupos (estratos) com base em características específicas, e uma amostra é sorteada proporcionalmente de cada estrato.

#### Quando Utilizar:
- **Populações heterogêneas**: Quando há subgrupos distintos na população que podem ter características significativamente diferentes.
- **Deseja garantir representatividade**: Ideal quando é importante garantir que cada subgrupo (estrato) seja representado na amostra.
- **Exemplo Prático**: Ao realizar uma pesquisa de satisfação em uma universidade, estratificando por departamento ou ano de curso para garantir que todos os grupos de estudantes estejam representados.

#### Aplicação:
No código, a amostragem estratificada é aplicada separando os dados por estrato (como "E", "G", "MD") e sorteando amostras de cada um. Isso garante que todos os estratos sejam representados de forma justa.

```r
vet.aux.est1 = c()  # Estrato 1
vet.aux.est2 = c()  # Estrato 2
vet.aux.est3 = c()  # Estrato 3
# Coletando amostras de cada estrato
amostra.est <- c(amostra.e1, amostra.e2, amostra.e3)
```

### 3. Amostragem Sistemática

#### Definição:
A **amostragem sistemática** envolve a seleção de elementos de maneira regular após a escolha de um ponto inicial aleatório. O intervalo de seleção é calculado dividindo o tamanho da população pelo tamanho da amostra.

#### Quando Utilizar:
- **Populações organizadas sem padrões periódicos**: Ideal para populações onde os elementos são listados em uma ordem aleatória ou sem uma periodicidade evidente.
- **Grande número de elementos**: Quando o tamanho da população é muito grande e uma amostragem completamente aleatória seria difícil ou demorada.
- **Exemplo Prático**: Selecionar uma amostra de clientes a partir de uma longa lista de registros, onde os registros estão ordenados de maneira aleatória.

#### Aplicação:
A amostragem sistemática é implementada no código com um ponto inicial sorteado e um intervalo fixo (R) entre os elementos selecionados:

```r
R = round(N / n, 0)  # Intervalo de amostragem
sort_elem1 = sample(R, 1)  # Sorteio do primeiro elemento
posicao = seq(sort_elem1, N, R)  # Seleção sistemática
```

---

### Construção da Tabela de Frequência e Histograma

Após coletar os dados amostrados, a construção da tabela de distribuição de frequências e do histograma é a etapa seguinte para analisar e visualizar a distribuição dos dados.

#### Tabela de Frequência

A tabela de frequência é construída utilizando as amostras obtidas, com os seguintes passos:
- **Definir as classes** (intervalos) dos dados.
- **Contar as frequências** de cada classe, ou seja, quantos valores caem em cada intervalo.
- **Calcular as frequências relativas e percentuais** para melhor interpretação.

No código, a função `tabela()` constrói essa tabela a partir dos dados amostrados:

```r
tabela <- function(dados, cd) {
    # Amplitude e número de classes
    A <- max(dados) - min(dados)
    n <- length(dados)
    k <- if (n <= 100) ceiling(sqrt(n)) else ceiling(5 * log10(n))
    
    C <- round(A / (k - 1), cd)
    LI <- c(rep(0, (k + 1)))
    media <- c(NA)
    LI[1] <- round(min(dados) - (C) / 2, cd)
    
    for (i in 2:(k + 1)) {
        LI[i] <- round(LI[i - 1] + C, cd)
        media[i - 1] <- mean(c(LI[i], LI[i - 1]))
    }
    
    limites <- LI
    TDF <- hist(dados, breaks = limites, plot = FALSE, right = FALSE)
    tabela <- matrix(c(rep(6 * k)), k, 6)
    
    for (i in 1:k) {
        tabela[i, 1] <- round(LI[i], cd)
        tabela[i, 2] <- round(LI[i + 1], cd)
        tabela[i, 3] <- round(media[i], cd)
        tabela[i, 4] <- TDF$counts[i]  # Frequência absoluta
        tabela[i, 5] <- round((TDF$counts[i] / n), 5)  # Frequência relativa
        tabela[i, 6] <- round((100 * TDF$counts[i]) / n, 3)  # Frequência percentual
    }
    
    colnames(tabela) <- c("LI", "LS", "X", "Fa", "Fr", "Fp")
    return(tabela)
}
```

#### Histograma

O **histograma** é uma representação gráfica da distribuição de frequências, onde cada barra representa a frequência de uma classe. O eixo X mostra os intervalos de classe, e o eixo Y a frequência absoluta ou relativa.

No código, o histograma é gerado com a função `hist()`:

```r
hist(
    dadosfin, 
    label = FALSE, 
    main = "", 
    xlab = "Peso (g)",  # Título do eixo X
    ylab = "Frequência absoluta",  # Título do eixo Y
    ylim = c(0, (max(TDF$counts) + 1)),  # Definição dos limites do eixo Y
    breaks = limites,  # Limites calculados
    axes = FALSE, 
    right = FALSE
)
```

### Quando utilizar a tabela de frequência e o histograma:
- **Tabela de Frequência**: Utilizada para análise detalhada e numérica dos dados amostrados, permitindo observar a distribuição em classes, frequência absoluta, frequência relativa e percentual.
- **Histograma**: Ideal para a visualização gráfica da distribuição dos dados, facilitando a compreensão da variação e concentração dos valores em cada intervalo.