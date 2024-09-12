# armazenas coluna de variável
dadosint <- nome-da-base-de-dados$nome-da-coluna

# tratamento para NAs
ta <- length(dadosint) - sum(is.na(dadosint))

# armazena apenas os dados filtratos
dadosfin <- dadosint[1:ta]

# criar tabela de distribuição

# A = amplitude de dados
# n = numero total de elementos
# k = numero de classes, com base em n
# C = tamanho da classe, arredondados para cd cadas decimais
# LI = limite inferior das classes
# media = média dos limites inferiores e superiores
# limites inferiores (LI), superiores (LS), ponto médio da classe (X), frequência absoluta (Fa), frequência relativa (Fr), e frequência percentual (Fp).

tabela <- function(dados, cd) {
    A <- max(dados) - min(dados)
    n <- length(dados)
    if (n <= 100) {
        k <- ceiling(sqrt(n))
    } else {
        k <- ceiling(5 * log10(n))
    }
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
        tabela[i, 4] <- (TDF$counts[i])
        tabela[i, 5] <- round(((TDF$counts[i]) / n), 5)
        tabela[i, 6] <- round((100 * TDF$counts[i]) / n, 3)
    }
    
    colnames(tabela) <- c("LI", "LS", "X", "Fa", "Fr", "Fp")
    return(tabela)
}

# Calcular quebras de limites para o histograma

quebras <- function(dados, cd) {
    A <- max(dados) - min(dados)
    n <- length(dados)
    if (n <= 100) {
        k <- ceiling(sqrt(n))
    } else {
        k <- ceiling(5 * log10(n))
    }
    C <- round(A / (k - 1), cd)
    
    LI <- c(rep(0, (k + 1)))
    LI[1] <- round(min(dados) - (C) / 2, cd)
    
    for (i in 2:(k + 1)) {
        LI[i] <- round(LI[i - 1] + C, cd)
    }
    
    return(LI)
}

# Construção do histograma e tabela de distribuição de frequência

TDF <- hist(dadosfin, breaks = limites, plot = FALSE, right = FALSE)
TDF

hist(
    dadosfin, label = FALSE, main = "", 
    xlab = "Peso (g)", ylab = "Frequência absoluta",
    ylim = c(0, (max(TDF$counts) + 1)),
    breaks = limites, axes = FALSE, right = FALSE
)
axis(1, at = limites, pos = c(0, 0))
axis(2, at = c(seq(0:(max(TDF$counts) + 1)) - 1))
