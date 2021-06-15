# Análise Exploratória de Dados com Liguagem R

# Definindo o diretório de trabalho
setwd("C:/FCD/R")
getwd()

# Carregando os pacotes

library(corrplot)
library(gridExtra)
library(ggplot2)


# Criando um Dataframe para teste

df <- data.frame(Estado =c("SP", "BA", "AM"),
                 Plano = c("Bronze", "Ouro", "Prata"),
                 Valor = c(500, 300, 150),
                 Total = c(1000, 300, 300),
                 Dependentes = c(2,1,2),
                 Naturalidade = c("BR", "BR", "US"),
                 Desconto = c("Sim", "Não", "Não"))
df

str(df)



# Limpeza dos Dados

# Verificando valores ausentes

sapply(df, function(x) sum(is.na(x)))



# Análise Preliminar dos Dados

# Correlação entre variáveis numéricas

numeric.var <- sapply(df, is.numeric)
corr.matrix <- cor(df[,numeric.var])
corrplot(corr.matrix, main="\n\nGráfico de Correlação para Variáveis Numéricas", method="number")

# Gráficos de barra de variáveis categóricas

p1 <- ggplot(df, aes(x=Estado)) + ggtitle("Localidade") + xlab("Estado") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p2 <- ggplot(df, aes(x=Plano)) + ggtitle("Modalidade") + xlab("Tipo De Plano") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p3 <- ggplot(df, aes(x=Dependentes)) + ggtitle("Outros") + xlab("Dependentes") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
p4 <- ggplot(df, aes(x=Naturalidade)) + ggtitle("Origem") + xlab("Naturalidade") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
grid.arrange(p1, p2, p3, p4, ncol=2)















