library(ggplot2)
dados <- read.csv("/Program Files/R/CSV/movies.csv")

head(dados)
summary(dados)
str(dados)
# Calcule a contagem de ocorrências de cada gênero
contagem_generos <- table(dados$genres)

# Ordene as contagens em ordem decrescente
top_10_generos <- head(sort(contagem_generos, decreasing = TRUE), 10)

# Crie um dataframe com os top 10 gêneros e suas contagens
df_top_10_generos <- data.frame(genre = names(top_10_generos), count = as.numeric(top_10_generos))

# Crie o gráfico de barras dos top 10 gêneros
grafico <- ggplot(df_top_10_generos, aes(x = reorder(genre, -count), y = count)) +
  geom_bar(stat = "identity", fill = "#2c7fb8", width = 0.5) +
  labs(x = "Gênero", y = "Contagem", title = "Top 10 Gêneros Mais Repetidos") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_blank())

# Exiba o gráfico
print(grafico)

dados$ano <- as.integer(gsub(".*\\(([0-9]{4})\\).*", "\\1", dados$title, perl = TRUE))

# Remover linhas com NA na coluna "ano"
dados <- dados[!is.na(dados$ano), ]

# Calcule a contagem de ocorrências de cada ano
contagem_ano <- table(dados$ano)

# Ordene as contagens em ordem crescente (filmes mais antigos)
top_10_filmes_antigos <- head(sort(contagem_ano), 10)

# Crie um dataframe com os top 10 anos e suas contagens
df_top_10_filmes_antigos <- data.frame(ano = as.numeric(names(top_10_filmes_antigos)), count = as.numeric(top_10_filmes_antigos))

# Crie o gráfico de barras dos top 10 filmes mais antigos
grafico <- ggplot(df_top_10_filmes_antigos, aes(x = reorder(factor(ano), -count), y = count)) +
  geom_bar(stat = "identity", fill = "#2c7fb8", width = 0.5) +
  labs(x = "Ano", y = "Contagem", title = "Top 10 Filmes Mais Antigos") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_blank())

# Exiba o gráfico
print(grafico)

# Filtrar os dados apenas para o ano de 1995
dados_1995 <- subset(dados, ano == 1995)

# Calcular a contagem de ocorrências de cada gênero em 1995
contagem_generos <- table(dados_1995$genres)

# Selecionar os top 10 gêneros mais repetidos
top_10_generos <- head(sort(contagem_generos, decreasing = TRUE), 10)

# Criar um dataframe com os top 10 gêneros e suas contagens
df_top_10_generos <- data.frame(genre = names(top_10_generos), count = as.numeric(top_10_generos))

# Ordenar o dataframe por contagem decrescente
df_top_10_generos <- df_top_10_generos[order(df_top_10_generos$count, decreasing = TRUE), ]

# Criar o gráfico de barras dos top 10 gêneros em 1995
grafico <- ggplot(df_top_10_generos, aes(x = reorder(genre, -count), y = count)) +
  geom_bar(stat = "identity", fill = "#2c7fb8", width = 0.5) +
  labs(x = "Gênero", y = "Contagem", title = "Top 10 Gêneros Mais Repetidos em 1995") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_blank())

# Exiba o gráfico
print(grafico)
