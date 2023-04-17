# ------------------------------------------------------------------------------
# Acadepol - Academia de Polícia Civil de Minas Gerais
# Gestão em Segurança Pública e Inteligência Aplicada
# Estatatística Aplicada à Segurança Pública
# 
# Prof: Adelino Pinheiro Silva
# adelinocpp@gmail.com, adelino.pinheiro@policiacivil.mg.gov.br
# https://github.com/adelinocpp/estatistica_aplicada_seguranca_publica
# Aula  3
# ------------------------------------------------------------------------------
# --- Comandos para limpar o terminal e as variáveis da memória
cat("\014")
rm(list = setdiff(ls(), lsf.str()))
# dev.off()

# --- Chamada das bibliotecas
# library(moments)
library(ggplot2)

# --- Carrega atabela no arquivo: "DB_CV_Jan_2018_Fev_2023.csv"
#       opções: ler cabecalho, separados por ponto e vírgula,
#               separador decimal vírgula, texto separado por aspas duplas 
df_SegPubMG <- read.table("./DB_CV_Jan_2018_Fev_2023.csv", header = TRUE, 
                         sep = ";",dec = ",",quote = "\"")

# --- Visualizacao básica da tabela:
#     Inicio da tabela, final da tabela e resumo
# --- Observacao: a tabela está em modo tinydata com uma entrada por linha
head(df_SegPubMG)
tail(df_SegPubMG)
summary(df_SegPubMG)

# --- Separando um sub grupo da tabela
# --- Filtrando partes da tabela
#     Exemplo 1: Município de BELO HORIZONTE
df_segPubBH <- df_SegPubMG[df_SegPubMG$Município == "BELO HORIZONTE",]
summary(df_SegPubMG)
#     Exemplo 2: Município de BELO HORIZONTE, ano 2020
df_SegPubBH2020 <- df_SegPubMG[(df_SegPubMG$Município == "BELO HORIZONTE") 
                            & (df_SegPubMG$Ano==2020),]
summary(df_SegPubBH2020)
#     Exemplo 3: Município de BELO HORIZONTE, ano 2020, natureza Extorsão
df_SegPubBH2020Ext <- df_SegPubMG[(df_SegPubMG$Município == "BELO HORIZONTE") 
                               & (df_SegPubMG$Ano==2020) 
                               & (df_SegPubMG$Natureza == "Extorsão Consumado"),]
#     Exemplo 4: Município de BELO HORIZONTE, natureza Extorsão
df_SegPubBHExt <- df_SegPubMG[(df_SegPubMG$Município == "BELO HORIZONTE") 
                                  & (df_SegPubMG$Natureza == "Extorsão Consumado"),]
# --- Visualizar dados
#     Forma de barras
barplot(df_SegPubBH2020Ext$Registros,names.arg=df_SegPubBH2020Ext$Mês,)
#     Sequencia de pontos
plot(x=df_SegPubBH2020Ext$Mês,y=df_SegPubBH2020Ext$Registros,type='b',pch=16,xlab="Mês",ylab="")
#     Graficos mais elaborados
#     Observação: o ggplot acumula o gráfico em uma variável, no caso chama-se "p"
p <- ggplot(df_SegPubBH2020Ext, aes(x=Mês, y=Registros))
p <- p + geom_bar(alpha=0.75, stat = "identity") 
p <- p + geom_line(data=df_SegPubBH2020Ext, aes(x=as.numeric(Mês), y=Registros), colour = "red")
#     salvar em arquivo
#     Observação: como já existe o gráfico na variável "p", basta chamá-la
png(file = "./Barras_linha.png",width = 864, height = 486, units = "px", bg = "transparent")
p
dev.off()
#     Histograma
hbreak = seq(min(df_SegPubBHExt$Registros)-1, max(df_SegPubBHExt$Registros)+1, by=2)
hist(df_SegPubBHExt$Registros, breaks=hbreak,
              main = "Histograma dos dados", 
              xlab = "N° de ocorrências", ylab="Frequência")  
#     No GG plot
h1 <- ggplot(df_SegPubBHExt, aes(Registros)) + 
      geom_histogram(color = "white", fill = "lightblue", bins=17) + 
      xlab("Dados") + ylab("Frequencia")
h1

# --- Extracao de estatisticas
median(df_SegPubBH2020Ext$Registros)
mean(df_SegPubBH2020Ext$Registros)
median(df_SegPubBH2020Ext$Registros)
sd(df_SegPubBH2020Ext$Registros)
quantile(df_SegPubBH2020Ext$Registros,  probs = c(0.5))
# mode(df_SegPubBH2020Ext$Registros)

