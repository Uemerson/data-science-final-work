# Projeto : UMSES - Uso das Mídias Sociais no Ensino Superior
# AED: Análise Exploratória de Dados
# Versão: 2.02 - junho de 2020
#
#Observação: 
# Salvando os gráficos em arquivos e não mostrando na aba "Plots" do RStudio!
# 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Lembrete 1: Para "rodar" este pacote no RStudio, deve-se intalar o pacote de Latex
## correspondente ao seu sistema operacional: 
# - Windows: MiKTeX (Complete) - http://miktex.org/2.9/setup (utilizo esta instalação neste exemplo!)
# - Mac OS X: TexLive 2013 (Full) - http://tug.org/mactex/ e
# - Linux: Utilizar o "system package manager"
# - Alternativa: Ou utilizar a versão "mais leve", 
#                TinyTex: https://yihui.org/tinytex/
#------------------------------------------------------------------------------------------
## Lembrete 2: Deve-se instalar também, os pacotes knitr e rmarkdown
if (!require(knitr)) install.packages('knitr')
if (!require(rmarkdown)) install.packages('rmarkdown')
#------------------------------------------------------------------------------------------
## Default repositório 
local({r <- getOption("repos")
r["CRAN"] <- "https://brieger.esalq.usp.br/CRAN/" # ESALQ/USP
options(repos=r)
})
#------------------------------------------------------------------------------------------
# Limpando a área de trabalho
rm(list = ls())
# Os gráficos aparecerão serão salvos no subdiretório "graphics"
# por meio do comando jpeg(filename = "./graphics/figurax.jpg")
if (!require(grDevices)) install.packages("grDevices")
#------------------------------------------------------------------------------------------
# Carregando os pacotes necessários
if (!require(tidyverse)) install.packages("tidyverse")
#"tidyverse" é um conjunto de pacotes selecionados desenvolvidos na linguagem R idealizados para Ciência de Dados. 
#Os pacotes que compõem o tidyverse compartilham uma filosofia básica de design, gramática e estruturas de dados.
#https://www.tidyverse.org/
library("readxl")
#O pacote readxl facilita a extração de dados do Excel.
#Não possui dependências externas sendo fácil de instalar e usar em todos os sistemas operacionais.
#------------------------------------------------------------------------------------------
# "Rodando" fora do rmarkdown, é preciso "setar" o diretório raiz para que os
# direcionamentos de diretórios funcionem...
## 25/03/2020 - Issue:
# Quando é UNIX like/MacOsx deve-se modificar o local do diretório raiz!!
# Então, deve-se verificar o tipo de SO
if(.Platform$OS.type == "windows") {
  setwd("D:/Transferência/Pesquisa Reprodutível")
} else {
  setwd("~/Library/Mobile Documents/com~apple~CloudDocs/GitLab")
}
#------------------------------------------------------------------------------------------
# Lendo a planilha para carregar no R.
# A opção "sheetName = "dados"" é o nome da área de trabalho que será lida
# de dentro da planilha de nome umses_data.xlsx.
#
df <- read_excel("./data/survey_testev2.xlsx", sheet = "dados")
#------------------------------------------------------------------------------------------
## Função criada para verificar se existe NA ou NaN nos campos
# Verifica NA - Versão junho de 2020
pct.res <- function (data.frame, i.position, n.column, out.labels) {
  pct = n.labels = c()
  n = 1
  for (i in  1:n.column) {
    if(sum(data.frame[i+i.position]) > 0) {
      pct[n] <- round(sum(data.frame[i+i.position])/nrow(data.frame)*100,1)
      n.labels[n] <- out.labels[i]
      n = n + 1} 
 }
    names(pct) <- n.labels
    return(pct)
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Número de respondentes
pesquisados <- nrow(df) # Sem o cabeçalho
pesquisados
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Seção 1 do Questiário/Survey
#------------------------------------------------------------------------------------------
# Dados censitarios.
#------------------------------------------------------------------------------------------
# Sexo dos respondentes
sexo=table(df$genero)
pct.s <- round(sexo/sum(sexo)*100, digits=1)
pct.s

#lbls <- c("Outro", "Masculino", "Feminino")
lbls <- c("Masculino", "Feminino")
lbls <- paste(lbls, pct.s, "%", sep=" ") # adicionar porcentagens aos rótulos 
jpeg(filename = "./graphics/figura-genero.jpg")
pie(sexo, labels = lbls, edges = 100, angle = 45, col = c("gray","blue","purple"))
dev.off()
# Idade dos respondentes
pct.i <- round(table(df$idade)/sum(table(df$idade))*100, digits=1)
pct.i

#lbls <- c("16/20","21/25 anos", "26/30 anos","30/35 anos", "36/40 anos","+40 anos")
lbls <- c("21/25 anos", "26/30 anos","30/35 anos", "36/40 anos","+40 anos")
jpeg(filename = "./graphics/figura-faixa_etaria.jpg")
barplot(table(df$idade), names.arg = lbls, col = c("gray", "red","gray", "green3", "gray", "red"),
        xlab = "Idade dos respondentes", ylim = c(0,max(table(df$idade) + 5)), ylab = "Num. de Respondentes")
dev.off()

# Profissão
pct.p <- round(table(df$profal)/sum(table(df$profal))*100, digits=1)
pct.p

#lbls <- c("Professor","Aluno","Professor e aluno","Outros")
lbls <- c("Professor","Aluno","Professor e aluno")
lbls <- paste(lbls, pct.p, "%", sep=" ") # adicionar porcentagens aos rótulos 
jpeg(filename = "./graphics/figura-profissao_pizza.jpg")
pie(table(df$profal), labels = lbls, edges = 200, radius = 0.8,
    clockwise = TRUE, angle = 60, col = c("purple", "green3","cyan","black"))
dev.off()

jpeg(filename = "./graphics/figura-profissao_barra.jpg")
barplot(table(df$profal), names.arg = lbls, 
        col = c("gray", "red","gray", "green3", "gray", "red"),
        xlab = "Profissão", 
        ylim = c(0,max(table(df$profal) + 3)), 
        ylab = "Num. de Respondentes")
#density = 60, 
dev.off()

#Situação Trabalhista
situacao = table(df$trabalha)
pct.t <- round(situacao/sum(situacao)*100, digits=1)
pct.t

#lbls <- c("Desempregado", "Parcial", "Integral","Estagiário",
#          "Conta própria", "Afastado", "Aposentado", "Bolsista")
lbls <- c("Desempregado", "Parcial", "Integral",
          "Conta própria", "Aposentado")
          
lbls <- paste(lbls, pct.t, "%", sep=" ") # adicionar porcentagens aos rótulos
jpeg(filename = "./graphics/figura-situacao_trabalho_pizza.jpg")
pie(situacao, labels = lbls, edges = 230, radius = 1.09,
    clockwise = F,  angle = 80, col = c("gray", "green3","blue", " yellow",'red'))
#density = 100,
dev.off()

jpeg(filename = "./graphics/figura-situacao_trabalho_barra.jpg")
barplot(situacao, names.arg = lbls, 
        col = c("gray", "red","gray", "green3", "gray", "red"),
        xlab = "Situação Trabalhista", 
        ylim = c(0,max(situacao) + 3), 
        ylab = "Num. de Respondentes")
dev.off()

# Estado Civil
marital = table(df$estadocivil)
pct.m <- round(marital/sum(marital)*100, digits=1)
pct.m

#lbls <- c("Solteiro", "Casado", "União Estável","Viúvo" ,"Separado","Outros")
lbls <- c("Solteiro", "Casado", "União Estável","Separado")
lbls <- paste(lbls, pct.m, "%",sep=" ") # add percents to labels 
jpeg(filename = "./graphics/figura-status_marital.jpg")
pie(marital, labels = lbls, edges = 200, radius = 0.9,
    clockwise = F, angle = 30, col = c("green3","purple","red", "black", "blue", "black"))
#density = 80,
dev.off()

# Número de filhos
filhos = table(df$filhos)
pct.f <- round(filhos/sum(filhos)*100, digits=1)
pct.f

lbls <- c("Sem filhos", "Um", "Dois", "Três", "+de Três")
lbls <- paste(lbls, pct.f,"%", sep=" ") # adicionar porcentagens aos rótulos
jpeg(filename = "./graphics/figura-numero-filhos.jpg")
pie(filhos, labels = lbls, edges = 200, radius = 0.9,
    clockwise = F, angle = 30, 
    col = c("red", "green3","cyan", "black", "blue", "red"))
dev.off()

# Idade dos filhos
idadefilhos = table(df$idadefilho)
pct.if <- round(idadefilhos/sum(idadefilhos)*100, digits=1)
pct.if

#lbls <- c("Sem filhos", "de 0 a 6 anos", "de 7 a 15 anos", "de 16 a 20 anos", "+de 20anos ", 
#          "de 0 a 6 e de 7 a 15 a anos", "de 0 a 6 e +20 anos",
#          "de 7 a 15 e de 16 a 20 anos","de 7 a 15 e +20 anos","de 16 a 20e +20 anos", 
#          "de 0 a 6 e de 16 a 20 e +20 anos")
lbls <- c("Sem filhos", "de 0 a 6 anos", "de 7 a 15 anos", "de 16 a 20 anos", "+de 20anos ", 
          "de 0 a 6 e de 7 a 15 anos", 
          "de 7 a 15 e +20 anos",
          "de 16 a 20e +20 anos")
jpeg(filename = "./graphics/figura-idade-filhos_barra.jpg")
barplot(idadefilhos,legend = lbls,col = rainbow(8), axisnames = FALSE,
       ylim = c(0, 20), ylab = "Num. de Respondentes", xlab = "Idade dos filhos")
dev.off()

lbls <- paste(lbls, pct.if,"%",sep=" ") # adicionar porcentagens aos rótulos 
jpeg(filename = "./graphics/figura-idade-filhos_pizza.jpg")
pie(idadefilhos, labels = lbls, edges = 200, radius = 0.7,
    clockwise = F, density = 80, angle = 30, 
    col = c("purple", "green3", "black", "blue", "red", "yellow",
            "mistyrose","lightcyan", "lavender"))
dev.off()

lbls <- c("Sem filhos", "0 a 6", "7 a 15", "16 a 20", "+de 20anos", 
          "0/6 e 7/15", 
          "7/15 e +20","16/20 e +20")
jpeg(filename = "./graphics/figura-idade-filhos_barra2.jpg")
barplot(idadefilhos, 
        names.arg = lbls, 
        legend =  paste(pct.if,"%",sep=" "),
        col = c("gray", "red","gray", "green3", "gray", "red"),
        xlab = "Idade dos filhos", 
        ylim = c(0,max(idadefilhos) + 3), 
        ylab = "Num. de Respondentes")
dev.off()

#ylim = c(0,max(idadefilhos) + 5))
#names.arg = lbls, col = c("gray", "red","gray", "green3", "gray", "red")
#------------------------------------------------------------------------------------------
# Seção 2 do Questiário/Survey
#------------------------------------------------------------------------------------------
# Plataformas de Redes Sociais.
#------------------------------------------------------------------------------------------
# Nomes das plataformas no survey
lbls = c("Facebook","Twitter","Whatsapp","Linkedin",
         "Youtube","Instagram", "Pinterest","Tumblr",
         "Snapchat")

pct.pla <- pct.res(df, 7, 9, lbls)
pct.pla
jpeg(filename = "./graphics/figura-plataformas_pizza.jpg")
pie(pct.pla, 
    labels = paste(names(pct.pla),pct.pla,"%"), 
    edges = 300, radius = 1,
    clockwise = F, angle = 30, 
    col = c("purple", "green3", "black", "blue", "red", "yellow",
            "mistyrose","lightcyan", "lavender"))
dev.off()
#density = 80,
jpeg(filename = "./graphics/figura-plataformas_barra.jpg")
barplot(unname(pct.pla), 
        legend.text = names(pct.pla),
        names.arg = paste(pct.pla,"%"),
        col = c("gray", "red","gray", "green3", "gray", "red"),
        xlab = "Plataformas mais utilizadas",
        ylab = "Num. de Respondentes",
        ylim = c(0,max(unname(pct.pla)) + 3))
dev.off()

linha.pla <-c()
k <- 0
for (i in 1:nrow(df)) {
  if ((!(is.na(df$outras_plataformas[i]))) & (nchar(as.character(df$outras_plataformas[i])) < 20)) {
    if (str_detect(df$outras_plataformas[i],",")) {
      pos <- regexpr(",", df$outras_plataformas[i])
      prima <- substr(df$outras_plataformas[i], 1, pos - 1)
      secondo <- trimws(substr(df$outras_plataformas[i], pos + 1, nchar(as.character(df$outras_plataformas[i]))))
      k=k+1
      linha.pla[k] <- prima
      k=k+1
      linha.pla[k] <- secondo
    } else {
    k=k+1
    linha.pla[k] <- df$outras_plataformas[i]
    }
  }
}

pct.ot.pla <- round(table(linha.pla)/sum(table(linha.pla))*100,1)
pct.ot.pla
jpeg(filename = "./graphics/figura-outras-plataformas_pizza.jpg")
pie(pct.ot.pla, 
    labels = paste(names(table(linha.pla)),pct.ot.pla,"%"), 
    edges = 300, radius = 1,
    clockwise = F, angle = 90, 
    col = rainbow(11))
dev.off()

#------------------------------------------------------------------------------------------
# Seção 3 do Questiário/Survey
#------------------------------------------------------------------------------------------
#Principais motivos do uso das redes sociais.
#------------------------------------------------------------------------------------------
#
lbls = c("Saber o que amigos fazem",
         "Manter-se atualizado",
         "Preencher tempo",
         "Encontrar conteúdo",
         "Compartilhar opinião",
         "Compartilhar fotos",
         "Os amigos já usam",
         "Networking profissional",
         "Conhecer novas pessoas",
         "Assuntos de trabalho")
pct.motivo <- pct.res(df, 17, 10, lbls)
pct.motivo
jpeg(filename = "./graphics/figura-motivo-uso.jpg")
pie(pct.motivo, 
    labels = paste(names(pct.motivo),pct.motivo,"%"),
    edges = 200, radius = 1,
    clockwise = F, angle = 30, 
    col = c("purple", "green3", "black", "blue", "red", "yellow",
            "mistyrose","lightcyan", "lavender"))
#density = 80,
dev.off()

# Se se listar diretamente o conteúdo da váriável "df$outros_motivos" no gráfico, o mesmo fica confuso.
# A variável do tipo caracter "linha" é utilizada para melhorar a apresentação 
# do gráfico, separando os dados e formatando-os.

linha.ot.r <-c()
k <- 0
for (i in 1:nrow(df)) {
  if ((!(is.na(df$outros_motivos[i])))) {
      k=k+1
      linha.ot.r[k] <- df$outros_motivos[i]
    }
}
substr(linha.ot.r, 1, 80)

#------------------------------------------------------------------------------------------
# Seção 4 do Questiário/Survey
#------------------------------------------------------------------------------------------
# Tipificação do Uuso das mídias sociais.
#------------------------------------------------------------------------------------------

# Tempo gasto em sites de redes sociais durante um dia normal 
#lbls <- c("Nenhum/Não uso", "5 a 10 min.","11 a 30 min.", "31 min. a 1 h.",  
#          "1 a 2 hs","2 a 3 hs","3 a 4 hs","4 a 5 h","+5 hs")
round(table(df$tempogasto))
pct.use.time <- round(table(df$tempogasto)/sum(table(df$tempogasto))*100, digits=1)
pct.use.time
lbls <- c("Nenhum/Não uso","11 a 30 min.", "31 min. a 1 h.",  
          "1 a 2 hs","2 a 3 hs","3 a 4 hs","4 a 5 h")

lbls <- paste(lbls, pct.use.time, "%",sep=" ") # add percents to labels 
jpeg(filename = "./graphics/figura-tempo-gasto_pizza.jpg")
pie(pct.use.time, labels = lbls, edges = 200, radius = 1,
    clockwise = F, angle = 30, 
    col = c("purple", "green3", "black", "blue", "red", "yellow",
            "mistyrose","lightcyan", "lavender"))
dev.off()

#outra opção: gráfico de barras
#gráficos de barras e de setores são adequados para representar esta variável
#par(mfrow = c(1, 2))

pct.use.time <- round(table(df$tempogasto)/sum(table(df$tempogasto))*100, digits=1)
pct.use.time
lbls <- c("Nenhum/Não uso","11 a 30 min.", "31 min. a 1 h.",  
          "1 a 2 hs","2 a 3 hs","3 a 4 hs","4 a 5 h")
names(pct.use.time) <- lbls
jpeg(filename = "./graphics/figura-tempo-gasto_barra.jpg")
barplot(unname(pct.use.time), 
        legend.text = names(pct.use.time),
        names.arg = paste(pct.use.time,"%"),
        col = c("gray", "red","gray", "green3", "gray", "red"),
        xlab = "Horas de uso",
        ylab = "Num. de Respondentes",
        ylim = c(0,max(unname(pct.use.time)) + 3))
dev.off()
jpeg(filename = "./graphics/figura-tempo-gasto_barra2.jpg")
barplot(prop.table(unname(pct.use.time)), main = "Tempo de uso das redes sociais",
        ylab = "Percentual de pessoas",
        ylim = c(.0, .25))
lbls.p <- paste0(pct.use.time, "%")
dev.off()

jpeg(filename = "./graphics/figura-tempo-gasto_barra3.jpg")
x <-barplot(prop.table(unname(pct.use.time)), 
            main = "Tempo de uso das redes sociais",
            ylab = "Percentual de pessoas",
            ylim = c(.0, .25))
y<-as.matrix(lbls.p)

#text(x,y+2,labels=y)
dev.off()

#par(mfrow = c(1,1))
#a moda
#names(use.time)[which.max(use.time)]
#jpeg(filename = "./graphics/figura-tempo-gasto_histograma.jpg")
#hist(pct.use.time)
#dev.off()

# Sobre o uso das redes sociais no ensino superior concorda/discorda/sem opinião
#lbls <- c("Não","Sim", "Sim com restrições","Não sei/Sem opinião")
pct.use.tool <- round(table(df$usoacademico)/sum(table(df$usoacademico))*100, digits=1)
pct.use.tool
lbls <- c("Sim", "Sim com restrições")
lbls <- paste(lbls, pct.use.tool, "%",sep=" ") # adicionar porcentagens aos rótulos 
jpeg(filename = "./graphics/figura-uso-academico.jpg")
pie(table(df$usoacademico), labels = lbls, edges = 200, radius = 0.9,
    clockwise = T, angle = 30, 
    col = c("purple", "green3", "black", "blue", "red", "yellow"),
    main = "Uso das Midias Sociais no ensino superior")
#density = 80, 
dev.off()

# Sobre se as midias sociais melhoram o contato entre professor-aluno
pct.use.close <- round(table(df$profchegaal)/sum(table(df$profchegaal))*100, digits=1)
pct.use.close
lbls <- c("Não","Sim","Não sei/Sem opinião")
lbls <- paste(lbls, pct.use.close, "%",sep=" ") # adicionar porcentagens aos rótulos 
jpeg(filename = "./graphics/figura-contato-professor-aluno.jpg")
pie(table(df$profchegaal), labels = lbls, edges = 200, radius = 0.7,
    clockwise = F, angle = 30, 
    col = c("purple", "green3", "black", "blue", "red", "yellow",
            "mistyrose","lightcyan", "lavender"),
    main = "Midias melhoram contato professor/aluno")
# density = 80,
dev.off()

# Sobre se midias sociais melhoram o resultado acadêmico dos alunos
pct.use.res <- round(table(df$melhoraresul)/sum(table(df$melhoraresul))*100, digits=1)
pct.use.res
lbls <- c("Não","Sim","Não sei/Sem opinião")
lbls <- paste(lbls, pct.use.res, "%",sep=" ") # add percents to labels 
jpeg(filename = "./graphics/figura-melhora-resultado.jpg")
pie(table(df$melhoraresul), labels = lbls, edges = 200, radius = 0.7,
    clockwise = F,  angle = 30, 
    col = c("purple", "green3", "black", "blue", "red", "yellow",
            "mistyrose","lightcyan", "lavender"),
    main = "Midias melhoram rendimento escolar")
#density = 80,
dev.off()

#------------------------------------------------------------------------------------------
# Seção 5 do Questiário/Survey
#------------------------------------------------------------------------------------------
#Principais dificuldades do uso das em ambiente educacional.
#------------------------------------------------------------------------------------------
# Principais dificuldades para o uso das plataformas: concorda/discorda/sem opinião

lbls = c("Mais distração em sala",
         "Cola, cópia, etc.",
         "Prejudica interação",
         "Favorece cyberbullying",
         "Conteúdo inadequado")
pct.dif <- pct.res(data.frame = df, i.position = 32, n.column = 5, out.labels = lbls)
pct.dif
jpeg(filename = "./graphics/figura-dificuldades-uso.jpg")
pie(unname(pct.dif), 
    labels = paste(names(pct.dif),pct.dif,"%"), 
    edges = 200, radius = 0.9,
    clockwise = F, angle = 30, 
    main = "Dificuldades no uso das plataformas digitais",
    col = c("purple", "green3", "black", "blue", "red"))
#density = 80, 
dev.off()

linha.dif <-c()
k <- 0
for (i in 1:nrow(df)) {
  if ((!(is.na(df$outras_dificuldades[i])))) {
    k=k+1
    linha.dif[k] <- df$outras_dificuldades[i]
  }
}
substr(linha.dif, 1, 80)

#------------------------------------------------------------------------------------------
# Seção 6 do Questiário/Survey
#------------------------------------------------------------------------------------------
#Avaliação de recursos.
#------------------------------------------------------------------------------------------
# Avaliação de recursos das plataformas: "1.Excelente, 2.Bom, 3.Indiferente, 4.Ruim, 5.Muito ruim"

#par(mfrow = c(2, 3))

pct.ei <- round(table(df$evioinfo)/sum(table(df$evioinfo))*100, digits=1)
lbls <- c("Excelente","Bom","Indiferente","Ruim","Muito ruim")
lbls <- paste(lbls, pct.ei, "%", sep=" ") # add percents to labels 
jpeg(filename = "./graphics/figura-avaliacao-envio.jpg")
pie(table(df$evioinfo), labels = lbls, edges = 200, radius = 0.9,
    clockwise = F, density = 80, angle = 30, 
    col = c("black","red","mistyrose","cyan", "yellow"),
    main = "Envio de informações escola/pais")
dev.off()

pct.pp <- round(table(df$grandeuso)/sum(table(df$grandeuso))*100, digits=1)
lbls <- c("Excelente","Bom","Indiferente","Ruim","Muito ruim")
lbls <- paste(lbls, pct.pp, "%", sep=" ") # add percents to labels 
jpeg(filename = "./graphics/figura-avaliacao-uso-mkt.jpg")
pie(table(df$grandeuso), labels = lbls, edges = 200, radius = 0.9,
    clockwise = F, angle = 30, 
    col = c("black","red","mistyrose","cyan", "yellow"),
    main = "Uso promocional das mídias sociais")
#, density = 80
dev.off()

lbls <- c("Excelente","Bom","Indiferente","Ruim","Muito ruim")
pct.gc <- round(table(df$facegrupo)/sum(table(df$facegrupo))*100, digits=1)
lbls <- paste(lbls, pct.gc, "%", sep=" ") # add percents to labels 
jpeg(filename = "./graphics/figura-avaliacao-grupo-face.jpg")
pie(table(df$facegrupo), labels = lbls, edges = 200, radius = 0.9,
    clockwise = F, angle = 30, 
    col = c("black","red","mistyrose","cyan", "yellow"),
    main = "Comunicação via Facebook com alunos")
#, density = 80
dev.off()

lbls <- c("Excelente","Bom","Indiferente","Ruim","Muito ruim")
pct.tet <- round(table(df$trocainfo)/sum(table(df$trocainfo))*100, digits=1)
lbls <- paste(lbls, pct.tet, "%", sep=" ") # add percents to labels 
jpeg(filename = "./graphics/figura-avaliacao-troca-info.jpg")
pie(table(df$trocainfo), labels = lbls, edges = 200, radius = 0.9,
    clockwise = F, angle = 30, 
    col = c("black","red","mistyrose","cyan", "yellow"),
    main = "Troca rápida de informações e vídeos")
#, density = 80
dev.off()

lbls <- c("Excelente","Bom","Indiferente","Ruim","Muito ruim")
pct.tpa <- round(table(df$compinfopal)/sum(table(df$compinfopal))*100, digits=1)
lbls <- paste(lbls, pct.tpa, "%", sep=" ") # add percents to labels 
jpeg(filename = "./graphics/figura-avaliacao-compa-info.jpg")
pie(table(df$compinfopal), labels = lbls, edges = 200, radius = 1,
    clockwise = F, angle = 60, 
#    clockwise = FALSE, init.angle = 155,
    col = c("black","red","mistyrose","cyan", "yellow"),
    main = "Compartilhamento de informação aluno-professor")
#, density = 80
dev.off()

lbls <- c("Excelente","Bom","Indiferente","Ruim","Muito ruim")
pct.qv <- round(table(df$quadrovirtual)/sum(table(df$quadrovirtual))*100, digits=1)
lbls <- paste(lbls, pct.qv, "%", sep=" ") # add percents to labels 
jpeg(filename = "./graphics/figura-avaliacao-quadro-virtual.jpg")
pie(table(df$quadrovirtual), labels = lbls, edges = 200, radius = 0.7,
    clockwise = F, angle = 30, 
    col = c("black","red","mistyrose","cyan", "yellow"),
    main = "Painel virtual para compartilhamento de conteúdos")
#, density = 80
dev.off()

#par(mfrow = c(1, 1))
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
