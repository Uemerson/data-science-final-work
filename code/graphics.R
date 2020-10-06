library("readxl")

data = read_excel("../data/survey-responses.xlsx", sheet = "responses")

pesquisados = nrow(data) # Sem o cabeçalho

resposta_questao1 = c(sum(data$questao1 == "Sim"), sum(data$questao1 == "Não"))
resposta_questao2 = c(sum(data$questao2 == "Sim"), sum(data$questao2 == "Não"))
resposta_questao3 = c(sum(data$questao3 == "Sim"), sum(data$questao3 == "Não"))

trim = function (x) gsub("^\\s+|\\s+$", "", x)

count_word_split_by = function (data, word, split_by){
  count = 0
  words = strsplit(data, split_by)
  
  for (values in words){
    for(value in values){
      if (trim(value) == word){
        count = count + 1
      }
    }
  }
  
  return (count)
}

resposta_questao4.1 = 
  c(
    count_word_split_by(data$questao4.1, "Homem cisgênero", ","), 
    count_word_split_by(data$questao4.1, "Mulher cisgênero", ","), 
    count_word_split_by(data$questao4.1, "Homem transexual", ","), 
    count_word_split_by(data$questao4.1, "Mulher transexual", ","))

resposta_questao4.2 = 
  c(
    sum(data$questao4.2 == "18 – 30 anos"), 
    sum(data$questao4.2 == "31 – 40 anos"),
    sum(data$questao4.2 == "41 – 50 anos"),
    sum(data$questao4.2 == "51 – 60 anos"),
    sum(data$questao4.2 == "Mais de 61 anos"))

resposta_questao4.3 = 
  c(
    sum(data$questao4.3 == "1 vez por semana"), 
    sum(data$questao4.3 == "2 vez por semana"),
    sum(data$questao4.3 == "3 vez por semana"),
    sum(data$questao4.3 == "4 vez por semana"),
    sum(data$questao4.3 == "Mais de 5 vezes por semana"))

resposta_questao5 = 
  c(
    sum(data$questao5 =="Zona Norte da cidade"), 
    sum(data$questao5 =="Zona Oeste da cidade"), 
    sum(data$questao5 =="Zona Sul da cidade"), 
    sum(data$questao5 =="Zona Leste da cidade"))

resposta_questao6 = 
  c(
    sum(data$questao6 == "Até 100 alunos"), 
    sum(data$questao6 == "De 101 a 200 alunos"), 
    sum(data$questao6 == "De 201 a 300 alunos"), 
    sum(data$questao6 == "De 301 a 400 alunos"), 
    sum(data$questao6 == "Mais de 400 alunos"))

resposta_questao7 = c(sum(data$questao7 == "Sim"), sum(data$questao7 == "Não"))
resposta_questao8 = c(sum(data$questao8 == "Sim"), sum(data$questao8 == "Não"))
resposta_questao9 = c(sum(data$questao9 == "Sim"), sum(data$questao9 == "Não"))
resposta_questao10 = c(sum(data$questao10 == "Sim"), sum(data$questao10 == "Não"))
resposta_questao11 = c(sum(data$questao11 == "Sim"), sum(data$questao11 == "Não"))
resposta_questao12 = c(sum(data$questao12 == "Sim"), sum(data$questao12 == "Não"))
resposta_questao13 = c(sum(data$questao13 == "Sim"), sum(data$questao13 == "Não"))

resposta_questao14 = 
  c(
    sum(data$questao14 == "De 5% a 15% do número total de alunos"), 
    sum(data$questao14 == "De 16% a 25% do número total de alunos"), 
    sum(data$questao14 == "26% do 35% do número total de alunos"), 
    sum(data$questao14 == "De 36% a 45% do número total de alunos"), 
    sum(data$questao14 == "Mais de 50% do número total de alunos"))

resposta_questao15 = c(sum(data$questao15 == "Sim"), sum(data$questao15 == "Não"))

resposta_questao16 =
  c(
    sum(data$questao16 == "Encerramento oficial das atividades"), 
    sum(data$questao16 == "Explorar outras alternativas de serviços (reinvenção do negócio)"),
    sum(data$questao16 == "Obter financiamento governamental"), 
    sum(data$questao16 == "Obter um “investidor anjo”"))

# Gráfico - Questão 1
jpeg(filename="../graphics/questão 1.jpg",width=480,height=480)
piepercent = round(resposta_questao1/sum(resposta_questao1)*100, digits=1)

lbls = c("Sim", "Não")
lbls = paste(lbls, piepercent, "%", sep=" ") # adicionar % (porcentagem) aos rótulos

pie(resposta_questao1, col=gray.colors(length(resposta_questao1)),
    labels=lbls)
dev.off()

# Gráfico - Questão 2
jpeg(filename="../graphics/questão 2.jpg",width=480,height=480)
piepercent = round(resposta_questao2/sum(resposta_questao2)*100, digits=1)

lbls = c("Sim", "Não")
lbls = paste(lbls, piepercent, "%", sep=" ") # adicionar % (porcentagem) aos rótulos

pie(resposta_questao2, col=gray.colors(length(resposta_questao2)),
    labels=lbls)
dev.off()

# Gráfico - Questão 3
jpeg(filename="../graphics/questão 3.jpg",width=480,height=480)
piepercent = round(resposta_questao3/sum(resposta_questao3)*100, digits=1)

lbls = c("Sim", "Não")
lbls = paste(lbls, piepercent, "%", sep=" ") # adicionar % (porcentagem) aos rótulos

pie(resposta_questao3, col=gray.colors(length(resposta_questao3)),
    labels=lbls)
dev.off()

# Gráfico - Questão 4.1
jpeg(filename="../graphics/questão 4-1.jpg",width=480,height=480)
lbls = c("Homem cisgênero", "Mulher cisgênero", "Homem transexual", "Mulher transexual")
lbls = paste(lbls, c(88.9, 88.9, 0, 11.1), "%", sep=" ") # adicionar % (porcentagem) aos rótulos

par(mar=c(3, 12, 3, 1))
barplot(
  resposta_questao4.1, 
  horiz = TRUE,
  col=gray.colors(length(resposta_questao4.1)),
  names.arg = lbls,
  las = 1)
dev.off()

# Gráfico - Questão 4.2
jpeg(filename="../graphics/questão 4-2.jpg",width=580,height=480)
piepercent = round(resposta_questao4.2/sum(resposta_questao4.2)*100, digits=1)

lbls = c("18 a 30 anos", "31 a 40 anos", "41 a 50 anos", "51 a 60 anos", "Mais de 61 anos")
lbls = paste(lbls, piepercent, "%", sep=" ") # adicionar % (porcentagem) aos rótulos

pie(resposta_questao4.2, col=gray.colors(length(resposta_questao4.2)),
    labels=lbls)
dev.off()

# Gráfico - Questão 4.3
jpeg(filename="../graphics/questão 4-3.jpg",width=580,height=480)
piepercent = round(resposta_questao4.3/sum(resposta_questao4.3)*100, digits=1)

lbls = c("1 vez por semana", "2 vez por semana", "3 vez por semana", "4 vez por semana", "Mais de 5 vezes\npor semana")
lbls = paste(lbls, piepercent, "%", sep=" ") # adicionar % (porcentagem) aos rótulos

pie(resposta_questao4.3, col=gray.colors(length(resposta_questao4.3)),
    labels=lbls)
dev.off()

# Gráfico - Questão 5
jpeg(filename="../graphics/questão 5.jpg",width=620,height=480)
piepercent = round(resposta_questao5/sum(resposta_questao5)*100, digits=1)

lbls = c("Zona Norte da cidade", "Zona Oeste da cidade", "Zona Sul da cidade", "Zona Leste da cidade")
lbls = paste(lbls, piepercent, "%", sep=" ") # adicionar % (porcentagem) aos rótulos

pie(resposta_questao5, col=gray.colors(length(resposta_questao5)),
    labels=lbls)
dev.off()

# Gráfico - Questão 6
jpeg(filename="../graphics/questão 6.jpg",width=580,height=480)
piepercent = round(resposta_questao6/sum(resposta_questao6)*100, digits=1)

lbls = c("Até 100 alunos", "De 101 a 200 alunos", "De 201 a 300 alunos", "De 301 a 400 alunos", "Mais de 400 alunos")
lbls = paste(lbls, piepercent, "%", sep=" ") # adicionar % (porcentagem) aos rótulos

pie(resposta_questao6, col=gray.colors(length(resposta_questao6)),
    labels=lbls)
dev.off()

# Gráfico - Questão 7
jpeg(filename="../graphics/questão 7.jpg",width=480,height=480)
piepercent = round(resposta_questao7/sum(resposta_questao7)*100, digits=1)

lbls = c("Sim", "Não")
lbls = paste(lbls, piepercent, "%", sep=" ") # adicionar % (porcentagem) aos rótulos

pie(resposta_questao7, col=gray.colors(length(resposta_questao7)),
    labels=lbls)
dev.off()

# Gráfico - Questão 8
jpeg(filename="../graphics/questão 8.jpg",width=480,height=480)
piepercent = round(resposta_questao8/sum(resposta_questao8)*100, digits=1)

lbls = c("Sim", "Não")
lbls = paste(lbls, piepercent, "%", sep=" ") # adicionar % (porcentagem) aos rótulos

pie(resposta_questao8, col=gray.colors(length(resposta_questao8)),
    labels=lbls)
dev.off()

# Gráfico - Questão 9
jpeg(filename="../graphics/questão 9.jpg",width=480,height=480)
piepercent = round(resposta_questao9/sum(resposta_questao9)*100, digits=1)

lbls = c("Sim", "Não")
lbls = paste(lbls, piepercent, "%", sep=" ") # adicionar % (porcentagem) aos rótulos

pie(resposta_questao9, col=gray.colors(length(resposta_questao9)),
    labels=lbls)
dev.off()

# Gráfico - Questão 10
jpeg(filename="../graphics/questão 10.jpg",width=480,height=480)
piepercent = round(resposta_questao10/sum(resposta_questao10)*100, digits=1)

lbls = c("Sim", "Não")
lbls = paste(lbls, piepercent, "%", sep=" ") # adicionar % (porcentagem) aos rótulos

pie(resposta_questao10, col=gray.colors(length(resposta_questao10)),
    labels=lbls)
dev.off()

# Gráfico - Questão 11
jpeg(filename="../graphics/questão 11.jpg",width=480,height=480)
piepercent = round(resposta_questao11/sum(resposta_questao11)*100, digits=1)

lbls = c("Sim", "Não")
lbls = paste(lbls, piepercent, "%", sep=" ") # adicionar % (porcentagem) aos rótulos

pie(resposta_questao11, col=gray.colors(length(resposta_questao11)),
    labels=lbls)
dev.off()

# Gráfico - Questão 12
jpeg(filename="../graphics/questão 12.jpg",width=480,height=480)
piepercent = round(resposta_questao12/sum(resposta_questao12)*100, digits=1)

lbls = c("Sim", "Não")
lbls = paste(lbls, piepercent, "%", sep=" ") # adicionar % (porcentagem) aos rótulos

pie(resposta_questao12, col=gray.colors(length(resposta_questao12)),
    labels=lbls)
dev.off()

# Gráfico - Questão 13
jpeg(filename="../graphics/questão 13.jpg",width=480,height=480)
piepercent = round(resposta_questao13/sum(resposta_questao13)*100, digits=1)

lbls = c("Sim", "Não")
lbls = paste(lbls, piepercent, "%", sep=" ") # adicionar % (porcentagem) aos rótulos

pie(resposta_questao13, col=gray.colors(length(resposta_questao13)),
    labels=lbls)
dev.off()

# Gráfico - Questão 14
jpeg(filename="../graphics/questão 14.jpg",width=620,height=480)
piepercent = round(resposta_questao14/sum(resposta_questao14)*100, digits=1)

lbls = c("De 5% a 15% do número\ntotal de alunos", "De 16% a 25% do número\ntotal de alunos", "26% do 35% do número\ntotal de alunos", "De 36% a 45% do número\ntotal de alunos", "Mais de 50% do número\ntotal de alunos")
lbls = paste(lbls, piepercent, "%", sep=" ") # adicionar % (porcentagem) aos rótulos

pie(resposta_questao14, col=gray.colors(length(resposta_questao14)),
    labels=lbls)
dev.off()

# Gráfico - Questão 15
jpeg(filename="../graphics/questão 15.jpg",width=480,height=480)
piepercent = round(resposta_questao15/sum(resposta_questao15)*100, digits=1)

lbls = c("Sim", "Não")
lbls = paste(lbls, piepercent, "%", sep=" ") # adicionar % (porcentagem) aos rótulos

pie(resposta_questao15, col=gray.colors(length(resposta_questao15)),
    labels=lbls)
dev.off()

# Gráfico - Questão 16
jpeg(filename="../graphics/questão 16.jpg",width=720,height=480)
piepercent = round(resposta_questao16/sum(resposta_questao16)*100, digits=1)

lbls = c("Encerramento oficial\ndas atividades", "Explorar outras alternativas de\nserviços (reinvenção do negócio)", "Obter financiamento\ngovernamental", "Obter um \"investidor\nanjo\"")
lbls = paste(lbls, piepercent, "%", sep=" ") # adicionar % (porcentagem) aos rótulos

pie(resposta_questao16, col=gray.colors(length(resposta_questao16)),
    labels=lbls)
dev.off()
