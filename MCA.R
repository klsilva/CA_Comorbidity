#Instala e carrega pacotes

pacotes <- c("summarytools", "extracat", "FactoMineR", "factoextra")

lapply(pacotes, install.packages)

lapply(pacotes, library, character.only = TRUE)

#Carrega o recorte da PNS 2013

load("DADOS.Rda")

#Análise Descritiva

#Tabela de Contigencia

tabela <- xtabs(~ V1 + V2 + V3, data = DADOS)

ftable(tabela)

summary(tabela)

#Analise descritiva

descritiva <- dfSummary(DADOS)

#Teste Qui-Quadrado

attach(DADOS)

chisq.test(V1, V2)

chisq.test(V1, V3)

chisq.test(V2, V3)

detach(DADOS)

#Matriz de Burt

matrix_burt <- Burt(DADOS)

#MCA

MCA_BURT <- MCA(DADOS, method = "Burt")

summary(MCA_BURT)

eig.val <- get_eigenvalue(MCA_BURT)

fviz_screeplot(
  MCA_BURT,
  addlabels = TRUE,
  ylim = c(0, 100),
  geom = "line",
  ylab = "Porcentagem de variância explicada",
  xlab = "Dimensões",
  main = ""
)


var <- get_mca_var(MCA_BURT)

fviz_mca_var(MCA_BURT,
             choice = "mca.cor",
             repel = TRUE,
             ggtheme = theme_minimal())


fviz_mca_var(
  MCA_BURT,
  col.var = "cos2",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE,
  ggtheme = theme_minimal(),
  title = ""
)

fviz_cos2(MCA_BURT, choice = "var", axes = 1:2)

# Contribuição das linhas a primeira dimensão
fviz_contrib(MCA_BURT,
             choice = "var",
             axes = 1,
             top = 15)

# Contribuição das linhas a segunda dimensão
fviz_contrib(MCA_BURT,
             choice = "var",
             axes = 2,
             top = 15)

MCA.desc <- dimdesc(MCA_BURT, axes = c(1, 2))

# Descrição da primeira dimensão

MCA.desc[[1]]
# Descrição da segunda dimensão
MCA.desc[[2]]
