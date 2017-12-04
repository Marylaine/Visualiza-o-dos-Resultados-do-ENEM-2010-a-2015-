library(ggplot2)
library(knitr)
library(kableExtra)
library(gridExtra)

function(input,output,session){

  output$plot = renderPlot({

    ## gráficos de 2015
    # boxplots das notas 2015
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebm15 = readRDS("ebm15.rds")
            bm15 = ggplot(ebm15, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.5)) +
              geom_boxplot(stat="identity", fill = "aquamarine4") + geom_point() + coord_flip() +
              labs(x="Brasil",y="Média no ENEM 2015 (Brasil)",
                   title = "Boxplot das Médias no ENEM 2015") + theme_bw() +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(bm15)
           }}}}
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebr15 = readRDS("ebr15.rds")
            br15 = ggplot(ebr15, aes(x = "x", middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              theme_bw() + coord_flip() +
              labs(x="Brasil",y="Média da Redação no ENEM 2015",
                   title = "Boxplot das Médias da Redação no ENEM 2015") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(br15)
          }}}}
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebn15 = readRDS("ebn15.rds")
            bn15 = ggplot(ebn15, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw()+
              theme(legend.position="none",axis.text.y=element_text(colour="white")) +
              labs(x="Brasil",y="Nota em Ciências da Natureza no ENEM 2015",
                   title = "Boxplot das Notas em Ciências da Natureza no ENEM 2015")
            print(bn15)
          }}}}
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebh15 = readRDS("ebh15.rds")
            bh15 = ggplot(ebh15, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw() +
              labs(x="Brasil",y="Nota em Ciências Humanas no ENEM 2015",
                   title = "Boxplot das Notas em Ciências Humanas no ENEM 2015") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(bh15)
          }}}}
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebl15 = readRDS("ebl15.rds")
            bl15 = ggplot(ebl15, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw() +
              labs(x="Brasil",y="Nota em Linguagens e Códigos no ENEM 2015",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM 2015") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(bl15)
          }}}}
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebmt15 = readRDS("ebmt15.rds")
            bmt15 = ggplot(ebmt15, aes(x = "x", y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       aplha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw() +
              labs(x="Brasil",y="Nota em Matemática no ENEM 2015",
                   title = "Boxplot das Notas em Matemática no ENEM 2015") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
              print(bmt15)
          }}}}
    # boxplots por Sexo 2015:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebms15 = readRDS("ebms15.rds")
            bms15 = ggplot(ebms15, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Sexo",y="Média no ENEM 2015",
                   title = "Boxplot das Médias no ENEM 2015 Segundo o Sexo") +
              theme(legend.position="none")
            print(bms15)
          }}}}
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebrs15 = readRDS("ebrs15.rds")
            brs15 = ggplot(ebrs15, aes(x = sexo, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + theme_bw() + coord_flip() +
              labs(x="Sexo",y="Média da Redação no ENEM 2015",
                   title="Boxplot das Médias da Redação no ENEM 2015 Segundo o Sexo")+
              theme(legend.position="none")
            print(brs15)
          }}}}
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebns15 = readRDS("ebns15.rds")
            bns15 = ggplot(ebns15, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Sexo",y="Nota em Ciências da Natureza no ENEM 2015",
                   title="Boxplot das Notas em Ciências da Natureza no ENEM 2015 Segundo o Sexo")+
              theme(legend.position="none")
            print(bns15)
          }}}}
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebhs15 = readRDS("ebhs15.rds")
            bhs15 = ggplot(ebhs15, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Sexo",y="Nota em Ciências Humanas no ENEM 2015",
                   title="Boxplot das Notas em Ciências Humanas no ENEM 2015 Segundo o Sexo")+
              theme(legend.position="none")
            print(bhs15)
          }}}}
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebls15 = readRDS("ebls15.rds")
            bls15 = ggplot(ebls15, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Nota em Linguagens e Códigos no ENEM 2015",
                   title="Boxplot das Notas em Linguagens e Códigos no ENEM 2015 Segundo o Sexo")+
              theme_bw() + theme(legend.position="none")
            print(bls15)
          }}}}
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebmts15 = readRDS("ebmts15.rds")
            bmts15 = ggplot(ebmts15, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Nota em Matemática",
                   title = "Boxplot das Notas em Matemática no ENEM 2015 Segundo o Sexo")+
              theme_bw() + theme(legend.position="none")
            print(bmts15)
          }}}}
    # boxplots por região 2015:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebmr15 = readRDS("ebmr15.rds")
            bmr15 = ggplot(ebmr15, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Média no ENEM 2015",
                   title="Boxplot das Médias no ENEM 2015 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(bmr15)
          }}}}
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebrr15 = readRDS("ebrr15.rds")
            brr15 = ggplot(ebrr15, aes(x = reg, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + coord_flip() +
              labs(x="Região",y="Média da Redação no ENEM 2015",
                   title="Boxplot das Médias da Redação no ENEM 2015 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(brr15)
          }}}}
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebnr15 = readRDS("ebnr15.rds")
            bnr15 = ggplot(ebnr15, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Ciências da Natureza no ENEM 2015",
                   title = "Boxplot das Notas em Ciências da Natureza no ENEM 2015 Segundo a Região") +
              theme_bw() + theme(legend.position="none")
            print(bnr15)
          }}}}
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebhr15 = readRDS("ebhr15.rds")
            bhr15 = ggplot(ebhr15, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Ciências Humanas no ENEM 2015",
                   title="Boxplot das Notas em Ciências Humanas no ENEM 2015 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(bhr15)
          }}}}
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            eblr15 = readRDS("eblr15.rds")
            blr15 = ggplot(eblr15, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Linguagens e Códigos no ENEM 2015",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM 2015 Segundo a Região") +
              theme_bw() + theme(legend.position="none")
            print(blr15)
          }}}}
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebmtr15 = readRDS("ebmtr15.rds")
            bmtr15 = ggplot(ebmtr15, aes(x = reg, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Matemática no ENEM 2015",
                   title = "Boxplot das Notas em Matemática no ENEM 2015 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(bmtr15)
          }}}}
    # boxplots por Sexo e região 2015:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebmsr15 = readRDS("ebmsr15.rds")
            bmsr15 = ggplot(ebmsr15, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Média no ENEM 2015",
                   title="Boxplot das Médias no ENEM 2015 Segundo o Sexo e Região")+
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bmsr15)
          }}}}
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebrsr15 = readRDS("ebrsr15.rds")
            brsr15 = ggplot(ebrsr15, aes(x = sexo, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + coord_flip() +
              labs(x="",y="Nota da Média da Redação no ENEM 2015",
                   title="Boxplot das Médias da Redação no ENEM 2015 Segundo o Sexo e Região")+
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(brsr15)
          }}}}
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebnsr15 = readRDS("ebnsr15.rds")
            bnsr15 = ggplot(ebnsr15, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Nota em Ciências Humanas no ENEM 2015",
                   title = "Boxplot das Notas em Ciências Humanas no ENEM 2015 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bnsr15)
          }}}}
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebhsr15 = readRDS("ebhsr15.rds")
            bhsr15 = ggplot(ebhsr15, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Nota em Linguagens e Códigos no ENEM 2015",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM 2015 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bhsr15)
          }}}}
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            eblsr15 = readRDS("eblsr15.rds")
            blsr15 =  ggplot(eblsr15, aes(x = sexo, y = out, middle = median,
                                          ymin = lower.whisker, ymax = upper.whisker,
                                          lower = lower.hinge, upper = upper.hinge,
                                          fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Nota em Linguagens e Códigos no ENEM 2015",
                   title = "Boxplot da Nota em Linguagens e Códigos no ENEM 2015 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(blsr15)
          }}}}
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebmtsr15 = readRDS("ebmtsr15.rds")
            bmtsr15 = ggplot(ebmtsr15, aes(x = sexo, y = out, middle = median,
                                           ymin = lower.whisker, ymax = upper.whisker,
                                           lower = lower.hinge, upper = upper.hinge,
                                           fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +

              labs(x="",y="Nota em Matemática no ENEM 2015",
                   title = "Boxplot da Nota em Matemática no ENEM 2015 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bmtsr15)
          }}}}
    # boxplots por renda 2015:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebmd15 = readRDS("ebmd15.rds")
            bmd15 = ggplot(ebmd15, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Média no ENEM 2015",
                   title = "Boxplot das Médias no ENEM 2015 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 788,00",
                                             "C: De R$ 788,01 até R$ 1.182,00",
                                             "D: De R$ 1.182,01 até R$ 1.572,00",
                                             "E: De R$ 1.572,01 até R$ 1.970,00",
                                             "F: De R$ 1.970,01 até R$ 2.364,00",
                                             "G: De R$ 2.364,01 até R$ 3.152,00",
                                             "H: De R$ 3.152,01 até R$ 3.940,00",
                                             "I: De R$ 3.940,01 até R$ 4.728,00",
                                             "J: De R$ 4.728,01 até R$ 5.516,00",
                                             "K: De R$ 5.516,01 até R$ 6.304,00",
                                             "L: De R$ 6.304,01 até R$ 7.092,00",
                                             "M: De R$ 7.092,01 até R$ 7.880,00",
                                             "N: De R$ 7.880,01 até R$ 9.456,00",
                                             "O: De R$ 9.456,01 até R$ 11.820,00",
                                             "P: De R$ 11.820,01 até R$ 15.760,00",
                                             "Q: Mais de 15.760,00"))
            print(bmd15)
          }}}}
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebrd15 = readRDS("ebrd15.rds")
            brd15 = ggplot(ebrd15, aes(x = renda, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha = 0.5) +
              labs(x="Renda",y="Média da Redação no ENEM 2015",
                   title="Boxplot das Médias da Redação no ENEM 2015 Segundo a Renda")+
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') + coord_flip() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 788,00",
                                             "C: De R$ 788,01 até R$ 1.182,00",
                                             "D: De R$ 1.182,01 até R$ 1.572,00",
                                             "E: De R$ 1.572,01 até R$ 1.970,00",
                                             "F: De R$ 1.970,01 até R$ 2.364,00",
                                             "G: De R$ 2.364,01 até R$ 3.152,00",
                                             "H: De R$ 3.152,01 até R$ 3.940,00",
                                             "I: De R$ 3.940,01 até R$ 4.728,00",
                                             "J: De R$ 4.728,01 até R$ 5.516,00",
                                             "K: De R$ 5.516,01 até R$ 6.304,00",
                                             "L: De R$ 6.304,01 até R$ 7.092,00",
                                             "M: De R$ 7.092,01 até R$ 7.880,00",
                                             "N: De R$ 7.880,01 até R$ 9.456,00",
                                             "O: De R$ 9.456,01 até R$ 11.820,00",
                                             "P: De R$ 11.820,01 até R$ 15.760,00",
                                             "Q: Mais de 15.760,00"))

            print(brd15)
          }}}}
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebnd15 = readRDS("ebnd15.rds")
            bnd15 = ggplot(ebnd15, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Ciências da Natureza no ENEM 2015",
                   title = "Boxplot das Notas em Ciências da Natureza no ENEM 2015 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 788,00",
                                             "C: De R$ 788,01 até R$ 1.182,00",
                                             "D: De R$ 1.182,01 até R$ 1.572,00",
                                             "E: De R$ 1.572,01 até R$ 1.970,00",
                                             "F: De R$ 1.970,01 até R$ 2.364,00",
                                             "G: De R$ 2.364,01 até R$ 3.152,00",
                                             "H: De R$ 3.152,01 até R$ 3.940,00",
                                             "I: De R$ 3.940,01 até R$ 4.728,00",
                                             "J: De R$ 4.728,01 até R$ 5.516,00",
                                             "K: De R$ 5.516,01 até R$ 6.304,00",
                                             "L: De R$ 6.304,01 até R$ 7.092,00",
                                             "M: De R$ 7.092,01 até R$ 7.880,00",
                                             "N: De R$ 7.880,01 até R$ 9.456,00",
                                             "O: De R$ 9.456,01 até R$ 11.820,00",
                                             "P: De R$ 11.820,01 até R$ 15.760,00",
                                             "Q: Mais de 15.760,00"))
            print(bnd15)
          }}}}
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebhd15 = readRDS("ebhd15.rds")
            bhd15 = ggplot(ebhd15, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Ciências Humanas no ENEM 2015",
                   title = "Boxplot das Notas em Ciências Humanas no ENEM 2015 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 788,00",
                                             "C: De R$ 788,01 até R$ 1.182,00",
                                             "D: De R$ 1.182,01 até R$ 1.572,00",
                                             "E: De R$ 1.572,01 até R$ 1.970,00",
                                             "F: De R$ 1.970,01 até R$ 2.364,00",
                                             "G: De R$ 2.364,01 até R$ 3.152,00",
                                             "H: De R$ 3.152,01 até R$ 3.940,00",
                                             "I: De R$ 3.940,01 até R$ 4.728,00",
                                             "J: De R$ 4.728,01 até R$ 5.516,00",
                                             "K: De R$ 5.516,01 até R$ 6.304,00",
                                             "L: De R$ 6.304,01 até R$ 7.092,00",
                                             "M: De R$ 7.092,01 até R$ 7.880,00",
                                             "N: De R$ 7.880,01 até R$ 9.456,00",
                                             "O: De R$ 9.456,01 até R$ 11.820,00",
                                             "P: De R$ 11.820,01 até R$ 15.760,00",
                                             "Q: Mais de 15.760,00"))
            print(bhd15)
          }}}}
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebld15 = readRDS("ebld15.rds")
            bld15 = ggplot(ebld15, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Linguagens e Códigos no ENEM 2015",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM 2015 Segundo a Renda") +
              theme_bw() + scale_alpha(guide = 'none') +
              theme(legend.position="right") +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 788,00",
                                             "C: De R$ 788,01 até R$ 1.182,00",
                                             "D: De R$ 1.182,01 até R$ 1.572,00",
                                             "E: De R$ 1.572,01 até R$ 1.970,00",
                                             "F: De R$ 1.970,01 até R$ 2.364,00",
                                             "G: De R$ 2.364,01 até R$ 3.152,00",
                                             "H: De R$ 3.152,01 até R$ 3.940,00",
                                             "I: De R$ 3.940,01 até R$ 4.728,00",
                                             "J: De R$ 4.728,01 até R$ 5.516,00",
                                             "K: De R$ 5.516,01 até R$ 6.304,00",
                                             "L: De R$ 6.304,01 até R$ 7.092,00",
                                             "M: De R$ 7.092,01 até R$ 7.880,00",
                                             "N: De R$ 7.880,01 até R$ 9.456,00",
                                             "O: De R$ 9.456,01 até R$ 11.820,00",
                                             "P: De R$ 11.820,01 até R$ 15.760,00",
                                             "Q: Mais de 15.760,00"))
            print(bld15)
          }}}}
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebmtd15 = readRDS("ebmtd15.rds")
            bmtd15 = ggplot(ebmtd15, aes(x = renda, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.85) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Matemática no ENEM 2015",
                   title = "Boxplot das Notas em Matemática no ENEM 2015 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 788,00",
                                             "C: De R$ 788,01 até R$ 1.182,00",
                                             "D: De R$ 1.182,01 até R$ 1.572,00",
                                             "E: De R$ 1.572,01 até R$ 1.970,00",
                                             "F: De R$ 1.970,01 até R$ 2.364,00",
                                             "G: De R$ 2.364,01 até R$ 3.152,00",
                                             "H: De R$ 3.152,01 até R$ 3.940,00",
                                             "I: De R$ 3.940,01 até R$ 4.728,00",
                                             "J: De R$ 4.728,01 até R$ 5.516,00",
                                             "K: De R$ 5.516,01 até R$ 6.304,00",
                                             "L: De R$ 6.304,01 até R$ 7.092,00",
                                             "M: De R$ 7.092,01 até R$ 7.880,00",
                                             "N: De R$ 7.880,01 até R$ 9.456,00",
                                             "O: De R$ 9.456,01 até R$ 11.820,00",
                                             "P: De R$ 11.820,01 até R$ 15.760,00",
                                             "Q: Mais de 15.760,00"))
            print(bmtd15)
          }}}}
    # histogramas das notas 2015:
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thm15 = readRDS("thm15.rds")
            k = thm15$intervalo[2] - thm15$intervalo[1]
            hm15 = ggplot(thm15, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Média no ENEM 2015", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM 2015") +
              theme_bw()
            print(hm15)
          }}}}
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thr15 = readRDS("thr15.rds")
            k = thr15$intervalo[2] - thr15$intervalo[1]
            hr15 = ggplot(thr15, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota da Redação no ENEM 2015", y="Proporção",
                   title = "Histograma das Proporções das Notas da Redação no ENEM 2015")+
              theme_bw()
            print(hr15)
          }}}}
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thn15 = readRDS("thn15.rds")
            k = thn15$intervalo[2] - thn15$intervalo[1]
            hn15 = ggplot(thn15, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências da Natureza", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2015")+
              theme_bw()
            print(hn15)
          }}}}
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thh15 = readRDS("thh15.rds")
            k = thh15$intervalo[2] - thh15$intervalo[1]
            hh15 = ggplot(thh15, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências Humanas", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências Humanas no ENEM 2015")+
              theme_bw()
            print(hh15)
          }}}}
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thl15 = readRDS("thl15.rds")
            k = thl15$intervalo[2] - thl15$intervalo[1]
            hl15 = ggplot(thl15, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Linguagens e Códigos", y="Proporção",
                   title="Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2015")+
              theme_bw()
            print(hl15)
          }}}}
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thmt15 = readRDS("thmt15.rds")
            k = thmt15$intervalo[2] - thmt15$intervalo[1]
            hmt15 = ggplot(thmt15, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Matemática", y="Proporção",
                   title="Histograma das Proporções das Notas em Matemática no ENEM 2015")+
              theme_bw()
            print(hmt15)
          }}}}
    # histogramas por Sexo 2015:####
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thms15 = readRDS("thms15.rds")
            k = thms15$intervalo[2] - thms15$intervalo[1]
            hms15 = ggplot(thms15) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Média no ENEM 2015", y="Proporção",
                   title="Histograma das Proporções das Médias no ENEM 2015 Segundo o Sexo")
            print(hms15)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thrs15 = readRDS("thrs15.rds")
            k = thrs15$intervalo[2] - thrs15$intervalo[1]
            hrs15 = ggplot(thrs15) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                     width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Média da Redação no ENEM 2015", y="Proporção",
                   title="Histograma das Proporções das Médias da Redação no ENEM 2015 Segundo o Sexo")
            print(hrs15)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            thns15 = readRDS("thns15.rds")
            k = thns15$intervalo[2] - thns15$intervalo[1]
            hns15 = ggplot(thns15) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Ciências da Natureza no ENEM 2015", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2015 Segundo o Sexo")+
              theme_bw()
            print(hns15)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thhs15 = readRDS("thhs15.rds")
            k = thhs15$intervalo[2] - thhs15$intervalo[1]
            hhs15 = ggplot(thhs15) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Ciências Humanas no ENEM 2015", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências Humanas no ENEM 2015 Segundo o Sexo")+
              theme_bw()
            print(hhs15)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thls15 = readRDS("thls15.rds")
            k = thls15$intervalo[2] - thls15$intervalo[1]
            hls15 = ggplot(thls15) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Linguagens e Códigos no ENEM 2015", y="Proporção",
                   title="Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2015 Segundo o Sexo")+
              theme_bw()
            print(hls15)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thmts15 = readRDS("thmts15.rds")
            k = thmts15$intervalo[2] - thmts15$intervalo[1]
            hmts15 = ggplot(thmts15) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Matemática no ENEM 2015", y="Proporção",
                   title = "Histograma das Proporções das Notas em Matemática no ENEM 2015 Segundo o Sexo")+
              theme_bw()
            print(hmts15)
          }
        }
      }
    }
    # histogramas por região 2015:####
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thmr15 = readRDS("thmr15.rds")
            k = thmr15$intervalo[2] - thmr15$intervalo[1]
            hmr15 = ggplot(thmr15,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Média no ENEM 2015", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM 2015 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hmr15)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thrr15 = readRDS("thrr15.rds")
            k = thrr15$intervalo[2] - thrr15$intervalo[1]
            hrr15 = ggplot(thrr15,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Média da Redação no ENEM 2015", y="Proporção",
                   title = "Histograma das Proporções das Médias na Redação no ENEM 2015 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hrr15)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thnr15 = readRDS("thnr15.rds")
            k = thnr15$intervalo[2] - thnr15$intervalo[1]
            hnr15 = ggplot(thnr15,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências da natureza no ENEM 2015", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2015 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hnr15)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thhr15 = readRDS("thhr15.rds")
            k = thhr15$intervalo[2] - thhr15$intervalo[1]
            hhr15 = ggplot(thhr15,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências Humanas no ENEM 2015", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências Humanas no ENEM 2015 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hhr15)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thlr15 = readRDS("thlr15.rds")
            k = thlr15$intervalo[2] - thlr15$intervalo[1]
            hlr15 = ggplot(thlr15,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Linguagens e Códigos no ENEM 2015", y="Proporção",
                   title = "Histograma das Proporções das Nota em Linguangens e Códigos no ENEM 2015 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hlr15)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thmtr15 = readRDS("thmtr15.rds")
            k = thmtr15$intervalo[2] - thmtr15$intervalo[1]
            hmtr15 = ggplot(thmtr15,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Matemática no ENEM 2015", y="Proporção",
                   title = "Histograma das Proporções das Notas em Matemática no ENEM 2015 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hmtr15)
          }
        }
      }
    }
    # histogramas por sexo e região 2015:####
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thmsr15 = readRDS("thmsr15.rds")
            k = thmsr15$intervalo[2] - thmsr15$intervalo[1]
            hmsr15 = ggplot(thmsr15) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              labs(x="Média no ENEM 2015", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM 2015 Segundo o Sexo e Região") +
              scale_fill_discrete(name="Legenda") + theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hmsr15)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thrsr15 = readRDS("thrsr15.rds")
            k = thrsr15$intervalo[2] - thrsr15$intervalo[1]
            hrsr15 = ggplot(thrsr15) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              labs(x="Média da Redação no ENEM 2015", y="Proporção",
                   title = "Histograma das Proporções das Médias na Redação no ENEM 2015 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hrsr15)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thnsr15 = readRDS("thnsr15.rds")
            k = thnsr15$intervalo[2] - thnsr15$intervalo[1]
            hnsr15 = ggplot(thnsr15) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              labs(x="Nota em Ciências da Natureza no ENEM 2015", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2015 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hnsr15)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thhsr15 = readRDS("thhsr15.rds")
            k = thhsr15$intervalo[2] - thhsr15$intervalo[1]
            hhsr15 = ggplot(thhsr15) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              labs(x="Nota em Ciências Humanas no ENEM 2015", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências Humanas no ENEM 2015 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hhsr15)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thlsr15 = readRDS("thlsr15.rds")
            k = thlsr15$intervalo[2] - thlsr15$intervalo[1]
            hlsr15 = ggplot(thlsr15) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              labs(x="Nota em Linguagens e Códigos no ENEM 2015", y="Proporção",
                   title = "Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2015 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hlsr15)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2015 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thmtsr15 = readRDS("thmtsr15.rds")
            k = thmtsr15$intervalo[2] - thmtsr15$intervalo[1]
            hmtsr15 = ggplot(thmtsr15) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              labs(x="Nota em Matemática no ENEM 2015", y="Proporção",
                   title = "Histograma das Proporções das Notas em Matemática no ENEM 2015 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hmtsr15)
          }
        }
      }
    }
    # histogramas por renda 2015:
    if("Média Geral" %in% input$nota){
    if("Histograma" %in% input$grafico){
      if(2015 %in% input$anos){
        if("Por renda" %in% input$vcategorica){
          rm(list = ls())
          thmd15 = readRDS("thmd15.rds")
          k = thmd15$intervalo[2] - thmd15$intervalo[1]
          hmd15 = ggplot(thmd15) +
            geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
            theme_bw() +
            labs(x="Média no ENEM 2015", y="Proporção",
                 title = "Histograma das Proporções das Médias no ENEM 2015 Segundo a Renda") +
            scale_fill_discrete(name = "Legenda",
                                labels = c("Não Informado","A: Nenhuma Renda",
                                           "B: até R$ 788,00",
                                           "C: De R$ 788,01 até R$ 1.182,00",
                                           "D: De R$ 1.182,01 até R$ 1.572,00",
                                           "E: De R$ 1.572,01 até R$ 1.970,00",
                                           "F: De R$ 1.970,01 até R$ 2.364,00",
                                           "G: De R$ 2.364,01 até R$ 3.152,00",
                                           "H: De R$ 3.152,01 até R$ 3.940,00",
                                           "I: De R$ 3.940,01 até R$ 4.728,00",
                                           "J: De R$ 4.728,01 até R$ 5.516,00",
                                           "K: De R$ 5.516,01 até R$ 6.304,00",
                                           "L: De R$ 6.304,01 até R$ 7.092,00",
                                           "M: De R$ 7.092,01 até R$ 7.880,00",
                                           "N: De R$ 7.880,01 até R$ 9.456,00",
                                           "O: De R$ 9.456,01 até R$ 11.820,00",
                                           "P: De R$ 11.820,01 até R$ 15.760,00",
                                           "Q: Mais de 15.760,00"))
          print(hmd15)
        }
      }
    }
  }
    if("Média da Redação" %in% input$nota){
    if("Histograma" %in% input$grafico){
      if(2015 %in% input$anos){
        if("Por renda" %in% input$vcategorica){
          rm(list = ls())
          thrd15 = readRDS("thrd15.rds")
          k = thrd15$intervalo[2] - thrd15$intervalo[1]
          hrd15 = ggplot(thrd15) +
            geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
            theme_bw() +
            labs(x="Média da Redação no ENEM 2015", y="Proporção",
                 title="Histograma das Proporções das Médias da Redação no ENEM 2015 Segundo a Renda")+
            theme_bw() +
            scale_fill_discrete(name = "Legenda",
                                labels = c("Não Informado","A: Nenhuma Renda",
                                           "B: até R$ 788,00",
                                           "C: De R$ 788,01 até R$ 1.182,00",
                                           "D: De R$ 1.182,01 até R$ 1.572,00",
                                           "E: De R$ 1.572,01 até R$ 1.970,00",
                                           "F: De R$ 1.970,01 até R$ 2.364,00",
                                           "G: De R$ 2.364,01 até R$ 3.152,00",
                                           "H: De R$ 3.152,01 até R$ 3.940,00",
                                           "I: De R$ 3.940,01 até R$ 4.728,00",
                                           "J: De R$ 4.728,01 até R$ 5.516,00",
                                           "K: De R$ 5.516,01 até R$ 6.304,00",
                                           "L: De R$ 6.304,01 até R$ 7.092,00",
                                           "M: De R$ 7.092,01 até R$ 7.880,00",
                                           "N: De R$ 7.880,01 até R$ 9.456,00",
                                           "O: De R$ 9.456,01 até R$ 11.820,00",
                                           "P: De R$ 11.820,01 até R$ 15.760,00",
                                           "Q: Mais de 15.760,00"))
          print(hrd15)
        }
      }
    }
  }
    if("Nota em Ciências da Natureza" %in% input$nota){
    if("Histograma" %in% input$grafico){
      if(2015 %in% input$anos){
        if("Por renda" %in% input$vcategorica){
          rm(list = ls())
          thnd15 = readRDS("thnd15.rds")
          k = thnd15$intervalo[2] - thnd15$intervalo[1]
          hnd15 = ggplot(thnd15) +
            geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
            theme_bw() +
            labs(x="Nota em Ciências da Natureza no ENEM 2015", y="Proporção",
                 title="Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2015 Segundo a Renda")+
            theme_bw() +
            scale_fill_discrete(name = "Legenda",
                                labels = c("Não Informado","A: Nenhuma Renda",
                                           "B: até R$ 788,00",
                                           "C: De R$ 788,01 até R$ 1.182,00",
                                           "D: De R$ 1.182,01 até R$ 1.572,00",
                                           "E: De R$ 1.572,01 até R$ 1.970,00",
                                           "F: De R$ 1.970,01 até R$ 2.364,00",
                                           "G: De R$ 2.364,01 até R$ 3.152,00",
                                           "H: De R$ 3.152,01 até R$ 3.940,00",
                                           "I: De R$ 3.940,01 até R$ 4.728,00",
                                           "J: De R$ 4.728,01 até R$ 5.516,00",
                                           "K: De R$ 5.516,01 até R$ 6.304,00",
                                           "L: De R$ 6.304,01 até R$ 7.092,00",
                                           "M: De R$ 7.092,01 até R$ 7.880,00",
                                           "N: De R$ 7.880,01 até R$ 9.456,00",
                                           "O: De R$ 9.456,01 até R$ 11.820,00",
                                           "P: De R$ 11.820,01 até R$ 15.760,00",
                                           "Q: Mais de 15.760,00"))
          print(hnd15)
        }
      }
    }
  }
    if("Nota em Ciências Humanas" %in% input$nota){
    if("Histograma" %in% input$grafico){
      if(2015 %in% input$anos){
        if("Por renda" %in% input$vcategorica){
          rm(list = ls())
          thhd15 = readRDS("thhd15.rds")
          k = thhd15$intervalo[2] - thhd15$intervalo[1]
          hhd15 = ggplot(thhd15) +
            geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
            theme_bw() +
            labs(x="Nota em Ciências Humanas no ENEM 2015", y="Proporção",
                 title="Histograma das Proporções das Notas em Ciências Humanas no ENEM 2015 Segundo a Renda")+
            theme_bw() +
            scale_fill_discrete(name = "Legenda",
                                labels = c("Não Informado","A: Nenhuma Renda",
                                           "B: até R$ 788,00",
                                           "C: De R$ 788,01 até R$ 1.182,00",
                                           "D: De R$ 1.182,01 até R$ 1.572,00",
                                           "E: De R$ 1.572,01 até R$ 1.970,00",
                                           "F: De R$ 1.970,01 até R$ 2.364,00",
                                           "G: De R$ 2.364,01 até R$ 3.152,00",
                                           "H: De R$ 3.152,01 até R$ 3.940,00",
                                           "I: De R$ 3.940,01 até R$ 4.728,00",
                                           "J: De R$ 4.728,01 até R$ 5.516,00",
                                           "K: De R$ 5.516,01 até R$ 6.304,00",
                                           "L: De R$ 6.304,01 até R$ 7.092,00",
                                           "M: De R$ 7.092,01 até R$ 7.880,00",
                                           "N: De R$ 7.880,01 até R$ 9.456,00",
                                           "O: De R$ 9.456,01 até R$ 11.820,00",
                                           "P: De R$ 11.820,01 até R$ 15.760,00",
                                           "Q: Mais de 15.760,00"))
          print(hhd15)
        }
      }
    }
  }
    if("Nota em Linguagens e Códigos" %in% input$nota){
    if("Histograma" %in% input$grafico){
      if(2015 %in% input$anos){
        if("Por renda" %in% input$vcategorica){
          rm(list = ls())
          thld15 = readRDS("thld15.rds")
          k = thld15$intervalo[2] - thld15$intervalo[1]
          hld15 = ggplot(thld15) +
            geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
            theme_bw() +
            labs(x="Nota em Linguagens e Códigos no ENEM 2015", y="Proporção",
                 title="Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2015 Segundo a Renda")+
            theme_bw() +
            scale_fill_discrete(name = "Legenda",
                                labels = c("Não Informado","A: Nenhuma Renda",
                                           "B: até R$ 788,00",
                                           "C: De R$ 788,01 até R$ 1.182,00",
                                           "D: De R$ 1.182,01 até R$ 1.572,00",
                                           "E: De R$ 1.572,01 até R$ 1.970,00",
                                           "F: De R$ 1.970,01 até R$ 2.364,00",
                                           "G: De R$ 2.364,01 até R$ 3.152,00",
                                           "H: De R$ 3.152,01 até R$ 3.940,00",
                                           "I: De R$ 3.940,01 até R$ 4.728,00",
                                           "J: De R$ 4.728,01 até R$ 5.516,00",
                                           "K: De R$ 5.516,01 até R$ 6.304,00",
                                           "L: De R$ 6.304,01 até R$ 7.092,00",
                                           "M: De R$ 7.092,01 até R$ 7.880,00",
                                           "N: De R$ 7.880,01 até R$ 9.456,00",
                                           "O: De R$ 9.456,01 até R$ 11.820,00",
                                           "P: De R$ 11.820,01 até R$ 15.760,00",
                                           "Q: Mais de 15.760,00"))
          print(hld15)
        }
      }
    }
  }
    if("Nota em Matemática" %in% input$nota){
    if("Histograma" %in% input$grafico){
      if(2015 %in% input$anos){
        if("Por renda" %in% input$vcategorica){
          rm(list = ls())
          thmtd15 = readRDS("thmtd15.rds")
          k = thmtd15$intervalo[2] - thmtd15$intervalo[1]
          hmtd15 = ggplot(thmtd15) +
            geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
            geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
            theme_bw() +
            labs(x="Nota em Matemática no ENEM 2015", y="Proporção",
                 title="Histograma das Proporções das Notas em Matemática no ENEM 2015 Segundo a Renda")+
            theme_bw() +
            scale_fill_discrete(name = "Legenda",
                                labels = c("Não Informado","A: Nenhuma Renda",
                                           "B: até R$ 788,00",
                                           "C: De R$ 788,01 até R$ 1.182,00",
                                           "D: De R$ 1.182,01 até R$ 1.572,00",
                                           "E: De R$ 1.572,01 até R$ 1.970,00",
                                           "F: De R$ 1.970,01 até R$ 2.364,00",
                                           "G: De R$ 2.364,01 até R$ 3.152,00",
                                           "H: De R$ 3.152,01 até R$ 3.940,00",
                                           "I: De R$ 3.940,01 até R$ 4.728,00",
                                           "J: De R$ 4.728,01 até R$ 5.516,00",
                                           "K: De R$ 5.516,01 até R$ 6.304,00",
                                           "L: De R$ 6.304,01 até R$ 7.092,00",
                                           "M: De R$ 7.092,01 até R$ 7.880,00",
                                           "N: De R$ 7.880,01 até R$ 9.456,00",
                                           "O: De R$ 9.456,01 até R$ 11.820,00",
                                           "P: De R$ 11.820,01 até R$ 15.760,00",
                                           "Q: Mais de 15.760,00"))
          print(hmtd15)
        }
      }
    }
    }

    ## gráficos de 2014
    # boxplots das notas 2014
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebm14 = readRDS("ebm14.rds")
            bm14 = ggplot(ebm14, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.5)) +
              geom_boxplot(stat="identity", fill = "aquamarine4") + geom_point() + coord_flip() +
              labs(x="Brasil",y="Média no ENEM 2014 (Brasil)",
                   title = "Boxplot das Médias no ENEM 2014") + theme_bw() +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(bm14)

          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebr14 = readRDS("ebr14.rds")
            br14 = ggplot(ebr14, aes(x = "x", middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              theme_bw() + coord_flip() +
              labs(x="Brasil",y="Média da Redação no ENEM 2014",
                   title = "Boxplot das Médias da Redação no ENEM 2014") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(br14)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebn14 = readRDS("ebn14.rds")
            bn14 = ggplot(ebn14, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw()+
              theme(legend.position="none",axis.text.y=element_text(colour="white")) +
              labs(x="Brasil",y="Nota em Ciências da Natureza no ENEM 2014",
                   title = "Boxplot das Notas em Ciências da Natureza no ENEM 2014")
            print(bn14)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebh14 = readRDS("ebh14.rds")
            bh14 = ggplot(ebh14, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw() +
              labs(x="Brasil",y="Nota em Ciências Humanas no ENEM 2014",
                   title = "Boxplot das Notas em Ciências Humanas no ENEM 2014") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(bh14)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebl14 = readRDS("ebl14.rds")
            bl14 = ggplot(ebl14, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw() +
              labs(x="Brasil",y="Nota em Linguagens e Códigos no ENEM 2014",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM 2014") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(bl14)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebmt14 = readRDS("ebmt14.rds")
            bmt14 = ggplot(ebmt14, aes(x = "x", y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       aplha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw() +
              labs(x="Brasil",y="Nota em Matemática no ENEM 2014",
                   title = "Boxplot das Notas em Matemática no ENEM 2014") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(bmt14)
          }
        }
      }
    }
    # boxplots por Sexo 2014:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebms14 = readRDS("ebms14.rds")
            bms14 = ggplot(ebms14, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Sexo",y="Média no ENEM 2014",
                   title = "Boxplot das Médias no ENEM 2014 Segundo o Sexo") +
              theme(legend.position="none")
            print(bms14)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebrs14 = readRDS("ebrs14.rds")
            brs14 = ggplot(ebrs14, aes(x = sexo, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + theme_bw() + coord_flip() +
              labs(x="Sexo",y="Média da Redação no ENEM 2014",
                   title="Boxplot das Médias da Redação no ENEM 2014 Segundo o Sexo")+
              theme(legend.position="none")
            print(brs14)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebns14 = readRDS("ebns14.rds")
            bns14 = ggplot(ebns14, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Sexo",y="Nota em Ciências da Natureza no ENEM 2014",
                   title="Boxplot das Notas em Ciências da Natureza no ENEM 2014 Segundo o Sexo")+
              theme(legend.position="none")
            print(bns14)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebhs14 = readRDS("ebhs14.rds")
            bhs14 = ggplot(ebhs14, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Sexo",y="Nota em Ciências Humanas no ENEM 2014",
                   title="Boxplot das Notas em Ciências Humanas no ENEM 2014 Segundo o Sexo")+
              theme(legend.position="none")
            print(bhs14)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebls14 = readRDS("ebls14.rds")
            bls14 = ggplot(ebls14, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Nota em Linguagens e Códigos no ENEM 2014",
                   title="Boxplot das Notas em Linguagens e Códigos no ENEM 2014 Segundo o Sexo")+
              theme_bw() + theme(legend.position="none")
            print(bls14)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebmts14 = readRDS("ebmts14.rds")
            bmts14 = ggplot(ebmts14, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Nota em Matemática",
                   title = "Boxplot das Notas em Matemática no ENEM 2014 Segundo o Sexo")+
              theme_bw() + theme(legend.position="none")
            print(bmts14)
          }
        }
      }
    }
    # boxplots por região 2014:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebmr14 = readRDS("ebmr14.rds")
            bmr14 = ggplot(ebmr14, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Média no ENEM 2014",
                   title="Boxplot das Médias no ENEM 2014 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(bmr14)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebrr14 = readRDS("ebrr14.rds")
            brr14 = ggplot(ebrr14, aes(x = reg, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + coord_flip() +
              labs(x="Região",y="Média da Redação no ENEM 2014",
                   title="Boxplot das Médias da Redação no ENEM 2014 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(brr14)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebnr14 = readRDS("ebnr14.rds")
            bnr14 = ggplot(ebnr14, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Ciências da Natureza no ENEM 2014",
                   title = "Boxplot das Notas em Ciências da Natureza no ENEM 2014 Segundo a Região") +
              theme_bw() + theme(legend.position="none")
            print(bnr14)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebhr14 = readRDS("ebhr14.rds")
            bhr14 = ggplot(ebhr14, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Ciências Humanas no ENEM 2014",
                   title="Boxplot das Notas em Ciências Humanas no ENEM 2014 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(bhr14)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            eblr14 = readRDS("eblr14.rds")
            blr14 = ggplot(eblr14, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Linguagens e Códigos no ENEM 2014",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM 2014 Segundo a Região") +
              theme_bw() + theme(legend.position="none")
            print(blr14)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebmtr14 = readRDS("ebmtr14.rds")
            bmtr14 = ggplot(ebmtr14, aes(x = reg, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Matemática no ENEM 2014",
                   title = "Boxplot das Notas em Matemática no ENEM 2014 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(bmtr14)
          }
        }
      }
    }
    # boxplots por Sexo e região 2014:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebmsr14 = readRDS("ebmsr14.rds")
            bmsr14 = ggplot(ebmsr14, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Média no ENEM 2014",
                   title="Boxplot das Médias no ENEM 2014 Segundo o Sexo e Região")+
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bmsr14)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebrsr14 = readRDS("ebrsr14.rds")
            brsr14 = ggplot(ebrsr14, aes(x = sexo, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + coord_flip() +
              labs(x="",y="Nota da Média da Redação no ENEM 2014",
                   title="Boxplot das Médias da Redação no ENEM 2014 Segundo o Sexo e Região")+
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(brsr14)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebnsr14 = readRDS("ebnsr14.rds")
            bnsr14 = ggplot(ebnsr14, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Nota em Ciências Humanas no ENEM 2014",
                   title = "Boxplot das Notas em Ciências Humanas no ENEM 2014 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bnsr14)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebhsr14 = readRDS("ebhsr14.rds")
            bhsr14 = ggplot(ebhsr14, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Nota em Linguagens e Códigos no ENEM 2014",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM 2014 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bhsr14)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            eblsr14 = readRDS("eblsr14.rds")
            blsr14 =  ggplot(eblsr14, aes(x = sexo, y = out, middle = median,
                                          ymin = lower.whisker, ymax = upper.whisker,
                                          lower = lower.hinge, upper = upper.hinge,
                                          fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Nota em Linguagens e Códigos no ENEM 2014",
                   title = "Boxplot da Nota em Linguagens e Códigos no ENEM 2014 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(blsr14)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebmtsr14 = readRDS("ebmtsr14.rds")
            bmtsr14 = ggplot(ebmtsr14, aes(x = sexo, y = out, middle = median,
                                           ymin = lower.whisker, ymax = upper.whisker,
                                           lower = lower.hinge, upper = upper.hinge,
                                           fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +

              labs(x="",y="Nota em Matemática no ENEM 2014",
                   title = "Boxplot da Nota em Matemática no ENEM 2014 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bmtsr14)
          }
        }
      }
    }
    # boxplots por renda 2014:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebmd14 = readRDS("ebmd14.rds")
            bmd14 = ggplot(ebmd14, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Média no ENEM 2014",
                   title = "Boxplot das Médias no ENEM 2014 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 724,00",
                                             "C: De R$ 724,01 até R$ 1.086,00",
                                             "D: De R$ 1.086,01 até R$ 1.448,00",
                                             "E: De R$ 1.448,01 até R$ 1.810,00",
                                             "F: De R$ 1.810,01 até R$ 2.172,00",
                                             "G: De R$ 2.172,01 até R$ 2.896,00",
                                             "H: De R$ 2.896,01 até R$ 3.620,00",
                                             "I: De R$ 3.620,01 até R$ 4.344,00",
                                             "J: De R$ 4.344,01 até R$ 5.068,00",
                                             "K: De R$ 5.068,01 até R$ 5.792,00",
                                             "L: De R$ 5.792,01 até R$ 6.516,00",
                                             "M: De R$ 6.516,01 até R$ 7.240,00",
                                             "N: De R$ 7.240,01 até R$ 8.688,00",
                                             "O: De R$ 8.688,01 até R$ 10.860,00",
                                             "P: De R$ 10.860,01 até R$ 14.480,00",
                                             "Q: Mais de 14.480,00"))
            print(bmd14)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebrd14 = readRDS("ebrd14.rds")
            brd14 = ggplot(ebrd14, aes(x = renda,y=out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha = 0.5) +
              labs(x="Renda",y="Média da Redação no ENEM 2014",
                   title="Boxplot das Médias da Redação no ENEM 2014 Segundo a Renda")+
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') + coord_flip() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 724,00",
                                             "C: De R$ 724,01 até R$ 1.086,00",
                                             "D: De R$ 1.086,01 até R$ 1.448,00",
                                             "E: De R$ 1.448,01 até R$ 1.810,00",
                                             "F: De R$ 1.810,01 até R$ 2.172,00",
                                             "G: De R$ 2.172,01 até R$ 2.896,00",
                                             "H: De R$ 2.896,01 até R$ 3.620,00",
                                             "I: De R$ 3.620,01 até R$ 4.344,00",
                                             "J: De R$ 4.344,01 até R$ 5.068,00",
                                             "K: De R$ 5.068,01 até R$ 5.792,00",
                                             "L: De R$ 5.792,01 até R$ 6.516,00",
                                             "M: De R$ 6.516,01 até R$ 7.240,00",
                                             "N: De R$ 7.240,01 até R$ 8.688,00",
                                             "O: De R$ 8.688,01 até R$ 10.860,00",
                                             "P: De R$ 10.860,01 até R$ 14.480,00",
                                             "Q: Mais de 14.480,00"))

            print(brd14)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebnd14 = readRDS("ebnd14.rds")
            bnd14 = ggplot(ebnd14, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Ciências da Natureza no ENEM 2014",
                   title = "Boxplot das Notas em Ciências da Natureza no ENEM 2014 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 724,00",
                                             "C: De R$ 724,01 até R$ 1.086,00",
                                             "D: De R$ 1.086,01 até R$ 1.448,00",
                                             "E: De R$ 1.448,01 até R$ 1.810,00",
                                             "F: De R$ 1.810,01 até R$ 2.172,00",
                                             "G: De R$ 2.172,01 até R$ 2.896,00",
                                             "H: De R$ 2.896,01 até R$ 3.620,00",
                                             "I: De R$ 3.620,01 até R$ 4.344,00",
                                             "J: De R$ 4.344,01 até R$ 5.068,00",
                                             "K: De R$ 5.068,01 até R$ 5.792,00",
                                             "L: De R$ 5.792,01 até R$ 6.516,00",
                                             "M: De R$ 6.516,01 até R$ 7.240,00",
                                             "N: De R$ 7.240,01 até R$ 8.688,00",
                                             "O: De R$ 8.688,01 até R$ 10.860,00",
                                             "P: De R$ 10.860,01 até R$ 14.480,00",
                                             "Q: Mais de 14.480,00"))
            print(bnd14)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebhd14 = readRDS("ebhd14.rds")
            bhd14 = ggplot(ebhd14, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Ciências Humanas no ENEM 2014",
                   title = "Boxplot das Notas em Ciências Humanas no ENEM 2014 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 724,00",
                                             "C: De R$ 724,01 até R$ 1.086,00",
                                             "D: De R$ 1.086,01 até R$ 1.448,00",
                                             "E: De R$ 1.448,01 até R$ 1.810,00",
                                             "F: De R$ 1.810,01 até R$ 2.172,00",
                                             "G: De R$ 2.172,01 até R$ 2.896,00",
                                             "H: De R$ 2.896,01 até R$ 3.620,00",
                                             "I: De R$ 3.620,01 até R$ 4.344,00",
                                             "J: De R$ 4.344,01 até R$ 5.068,00",
                                             "K: De R$ 5.068,01 até R$ 5.792,00",
                                             "L: De R$ 5.792,01 até R$ 6.516,00",
                                             "M: De R$ 6.516,01 até R$ 7.240,00",
                                             "N: De R$ 7.240,01 até R$ 8.688,00",
                                             "O: De R$ 8.688,01 até R$ 10.860,00",
                                             "P: De R$ 10.860,01 até R$ 14.480,00",
                                             "Q: Mais de 14.480,00"))
            print(bhd14)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebld14 = readRDS("ebld14.rds")
            bld14 = ggplot(ebld14, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Linguagens e Códigos no ENEM 2014",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM 2014 Segundo a Renda") +
              theme_bw() + scale_alpha(guide = 'none') +
              theme(legend.position="right") +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 724,00",
                                             "C: De R$ 724,01 até R$ 1.086,00",
                                             "D: De R$ 1.086,01 até R$ 1.448,00",
                                             "E: De R$ 1.448,01 até R$ 1.810,00",
                                             "F: De R$ 1.810,01 até R$ 2.172,00",
                                             "G: De R$ 2.172,01 até R$ 2.896,00",
                                             "H: De R$ 2.896,01 até R$ 3.620,00",
                                             "I: De R$ 3.620,01 até R$ 4.344,00",
                                             "J: De R$ 4.344,01 até R$ 5.068,00",
                                             "K: De R$ 5.068,01 até R$ 5.792,00",
                                             "L: De R$ 5.792,01 até R$ 6.516,00",
                                             "M: De R$ 6.516,01 até R$ 7.240,00",
                                             "N: De R$ 7.240,01 até R$ 8.688,00",
                                             "O: De R$ 8.688,01 até R$ 10.860,00",
                                             "P: De R$ 10.860,01 até R$ 14.480,00",
                                             "Q: Mais de 14.480,00"))
            print(bld14)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebmtd14 = readRDS("ebmtd14.rds")
            bmtd14 = ggplot(ebmtd14, aes(x = renda, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.85) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Matemática no ENEM 2014",
                   title = "Boxplot das Notas em Matemática no ENEM 2014 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 724,00",
                                             "C: De R$ 724,01 até R$ 1.086,00",
                                             "D: De R$ 1.086,01 até R$ 1.448,00",
                                             "E: De R$ 1.448,01 até R$ 1.810,00",
                                             "F: De R$ 1.810,01 até R$ 2.172,00",
                                             "G: De R$ 2.172,01 até R$ 2.896,00",
                                             "H: De R$ 2.896,01 até R$ 3.620,00",
                                             "I: De R$ 3.620,01 até R$ 4.344,00",
                                             "J: De R$ 4.344,01 até R$ 5.068,00",
                                             "K: De R$ 5.068,01 até R$ 5.792,00",
                                             "L: De R$ 5.792,01 até R$ 6.516,00",
                                             "M: De R$ 6.516,01 até R$ 7.240,00",
                                             "N: De R$ 7.240,01 até R$ 8.688,00",
                                             "O: De R$ 8.688,01 até R$ 10.860,00",
                                             "P: De R$ 10.860,01 até R$ 14.480,00",
                                             "Q: Mais de 14.480,00"))
            print(bmtd14)
          }
        }
      }
    }
    # histogramas das notas 2014:
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thm14 = readRDS("thm14.rds")
            k = thm14$intervalo[2] - thm14$intervalo[1]
            hm14 = ggplot(thm14, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Média no ENEM 2014", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM 2014") +
              theme_bw()
            print(hm14)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thr14 = readRDS("thr14.rds")
            k = thr14$intervalo[2] - thr14$intervalo[1]
            hr14 = ggplot(thr14, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota da Redação no ENEM 2014", y="Proporção",
                   title = "Histograma das Proporções das Notas da Redação no ENEM 2014")+
              theme_bw()
            print(hr14)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thn14 = readRDS("thn14.rds")
            k = thn14$intervalo[2] - thn14$intervalo[1]
            hn14 = ggplot(thn14, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências da Natureza", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2014")+
              theme_bw()
            print(hn14)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thh14 = readRDS("thh14.rds")
            k = thh14$intervalo[2] - thh14$intervalo[1]
            hh14 = ggplot(thh14, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências Humanas", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências Humanas no ENEM 2014")+
              theme_bw()
            print(hh14)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thl14 = readRDS("thl14.rds")
            k = thl14$intervalo[2] - thl14$intervalo[1]
            hl14 = ggplot(thl14, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Linguagens e Códigos", y="Proporção",
                   title="Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2014")+
              theme_bw()
            print(hl14)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thmt14 = readRDS("thmt14.rds")
            k = thmt14$intervalo[2] - thmt14$intervalo[1]
            hmt14 = ggplot(thmt14, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Matemática", y="Proporção",
                   title="Histograma das Proporções das Notas em Matemática no ENEM 2014")+
              theme_bw()
            print(hmt14)
          }
        }
      }
    }
    # histogramas por Sexo 2014:####
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thms14 = readRDS("thms14.rds")
            k = thms14$intervalo[2] - thms14$intervalo[1]
            hms14 = ggplot(thms14) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Média no ENEM 2014", y="Proporção",
                   title="Histograma das Proporções das Médias no ENEM 2014 Segundo o Sexo")
            print(hms14)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thrs14 = readRDS("thrs14.rds")
            k = thrs14$intervalo[2] - thrs14$intervalo[1]
            hrs14 = ggplot(thrs14) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Média da Redação no ENEM 2014", y="Proporção",
                   title="Histograma das Proporções das Médias da Redação no ENEM 2014 Segundo o Sexo")
            print(hrs14)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            thns14 = readRDS("thns14.rds")
            k = thns14$intervalo[2] - thns14$intervalo[1]
            hns14 = ggplot(thns14) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Ciências da Natureza no ENEM 2014", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2014 Segundo o Sexo")+
              theme_bw()
            print(hns14)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thhs14 = readRDS("thhs14.rds")
            k = thhs14$intervalo[2] - thhs14$intervalo[1]
            hhs14 = ggplot(thhs14) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Ciências Humanas no ENEM 2014", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências Humanas no ENEM 2014 Segundo o Sexo")+
              theme_bw()
            print(hhs14)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thls14 = readRDS("thls14.rds")
            k = thls14$intervalo[2] - thls14$intervalo[1]
            hls14 = ggplot(thls14) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Linguagens e Códigos no ENEM 2014", y="Proporção",
                   title="Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2014 Segundo o Sexo")+
              theme_bw()
            print(hls14)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thmts14 = readRDS("thmts14.rds")
            k = thmts14$intervalo[2] - thmts14$intervalo[1]
            hmts14 = ggplot(thmts14) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Matemática no ENEM 2014", y="Proporção",
                   title = "Histograma das Proporções das Notas em Matemática no ENEM 2014 Segundo o Sexo")+
              theme_bw()
            print(hmts14)
          }
        }
      }
    }
    # histogramas por região 2014:####
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thmr14 = readRDS("thmr14.rds")
            k = thmr14$intervalo[2] - thmr14$intervalo[1]
            hmr14 = ggplot(thmr14,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Média no ENEM 2014", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM 2014 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hmr14)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thrr14 = readRDS("thrr14.rds")
            k = thrr14$intervalo[2] - thrr14$intervalo[1]
            hrr14 = ggplot(thrr14,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Média da Redação no ENEM 2014", y="Proporção",
                   title = "Histograma das Proporções das Médias na Redação no ENEM 2014 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hrr14)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thnr14 = readRDS("thnr14.rds")
            k = thnr14$intervalo[2] - thnr14$intervalo[1]
            hnr14 = ggplot(thnr14,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências da natureza no ENEM 2014", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2014 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hnr14)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thhr14 = readRDS("thhr14.rds")
            k = thhr14$intervalo[2] - thhr14$intervalo[1]
            hhr14 = ggplot(thhr14,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências Humanas no ENEM 2014", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências Humanas no ENEM 2014 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hhr14)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thlr14 = readRDS("thlr14.rds")
            k = thlr14$intervalo[2] - thlr14$intervalo[1]
            hlr14 = ggplot(thlr14,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Linguagens e Códigos no ENEM 2014", y="Proporção",
                   title = "Histograma das Proporções das Nota em Linguangens e Códigos no ENEM 2014 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hlr14)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thmtr14 = readRDS("thmtr14.rds")
            k = thmtr14$intervalo[2] - thmtr14$intervalo[1]
            hmtr14 = ggplot(thmtr14,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Matemática no ENEM 2014", y="Proporção",
                   title = "Histograma das Proporções das Notas em Matemática no ENEM 2014 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hmtr14)
          }
        }
      }
    }
    # histogramas por sexo e região 2014:####
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thmsr14 = readRDS("thmsr14.rds")
            k = thmsr14$intervalo[2] - thmsr14$intervalo[1]
            hmsr14 = ggplot(thmsr14) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              labs(x="Média no ENEM 2014", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM 2014 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hmsr14)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thrsr14 = readRDS("thrsr14.rds")
            k = thrsr14$intervalo[2] - thrsr14$intervalo[1]
            hrsr14 = ggplot(thrsr14) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              labs(x="Média da Redação no ENEM 2014", y="Proporção",
                   title = "Histograma das Proporções das Médias na Redação no ENEM 2014 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hrsr14)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thnsr14 = readRDS("thnsr14.rds")
            k = thnsr14$intervalo[2] - thnsr14$intervalo[1]
            hnsr14 = ggplot(thnsr14) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              labs(x="Nota em Ciências da Natureza no ENEM 2014", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2014 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hnsr14)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thhsr14 = readRDS("thhsr14.rds")
            k = thhsr14$intervalo[2] - thhsr14$intervalo[1]
            hhsr14 = ggplot(thhsr14) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              labs(x="Nota em Ciências Humanas no ENEM 2014", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências Humanas no ENEM 2014 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hhsr14)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thlsr14 = readRDS("thlsr14.rds")
            k = thlsr14$intervalo[2] - thlsr14$intervalo[1]
            hlsr14 = ggplot(thlsr14) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              labs(x="Nota em Linguagens e Códigos no ENEM 2014", y="Proporção",
                   title = "Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2014 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hlsr14)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thmtsr14 = readRDS("thmtsr14.rds")
            k = thmtsr14$intervalo[2] - thmtsr14$intervalo[1]
            hmtsr14 = ggplot(thmtsr14) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              labs(x="Nota em Matemática no ENEM 2014", y="Proporção",
                   title = "Histograma das Proporções das Notas em Matemática no ENEM 2014 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hmtsr14)
          }
        }
      }
    }
    # histogramas por renda 2014:
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thmd14 = readRDS("thmd14.rds")
            k = thmd14$intervalo[2] - thmd14$intervalo[1]
            hmd14 = ggplot(thmd14) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Média no ENEM 2014", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM 2014 Segundo a Renda") +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 724,00",
                                             "C: De R$ 724,01 até R$ 1.086,00",
                                             "D: De R$ 1.086,01 até R$ 1.448,00",
                                             "E: De R$ 1.448,01 até R$ 1.810,00",
                                             "F: De R$ 1.810,01 até R$ 2.172,00",
                                             "G: De R$ 2.172,01 até R$ 2.896,00",
                                             "H: De R$ 2.896,01 até R$ 3.620,00",
                                             "I: De R$ 3.620,01 até R$ 4.344,00",
                                             "J: De R$ 4.344,01 até R$ 5.068,00",
                                             "K: De R$ 5.068,01 até R$ 5.792,00",
                                             "L: De R$ 5.792,01 até R$ 6.516,00",
                                             "M: De R$ 6.516,01 até R$ 7.240,00",
                                             "N: De R$ 7.240,01 até R$ 8.688,00",
                                             "O: De R$ 8.688,01 até R$ 10.860,00",
                                             "P: De R$ 10.860,01 até R$ 14.480,00",
                                             "Q: Mais de 14.480,00"))
            print(hmd14)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thrd14 = readRDS("thrd14.rds")
            k = thrd14$intervalo[2] - thrd14$intervalo[1]
            hrd14 = ggplot(thrd14) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Média da Redação no ENEM 2014", y="Proporção",
                   title="Histograma das Proporções das Médias da Redação no ENEM 2014 Segundo a Renda")+
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 724,00",
                                             "C: De R$ 724,01 até R$ 1.086,00",
                                             "D: De R$ 1.086,01 até R$ 1.448,00",
                                             "E: De R$ 1.448,01 até R$ 1.810,00",
                                             "F: De R$ 1.810,01 até R$ 2.172,00",
                                             "G: De R$ 2.172,01 até R$ 2.896,00",
                                             "H: De R$ 2.896,01 até R$ 3.620,00",
                                             "I: De R$ 3.620,01 até R$ 4.344,00",
                                             "J: De R$ 4.344,01 até R$ 5.068,00",
                                             "K: De R$ 5.068,01 até R$ 5.792,00",
                                             "L: De R$ 5.792,01 até R$ 6.516,00",
                                             "M: De R$ 6.516,01 até R$ 7.240,00",
                                             "N: De R$ 7.240,01 até R$ 8.688,00",
                                             "O: De R$ 8.688,01 até R$ 10.860,00",
                                             "P: De R$ 10.860,01 até R$ 14.480,00",
                                             "Q: Mais de 14.480,00"))
            print(hrd14)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thnd14 = readRDS("thnd14.rds")
            k = thnd14$intervalo[2] - thnd14$intervalo[1]
            hnd14 = ggplot(thnd14) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Nota em Ciências da Natureza no ENEM 2014", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2014 Segundo a Renda")+
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 724,00",
                                             "C: De R$ 724,01 até R$ 1.086,00",
                                             "D: De R$ 1.086,01 até R$ 1.448,00",
                                             "E: De R$ 1.448,01 até R$ 1.810,00",
                                             "F: De R$ 1.810,01 até R$ 2.172,00",
                                             "G: De R$ 2.172,01 até R$ 2.896,00",
                                             "H: De R$ 2.896,01 até R$ 3.620,00",
                                             "I: De R$ 3.620,01 até R$ 4.344,00",
                                             "J: De R$ 4.344,01 até R$ 5.068,00",
                                             "K: De R$ 5.068,01 até R$ 5.792,00",
                                             "L: De R$ 5.792,01 até R$ 6.516,00",
                                             "M: De R$ 6.516,01 até R$ 7.240,00",
                                             "N: De R$ 7.240,01 até R$ 8.688,00",
                                             "O: De R$ 8.688,01 até R$ 10.860,00",
                                             "P: De R$ 10.860,01 até R$ 14.480,00",
                                             "Q: Mais de 14.480,00"))
            print(hnd14)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thhd14 = readRDS("thhd14.rds")
            k = thhd14$intervalo[2] - thhd14$intervalo[1]
            hhd14 = ggplot(thhd14) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Nota em Ciências Humanas no ENEM 2014", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências Humanas no ENEM 2014 Segundo a Renda")+
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 724,00",
                                             "C: De R$ 724,01 até R$ 1.086,00",
                                             "D: De R$ 1.086,01 até R$ 1.448,00",
                                             "E: De R$ 1.448,01 até R$ 1.810,00",
                                             "F: De R$ 1.810,01 até R$ 2.172,00",
                                             "G: De R$ 2.172,01 até R$ 2.896,00",
                                             "H: De R$ 2.896,01 até R$ 3.620,00",
                                             "I: De R$ 3.620,01 até R$ 4.344,00",
                                             "J: De R$ 4.344,01 até R$ 5.068,00",
                                             "K: De R$ 5.068,01 até R$ 5.792,00",
                                             "L: De R$ 5.792,01 até R$ 6.516,00",
                                             "M: De R$ 6.516,01 até R$ 7.240,00",
                                             "N: De R$ 7.240,01 até R$ 8.688,00",
                                             "O: De R$ 8.688,01 até R$ 10.860,00",
                                             "P: De R$ 10.860,01 até R$ 14.480,00",
                                             "Q: Mais de 14.480,00"))
            print(hhd14)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thld14 = readRDS("thld14.rds")
            k = thld14$intervalo[2] - thld14$intervalo[1]
            hld14 = ggplot(thld14) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Nota em Linguagens e Códigos no ENEM 2014", y="Proporção",
                   title="Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2014 Segundo a Renda")+
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 724,00",
                                             "C: De R$ 724,01 até R$ 1.086,00",
                                             "D: De R$ 1.086,01 até R$ 1.448,00",
                                             "E: De R$ 1.448,01 até R$ 1.810,00",
                                             "F: De R$ 1.810,01 até R$ 2.172,00",
                                             "G: De R$ 2.172,01 até R$ 2.896,00",
                                             "H: De R$ 2.896,01 até R$ 3.620,00",
                                             "I: De R$ 3.620,01 até R$ 4.344,00",
                                             "J: De R$ 4.344,01 até R$ 5.068,00",
                                             "K: De R$ 5.068,01 até R$ 5.792,00",
                                             "L: De R$ 5.792,01 até R$ 6.516,00",
                                             "M: De R$ 6.516,01 até R$ 7.240,00",
                                             "N: De R$ 7.240,01 até R$ 8.688,00",
                                             "O: De R$ 8.688,01 até R$ 10.860,00",
                                             "P: De R$ 10.860,01 até R$ 14.480,00",
                                             "Q: Mais de 14.480,00"))
            print(hld14)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2014 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thmtd14 = readRDS("thmtd14.rds")
            k = thmtd14$intervalo[2] - thmtd14$intervalo[1]
            hmtd14 = ggplot(thmtd14) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Nota em Matemática no ENEM 2014", y="Proporção",
                   title="Histograma das Proporções das Notas em Matemática no ENEM 2014 Segundo a Renda")+
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 724,00",
                                             "C: De R$ 724,01 até R$ 1.086,00",
                                             "D: De R$ 1.086,01 até R$ 1.448,00",
                                             "E: De R$ 1.448,01 até R$ 1.810,00",
                                             "F: De R$ 1.810,01 até R$ 2.172,00",
                                             "G: De R$ 2.172,01 até R$ 2.896,00",
                                             "H: De R$ 2.896,01 até R$ 3.620,00",
                                             "I: De R$ 3.620,01 até R$ 4.344,00",
                                             "J: De R$ 4.344,01 até R$ 5.068,00",
                                             "K: De R$ 5.068,01 até R$ 5.792,00",
                                             "L: De R$ 5.792,01 até R$ 6.516,00",
                                             "M: De R$ 6.516,01 até R$ 7.240,00",
                                             "N: De R$ 7.240,01 até R$ 8.688,00",
                                             "O: De R$ 8.688,01 até R$ 10.860,00",
                                             "P: De R$ 10.860,01 até R$ 14.480,00",
                                             "Q: Mais de 14.480,00"))
            print(hmtd14)
          }
        }
      }
    }

    ## gráficos de 2013
    # boxplots das notas 2013
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebm13 = readRDS("ebm13.rds")
            bm13 = ggplot(ebm13, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.5)) +
              geom_boxplot(stat="identity", fill = "aquamarine4") + geom_point() + coord_flip() +
              labs(x="Brasil",y="Média no ENEM 2013 (Brasil)",
                   title = "Boxplot das Médias no ENEM 2013") + theme_bw() +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(bm13)

          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebr13 = readRDS("ebr13.rds")
            br13 = ggplot(ebr13, aes(x = "x", middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              theme_bw() + coord_flip() +
              labs(x="Brasil",y="Média da Redação no ENEM 2013",
                   title = "Boxplot das Médias da Redação no ENEM 2013") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(br13)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebn13 = readRDS("ebn13.rds")
            bn13 = ggplot(ebn13, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw()+
              theme(legend.position="none",axis.text.y=element_text(colour="white")) +
              labs(x="Brasil",y="Nota em Ciências da Natureza no ENEM 2013",
                   title = "Boxplot das Notas em Ciências da Natureza no ENEM 2013")
            print(bn13)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebh13 = readRDS("ebh13.rds")
            bh13 = ggplot(ebh13, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw() +
              labs(x="Brasil",y="Nota em Ciências Humanas no ENEM 2013",
                   title = "Boxplot das Notas em Ciências Humanas no ENEM 2013") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(bh13)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebl13 = readRDS("ebl13.rds")
            bl13 = ggplot(ebl13, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw() +
              labs(x="Brasil",y="Nota em Linguagens e Códigos no ENEM 2013",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM 2013") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(bl13)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebmt13 = readRDS("ebmt13.rds")
            bmt13 = ggplot(ebmt13, aes(x = "x", y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       aplha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw() +
              labs(x="Brasil",y="Nota em Matemática no ENEM 2013",
                   title = "Boxplot das Notas em Matemática no ENEM 2013") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(bmt13)
          }
        }
      }
    }
    # boxplots por Sexo 2013:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebms13 = readRDS("ebms13.rds")
            bms13 = ggplot(ebms13, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Sexo",y="Média no ENEM 2013",
                   title = "Boxplot das Médias no ENEM 2013 Segundo o Sexo") +
              theme(legend.position="none")
            print(bms13)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebrs13 = readRDS("ebrs13.rds")
            brs13 = ggplot(ebrs13, aes(x = sexo, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + theme_bw() + coord_flip() +
              labs(x="Sexo",y="Média da Redação no ENEM 2013",
                   title="Boxplot das Médias da Redação no ENEM 2013 Segundo o Sexo")+
              theme(legend.position="none")
            print(brs13)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebns13 = readRDS("ebns13.rds")
            bns13 = ggplot(ebns13, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Sexo",y="Nota em Ciências da Natureza no ENEM 2013",
                   title="Boxplot das Notas em Ciências da Natureza no ENEM 2013 Segundo o Sexo")+
              theme(legend.position="none")
            print(bns13)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebhs13 = readRDS("ebhs13.rds")
            bhs13 = ggplot(ebhs13, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Sexo",y="Nota em Ciências Humanas no ENEM 2013",
                   title="Boxplot das Notas em Ciências Humanas no ENEM 2013 Segundo o Sexo")+
              theme(legend.position="none")
            print(bhs13)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebls13 = readRDS("ebls13.rds")
            bls13 = ggplot(ebls13, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Nota em Linguagens e Códigos no ENEM 2013",
                   title="Boxplot das Notas em Linguagens e Códigos no ENEM 2013 Segundo o Sexo")+
              theme_bw() + theme(legend.position="none")
            print(bls13)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebmts13 = readRDS("ebmts13.rds")
            bmts13 = ggplot(ebmts13, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Nota em Matemática",
                   title = "Boxplot das Notas em Matemática no ENEM 2013 Segundo o Sexo")+
              theme_bw() + theme(legend.position="none")
            print(bmts13)
          }
        }
      }
    }
    # boxplots por região 2013:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebmr13 = readRDS("ebmr13.rds")
            bmr13 = ggplot(ebmr13, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Média no ENEM 2013",
                   title="Boxplot das Médias no ENEM 2013 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(bmr13)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebrr13 = readRDS("ebrr13.rds")
            brr13 = ggplot(ebrr13, aes(x = reg, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + coord_flip() +
              labs(x="Região",y="Média da Redação no ENEM 2013",
                   title="Boxplot das Médias da Redação no ENEM 2013 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(brr13)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebnr13 = readRDS("ebnr13.rds")
            bnr13 = ggplot(ebnr13, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Ciências da Natureza no ENEM 2013",
                   title = "Boxplot das Notas em Ciências da Natureza no ENEM 2013 Segundo a Região") +
              theme_bw() + theme(legend.position="none")
            print(bnr13)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebhr13 = readRDS("ebhr13.rds")
            bhr13 = ggplot(ebhr13, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Ciências Humanas no ENEM 2013",
                   title="Boxplot das Notas em Ciências Humanas no ENEM 2013 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(bhr13)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            eblr13 = readRDS("eblr13.rds")
            blr13 = ggplot(eblr13, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Linguagens e Códigos no ENEM 2013",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM 2013 Segundo a Região") +
              theme_bw() + theme(legend.position="none")
            print(blr13)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebmtr13 = readRDS("ebmtr13.rds")
            bmtr13 = ggplot(ebmtr13, aes(x = reg, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Matemática no ENEM 2013",
                   title = "Boxplot das Notas em Matemática no ENEM 2013 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(bmtr13)
          }
        }
      }
    }
    # boxplots por Sexo e região 2013:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebmsr13 = readRDS("ebmsr13.rds")
            bmsr13 = ggplot(ebmsr13, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Média no ENEM 2013",
                   title="Boxplot das Médias no ENEM 2013 Segundo o Sexo e Região")+
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bmsr13)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebrsr13 = readRDS("ebrsr13.rds")
            brsr13 = ggplot(ebrsr13, aes(x = sexo, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + coord_flip() +
              labs(x="",y="Nota da Média da Redação no ENEM 2013",
                   title="Boxplot das Médias da Redação no ENEM 2013 Segundo o Sexo e Região")+
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(brsr13)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebnsr13 = readRDS("ebnsr13.rds")
            bnsr13 = ggplot(ebnsr13, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Nota em Ciências Humanas no ENEM 2013",
                   title = "Boxplot das Notas em Ciências Humanas no ENEM 2013 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bnsr13)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebhsr13 = readRDS("ebhsr13.rds")
            bhsr13 = ggplot(ebhsr13, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Nota em Linguagens e Códigos no ENEM 2013",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM 2013 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bhsr13)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            eblsr13 = readRDS("eblsr13.rds")
            blsr13 =  ggplot(eblsr13, aes(x = sexo, y = out, middle = median,
                                          ymin = lower.whisker, ymax = upper.whisker,
                                          lower = lower.hinge, upper = upper.hinge,
                                          fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Nota em Linguagens e Códigos no ENEM 2013",
                   title = "Boxplot da Nota em Linguagens e Códigos no ENEM 2013 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(blsr13)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebmtsr13 = readRDS("ebmtsr13.rds")
            bmtsr13 = ggplot(ebmtsr13, aes(x = sexo, y = out, middle = median,
                                           ymin = lower.whisker, ymax = upper.whisker,
                                           lower = lower.hinge, upper = upper.hinge,
                                           fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +

              labs(x="",y="Nota em Matemática no ENEM 2013",
                   title = "Boxplot da Nota em Matemática no ENEM 2013 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bmtsr13)
          }
        }
      }
    }
    # boxplots por renda 2013:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebmd13 = readRDS("ebmd13.rds")
            bmd13 = ggplot(ebmd13, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Média no ENEM 2013",
                   title = "Boxplot das Médias no ENEM 2013 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 678,00",
                                             "C: De R$ 678,01 até R$ 1.017,00",
                                             "D: De R$ 1.017,01 até R$ 1.356,00",
                                             "E: De R$ 1.356,01 até R$ 1.695,00",
                                             "F: De R$ 1.695,01 até R$ 2.034,00",
                                             "G: De R$ 2.034,01 até R$ 2.712,00",
                                             "H: De R$ 2.712,01 até R$ 3.390,00",
                                             "I: De R$ 3.390,01 até R$ 4.068,00",
                                             "J: De R$ 4.068,01 até R$ 4.746,00",
                                             "K: De R$ 4.746,01 até R$ 5.424,00",
                                             "L: De R$ 5.424,01 até R$ 6.102,00",
                                             "M: De R$ 6.102,01 até R$ 6.780,00",
                                             "N: De R$ 6.780,01 até R$ 8.136,00",
                                             "O: De R$ 8.136,01 até R$ 10.170,00",
                                             "P: De R$ 10.170,01 até R$ 13.560,00",
                                             "Q: Mais de 13.560,00"))
            print(bmd13)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebrd13 = readRDS("ebrd13.rds")
            brd13 = ggplot(ebrd13, aes(x = renda, y=out,middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha = 0.5) +
              labs(x="Renda",y="Média da Redação no ENEM 2013",
                   title="Boxplot das Médias da Redação no ENEM 2013 Segundo a Renda")+
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') + coord_flip() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 678,00",
                                             "C: De R$ 678,01 até R$ 1.017,00",
                                             "D: De R$ 1.017,01 até R$ 1.356,00",
                                             "E: De R$ 1.356,01 até R$ 1.695,00",
                                             "F: De R$ 1.695,01 até R$ 2.034,00",
                                             "G: De R$ 2.034,01 até R$ 2.712,00",
                                             "H: De R$ 2.712,01 até R$ 3.390,00",
                                             "I: De R$ 3.390,01 até R$ 4.068,00",
                                             "J: De R$ 4.068,01 até R$ 4.746,00",
                                             "K: De R$ 4.746,01 até R$ 5.424,00",
                                             "L: De R$ 5.424,01 até R$ 6.102,00",
                                             "M: De R$ 6.102,01 até R$ 6.780,00",
                                             "N: De R$ 6.780,01 até R$ 8.136,00",
                                             "O: De R$ 8.136,01 até R$ 10.170,00",
                                             "P: De R$ 10.170,01 até R$ 13.560,00",
                                             "Q: Mais de 13.560,00"))

            print(brd13)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebnd13 = readRDS("ebnd13.rds")
            bnd13 = ggplot(ebnd13, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Ciências da Natureza no ENEM 2013",
                   title = "Boxplot das Notas em Ciências da Natureza no ENEM 2013 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 678,00",
                                             "C: De R$ 678,01 até R$ 1.017,00",
                                             "D: De R$ 1.017,01 até R$ 1.356,00",
                                             "E: De R$ 1.356,01 até R$ 1.695,00",
                                             "F: De R$ 1.695,01 até R$ 2.034,00",
                                             "G: De R$ 2.034,01 até R$ 2.712,00",
                                             "H: De R$ 2.712,01 até R$ 3.390,00",
                                             "I: De R$ 3.390,01 até R$ 4.068,00",
                                             "J: De R$ 4.068,01 até R$ 4.746,00",
                                             "K: De R$ 4.746,01 até R$ 5.424,00",
                                             "L: De R$ 5.424,01 até R$ 6.102,00",
                                             "M: De R$ 6.102,01 até R$ 6.780,00",
                                             "N: De R$ 6.780,01 até R$ 8.136,00",
                                             "O: De R$ 8.136,01 até R$ 10.170,00",
                                             "P: De R$ 10.170,01 até R$ 13.560,00",
                                             "Q: Mais de 13.560,00"))
            print(bnd13)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebhd13 = readRDS("ebhd13.rds")
            bhd13 = ggplot(ebhd13, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Ciências Humanas no ENEM 2013",
                   title = "Boxplot das Notas em Ciências Humanas no ENEM 2013 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 678,00",
                                             "C: De R$ 678,01 até R$ 1.017,00",
                                             "D: De R$ 1.017,01 até R$ 1.356,00",
                                             "E: De R$ 1.356,01 até R$ 1.695,00",
                                             "F: De R$ 1.695,01 até R$ 2.034,00",
                                             "G: De R$ 2.034,01 até R$ 2.712,00",
                                             "H: De R$ 2.712,01 até R$ 3.390,00",
                                             "I: De R$ 3.390,01 até R$ 4.068,00",
                                             "J: De R$ 4.068,01 até R$ 4.746,00",
                                             "K: De R$ 4.746,01 até R$ 5.424,00",
                                             "L: De R$ 5.424,01 até R$ 6.102,00",
                                             "M: De R$ 6.102,01 até R$ 6.780,00",
                                             "N: De R$ 6.780,01 até R$ 8.136,00",
                                             "O: De R$ 8.136,01 até R$ 10.170,00",
                                             "P: De R$ 10.170,01 até R$ 13.560,00",
                                             "Q: Mais de 13.560,00"))
            print(bhd13)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebld13 = readRDS("ebld13.rds")
            bld13 = ggplot(ebld13, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Linguagens e Códigos no ENEM 2013",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM 2013 Segundo a Renda") +
              theme_bw() + scale_alpha(guide = 'none') +
              theme(legend.position="right") +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 678,00",
                                             "C: De R$ 678,01 até R$ 1.017,00",
                                             "D: De R$ 1.017,01 até R$ 1.356,00",
                                             "E: De R$ 1.356,01 até R$ 1.695,00",
                                             "F: De R$ 1.695,01 até R$ 2.034,00",
                                             "G: De R$ 2.034,01 até R$ 2.712,00",
                                             "H: De R$ 2.712,01 até R$ 3.390,00",
                                             "I: De R$ 3.390,01 até R$ 4.068,00",
                                             "J: De R$ 4.068,01 até R$ 4.746,00",
                                             "K: De R$ 4.746,01 até R$ 5.424,00",
                                             "L: De R$ 5.424,01 até R$ 6.102,00",
                                             "M: De R$ 6.102,01 até R$ 6.780,00",
                                             "N: De R$ 6.780,01 até R$ 8.136,00",
                                             "O: De R$ 8.136,01 até R$ 10.170,00",
                                             "P: De R$ 10.170,01 até R$ 13.560,00",
                                             "Q: Mais de 13.560,00"))
            print(bld13)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebmtd13 = readRDS("ebmtd13.rds")
            bmtd13 = ggplot(ebmtd13, aes(x = renda, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.85) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Matemática no ENEM 2013",
                   title = "Boxplot das Notas em Matemática no ENEM 2013 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 678,00",
                                             "C: De R$ 678,01 até R$ 1.017,00",
                                             "D: De R$ 1.017,01 até R$ 1.356,00",
                                             "E: De R$ 1.356,01 até R$ 1.695,00",
                                             "F: De R$ 1.695,01 até R$ 2.034,00",
                                             "G: De R$ 2.034,01 até R$ 2.712,00",
                                             "H: De R$ 2.712,01 até R$ 3.390,00",
                                             "I: De R$ 3.390,01 até R$ 4.068,00",
                                             "J: De R$ 4.068,01 até R$ 4.746,00",
                                             "K: De R$ 4.746,01 até R$ 5.424,00",
                                             "L: De R$ 5.424,01 até R$ 6.102,00",
                                             "M: De R$ 6.102,01 até R$ 6.780,00",
                                             "N: De R$ 6.780,01 até R$ 8.136,00",
                                             "O: De R$ 8.136,01 até R$ 10.170,00",
                                             "P: De R$ 10.170,01 até R$ 13.560,00",
                                             "Q: Mais de 13.560,00"))
            print(bmtd13)
          }
        }
      }
    }
    # histogramas das notas 2013:
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thm13 = readRDS("thm13.rds")
            k = thm13$intervalo[2] - thm13$intervalo[1]
            hm13 = ggplot(thm13, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Média no ENEM 2013", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM 2013") +
              theme_bw()
            print(hm13)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thr13 = readRDS("thr13.rds")
            k = thr13$intervalo[2] - thr13$intervalo[1]
            hr13 = ggplot(thr13, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota da Redação no ENEM 2013", y="Proporção",
                   title = "Histograma das Proporções das Notas da Redação no ENEM 2013")+
              theme_bw()
            print(hr13)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thn13 = readRDS("thn13.rds")
            k = thn13$intervalo[2] - thn13$intervalo[1]
            hn13 = ggplot(thn13, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências da Natureza", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2013")+
              theme_bw()
            print(hn13)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thh13 = readRDS("thh13.rds")
            k = thh13$intervalo[2] - thh13$intervalo[1]
            hh13 = ggplot(thh13, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências Humanas", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências Humanas no ENEM 2013")+
              theme_bw()
            print(hh13)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thl13 = readRDS("thl13.rds")
            k = thl13$intervalo[2] - thl13$intervalo[1]
            hl13 = ggplot(thl13, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Linguagens e Códigos", y="Proporção",
                   title="Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2013")+
              theme_bw()
            print(hl13)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thmt13 = readRDS("thmt13.rds")
            k = thmt13$intervalo[2] - thmt13$intervalo[1]
            hmt13 = ggplot(thmt13, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Matemática", y="Proporção",
                   title="Histograma das Proporções das Notas em Matemática no ENEM 2013")+
              theme_bw()
            print(hmt13)
          }
        }
      }
    }
    # histogramas por Sexo 2013:####
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thms13 = readRDS("thms13.rds")
            k = thms13$intervalo[2] - thms13$intervalo[1]
            hms13 = ggplot(thms13) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Média no ENEM 2013", y="Proporção",
                   title="Histograma das Proporções das Médias no ENEM 2013 Segundo o Sexo")
            print(hms13)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thrs13 = readRDS("thrs13.rds")
            k = thrs13$intervalo[2] - thrs13$intervalo[1]
            hrs13 = ggplot(thrs13) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Média da Redação no ENEM 2013", y="Proporção",
                   title="Histograma das Proporções das Médias da Redação no ENEM 2013 Segundo o Sexo")
            print(hrs13)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            thns13 = readRDS("thns13.rds")
            k = thns13$intervalo[2] - thns13$intervalo[1]
            hns13 = ggplot(thns13) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Ciências da Natureza no ENEM 2013", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2013 Segundo o Sexo")+
              theme_bw()
            print(hns13)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thhs13 = readRDS("thhs13.rds")
            k = thhs13$intervalo[2] - thhs13$intervalo[1]
            hhs13 = ggplot(thhs13) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Ciências Humanas no ENEM 2013", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências Humanas no ENEM 2013 Segundo o Sexo")+
              theme_bw()
            print(hhs13)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thls13 = readRDS("thls13.rds")
            k = thls13$intervalo[2] - thls13$intervalo[1]
            hls13 = ggplot(thls13) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Linguagens e Códigos no ENEM 2013", y="Proporção",
                   title="Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2013 Segundo o Sexo")+
              theme_bw()
            print(hls13)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thmts13 = readRDS("thmts13.rds")
            k = thmts13$intervalo[2] - thmts13$intervalo[1]
            hmts13 = ggplot(thmts13) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Matemática no ENEM 2013", y="Proporção",
                   title = "Histograma das Proporções das Notas em Matemática no ENEM 2013 Segundo o Sexo")+
              theme_bw()
            print(hmts13)
          }
        }
      }
    }
    # histogramas por região 2013:####
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thmr13 = readRDS("thmr13.rds")
            k = thmr13$intervalo[2] - thmr13$intervalo[1]
            hmr13 = ggplot(thmr13,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Média no ENEM 2013", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM 2013 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hmr13)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thrr13 = readRDS("thrr13.rds")
            k = thrr13$intervalo[2] - thrr13$intervalo[1]
            hrr13 = ggplot(thrr13,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Média da Redação no ENEM 2013", y="Proporção",
                   title = "Histograma das Proporções das Médias na Redação no ENEM 2013 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hrr13)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thnr13 = readRDS("thnr13.rds")
            k = thnr13$intervalo[2] - thnr13$intervalo[1]
            hnr13 = ggplot(thnr13,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências da natureza no ENEM 2013", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2013 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hnr13)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thhr13 = readRDS("thhr13.rds")
            k = thhr13$intervalo[2] - thhr13$intervalo[1]
            hhr13 = ggplot(thhr13,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências Humanas no ENEM 2013", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências Humanas no ENEM 2013 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hhr13)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thlr13 = readRDS("thlr13.rds")
            k = thlr13$intervalo[2] - thlr13$intervalo[1]
            hlr13 = ggplot(thlr13,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Linguagens e Códigos no ENEM 2013", y="Proporção",
                   title = "Histograma das Proporções das Nota em Linguangens e Códigos no ENEM 2013 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hlr13)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thmtr13 = readRDS("thmtr13.rds")
            k = thmtr13$intervalo[2] - thmtr13$intervalo[1]
            hmtr13 = ggplot(thmtr13,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Matemática no ENEM 2013", y="Proporção",
                   title = "Histograma das Proporções das Notas em Matemática no ENEM 2013 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hmtr13)
          }
        }
      }
    }
    # histogramas por sexo e região 2013:####
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thmsr13 = readRDS("thmsr13.rds")
            k = thmsr13$intervalo[2] - thmsr13$intervalo[1]
            hmsr13 = ggplot(thmsr13) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              labs(x="Média no ENEM 2013", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM 2013 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hmsr13)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thrsr13 = readRDS("thrsr13.rds")
            k = thrsr13$intervalo[2] - thrsr13$intervalo[1]
            hrsr13 = ggplot(thrsr13) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              labs(x="Média da Redação no ENEM 2013", y="Proporção",
                   title = "Histograma das Proporções das Médias na Redação no ENEM 2013 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hrsr13)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thnsr13 = readRDS("thnsr13.rds")
            k = thnsr13$intervalo[2] - thnsr13$intervalo[1]
            hnsr13 = ggplot(thnsr13) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              labs(x="Nota em Ciências da Natureza no ENEM 2013", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2013 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hnsr13)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thhsr13 = readRDS("thhsr13.rds")
            k = thhsr13$intervalo[2] - thhsr13$intervalo[1]
            hhsr13 = ggplot(thhsr13) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              labs(x="Nota em Ciências Humanas no ENEM 2013", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências Humanas no ENEM 2013 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hhsr13)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thlsr13 = readRDS("thlsr13.rds")
            k = thlsr13$intervalo[2] - thlsr13$intervalo[1]
            hlsr13 = ggplot(thlsr13) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              labs(x="Nota em Linguagens e Códigos no ENEM 2013", y="Proporção",
                   title = "Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2013 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hlsr13)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thmtsr13 = readRDS("thmtsr13.rds")
            k = thmtsr13$intervalo[2] - thmtsr13$intervalo[1]
            hmtsr13 = ggplot(thmtsr13) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              labs(x="Nota em Matemática no ENEM 2013", y="Proporção",
                   title = "Histograma das Proporções das Notas em Matemática no ENEM 2013 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hmtsr13)
          }
        }
      }
    }
    # histogramas por renda 2013:
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thmd13 = readRDS("thmd13.rds")
            k = thmd13$intervalo[2] - thmd13$intervalo[1]
            hmd13 = ggplot(thmd13) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Média no ENEM 2013", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM 2013 Segundo a Renda") +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 678,00",
                                             "C: De R$ 678,01 até R$ 1.017,00",
                                             "D: De R$ 1.017,01 até R$ 1.356,00",
                                             "E: De R$ 1.356,01 até R$ 1.695,00",
                                             "F: De R$ 1.695,01 até R$ 2.034,00",
                                             "G: De R$ 2.034,01 até R$ 2.712,00",
                                             "H: De R$ 2.712,01 até R$ 3.390,00",
                                             "I: De R$ 3.390,01 até R$ 4.068,00",
                                             "J: De R$ 4.068,01 até R$ 4.746,00",
                                             "K: De R$ 4.746,01 até R$ 5.424,00",
                                             "L: De R$ 5.424,01 até R$ 6.102,00",
                                             "M: De R$ 6.102,01 até R$ 6.780,00",
                                             "N: De R$ 6.780,01 até R$ 8.136,00",
                                             "O: De R$ 8.136,01 até R$ 10.170,00",
                                             "P: De R$ 10.170,01 até R$ 13.560,00",
                                             "Q: Mais de 13.560,00"))
            print(hmd13)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thrd13 = readRDS("thrd13.rds")
            k = thrd13$intervalo[2] - thrd13$intervalo[1]
            hrd13 = ggplot(thrd13) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Média da Redação no ENEM 2013", y="Proporção",
                   title="Histograma das Proporções das Médias da Redação no ENEM 2013 Segundo a Renda")+
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 678,00",
                                             "C: De R$ 678,01 até R$ 1.017,00",
                                             "D: De R$ 1.017,01 até R$ 1.356,00",
                                             "E: De R$ 1.356,01 até R$ 1.695,00",
                                             "F: De R$ 1.695,01 até R$ 2.034,00",
                                             "G: De R$ 2.034,01 até R$ 2.712,00",
                                             "H: De R$ 2.712,01 até R$ 3.390,00",
                                             "I: De R$ 3.390,01 até R$ 4.068,00",
                                             "J: De R$ 4.068,01 até R$ 4.746,00",
                                             "K: De R$ 4.746,01 até R$ 5.424,00",
                                             "L: De R$ 5.424,01 até R$ 6.102,00",
                                             "M: De R$ 6.102,01 até R$ 6.780,00",
                                             "N: De R$ 6.780,01 até R$ 8.136,00",
                                             "O: De R$ 8.136,01 até R$ 10.170,00",
                                             "P: De R$ 10.170,01 até R$ 13.560,00",
                                             "Q: Mais de 13.560,00"))
            print(hrd13)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thnd13 = readRDS("thnd13.rds")
            k = thnd13$intervalo[2] - thnd13$intervalo[1]
            hnd13 = ggplot(thnd13) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Nota em Ciências da Natureza no ENEM 2013", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2013 Segundo a Renda")+
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 678,00",
                                             "C: De R$ 678,01 até R$ 1.017,00",
                                             "D: De R$ 1.017,01 até R$ 1.356,00",
                                             "E: De R$ 1.356,01 até R$ 1.695,00",
                                             "F: De R$ 1.695,01 até R$ 2.034,00",
                                             "G: De R$ 2.034,01 até R$ 2.712,00",
                                             "H: De R$ 2.712,01 até R$ 3.390,00",
                                             "I: De R$ 3.390,01 até R$ 4.068,00",
                                             "J: De R$ 4.068,01 até R$ 4.746,00",
                                             "K: De R$ 4.746,01 até R$ 5.424,00",
                                             "L: De R$ 5.424,01 até R$ 6.102,00",
                                             "M: De R$ 6.102,01 até R$ 6.780,00",
                                             "N: De R$ 6.780,01 até R$ 8.136,00",
                                             "O: De R$ 8.136,01 até R$ 10.170,00",
                                             "P: De R$ 10.170,01 até R$ 13.560,00",
                                             "Q: Mais de 13.560,00"))
            print(hnd13)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thhd13 = readRDS("thhd13.rds")
            k = thhd13$intervalo[2] - thhd13$intervalo[1]
            hhd13 = ggplot(thhd13) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Nota em Ciências Humanas no ENEM 2013", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências Humanas no ENEM 2013 Segundo a Renda")+
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 678,00",
                                             "C: De R$ 678,01 até R$ 1.017,00",
                                             "D: De R$ 1.017,01 até R$ 1.356,00",
                                             "E: De R$ 1.356,01 até R$ 1.695,00",
                                             "F: De R$ 1.695,01 até R$ 2.034,00",
                                             "G: De R$ 2.034,01 até R$ 2.712,00",
                                             "H: De R$ 2.712,01 até R$ 3.390,00",
                                             "I: De R$ 3.390,01 até R$ 4.068,00",
                                             "J: De R$ 4.068,01 até R$ 4.746,00",
                                             "K: De R$ 4.746,01 até R$ 5.424,00",
                                             "L: De R$ 5.424,01 até R$ 6.102,00",
                                             "M: De R$ 6.102,01 até R$ 6.780,00",
                                             "N: De R$ 6.780,01 até R$ 8.136,00",
                                             "O: De R$ 8.136,01 até R$ 10.170,00",
                                             "P: De R$ 10.170,01 até R$ 13.560,00",
                                             "Q: Mais de 13.560,00"))
            print(hhd13)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thld13 = readRDS("thld13.rds")
            k = thld13$intervalo[2] - thld13$intervalo[1]
            hld13 = ggplot(thld13) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Nota em Linguagens e Códigos no ENEM 2013", y="Proporção",
                   title="Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2013 Segundo a Renda")+
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 678,00",
                                             "C: De R$ 678,01 até R$ 1.017,00",
                                             "D: De R$ 1.017,01 até R$ 1.356,00",
                                             "E: De R$ 1.356,01 até R$ 1.695,00",
                                             "F: De R$ 1.695,01 até R$ 2.034,00",
                                             "G: De R$ 2.034,01 até R$ 2.712,00",
                                             "H: De R$ 2.712,01 até R$ 3.390,00",
                                             "I: De R$ 3.390,01 até R$ 4.068,00",
                                             "J: De R$ 4.068,01 até R$ 4.746,00",
                                             "K: De R$ 4.746,01 até R$ 5.424,00",
                                             "L: De R$ 5.424,01 até R$ 6.102,00",
                                             "M: De R$ 6.102,01 até R$ 6.780,00",
                                             "N: De R$ 6.780,01 até R$ 8.136,00",
                                             "O: De R$ 8.136,01 até R$ 10.170,00",
                                             "P: De R$ 10.170,01 até R$ 13.560,00",
                                             "Q: Mais de 13.560,00"))
            print(hld13)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2013 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thmtd13 = readRDS("thmtd13.rds")
            k = thmtd13$intervalo[2] - thmtd13$intervalo[1]
            hmtd13 = ggplot(thmtd13) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Nota em Matemática no ENEM 2013", y="Proporção",
                   title="Histograma das Proporções das Notas em Matemática no ENEM 2013 Segundo a Renda")+
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 678,00",
                                             "C: De R$ 678,01 até R$ 1.017,00",
                                             "D: De R$ 1.017,01 até R$ 1.356,00",
                                             "E: De R$ 1.356,01 até R$ 1.695,00",
                                             "F: De R$ 1.695,01 até R$ 2.034,00",
                                             "G: De R$ 2.034,01 até R$ 2.712,00",
                                             "H: De R$ 2.712,01 até R$ 3.390,00",
                                             "I: De R$ 3.390,01 até R$ 4.068,00",
                                             "J: De R$ 4.068,01 até R$ 4.746,00",
                                             "K: De R$ 4.746,01 até R$ 5.424,00",
                                             "L: De R$ 5.424,01 até R$ 6.102,00",
                                             "M: De R$ 6.102,01 até R$ 6.780,00",
                                             "N: De R$ 6.780,01 até R$ 8.136,00",
                                             "O: De R$ 8.136,01 até R$ 10.170,00",
                                             "P: De R$ 10.170,01 até R$ 13.560,00",
                                             "Q: Mais de 13.560,00"))
            print(hmtd13)
          }
        }
      }
    }

    ## gráficos de 2012
    # boxplots das notas 2012
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebm12 = readRDS("ebm12.rds")
            bm12 = ggplot(ebm12, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.5)) +
              geom_boxplot(stat="identity", fill = "aquamarine4") + geom_point() + coord_flip() +
              labs(x="Brasil",y="Média no ENEM 2012 (Brasil)",
                   title = "Boxplot das Médias no ENEM 2012") + theme_bw() +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(bm12)

          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebr12 = readRDS("ebr12.rds")
            br12 = ggplot(ebr12, aes(x = "x", middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              theme_bw() + coord_flip() +
              labs(x="Brasil",y="Média da Redação no ENEM 2012",
                   title = "Boxplot das Médias da Redação no ENEM 2012") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(br12)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebn12 = readRDS("ebn12.rds")
            bn12 = ggplot(ebn12, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw() +
              theme(legend.position="none",axis.text.y=element_text(colour="white")) +
              labs(x="Brasil",y="Nota em Ciências da Natureza no ENEM 2012",
                   title = "Boxplot das Notas em Ciências da Natureza no ENEM 2012")
            print(bn12)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebh12 = readRDS("ebh12.rds")
            bh12 = ggplot(ebh12, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw() +
              labs(x="Brasil",y="Nota em Ciências Humanas no ENEM 2012",
                   title = "Boxplot das Notas em Ciências Humanas no ENEM 2012") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(bh12)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebl12 = readRDS("ebl12.rds")
            bl12 = ggplot(ebl12, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw() +
              labs(x="Brasil",y="Nota em Linguagens e Códigos no ENEM 2012",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM 2012") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(bl12)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebmt12 = readRDS("ebmt12.rds")
            bmt12 = ggplot(ebmt12, aes(x = "x", y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       aplha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw() +
              labs(x="Brasil",y="Nota em Matemática no ENEM 2012",
                   title = "Boxplot das Notas em Matemática no ENEM 2012") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(bmt12)
          }
        }
      }
    }
    # boxplots por Sexo 2012:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebms12 = readRDS("ebms12.rds")
            bms12 = ggplot(ebms12, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Sexo",y="Média no ENEM 2012",
                   title = "Boxplot das Médias no ENEM 2012 Segundo o Sexo") +
              theme(legend.position="none")
            print(bms12)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebrs12 = readRDS("ebrs12.rds")
            brs12 = ggplot(ebrs12, aes(x = sexo, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + theme_bw() + coord_flip() +
              labs(x="Sexo",y="Média da Redação no ENEM 2012",
                   title="Boxplot das Médias da Redação no ENEM 2012 Segundo o Sexo")+
              theme(legend.position="none")
            print(brs12)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebns12 = readRDS("ebns12.rds")
            bns12 = ggplot(ebns12, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Sexo",y="Nota em Ciências da Natureza no ENEM 2012",
                   title="Boxplot das Notas em Ciências da Natureza no ENEM 2012 Segundo o Sexo")+
              theme(legend.position="none")
            print(bns12)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebhs12 = readRDS("ebhs12.rds")
            bhs12 = ggplot(ebhs12, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Sexo",y="Nota em Ciências Humanas no ENEM 2012",
                   title="Boxplot das Notas em Ciências Humanas no ENEM 2012 Segundo o Sexo")+
              theme(legend.position="none")
            print(bhs12)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebls12 = readRDS("ebls12.rds")
            bls12 = ggplot(ebls12, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Nota em Linguagens e Códigos no ENEM 2012",
                   title="Boxplot das Notas em Linguagens e Códigos no ENEM 2012 Segundo o Sexo")+
              theme_bw() + theme(legend.position="none")
            print(bls12)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebmts12 = readRDS("ebmts12.rds")
            bmts12 = ggplot(ebmts12, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Nota em Matemática",
                   title = "Boxplot das Notas em Matemática no ENEM 2012 Segundo o Sexo")+
              theme_bw() + theme(legend.position="none")
            print(bmts12)
          }
        }
      }
    }
    # boxplots por região 2012:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebmr12 = readRDS("ebmr12.rds")
            bmr12 = ggplot(ebmr12, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Média no ENEM 2012",
                   title="Boxplot das Médias no ENEM 2012 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(bmr12)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebrr12 = readRDS("ebrr12.rds")
            brr12 = ggplot(ebrr12, aes(x = reg, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + coord_flip() +
              labs(x="Região",y="Média da Redação no ENEM 2012",
                   title="Boxplot das Médias da Redação no ENEM 2012 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(brr12)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebnr12 = readRDS("ebnr12.rds")
            bnr12 = ggplot(ebnr12, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Ciências da Natureza no ENEM 2012",
                   title = "Boxplot das Notas em Ciências da Natureza no ENEM 2012 Segundo a Região") +
              theme_bw() + theme(legend.position="none")
            print(bnr12)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebhr12 = readRDS("ebhr12.rds")
            bhr12 = ggplot(ebhr12, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Ciências Humanas no ENEM 2012",
                   title="Boxplot das Notas em Ciências Humanas no ENEM 2012 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(bhr12)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            eblr12 = readRDS("eblr12.rds")
            blr12 = ggplot(eblr12, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Linguagens e Códigos no ENEM 2012",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM 2012 Segundo a Região") +
              theme_bw() + theme(legend.position="none")
            print(blr12)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebmtr12 = readRDS("ebmtr12.rds")
            bmtr12 = ggplot(ebmtr12, aes(x = reg, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Matemática no ENEM 2012",
                   title = "Boxplot das Notas em Matemática no ENEM 2012 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(bmtr12)
          }
        }
      }
    }
    # boxplots por Sexo e região 2012:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebmsr12 = readRDS("ebmsr12.rds")
            bmsr12 = ggplot(ebmsr12, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Média no ENEM 2012",
                   title="Boxplot das Médias no ENEM 2012 Segundo o Sexo e Região")+
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bmsr12)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebrsr12 = readRDS("ebrsr12.rds")
            brsr12 = ggplot(ebrsr12, aes(x = sexo, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + coord_flip() +
              labs(x="",y="Nota da Média da Redação no ENEM 2012",
                   title="Boxplot das Médias da Redação no ENEM 2012 Segundo o Sexo e Região")+
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(brsr12)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebnsr12 = readRDS("ebnsr12.rds")
            bnsr12 = ggplot(ebnsr12, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Nota em Ciências da Natureza no ENEM 2012",
                   title = "Boxplot das Notas em Ciências da Natureza no ENEM 2012 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bnsr12)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebhsr12 = readRDS("ebhsr12.rds")
            bhsr12 = ggplot(ebhsr12, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Nota em Ciências Humanas no ENEM 2012",
                   title = "Boxplot das Notas em Ciências Humanas no ENEM 2012 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bhsr12)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            eblsr12 = readRDS("eblsr12.rds")
            blsr12 =  ggplot(eblsr12, aes(x = sexo, y = out, middle = median,
                                          ymin = lower.whisker, ymax = upper.whisker,
                                          lower = lower.hinge, upper = upper.hinge,
                                          fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Nota em Linguagens e Códigos no ENEM 2012",
                   title = "Boxplot da Nota em Linguagens e Códigos no ENEM 2012 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(blsr12)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebmtsr12 = readRDS("ebmtsr12.rds")
            bmtsr12 = ggplot(ebmtsr12, aes(x = sexo, y = out, middle = median,
                                           ymin = lower.whisker, ymax = upper.whisker,
                                           lower = lower.hinge, upper = upper.hinge,
                                           fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +

              labs(x="",y="Nota em Matemática no ENEM 2012",
                   title = "Boxplot da Nota em Matemática no ENEM 2012 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bmtsr12)
          }
        }
      }
    }
    # boxplots por renda 2012:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebmd12 = readRDS("ebmd12.rds")
            bmd12 = ggplot(ebmd12, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Média no ENEM 2012",
                   title = "Boxplot das Médias no ENEM 2012 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 622,00",
                                             "C: De R$ 622,01 até R$ 933,00",
                                             "D: De R$ 933,01 até R$ 1.244,00",
                                             "E: De R$ 1.244,01 até R$ 1.555,00",
                                             "F: De R$ 1.555,01 até R$ 1.866,00",
                                             "G: De R$ 1.866,01 até R$ 2.488,00",
                                             "H: De R$ 2.488,01 até R$ 3.110,00",
                                             "I: De R$ 3.110,01 até R$ 3.732,00",
                                             "J: De R$ 3.732,01 até R$ 4.354,00",
                                             "K: De R$ 4.354,01 até R$ 4.976,00",
                                             "L: De R$ 4.976,01 até R$ 5.598,00",
                                             "M: De R$ 5.598,01 até R$ 6.220,00",
                                             "N: De R$ 6.220,01 até R$ 7.464,00",
                                             "O: De R$ 7.464,01 até R$ 9.330,00",
                                             "P: De R$ 9.330,01 até R$ 12.440,00",
                                             "Q: Mais de 12.440,00"))
            print(bmd12)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebrd12 = readRDS("ebrd12.rds")
            brd12 = ggplot(ebrd12, aes(x = renda, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha = 0.5) +
              labs(x="Renda",y="Média da Redação no ENEM 2012",
                   title="Boxplot das Médias da Redação no ENEM 2012 Segundo a Renda")+
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') + coord_flip() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 622,00",
                                             "C: De R$ 622,01 até R$ 933,00",
                                             "D: De R$ 933,01 até R$ 1.244,00",
                                             "E: De R$ 1.244,01 até R$ 1.555,00",
                                             "F: De R$ 1.555,01 até R$ 1.866,00",
                                             "G: De R$ 1.866,01 até R$ 2.488,00",
                                             "H: De R$ 2.488,01 até R$ 3.110,00",
                                             "I: De R$ 3.110,01 até R$ 3.732,00",
                                             "J: De R$ 3.732,01 até R$ 4.354,00",
                                             "K: De R$ 4.354,01 até R$ 4.976,00",
                                             "L: De R$ 4.976,01 até R$ 5.598,00",
                                             "M: De R$ 5.598,01 até R$ 6.220,00",
                                             "N: De R$ 6.220,01 até R$ 7.464,00",
                                             "O: De R$ 7.464,01 até R$ 9.330,00",
                                             "P: De R$ 9.330,01 até R$ 12.440,00",
                                             "Q: Mais de 12.440,00"))

            print(brd12)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebnd12 = readRDS("ebnd12.rds")
            bnd12 = ggplot(ebnd12, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Ciências da Natureza no ENEM 2012",
                   title = "Boxplot das Notas em Ciências da Natureza no ENEM 2012 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 622,00",
                                             "C: De R$ 622,01 até R$ 933,00",
                                             "D: De R$ 933,01 até R$ 1.244,00",
                                             "E: De R$ 1.244,01 até R$ 1.555,00",
                                             "F: De R$ 1.555,01 até R$ 1.866,00",
                                             "G: De R$ 1.866,01 até R$ 2.488,00",
                                             "H: De R$ 2.488,01 até R$ 3.110,00",
                                             "I: De R$ 3.110,01 até R$ 3.732,00",
                                             "J: De R$ 3.732,01 até R$ 4.354,00",
                                             "K: De R$ 4.354,01 até R$ 4.976,00",
                                             "L: De R$ 4.976,01 até R$ 5.598,00",
                                             "M: De R$ 5.598,01 até R$ 6.220,00",
                                             "N: De R$ 6.220,01 até R$ 7.464,00",
                                             "O: De R$ 7.464,01 até R$ 9.330,00",
                                             "P: De R$ 9.330,01 até R$ 12.440,00",
                                             "Q: Mais de 12.440,00"))
            print(bnd12)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebhd12 = readRDS("ebhd12.rds")
            bhd12 = ggplot(ebhd12, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Ciências Humanas no ENEM 2012",
                   title = "Boxplot das Notas em Ciências Humanas no ENEM 2012 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 622,00",
                                             "C: De R$ 622,01 até R$ 933,00",
                                             "D: De R$ 933,01 até R$ 1.244,00",
                                             "E: De R$ 1.244,01 até R$ 1.555,00",
                                             "F: De R$ 1.555,01 até R$ 1.866,00",
                                             "G: De R$ 1.866,01 até R$ 2.488,00",
                                             "H: De R$ 2.488,01 até R$ 3.110,00",
                                             "I: De R$ 3.110,01 até R$ 3.732,00",
                                             "J: De R$ 3.732,01 até R$ 4.354,00",
                                             "K: De R$ 4.354,01 até R$ 4.976,00",
                                             "L: De R$ 4.976,01 até R$ 5.598,00",
                                             "M: De R$ 5.598,01 até R$ 6.220,00",
                                             "N: De R$ 6.220,01 até R$ 7.464,00",
                                             "O: De R$ 7.464,01 até R$ 9.330,00",
                                             "P: De R$ 9.330,01 até R$ 12.440,00",
                                             "Q: Mais de 12.440,00"))
            print(bhd12)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebld12 = readRDS("ebld12.rds")
            bld12 = ggplot(ebld12, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Linguagens e Códigos no ENEM 2012",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM 2012 Segundo a Renda") +
              theme_bw() + scale_alpha(guide = 'none') +
              theme(legend.position="right") +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 622,00",
                                             "C: De R$ 622,01 até R$ 933,00",
                                             "D: De R$ 933,01 até R$ 1.244,00",
                                             "E: De R$ 1.244,01 até R$ 1.555,00",
                                             "F: De R$ 1.555,01 até R$ 1.866,00",
                                             "G: De R$ 1.866,01 até R$ 2.488,00",
                                             "H: De R$ 2.488,01 até R$ 3.110,00",
                                             "I: De R$ 3.110,01 até R$ 3.732,00",
                                             "J: De R$ 3.732,01 até R$ 4.354,00",
                                             "K: De R$ 4.354,01 até R$ 4.976,00",
                                             "L: De R$ 4.976,01 até R$ 5.598,00",
                                             "M: De R$ 5.598,01 até R$ 6.220,00",
                                             "N: De R$ 6.220,01 até R$ 7.464,00",
                                             "O: De R$ 7.464,01 até R$ 9.330,00",
                                             "P: De R$ 9.330,01 até R$ 12.440,00",
                                             "Q: Mais de 12.440,00"))
            print(bld12)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebmtd12 = readRDS("ebmtd12.rds")
            bmtd12 = ggplot(ebmtd12, aes(x = renda, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.85) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Matemática no ENEM 2012",
                   title = "Boxplot das Notas em Matemática no ENEM 2012 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 622,00",
                                             "C: De R$ 622,01 até R$ 933,00",
                                             "D: De R$ 933,01 até R$ 1.244,00",
                                             "E: De R$ 1.244,01 até R$ 1.555,00",
                                             "F: De R$ 1.555,01 até R$ 1.866,00",
                                             "G: De R$ 1.866,01 até R$ 2.488,00",
                                             "H: De R$ 2.488,01 até R$ 3.110,00",
                                             "I: De R$ 3.110,01 até R$ 3.732,00",
                                             "J: De R$ 3.732,01 até R$ 4.354,00",
                                             "K: De R$ 4.354,01 até R$ 4.976,00",
                                             "L: De R$ 4.976,01 até R$ 5.598,00",
                                             "M: De R$ 5.598,01 até R$ 6.220,00",
                                             "N: De R$ 6.220,01 até R$ 7.464,00",
                                             "O: De R$ 7.464,01 até R$ 9.330,00",
                                             "P: De R$ 9.330,01 até R$ 12.440,00",
                                             "Q: Mais de 12.440,00"))
            print(bmtd12)
          }
        }
      }
    }
    # histogramas das notas 2012:
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thm12 = readRDS("thm12.rds")
            k = thm12$intervalo[2] - thm12$intervalo[1]
            hm12 = ggplot(thm12, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Média no ENEM 2012", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM 2012") +
              theme_bw()
            print(hm12)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thr12 = readRDS("thr12.rds")
            k = thr12$intervalo[2] - thr12$intervalo[1]
            hr12 = ggplot(thr12, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota da Redação no ENEM 2012", y="Proporção",
                   title = "Histograma das Proporções das Notas da Redação no ENEM 2012")+
              theme_bw()
            print(hr12)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thn12 = readRDS("thn12.rds")
            k = thn12$intervalo[2] - thn12$intervalo[1]
            hn12 = ggplot(thn12, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências da Natureza", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2012")+
              theme_bw()
            print(hn12)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thh12 = readRDS("thh12.rds")
            k = thh12$intervalo[2] - thh12$intervalo[1]
            hh12 = ggplot(thh12, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências Humanas", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências Humanas no ENEM 2012")+
              theme_bw()
            print(hh12)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thl12 = readRDS("thl12.rds")
            k = thl12$intervalo[2] - thl12$intervalo[1]
            hl12 = ggplot(thl12, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Linguagens e Códigos", y="Proporção",
                   title="Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2012")+
              theme_bw()
            print(hl12)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thmt12 = readRDS("thmt12.rds")
            k = thmt12$intervalo[2] - thmt12$intervalo[1]
            hmt12 = ggplot(thmt12, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Matemática", y="Proporção",
                   title="Histograma das Proporções das Notas em Matemática no ENEM 2012")+
              theme_bw()
            print(hmt12)
          }
        }
      }
    }
    # histogramas por Sexo 2012:####
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thms12 = readRDS("thms12.rds")
            k = thms12$intervalo[2] - thms12$intervalo[1]
            hms12 = ggplot(thms12) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Média no ENEM 2012", y="Proporção",
                   title="Histograma das Proporções das Médias no ENEM 2012 Segundo o Sexo")
            print(hms12)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thrs12 = readRDS("thrs12.rds")
            k = thrs12$intervalo[2] - thrs12$intervalo[1]
            hrs12 = ggplot(thrs12) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Média da Redação no ENEM 2012", y="Proporção",
                   title="Histograma das Proporções das Médias da Redação no ENEM 2012 Segundo o Sexo")
            print(hrs12)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            thns12 = readRDS("thns12.rds")
            k = thns12$intervalo[2] - thns12$intervalo[1]
            hns12 = ggplot(thns12) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Ciências da Natureza no ENEM 2012", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2012 Segundo o Sexo")+
              theme_bw()
            print(hns12)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thhs12 = readRDS("thhs12.rds")
            k = thhs12$intervalo[2] - thhs12$intervalo[1]
            hhs12 = ggplot(thhs12) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Ciências Humanas no ENEM 2012", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências Humanas no ENEM 2012 Segundo o Sexo")+
              theme_bw()
            print(hhs12)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thls12 = readRDS("thls12.rds")
            k = thls12$intervalo[2] - thls12$intervalo[1]
            hls12 = ggplot(thls12) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Linguagens e Códigos no ENEM 2012", y="Proporção",
                   title="Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2012 Segundo o Sexo")+
              theme_bw()
            print(hls12)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thmts12 = readRDS("thmts12.rds")
            k = thmts12$intervalo[2] - thmts12$intervalo[1]
            hmts12 = ggplot(thmts12) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Matemática no ENEM 2012", y="Proporção",
                   title = "Histograma das Proporções das Notas em Matemática no ENEM 2012 Segundo o Sexo")+
              theme_bw()
            print(hmts12)
          }
        }
      }
    }
    # histogramas por região 2012:####
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thmr12 = readRDS("thmr12.rds")
            k = thmr12$intervalo[2] - thmr12$intervalo[1]
            hmr12 = ggplot(thmr12,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Média no ENEM 2012", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM 2012 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hmr12)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thrr12 = readRDS("thrr12.rds")
            k = thrr12$intervalo[2] - thrr12$intervalo[1]
            hrr12 = ggplot(thrr12,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Média da Redação no ENEM 2012", y="Proporção",
                   title = "Histograma das Proporções das Médias na Redação no ENEM 2012 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hrr12)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thnr12 = readRDS("thnr12.rds")
            k = thnr12$intervalo[2] - thnr12$intervalo[1]
            hnr12 = ggplot(thnr12,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências da natureza no ENEM 2012", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2012 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hnr12)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thhr12 = readRDS("thhr12.rds")
            k = thhr12$intervalo[2] - thhr12$intervalo[1]
            hhr12 = ggplot(thhr12,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências Humanas no ENEM 2012", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências Humanas no ENEM 2012 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hhr12)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thlr12 = readRDS("thlr12.rds")
            k = thlr12$intervalo[2] - thlr12$intervalo[1]
            hlr12 = ggplot(thlr12,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Linguagens e Códigos no ENEM 2012", y="Proporção",
                   title = "Histograma das Proporções das Nota em Linguangens e Códigos no ENEM 2012 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hlr12)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thmtr12 = readRDS("thmtr12.rds")
            k = thmtr12$intervalo[2] - thmtr12$intervalo[1]
            hmtr12 = ggplot(thmtr12,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Matemática no ENEM 2012", y="Proporção",
                   title = "Histograma das Proporções das Notas em Matemática no ENEM 2012 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hmtr12)
          }
        }
      }
    }
    # histogramas por sexo e região 2012:####
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thmsr12 = readRDS("thmsr12.rds")
            k = thmsr12$intervalo[2] - thmsr12$intervalo[1]
            hmsr12 = ggplot(thmsr12) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              labs(x="Média no ENEM 2012", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM 2012 Segundo o Sexo e Região") +
              scale_fill_discrete(name="Legenda") + theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hmsr12)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thrsr12 = readRDS("thrsr12.rds")
            k = thrsr12$intervalo[2] - thrsr12$intervalo[1]
            hrsr12 = ggplot(thrsr12) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              labs(x="Média no ENEM 2012", y="Proporção",
                   title = "Histograma das Proporções das Médias da Redação no ENEM 2012 Segundo o Sexo e Região") +
              scale_fill_discrete(name="Legenda") + theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hrsr12)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thnsr12 = readRDS("thnsr12.rds")
            k = thnsr12$intervalo[2] - thnsr12$intervalo[1]
            hnsr12 = ggplot(thnsr12) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Ciências da Natureza no ENEM 2012", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2012 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hnsr12)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thhsr12 = readRDS("thhsr12.rds")
            k = thhsr12$intervalo[2] - thhsr12$intervalo[1]
            hhsr12 = ggplot(thhsr12) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Ciências Humanas no ENEM 2012", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências Humanas no ENEM 2012 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hhsr12)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thlsr12 = readRDS("thlsr12.rds")
            k = thlsr12$intervalo[2] - thlsr12$intervalo[1]
            hlsr12 = ggplot(thlsr12) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Linguagens e Códigos no ENEM 2012", y="Proporção",
                   title = "Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2012 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hlsr12)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thmtsr12 = readRDS("thmtsr12.rds")
            k = thmtsr12$intervalo[2] - thmtsr12$intervalo[1]
            hmtsr12 = ggplot(thmtsr12) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Matemática no ENEM 2012", y="Proporção",
                   title = "Histograma das Proporções das Notas em Matemática no ENEM 2012 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hmtsr12)
          }
        }
      }
    }
    # histogramas por renda 2012:
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thmd12 = readRDS("thmd12.rds")
            k = thmd12$intervalo[2] - thmd12$intervalo[1]
            hmd12 = ggplot(thmd12) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Média no ENEM 2012", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM 2012 Segundo a Renda") +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 622,00",
                                             "C: De R$ 622,01 até R$ 933,00",
                                             "D: De R$ 933,01 até R$ 1.244,00",
                                             "E: De R$ 1.244,01 até R$ 1.555,00",
                                             "F: De R$ 1.555,01 até R$ 1.866,00",
                                             "G: De R$ 1.866,01 até R$ 2.488,00",
                                             "H: De R$ 2.488,01 até R$ 3.110,00",
                                             "I: De R$ 3.110,01 até R$ 3.732,00",
                                             "J: De R$ 3.732,01 até R$ 4.354,00",
                                             "K: De R$ 4.354,01 até R$ 4.976,00",
                                             "L: De R$ 4.976,01 até R$ 5.598,00",
                                             "M: De R$ 5.598,01 até R$ 6.220,00",
                                             "N: De R$ 6.220,01 até R$ 7.464,00",
                                             "O: De R$ 7.464,01 até R$ 9.330,00",
                                             "P: De R$ 9.330,01 até R$ 12.440,00",
                                             "Q: Mais de 12.440,00"))
            print(hmd12)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thrd12 = readRDS("thrd12.rds")
            k = thrd12$intervalo[2] - thrd12$intervalo[1]
            hrd12 = ggplot(thrd12) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Média da Redação no ENEM 2012", y="Proporção",
                   title="Histograma das Proporções das Médias da Redação no ENEM 2012 Segundo a Renda")+
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 622,00",
                                             "C: De R$ 622,01 até R$ 933,00",
                                             "D: De R$ 933,01 até R$ 1.244,00",
                                             "E: De R$ 1.244,01 até R$ 1.555,00",
                                             "F: De R$ 1.555,01 até R$ 1.866,00",
                                             "G: De R$ 1.866,01 até R$ 2.488,00",
                                             "H: De R$ 2.488,01 até R$ 3.110,00",
                                             "I: De R$ 3.110,01 até R$ 3.732,00",
                                             "J: De R$ 3.732,01 até R$ 4.354,00",
                                             "K: De R$ 4.354,01 até R$ 4.976,00",
                                             "L: De R$ 4.976,01 até R$ 5.598,00",
                                             "M: De R$ 5.598,01 até R$ 6.220,00",
                                             "N: De R$ 6.220,01 até R$ 7.464,00",
                                             "O: De R$ 7.464,01 até R$ 9.330,00",
                                             "P: De R$ 9.330,01 até R$ 12.440,00",
                                             "Q: Mais de 12.440,00"))
            print(hrd12)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thnd12 = readRDS("thnd12.rds")
            k = thnd12$intervalo[2] - thnd12$intervalo[1]
            hnd12 = ggplot(thnd12) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Nota em Ciências da Natureza no ENEM 2012", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2012 Segundo a Renda")+
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 622,00",
                                             "C: De R$ 622,01 até R$ 933,00",
                                             "D: De R$ 933,01 até R$ 1.244,00",
                                             "E: De R$ 1.244,01 até R$ 1.555,00",
                                             "F: De R$ 1.555,01 até R$ 1.866,00",
                                             "G: De R$ 1.866,01 até R$ 2.488,00",
                                             "H: De R$ 2.488,01 até R$ 3.110,00",
                                             "I: De R$ 3.110,01 até R$ 3.732,00",
                                             "J: De R$ 3.732,01 até R$ 4.354,00",
                                             "K: De R$ 4.354,01 até R$ 4.976,00",
                                             "L: De R$ 4.976,01 até R$ 5.598,00",
                                             "M: De R$ 5.598,01 até R$ 6.220,00",
                                             "N: De R$ 6.220,01 até R$ 7.464,00",
                                             "O: De R$ 7.464,01 até R$ 9.330,00",
                                             "P: De R$ 9.330,01 até R$ 12.440,00",
                                             "Q: Mais de 12.440,00"))
            print(hnd12)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thhd12 = readRDS("thhd12.rds")
            k = thhd12$intervalo[2] - thhd12$intervalo[1]
            hhd12 = ggplot(thhd12) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Nota em Ciências Humanas no ENEM 2012", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências Humanas no ENEM 2012 Segundo a Renda")+
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 622,00",
                                             "C: De R$ 622,01 até R$ 933,00",
                                             "D: De R$ 933,01 até R$ 1.244,00",
                                             "E: De R$ 1.244,01 até R$ 1.555,00",
                                             "F: De R$ 1.555,01 até R$ 1.866,00",
                                             "G: De R$ 1.866,01 até R$ 2.488,00",
                                             "H: De R$ 2.488,01 até R$ 3.110,00",
                                             "I: De R$ 3.110,01 até R$ 3.732,00",
                                             "J: De R$ 3.732,01 até R$ 4.354,00",
                                             "K: De R$ 4.354,01 até R$ 4.976,00",
                                             "L: De R$ 4.976,01 até R$ 5.598,00",
                                             "M: De R$ 5.598,01 até R$ 6.220,00",
                                             "N: De R$ 6.220,01 até R$ 7.464,00",
                                             "O: De R$ 7.464,01 até R$ 9.330,00",
                                             "P: De R$ 9.330,01 até R$ 12.440,00",
                                             "Q: Mais de 12.440,00"))
            print(hhd12)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thld12 = readRDS("thld12.rds")
            k = thld12$intervalo[2] - thld12$intervalo[1]
            hld12 = ggplot(thld12) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Nota em Linguagens e Códigos no ENEM 2012", y="Proporção",
                   title="Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2012 Segundo a Renda")+
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 622,00",
                                             "C: De R$ 622,01 até R$ 933,00",
                                             "D: De R$ 933,01 até R$ 1.244,00",
                                             "E: De R$ 1.244,01 até R$ 1.555,00",
                                             "F: De R$ 1.555,01 até R$ 1.866,00",
                                             "G: De R$ 1.866,01 até R$ 2.488,00",
                                             "H: De R$ 2.488,01 até R$ 3.110,00",
                                             "I: De R$ 3.110,01 até R$ 3.732,00",
                                             "J: De R$ 3.732,01 até R$ 4.354,00",
                                             "K: De R$ 4.354,01 até R$ 4.976,00",
                                             "L: De R$ 4.976,01 até R$ 5.598,00",
                                             "M: De R$ 5.598,01 até R$ 6.220,00",
                                             "N: De R$ 6.220,01 até R$ 7.464,00",
                                             "O: De R$ 7.464,01 até R$ 9.330,00",
                                             "P: De R$ 9.330,01 até R$ 12.440,00",
                                             "Q: Mais de 12.440,00"))
            print(hld12)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2012 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thmtd12 = readRDS("thmtd12.rds")
            k = thmtd12$intervalo[2] - thmtd12$intervalo[1]
            hmtd12 = ggplot(thmtd12) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Nota em Matemática no ENEM 2012", y="Proporção",
                   title="Histograma das Proporções das Notas em Matemática no ENEM 2012 Segundo a Renda")+
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 622,00",
                                             "C: De R$ 622,01 até R$ 933,00",
                                             "D: De R$ 933,01 até R$ 1.244,00",
                                             "E: De R$ 1.244,01 até R$ 1.555,00",
                                             "F: De R$ 1.555,01 até R$ 1.866,00",
                                             "G: De R$ 1.866,01 até R$ 2.488,00",
                                             "H: De R$ 2.488,01 até R$ 3.110,00",
                                             "I: De R$ 3.110,01 até R$ 3.732,00",
                                             "J: De R$ 3.732,01 até R$ 4.354,00",
                                             "K: De R$ 4.354,01 até R$ 4.976,00",
                                             "L: De R$ 4.976,01 até R$ 5.598,00",
                                             "M: De R$ 5.598,01 até R$ 6.220,00",
                                             "N: De R$ 6.220,01 até R$ 7.464,00",
                                             "O: De R$ 7.464,01 até R$ 9.330,00",
                                             "P: De R$ 9.330,01 até R$ 12.440,00",
                                             "Q: Mais de 12.440,00"))
            print(hmtd12)
          }
        }
      }
    }

    ## gráficos de 2011
    # boxplots das notas 2011
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebm11 = readRDS("ebm11.rds")
            bm11 = ggplot(ebm11, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.5)) +
              geom_boxplot(stat="identity", fill = "aquamarine4") + geom_point() + coord_flip() +
              labs(x="Brasil",y="Média no ENEM 2011 (Brasil)",
                   title = "Boxplot das Médias no ENEM 2011") + theme_bw() +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(bm11)

          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebr11 = readRDS("ebr11.rds")
            br11 = ggplot(ebr11, aes(x = "x", middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              theme_bw() + coord_flip() +
              labs(x="Brasil",y="Média da Redação no ENEM 2011",
                   title = "Boxplot das Médias da Redação no ENEM 2011") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(br11)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebn11 = readRDS("ebn11.rds")
            bn11 = ggplot(ebn11, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw() +
              theme(legend.position="none",axis.text.y=element_text(colour="white")) +
              labs(x="Brasil",y="Nota em Ciências da Natureza no ENEM 2011",
                   title = "Boxplot das Notas em Ciências da Natureza no ENEM 2011")
            print(bn11)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebh11 = readRDS("ebh11.rds")
            bh11 = ggplot(ebh11, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw() +
              labs(x="Brasil",y="Nota em Ciências Humanas no ENEM 2011",
                   title = "Boxplot das Notas em Ciências Humanas no ENEM 2011") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(bh11)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebl11 = readRDS("ebl11.rds")
            bl11 = ggplot(ebl11, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw() +
              labs(x="Brasil",y="Nota em Linguagens e Códigos no ENEM 2011",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM 2011") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(bl11)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebmt11 = readRDS("ebmt11.rds")
            bmt11 = ggplot(ebmt11, aes(x = "x", y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       aplha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw() +
              labs(x="Brasil",y="Nota em Matemática no ENEM 2011",
                   title = "Boxplot das Notas em Matemática no ENEM 2011") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(bmt11)
          }
        }
      }
    }
    # boxplots por Sexo 2011:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebms11 = readRDS("ebms11.rds")
            bms11 = ggplot(ebms11, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Sexo",y="Média no ENEM 2011",
                   title = "Boxplot das Médias no ENEM 2011 Segundo o Sexo") +
              theme(legend.position="none")
            print(bms11)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebrs11 = readRDS("ebrs11.rds")
            brs11 = ggplot(ebrs11, aes(x = sexo, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + theme_bw() + coord_flip() +
              labs(x="Sexo",y="Média da Redação no ENEM 2011",
                   title="Boxplot das Médias da Redação no ENEM 2011 Segundo o Sexo")+
              theme(legend.position="none")
            print(brs11)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebns11 = readRDS("ebns11.rds")
            bns11 = ggplot(ebns11, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Sexo",y="Nota em Ciências da Natureza no ENEM 2011",
                   title="Boxplot das Notas em Ciências da Natureza no ENEM 2011 Segundo o Sexo")+
              theme(legend.position="none")
            print(bns11)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebhs11 = readRDS("ebhs11.rds")
            bhs11 = ggplot(ebhs11, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Sexo",y="Nota em Ciências Humanas no ENEM 2011",
                   title="Boxplot das Notas em Ciências Humanas no ENEM 2011 Segundo o Sexo")+
              theme(legend.position="none")
            print(bhs11)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebls11 = readRDS("ebls11.rds")
            bls11 = ggplot(ebls11, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Nota em Linguagens e Códigos no ENEM 2011",
                   title="Boxplot das Notas em Linguagens e Códigos no ENEM 2011 Segundo o Sexo")+
              theme_bw() + theme(legend.position="none")
            print(bls11)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebmts11 = readRDS("ebmts11.rds")
            bmts11 = ggplot(ebmts11, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Nota em Matemática",
                   title = "Boxplot das Notas em Matemática no ENEM 2011 Segundo o Sexo")+
              theme_bw() + theme(legend.position="none")
            print(bmts11)
          }
        }
      }
    }
    # boxplots por região 2011:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebmr11 = readRDS("ebmr11.rds")
            bmr11 = ggplot(ebmr11, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Média no ENEM 2011",
                   title="Boxplot das Médias no ENEM 2011 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(bmr11)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebrr11 = readRDS("ebrr11.rds")
            brr11 = ggplot(ebrr11, aes(x = reg, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + coord_flip() +
              labs(x="Região",y="Média da Redação no ENEM 2011",
                   title="Boxplot das Médias da Redação no ENEM 2011 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(brr11)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebnr11 = readRDS("ebnr11.rds")
            bnr11 = ggplot(ebnr11, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Ciências da Natureza no ENEM 2011",
                   title = "Boxplot das Notas em Ciências da Natureza no ENEM 2011 Segundo a Região") +
              theme_bw() + theme(legend.position="none")
            print(bnr11)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebhr11 = readRDS("ebhr11.rds")
            bhr11 = ggplot(ebhr11, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Ciências Humanas no ENEM 2011",
                   title="Boxplot das Notas em Ciências Humanas no ENEM 2011 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(bhr11)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            eblr11 = readRDS("eblr11.rds")
            blr11 = ggplot(eblr11, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Linguagens e Códigos no ENEM 2011",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM 2011 Segundo a Região") +
              theme_bw() + theme(legend.position="none")
            print(blr11)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebmtr11 = readRDS("ebmtr11.rds")
            bmtr11 = ggplot(ebmtr11, aes(x = reg, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Matemática no ENEM 2011",
                   title = "Boxplot das Notas em Matemática no ENEM 2011 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(bmtr11)
          }
        }
      }
    }
    # boxplots por Sexo e região 2011:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebmsr11 = readRDS("ebmsr11.rds")
            bmsr11 = ggplot(ebmsr11, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Média no ENEM 2011",
                   title="Boxplot das Médias no ENEM 2011 Segundo o Sexo e Região")+
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bmsr11)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebrsr11 = readRDS("ebrsr11.rds")
            brsr11 = ggplot(ebrsr11, aes(x = sexo, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + coord_flip() +
              labs(x="",y="Nota da Média da Redação no ENEM 2011",
                   title="Boxplot das Médias da Redação no ENEM 2011 Segundo o Sexo e Região")+
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(brsr11)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebnsr11 = readRDS("ebnsr11.rds")
            bnsr11 = ggplot(ebnsr11, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Nota em Ciências da Natureza no ENEM 2011",
                   title = "Boxplot das Notas em Ciências da Natureza no ENEM 2011 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bnsr11)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebhsr11 = readRDS("ebhsr11.rds")
            bhsr11 = ggplot(ebhsr11, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Nota em Ciências Humanas no ENEM 2011",
                   title = "Boxplot das Notas em Ciências Humanas no ENEM 2011 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bhsr11)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            eblsr11 = readRDS("eblsr11.rds")
            blsr11 =  ggplot(eblsr11, aes(x = sexo, y = out, middle = median,
                                          ymin = lower.whisker, ymax = upper.whisker,
                                          lower = lower.hinge, upper = upper.hinge,
                                          fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Nota em Linguagens e Códigos no ENEM 2011",
                   title = "Boxplot da Nota em Linguagens e Códigos no ENEM 2011 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(blsr11)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebmtsr11 = readRDS("ebmtsr11.rds")
            bmtsr11 = ggplot(ebmtsr11, aes(x = sexo, y = out, middle = median,
                                           ymin = lower.whisker, ymax = upper.whisker,
                                           lower = lower.hinge, upper = upper.hinge,
                                           fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +

              labs(x="",y="Nota em Matemática no ENEM 2011",
                   title = "Boxplot da Nota em Matemática no ENEM 2011 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bmtsr11)
          }
        }
      }
    }
    # boxplots por renda 2011:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebmd11 = readRDS("ebmd11.rds")
            bmd11 = ggplot(ebmd11, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Média no ENEM 2011",
                   title = "Boxplot das Médias no ENEM 2011 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 545,00",
                                             "C: De R$ 545,01 até R$ 817,50",
                                             "D: De R$ 817,01 até R$ 1.090,00",
                                             "E: De R$ 1.090,01 até R$ 2.725,00",
                                             "F: De R$ 2.725,01 até R$ 3.815,00",
                                             "G: De R$ 3.815,01 até R$ 5.450,00",
                                             "H: De R$ 5.450,01 até R$ 6.540,00",
                                             "I: De R$ 6.540,01 até R$ 8.175,00",
                                             "J: De R$ 8.175,01 até R$ 16.350,00",
                                             "K: Mais de 16.350,00"))
            print(bmd11)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebrd11 = readRDS("ebrd11.rds")
            brd11 = ggplot(ebrd11, aes(x = renda, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha = 0.5) +
              labs(x="Renda",y="Média da Redação no ENEM 2011",
                   title="Boxplot das Médias da Redação no ENEM 2011 Segundo a Renda")+
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') + coord_flip() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 545,00",
                                             "C: De R$ 545,01 até R$ 817,50",
                                             "D: De R$ 817,01 até R$ 1.090,00",
                                             "E: De R$ 1.090,01 até R$ 2.725,00",
                                             "F: De R$ 2.725,01 até R$ 3.815,00",
                                             "G: De R$ 3.815,01 até R$ 5.450,00",
                                             "H: De R$ 5.450,01 até R$ 6.540,00",
                                             "I: De R$ 6.540,01 até R$ 8.175,00",
                                             "J: De R$ 8.175,01 até R$ 16.350,00",
                                             "K: Mais de 16.350,00"))

            print(brd11)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebnd11 = readRDS("ebnd11.rds")
            bnd11 = ggplot(ebnd11, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Ciências da Natureza no ENEM 2011",
                   title = "Boxplot das Notas em Ciências da Natureza no ENEM 2011 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 545,00",
                                             "C: De R$ 545,01 até R$ 817,50",
                                             "D: De R$ 817,01 até R$ 1.090,00",
                                             "E: De R$ 1.090,01 até R$ 2.725,00",
                                             "F: De R$ 2.725,01 até R$ 3.815,00",
                                             "G: De R$ 3.815,01 até R$ 5.450,00",
                                             "H: De R$ 5.450,01 até R$ 6.540,00",
                                             "I: De R$ 6.540,01 até R$ 8.175,00",
                                             "J: De R$ 8.175,01 até R$ 16.350,00",
                                             "K: Mais de 16.350,00"))
            print(bnd11)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebhd11 = readRDS("ebhd11.rds")
            bhd11 = ggplot(ebhd11, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Ciências Humanas no ENEM 2011",
                   title = "Boxplot das Notas em Ciências Humanas no ENEM 2011 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 545,00",
                                             "C: De R$ 545,01 até R$ 817,50",
                                             "D: De R$ 817,01 até R$ 1.090,00",
                                             "E: De R$ 1.090,01 até R$ 2.725,00",
                                             "F: De R$ 2.725,01 até R$ 3.815,00",
                                             "G: De R$ 3.815,01 até R$ 5.450,00",
                                             "H: De R$ 5.450,01 até R$ 6.540,00",
                                             "I: De R$ 6.540,01 até R$ 8.175,00",
                                             "J: De R$ 8.175,01 até R$ 16.350,00",
                                             "K: Mais de 16.350,00"))
            print(bhd11)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebld11 = readRDS("ebld11.rds")
            bld11 = ggplot(ebld11, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Linguagens e Códigos no ENEM 2011",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM 2011 Segundo a Renda") +
              theme_bw() + scale_alpha(guide = 'none') +
              theme(legend.position="right") +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 545,00",
                                             "C: De R$ 545,01 até R$ 817,50",
                                             "D: De R$ 817,01 até R$ 1.090,00",
                                             "E: De R$ 1.090,01 até R$ 2.725,00",
                                             "F: De R$ 2.725,01 até R$ 3.815,00",
                                             "G: De R$ 3.815,01 até R$ 5.450,00",
                                             "H: De R$ 5.450,01 até R$ 6.540,00",
                                             "I: De R$ 6.540,01 até R$ 8.175,00",
                                             "J: De R$ 8.175,01 até R$ 16.350,00",
                                             "K: Mais de 16.350,00"))
            print(bld11)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebmtd11 = readRDS("ebmtd11.rds")
            bmtd11 = ggplot(ebmtd11, aes(x = renda, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.85) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Matemática no ENEM 2011",
                   title = "Boxplot das Notas em Matemática no ENEM 2011 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 545,00",
                                             "C: De R$ 545,01 até R$ 817,50",
                                             "D: De R$ 817,01 até R$ 1.090,00",
                                             "E: De R$ 1.090,01 até R$ 2.725,00",
                                             "F: De R$ 2.725,01 até R$ 3.815,00",
                                             "G: De R$ 3.815,01 até R$ 5.450,00",
                                             "H: De R$ 5.450,01 até R$ 6.540,00",
                                             "I: De R$ 6.540,01 até R$ 8.175,00",
                                             "J: De R$ 8.175,01 até R$ 16.350,00",
                                             "K: Mais de 16.350,00"))
            print(bmtd11)
          }
        }
      }
    }
    # histogramas das notas 2011:
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thm11 = readRDS("thm11.rds")
            k = thm11$intervalo[2] - thm11$intervalo[1]
            hm11 = ggplot(thm11, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Média no ENEM 2011", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM 2011") +
              theme_bw()
            print(hm11)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thr11 = readRDS("thr11.rds")
            k = thr11$intervalo[2] - thr11$intervalo[1]
            hr11 = ggplot(thr11, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota da Redação no ENEM 2011", y="Proporção",
                   title = "Histograma das Proporções das Notas da Redação no ENEM 2011")+
              theme_bw()
            print(hr11)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thn11 = readRDS("thn11.rds")
            k = thn11$intervalo[2] - thn11$intervalo[1]
            hn11 = ggplot(thn11, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências da Natureza", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2011")+
              theme_bw()
            print(hn11)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thh11 = readRDS("thh11.rds")
            k = thh11$intervalo[2] - thh11$intervalo[1]
            hh11 = ggplot(thh11, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências Humanas", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências Humanas no ENEM 2011")+
              theme_bw()
            print(hh11)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thl11 = readRDS("thl11.rds")
            k = thl11$intervalo[2] - thl11$intervalo[1]
            hl11 = ggplot(thl11, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Linguagens e Códigos", y="Proporção",
                   title="Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2011")+
              theme_bw()
            print(hl11)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thmt11 = readRDS("thmt11.rds")
            k = thmt11$intervalo[2] - thmt11$intervalo[1]
            hmt11 = ggplot(thmt11, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Matemática", y="Proporção",
                   title="Histograma das Proporções das Notas em Matemática no ENEM 2011")+
              theme_bw()
            print(hmt11)
          }
        }
      }
    }
    # histogramas por Sexo 2011:####
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thms11 = readRDS("thms11.rds")
            k = thms11$intervalo[2] - thms11$intervalo[1]
            hms11 = ggplot(thms11) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Média no ENEM 2011", y="Proporção",
                   title="Histograma das Proporções das Médias no ENEM 2011 Segundo o Sexo")
            print(hms11)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thrs11 = readRDS("thrs11.rds")
            k = thrs11$intervalo[2] - thrs11$intervalo[1]
            hrs11 = ggplot(thrs11) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Média da Redação no ENEM 2011", y="Proporção",
                   title="Histograma das Proporções das Médias da Redação no ENEM 2011 Segundo o Sexo")
            print(hrs11)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            thns11 = readRDS("thns11.rds")
            k = thns11$intervalo[2] - thns11$intervalo[1]
            hns11 = ggplot(thns11) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Ciências da Natureza no ENEM 2011", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2011 Segundo o Sexo")+
              theme_bw()
            print(hns11)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thhs11 = readRDS("thhs11.rds")
            k = thhs11$intervalo[2] - thhs11$intervalo[1]
            hhs11 = ggplot(thhs11) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Ciências Humanas no ENEM 2011", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências Humanas no ENEM 2011 Segundo o Sexo")+
              theme_bw()
            print(hhs11)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thls11 = readRDS("thls11.rds")
            k = thls11$intervalo[2] - thls11$intervalo[1]
            hls11 = ggplot(thls11) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Linguagens e Códigos no ENEM 2011", y="Proporção",
                   title="Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2011 Segundo o Sexo")+
              theme_bw()
            print(hls11)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thmts11 = readRDS("thmts11.rds")
            k = thmts11$intervalo[2] - thmts11$intervalo[1]
            hmts11 = ggplot(thmts11) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Matemática no ENEM 2011", y="Proporção",
                   title = "Histograma das Proporções das Notas em Matemática no ENEM 2011 Segundo o Sexo")+
              theme_bw()
            print(hmts11)
          }
        }
      }
    }
    # histogramas por região 2011:####
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thmr11 = readRDS("thmr11.rds")
            k = thmr11$intervalo[2] - thmr11$intervalo[1]
            hmr11 = ggplot(thmr11,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Média no ENEM 2011", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM 2011 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hmr11)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thrr11 = readRDS("thrr11.rds")
            k = thrr11$intervalo[2] - thrr11$intervalo[1]
            hrr11 = ggplot(thrr11,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Média da Redação no ENEM 2011", y="Proporção",
                   title = "Histograma das Proporções das Médias na Redação no ENEM 2011 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hrr11)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thnr11 = readRDS("thnr11.rds")
            k = thnr11$intervalo[2] - thnr11$intervalo[1]
            hnr11 = ggplot(thnr11,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências da natureza no ENEM 2011", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2011 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hnr11)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thhr11 = readRDS("thhr11.rds")
            k = thhr11$intervalo[2] - thhr11$intervalo[1]
            hhr11 = ggplot(thhr11,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências Humanas no ENEM 2011", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências Humanas no ENEM 2011 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hhr11)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thlr11 = readRDS("thlr11.rds")
            k = thlr11$intervalo[2] - thlr11$intervalo[1]
            hlr11 = ggplot(thlr11,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Linguagens e Códigos no ENEM 2011", y="Proporção",
                   title = "Histograma das Proporções das Nota em Linguangens e Códigos no ENEM 2011 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hlr11)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thmtr11 = readRDS("thmtr11.rds")
            k = thmtr11$intervalo[2] - thmtr11$intervalo[1]
            hmtr11 = ggplot(thmtr11,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Matemática no ENEM 2011", y="Proporção",
                   title = "Histograma das Proporções das Notas em Matemática no ENEM 2011 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hmtr11)
          }
        }
      }
    }
    # histogramas por sexo e região 2011:####
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thmsr11 = readRDS("thmsr11.rds")
            k = thmsr11$intervalo[2] - thmsr11$intervalo[1]
            hmsr11 = ggplot(thmsr11) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") + 
              labs(x="Média no ENEM 2011", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM 2011 Segundo o Sexo e Região") +
              facet_wrap(~reg, nrow = 1) + theme_bw()
            print(hmsr11)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thrsr11 = readRDS("thrsr11.rds")
            k = thrsr11$intervalo[2] - thrsr11$intervalo[1]
            hrsr11 = ggplot(thrsr11) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") + 
              labs(x="Média da Redação no ENEM 2011", y="Proporção",
                   title = "Histograma das Proporções das Médias na Redação no ENEM 2011 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hrsr11)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thnsr11 = readRDS("thnsr11.rds")
            k = thnsr11$intervalo[2] - thnsr11$intervalo[1]
            hnsr11 = ggplot(thnsr11) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") + 
              labs(x="Nota em Ciências da Natureza no ENEM 2011", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2011 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hnsr11)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thhsr11 = readRDS("thhsr11.rds")
            k = thhsr11$intervalo[2] - thhsr11$intervalo[1]
            hhsr11 = ggplot(thhsr11) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") + 
              labs(x="Nota em Ciências Humanas no ENEM 2011", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências Humanas no ENEM 2011 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hhsr11)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thlsr11 = readRDS("thlsr11.rds")
            k = thlsr11$intervalo[2] - thlsr11$intervalo[1]
            hlsr11 = ggplot(thlsr11) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") + 
              labs(x="Nota em Linguagens e Códigos no ENEM 2011", y="Proporção",
                   title = "Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2011 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hlsr11)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thmtsr11 = readRDS("thmtsr11.rds")
            k = thmtsr11$intervalo[2] - thmtsr11$intervalo[1]
            hmtsr11 = ggplot(thmtsr11) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") + 
              labs(x="Nota em Matemática no ENEM 2011", y="Proporção",
                   title = "Histograma das Proporções das Notas em Matemática no ENEM 2011 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hmtsr11)
          }
        }
      }
    }
    # histogramas por renda 2011:
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thmd11 = readRDS("thmd11.rds")
            k = thmd11$intervalo[2] - thmd11$intervalo[1]
            hmd11 = ggplot(thmd11) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 545,00",
                                             "C: De R$ 545,01 até R$ 817,50",
                                             "D: De R$ 817,01 até R$ 1.090,00",
                                             "E: De R$ 1.090,01 até R$ 2.725,00",
                                             "F: De R$ 2.725,01 até R$ 3.815,00",
                                             "G: De R$ 3.815,01 até R$ 5.450,00",
                                             "H: De R$ 5.450,01 até R$ 6.540,00",
                                             "I: De R$ 6.540,01 até R$ 8.175,00",
                                             "J: De R$ 8.175,01 até R$ 16.350,00",
                                             "K: Mais de 16.350,00")) +
            labs(x="Média no ENEM 2011", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM 2011 Segundo a Renda")
            print(hmd11)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thrd11 = readRDS("thrd11.rds")
            k = thrd11$intervalo[2] - thrd11$intervalo[1]
            hrd11 = ggplot(thrd11) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 545,00",
                                             "C: De R$ 545,01 até R$ 817,50",
                                             "D: De R$ 817,01 até R$ 1.090,00",
                                             "E: De R$ 1.090,01 até R$ 2.725,00",
                                             "F: De R$ 2.725,01 até R$ 3.815,00",
                                             "G: De R$ 3.815,01 até R$ 5.450,00",
                                             "H: De R$ 5.450,01 até R$ 6.540,00",
                                             "I: De R$ 6.540,01 até R$ 8.175,00",
                                             "J: De R$ 8.175,01 até R$ 16.350,00",
                                             "K: Mais de 16.350,00")) +
              labs(x="Média da Redação no ENEM 2011", y="Proporção",
                   title="Histograma das Proporções das Médias da Redação no ENEM 2011 Segundo a Renda")+
              theme_bw() 
            print(hrd11)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thnd11 = readRDS("thnd11.rds")
            k = thnd11$intervalo[2] - thnd11$intervalo[1]
            hnd11 = ggplot(thnd11) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 545,00",
                                             "C: De R$ 545,01 até R$ 817,50",
                                             "D: De R$ 817,01 até R$ 1.090,00",
                                             "E: De R$ 1.090,01 até R$ 2.725,00",
                                             "F: De R$ 2.725,01 até R$ 3.815,00",
                                             "G: De R$ 3.815,01 até R$ 5.450,00",
                                             "H: De R$ 5.450,01 até R$ 6.540,00",
                                             "I: De R$ 6.540,01 até R$ 8.175,00",
                                             "J: De R$ 8.175,01 até R$ 16.350,00",
                                             "K: Mais de 16.350,00")) +
              labs(x="Nota em Ciências da Natureza no ENEM 2011", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2011 Segundo a Renda")
            print(hnd11)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thhd11 = readRDS("thhd11.rds")
            k = thhd11$intervalo[2] - thhd11$intervalo[1]
            hhd11 = ggplot(thhd11) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 545,00",
                                             "C: De R$ 545,01 até R$ 817,50",
                                             "D: De R$ 817,01 até R$ 1.090,00",
                                             "E: De R$ 1.090,01 até R$ 2.725,00",
                                             "F: De R$ 2.725,01 até R$ 3.815,00",
                                             "G: De R$ 3.815,01 até R$ 5.450,00",
                                             "H: De R$ 5.450,01 até R$ 6.540,00",
                                             "I: De R$ 6.540,01 até R$ 8.175,00",
                                             "J: De R$ 8.175,01 até R$ 16.350,00",
                                             "K: Mais de 16.350,00"))
            print(hhd11)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thld11 = readRDS("thld11.rds")
            k = thld11$intervalo[2] - thld11$intervalo[1]
            hld11 = ggplot(thld11) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 545,00",
                                             "C: De R$ 545,01 até R$ 817,50",
                                             "D: De R$ 817,01 até R$ 1.090,00",
                                             "E: De R$ 1.090,01 até R$ 2.725,00",
                                             "F: De R$ 2.725,01 até R$ 3.815,00",
                                             "G: De R$ 3.815,01 até R$ 5.450,00",
                                             "H: De R$ 5.450,01 até R$ 6.540,00",
                                             "I: De R$ 6.540,01 até R$ 8.175,00",
                                             "J: De R$ 8.175,01 até R$ 16.350,00",
                                             "K: Mais de 16.350,00")) +
              theme_bw() +
              labs(x="Nota em Linguagens e Códigos no ENEM 2011", y="Proporção",
                   title="Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2011 Segundo a Renda")+
            print(hld11)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2011 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thmtd11 = readRDS("thmtd11.rds")
            k = thmtd11$intervalo[2] - thmtd11$intervalo[1]
            hmtd11 = ggplot(thmtd11) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=k, stat="identity", alpha = 0.65) +
              labs(x="Nota em Matemática no ENEM 2011", y="Proporção",
                   title="Histograma das Proporções das Notas em Matemática no ENEM 2011 Segundo a Renda")+
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: Nenhuma Renda",
                                             "B: até R$ 788,00",
                                             "C: De R$ 788,01 até R$ 1.182,00",
                                             "D: De R$ 1.182,01 até R$ 1.572,00",
                                             "E: De R$ 1.572,01 até R$ 1.970,00",
                                             "F: De R$ 1.970,01 até R$ 2.364,00",
                                             "G: De R$ 2.364,01 até R$ 3.152,00",
                                             "H: De R$ 3.152,01 até R$ 3.940,00",
                                             "I: De R$ 3.940,01 até R$ 4.728,00",
                                             "J: De R$ 4.728,01 até R$ 5.516,00",
                                             "K: De R$ 5.516,01 até R$ 6.304,00",
                                             "L: De R$ 6.304,01 até R$ 7.092,00",
                                             "M: De R$ 7.092,01 até R$ 7.880,00",
                                             "N: De R$ 7.880,01 até R$ 9.456,00",
                                             "O: De R$ 9.456,01 até R$ 11.820,00",
                                             "P: De R$ 11.820,01 até R$ 15.760,00",
                                             "Q: Mais de 15.760,00"))
            print(hmtd11)
          }
        }
      }
    }

    ## gráficos de 2010
    # boxplots das notas 2010
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebm10 = readRDS("ebm10.rds")
            bm10 = ggplot(ebm10, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.5)) +
              geom_boxplot(stat="identity", fill = "aquamarine4") + geom_point() + coord_flip() +
              labs(x="Brasil",y="Média no ENEM 2010 (Brasil)",
                   title = "Boxplot das Médias no ENEM 2010") + theme_bw() +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(bm10)

          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebr10 = readRDS("ebr10.rds")
            br10 = ggplot(ebr10, aes(x = "x", middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              theme_bw() + coord_flip() +
              labs(x="Brasil",y="Média da Redação no ENEM 2010",
                   title = "Boxplot das Médias da Redação no ENEM 2010") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(br10)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebn10 = readRDS("ebn10.rds")
            bn10 = ggplot(ebn10, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw() +
              theme(legend.position="none",axis.text.y=element_text(colour="white")) +
              labs(x="Brasil",y="Nota em Ciências da Natureza no ENEM 2010",
                   title = "Boxplot das Notas em Ciências da Natureza no ENEM 2010")
            print(bn10)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebh10 = readRDS("ebh10.rds")
            bh10 = ggplot(ebh10, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     alpha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw() +
              labs(x="Brasil",y="Nota em Ciências Humanas no ENEM 2010",
                   title = "Boxplot das Notas em Ciências Humanas no ENEM 2010") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(bh10)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebl10 = readRDS("ebl10.rds")
            bl10 = ggplot(ebl10, aes(x = "x", y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw() +
              labs(x="Brasil",y="Nota em Linguagens e Códigos no ENEM 2010",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM 2010") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(bl10)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebmt10 = readRDS("ebmt10.rds")
            bmt10 = ggplot(ebmt10, aes(x = "x", y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       aplha = 0.85)) +
              geom_boxplot(stat = "identity", fill = "aquamarine4") +
              geom_point() + coord_flip() + theme_bw() +
              labs(x="Brasil",y="Nota em Matemática no ENEM 2010",
                   title = "Boxplot das Notas em Matemática no ENEM 2010") +
              theme(legend.position="none",axis.text.y=element_text(colour="white"))
            print(bmt10)
          }
        }
      }
    }
    # boxplots por Sexo 2010:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebms10 = readRDS("ebms10.rds")
            bms10 = ggplot(ebms10, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Sexo",y="Média no ENEM 2010",
                   title = "Boxplot das Médias no ENEM 2010 Segundo o Sexo") +
              theme(legend.position="none")
            print(bms10)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebrs10 = readRDS("ebrs10.rds")
            brs10 = ggplot(ebrs10, aes(x = sexo, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + theme_bw() + coord_flip() +
              labs(x="Sexo",y="Média da Redação no ENEM 2010",
                   title="Boxplot das Médias da Redação no ENEM 2010 Segundo o Sexo")+
              theme(legend.position="none")
            print(brs10)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebns10 = readRDS("ebns10.rds")
            bns10 = ggplot(ebns10, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Sexo",y="Nota em Ciências da Natureza no ENEM 2010",
                   title="Boxplot das Notas em Ciências da Natureza no ENEM 2010 Segundo o Sexo")+
              theme(legend.position="none")
            print(bns10)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebhs10 = readRDS("ebhs10.rds")
            bhs10 = ggplot(ebhs10, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Sexo",y="Nota em Ciências Humanas no ENEM 2010",
                   title="Boxplot das Notas em Ciências Humanas no ENEM 2010 Segundo o Sexo")+
              theme(legend.position="none")
            print(bhs10)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebls10 = readRDS("ebls10.rds")
            bls10 = ggplot(ebls10, aes(x = sexo, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Nota em Linguagens e Códigos no ENEM 2010",
                   title="Boxplot das Notas em Linguagens e Códigos no ENEM 2010 Segundo o Sexo")+
              theme_bw() + theme(legend.position="none")
            print(bls10)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebmts10 = readRDS("ebmts10.rds")
            bmts10 = ggplot(ebmts10, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Nota em Matemática",
                   title = "Boxplot das Notas em Matemática no ENEM 2010 Segundo o Sexo")+
              theme_bw() + theme(legend.position="none")
            print(bmts10)
          }
        }
      }
    }
    # boxplots por região 2010:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebmr10 = readRDS("ebmr10.rds")
            bmr10 = ggplot(ebmr10, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Média no ENEM 2010",
                   title="Boxplot das Médias no ENEM 2010 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(bmr10)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebrr10 = readRDS("ebrr10.rds")
            brr10 = ggplot(ebrr10, aes(x = reg, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + coord_flip() +
              labs(x="Região",y="Média da Redação no ENEM 2010",
                   title="Boxplot das Médias da Redação no ENEM 2010 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(brr10)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebnr10 = readRDS("ebnr10.rds")
            bnr10 = ggplot(ebnr10, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Ciências da Natureza no ENEM 2010",
                   title = "Boxplot das Notas em Ciências da Natureza no ENEM 2010 Segundo a Região") +
              theme_bw() + theme(legend.position="none")
            print(bnr10)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebhr10 = readRDS("ebhr10.rds")
            bhr10 = ggplot(ebhr10, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Ciências Humanas no ENEM 2010",
                   title="Boxplot das Notas em Ciências Humanas no ENEM 2010 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(bhr10)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            eblr10 = readRDS("eblr10.rds")
            blr10 = ggplot(eblr10, aes(x = reg, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Linguagens e Códigos no ENEM 2010",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM 2010 Segundo a Região") +
              theme_bw() + theme(legend.position="none")
            print(blr10)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebmtr10 = readRDS("ebmtr10.rds")
            bmtr10 = ggplot(ebmtr10, aes(x = reg, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Matemática no ENEM 2010",
                   title = "Boxplot das Notas em Matemática no ENEM 2010 Segundo a Região")+
              theme_bw() + theme(legend.position="none")
            print(bmtr10)
          }
        }
      }
    }
    # boxplots por Sexo e região 2010:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebmsr10 = readRDS("ebmsr10.rds")
            bmsr10 = ggplot(ebmsr10, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Média no ENEM 2010",
                   title="Boxplot das Médias no ENEM 2010 Segundo o Sexo e Região")+
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bmsr10)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebrsr10 = readRDS("ebrsr10.rds")
            brsr10 = ggplot(ebrsr10, aes(x = sexo, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + coord_flip() +
              labs(x="",y="Nota da Média da Redação no ENEM 2010",
                   title="Boxplot das Médias da Redação no ENEM 2010 Segundo o Sexo e Região")+
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(brsr10)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebnsr10 = readRDS("ebnsr10.rds")
            bnsr10 = ggplot(ebnsr10, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Nota em Ciências Humanas no ENEM 2010",
                   title = "Boxplot das Notas em Ciências Humanas no ENEM 2010 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bnsr10)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebhsr10 = readRDS("ebhsr10.rds")
            bhsr10 = ggplot(ebhsr10, aes(x = sexo, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Nota em Linguagens e Códigos no ENEM 2010",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM 2010 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bhsr10)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            eblsr10 = readRDS("eblsr10.rds")
            blsr10 =  ggplot(eblsr10, aes(x = sexo, y = out, middle = median,
                                          ymin = lower.whisker, ymax = upper.whisker,
                                          lower = lower.hinge, upper = upper.hinge,
                                          fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="",y="Nota em Linguagens e Códigos no ENEM 2010",
                   title = "Boxplot da Nota em Linguagens e Códigos no ENEM 2010 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(blsr10)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebmtsr10 = readRDS("ebmtsr10.rds")
            bmtsr10 = ggplot(ebmtsr10, aes(x = sexo, y = out, middle = median,
                                           ymin = lower.whisker, ymax = upper.whisker,
                                           lower = lower.hinge, upper = upper.hinge,
                                           fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +

              labs(x="",y="Nota em Matemática no ENEM 2010",
                   title = "Boxplot da Nota em Matemática no ENEM 2010 Segundo o Sexo e Região") +
              theme_bw() + theme(legend.position="none") + facet_grid(~reg)
            print(bmtsr10)
          }
        }
      }
    }
    # boxplots por renda 2010:
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebmd10 = readRDS("ebmd10.rds")
            bmd10 = ggplot(ebmd10, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Média no ENEM 2010",
                   title = "Boxplot das Médias no ENEM 2010 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: até R$ 510,00",
                                             "B: De R$ 510,01 até R$ 1.530,00",
                                             "C: De R$ 1.530,01 até R$ 3.060,00",
                                             "D: De R$ 3.060,01 até R$ 4.590,00",
                                             "E: De R$ 4.590,01 até R$ 6.120,00",
                                             "F: De R$ 6.120,01 até R$ 7.650,00",
                                             "G: Mais de R$ 7.650,00 ",
                                             "H: Nenhuma Renda"))
            print(bmd10)  }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebrd10 = readRDS("ebrd10.rds")
            brd10 = ggplot(ebrd10, aes(x = renda, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha = 0.5) +
              labs(x="Renda",y="Média da Redação no ENEM 2010",
                   title="Boxplot das Médias da Redação no ENEM 2010 Segundo a Renda")+
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') + coord_flip() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: até R$ 510,00",
                                             "B: De R$ 510,01 até R$ 1.530,00",
                                             "C: De R$ 1.530,01 até R$ 3.060,00",
                                             "D: De R$ 3.060,01 até R$ 4.590,00",
                                             "E: De R$ 4.590,01 até R$ 6.120,00",
                                             "F: De R$ 6.120,01 até R$ 7.650,00",
                                             "G: Mais de R$ 7.650,00 ",
                                             "H: Nenhuma Renda"))
            print(brd10)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebnd10 = readRDS("ebnd10.rds")
            bnd10 = ggplot(ebnd10, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Ciências da Natureza no ENEM 2010",
                   title = "Boxplot das Notas em Ciências da Natureza no ENEM 2010 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: até R$ 510,00",
                                             "B: De R$ 510,01 até R$ 1.530,00",
                                             "C: De R$ 1.530,01 até R$ 3.060,00",
                                             "D: De R$ 3.060,01 até R$ 4.590,00",
                                             "E: De R$ 4.590,01 até R$ 6.120,00",
                                             "F: De R$ 6.120,01 até R$ 7.650,00",
                                             "G: Mais de R$ 7.650,00 ",
                                             "H: Nenhuma Renda"))
            print(bnd10)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebhd10 = readRDS("ebhd10.rds")
            bhd10 = ggplot(ebhd10, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Ciências Humanas no ENEM 2010",
                   title = "Boxplot das Notas em Ciências Humanas no ENEM 2010 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: até R$ 510,00",
                                             "B: De R$ 510,01 até R$ 1.530,00",
                                             "C: De R$ 1.530,01 até R$ 3.060,00",
                                             "D: De R$ 3.060,01 até R$ 4.590,00",
                                             "E: De R$ 4.590,01 até R$ 6.120,00",
                                             "F: De R$ 6.120,01 até R$ 7.650,00",
                                             "G: Mais de R$ 7.650,00 ",
                                             "H: Nenhuma Renda"))
            print(bhd10)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebld10 = readRDS("ebld10.rds")
            bld10 = ggplot(ebld10, aes(x = renda, y = out, middle = median,
                                       ymin = lower.whisker, ymax = upper.whisker,
                                       lower = lower.hinge, upper = upper.hinge,
                                       fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.5) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Linguagens e Códigos no ENEM 2010",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM 2010 Segundo a Renda") +
              theme_bw() + scale_alpha(guide = 'none') +
              theme(legend.position="right") +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: até R$ 510,00",
                                             "B: De R$ 510,01 até R$ 1.530,00",
                                             "C: De R$ 1.530,01 até R$ 3.060,00",
                                             "D: De R$ 3.060,01 até R$ 4.590,00",
                                             "E: De R$ 4.590,01 até R$ 6.120,00",
                                             "F: De R$ 6.120,01 até R$ 7.650,00",
                                             "G: Mais de R$ 7.650,00 ",
                                             "H: Nenhuma Renda"))
            print(bld10)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebmtd10 = readRDS("ebmtd10.rds")
            bmtd10 = ggplot(ebmtd10, aes(x = renda, y = out, middle = median,
                                         ymin = lower.whisker, ymax = upper.whisker,
                                         lower = lower.hinge, upper = upper.hinge,
                                         fill = renda)) +
              geom_boxplot(stat = "identity", alpha=0.85) + geom_point() + coord_flip() +
              labs(x="Renda",y="Nota em Matemática no ENEM 2010",
                   title = "Boxplot das Notas em Matemática no ENEM 2010 Segundo a Renda") +
              theme_bw() + theme(legend.position="right") +
              scale_alpha(guide = 'none') +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: até R$ 510,00",
                                             "B: De R$ 510,01 até R$ 1.530,00",
                                             "C: De R$ 1.530,01 até R$ 3.060,00",
                                             "D: De R$ 3.060,01 até R$ 4.590,00",
                                             "E: De R$ 4.590,01 até R$ 6.120,00",
                                             "F: De R$ 6.120,01 até R$ 7.650,00",
                                             "G: Mais de R$ 7.650,00 ",
                                             "H: Nenhuma Renda"))
            print(bmtd10)
          }
        }
      }
    }
    # histogramas das notas 2010:
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thm10 = readRDS("thm10.rds")
            k = thm10$intervalo[2] - thm10$intervalo[1]
            hm10 = ggplot(thm10, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Média no ENEM 2010", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM 2010") +
              theme_bw()
            print(hm10)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thr10 = readRDS("thr10.rds")
            k = thr10$intervalo[2] - thr10$intervalo[1]
            hr10 = ggplot(thr10, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota da Redação no ENEM 2010", y="Proporção",
                   title = "Histograma das Proporções das Notas da Redação no ENEM 2010")+
              theme_bw()
            print(hr10)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thn10 = readRDS("thn10.rds")
            k = thn10$intervalo[2] - thn10$intervalo[1]
            hn10 = ggplot(thn10, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências da Natureza", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2010")+
              theme_bw()
            print(hn10)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thh10 = readRDS("thh10.rds")
            k = thh10$intervalo[2] - thh10$intervalo[1]
            hh10 = ggplot(thh10, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências Humanas", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências Humanas no ENEM 2010")+
              theme_bw()
            print(hh10)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thl10 = readRDS("thl10.rds")
            k = thl10$intervalo[2] - thl10$intervalo[1]
            hl10 = ggplot(thl10, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Linguagens e Códigos", y="Proporção",
                   title="Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2010")+
              theme_bw()
            print(hl10)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thmt10 = readRDS("thmt10.rds")
            k = thmt10$intervalo[2] - thmt10$intervalo[1]
            hmt10 = ggplot(thmt10, aes(x=intervalo, y=freq)) +
              geom_bar(width = k,stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Matemática", y="Proporção",
                   title="Histograma das Proporções das Notas em Matemática no ENEM 2010")+
              theme_bw()
            print(hmt10)
          }
        }
      }
    }
    # histogramas por Sexo 2010:####
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thms10 = readRDS("thms10.rds")
            k = thms10$intervalo[2] - thms10$intervalo[1]
            hms10 = ggplot(thms10) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Média no ENEM 2010", y="Proporção",
                   title="Histograma das Proporções das Médias no ENEM 2010 Segundo o Sexo")
            print(hms10)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thrs10 = readRDS("thrs10.rds")
            k = thrs10$intervalo[2] - thrs10$intervalo[1]
            hrs10 = ggplot(thrs10) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Média da Redação no ENEM 2010", y="Proporção",
                   title="Histograma das Proporções das Médias da Redação no ENEM 2010 Segundo o Sexo")
            print(hrs10)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            thns10 = readRDS("thns10.rds")
            k = thns10$intervalo[2] - thns10$intervalo[1]
            hns10 = ggplot(thns10) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Ciências da Natureza no ENEM 2010", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2010 Segundo o Sexo")+
              theme_bw()
            print(hns10)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thhs10 = readRDS("thhs10.rds")
            k = thhs10$intervalo[2] - thhs10$intervalo[1]
            hhs10 = ggplot(thhs10) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Ciências Humanas no ENEM 2010", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências Humanas no ENEM 2010 Segundo o Sexo")+
              theme_bw()
            print(hhs10)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thls10 = readRDS("thls10.rds")
            k = thls10$intervalo[2] - thls10$intervalo[1]
            hls10 = ggplot(thls10) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Linguagens e Códigos no ENEM 2010", y="Proporção",
                   title="Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2010 Segundo o Sexo")+
              theme_bw()
            print(hls10)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thmts10 = readRDS("thmts10.rds")
            k = thmts10$intervalo[2] - thmts10$intervalo[1]
            hmts10 = ggplot(thmts10) + theme_bw() +
              geom_bar(aes(x=intervalo, y=F, fill="F"),
                       width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),
                       width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") +
              labs(x="Nota em Matemática no ENEM 2010", y="Proporção",
                   title = "Histograma das Proporções das Notas em Matemática no ENEM 2010 Segundo o Sexo")+
              theme_bw()
            print(hmts10)
          }}}}
    # histogramas por região 2010:####
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thmr10 = readRDS("thmr10.rds")
            k = thmr10$intervalo[2] - thmr10$intervalo[1]
            hmr10 = ggplot(thmr10,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Média no ENEM 2010", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM 2010 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hmr10)
          }}}}
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thrr10 = readRDS("thrr10.rds")
            k = thrr10$intervalo[2] - thrr10$intervalo[1]
            hrr10 = ggplot(thrr10,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Média da Redação no ENEM 2010", y="Proporção",
                   title = "Histograma das Proporções das Médias na Redação no ENEM 2010 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hrr10)
          }}}}
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thnr10 = readRDS("thnr10.rds")
            k = thnr10$intervalo[2] - thnr10$intervalo[1]
            hnr10 = ggplot(thnr10,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências da natureza no ENEM 2010", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2010 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hnr10)
          }}}}
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thhr10 = readRDS("thhr10.rds")
            k = thhr10$intervalo[2] - thhr10$intervalo[1]
            hhr10 = ggplot(thhr10,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências Humanas no ENEM 2010", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências Humanas no ENEM 2010 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hhr10)
          }}}}
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thlr10 = readRDS("thlr10.rds")
            k = thlr10$intervalo[2] - thlr10$intervalo[1]
            hlr10 = ggplot(thlr10,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Linguagens e Códigos no ENEM 2010", y="Proporção",
                   title = "Histograma das Proporções das Nota em Linguangens e Códigos no ENEM 2010 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hlr10)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thmtr10 = readRDS("thmtr10.rds")
            k = thmtr10$intervalo[2] - thmtr10$intervalo[1]
            hmtr10 = ggplot(thmtr10,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=k, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Matemática no ENEM 2010", y="Proporção",
                   title = "Histograma das Proporções das Notas em Matemática no ENEM 2010 Segundo a Região") +
              theme_bw() + facet_wrap(~reg, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hmtr10)
          }
        }
      }
    }
    # histogramas por sexo e região 2010:####
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thmsr10 = readRDS("thmsr10.rds")
            k = thmsr10$intervalo[2] - thmsr10$intervalo[1]
            hmsr10 = ggplot(thmsr10) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") + 
              labs(x="Média no ENEM 2010", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM 2010 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hmsr10)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thrsr10 = readRDS("thrsr10.rds")
            k = thrsr10$intervalo[2] - thrsr10$intervalo[1]
            hrsr10 = ggplot(thrsr10) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") + 
              labs(x="Média da Redação no ENEM 2010", y="Proporção",
                   title = "Histograma das Proporções das Médias na Redação no ENEM 2010 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hrsr10)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thnsr10 = readRDS("thnsr10.rds")
            k = thnsr10$intervalo[2] - thnsr10$intervalo[1]
            hnsr10 = ggplot(thnsr10) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") + 
              labs(x="Nota em Ciências da Natureza no ENEM 2010", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2010 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hnsr10)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thhsr10 = readRDS("thhsr10.rds")
            k = thhsr10$intervalo[2] - thhsr10$intervalo[1]
            hhsr10 = ggplot(thhsr10) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") + 
              labs(x="Nota em Ciências Humanas no ENEM 2010", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências Humanas no ENEM 2010 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hhsr10)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thlsr10 = readRDS("thlsr10.rds")
            k = thlsr10$intervalo[2] - thlsr10$intervalo[1]
            hlsr10 = ggplot(thlsr10) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") + 
              labs(x="Nota em Linguagens e Códigos no ENEM 2010", y="Proporção",
                   title = "Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2010 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hlsr10)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thmtsr10 = readRDS("thmtsr10.rds")
            k = thmtsr10$intervalo[2] - thmtsr10$intervalo[1]
            hmtsr10 = ggplot(thmtsr10) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=k, stat="identity", alpha = 0.5) +
              scale_fill_discrete(name="Legenda") + 
              labs(x="Nota em Matemática no ENEM 2010", y="Proporção",
                   title = "Histograma das Proporções das Notas em Matemática no ENEM 2010 Segundo o Sexo e Região") +
              theme_bw() + facet_wrap(~reg, nrow = 2)
            print(hmtsr10)
          }
        }
      }
    }
    # histogramas por renda 2010:
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thmd10 = readRDS("thmd10.rds")
            k = thmd10$intervalo[2] - thmd10$intervalo[1]
            hmd10 = ggplot(thmd10) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Média no ENEM 2010", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM 2010 Segundo a Renda") +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: até R$ 510,00",
                                             "B: De R$ 510,01 até R$ 1.530,00",
                                             "C: De R$ 1.530,01 até R$ 3.060,00",
                                             "D: De R$ 3.060,01 até R$ 4.590,00",
                                             "E: De R$ 4.590,01 até R$ 6.120,00",
                                             "F: De R$ 6.120,01 até R$ 7.650,00",
                                             "G: Mais de R$ 7.650,00 ",
                                             "H: Nenhuma Renda"))
            print(hmd10)
          }
        }
      }
    }
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thrd10 = readRDS("thrd10.rds")
            k = thrd10$intervalo[2] - thrd10$intervalo[1]
            hrd10 = ggplot(thrd10) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Média da Redação no ENEM 2010", y="Proporção",
                   title="Histograma das Proporções das Médias da Redação no ENEM 2010 Segundo a Renda")+
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: até R$ 510,00",
                                             "B: De R$ 510,01 até R$ 1.530,00",
                                             "C: De R$ 1.530,01 até R$ 3.060,00",
                                             "D: De R$ 3.060,01 até R$ 4.590,00",
                                             "E: De R$ 4.590,01 até R$ 6.120,00",
                                             "F: De R$ 6.120,01 até R$ 7.650,00",
                                             "G: Mais de R$ 7.650,00 ",
                                             "H: Nenhuma Renda"))
            print(hrd10)
          }
        }
      }
    }
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thnd10 = readRDS("thnd10.rds")
            k = thnd10$intervalo[2] - thnd10$intervalo[1]
            hnd10 = ggplot(thnd10) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Nota em Ciências da Natureza no ENEM 2010", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências da Natureza no ENEM 2010 Segundo a Renda")+
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: até R$ 510,00",
                                             "B: De R$ 510,01 até R$ 1.530,00",
                                             "C: De R$ 1.530,01 até R$ 3.060,00",
                                             "D: De R$ 3.060,01 até R$ 4.590,00",
                                             "E: De R$ 4.590,01 até R$ 6.120,00",
                                             "F: De R$ 6.120,01 até R$ 7.650,00",
                                             "G: Mais de R$ 7.650,00 ",
                                             "H: Nenhuma Renda"))
            print(hnd10)
          }
        }
      }
    }
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thhd10 = readRDS("thhd10.rds")
            k = thhd10$intervalo[2] - thhd10$intervalo[1]
            hhd10 = ggplot(thhd10) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Nota em Ciências Humanas no ENEM 2010", y="Proporção",
                   title="Histograma das Proporções das Notas em Ciências Humanas no ENEM 2010 Segundo a Renda")+
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: até R$ 510,00",
                                             "B: De R$ 510,01 até R$ 1.530,00",
                                             "C: De R$ 1.530,01 até R$ 3.060,00",
                                             "D: De R$ 3.060,01 até R$ 4.590,00",
                                             "E: De R$ 4.590,01 até R$ 6.120,00",
                                             "F: De R$ 6.120,01 até R$ 7.650,00",
                                             "G: Mais de R$ 7.650,00 ",
                                             "H: Nenhuma Renda"))
            print(hhd10)
          }
        }
      }
    }
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thld10 = readRDS("thld10.rds")
            k = thld10$intervalo[2] - thld10$intervalo[1]
            hld10 = ggplot(thld10) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Nota em Linguagens e Códigos no ENEM 2010", y="Proporção",
                   title="Histograma das Proporções das Notas em Linguagens e Códigos no ENEM 2010 Segundo a Renda")+
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: até R$ 510,00",
                                             "B: De R$ 510,01 até R$ 1.530,00",
                                             "C: De R$ 1.530,01 até R$ 3.060,00",
                                             "D: De R$ 3.060,01 até R$ 4.590,00",
                                             "E: De R$ 4.590,01 até R$ 6.120,00",
                                             "F: De R$ 6.120,01 até R$ 7.650,00",
                                             "G: Mais de R$ 7.650,00 ",
                                             "H: Nenhuma Renda"))
            print(hld10)
          }
        }
      }
    }
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if(2010 %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thmtd10 = readRDS("thmtd10.rds")
            k = thmtd10$intervalo[2] - thmtd10$intervalo[1]
            hmtd10 = ggplot(thmtd10) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=k, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=k, stat="identity", alpha = 0.65) +
              theme_bw() +
              labs(x="Nota em Matemática no ENEM 2010", y="Proporção",
                   title="Histograma das Proporções das Notas em Matemática no ENEM 2010 Segundo a Renda")+
              theme_bw() +
              scale_fill_discrete(name = "Legenda",
                                  labels = c("Não Informado","A: até R$ 510,00",
                                             "B: De R$ 510,01 até R$ 1.530,00",
                                             "C: De R$ 1.530,01 até R$ 3.060,00",
                                             "D: De R$ 3.060,01 até R$ 4.590,00",
                                             "E: De R$ 4.590,01 até R$ 6.120,00",
                                             "F: De R$ 6.120,01 até R$ 7.650,00",
                                             "G: Mais de R$ 7.650,00 ",
                                             "H: Nenhuma Renda"))
            print(hmtd10)
          }
        }
      }
    }

    # gráficos de todos os anos
    ## Brasil
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebm = readRDS("ebm.rds")
            bm = ggplot(ebm, aes(x = e, y = out, middle = median,
                                  ymin = lower.whisker, ymax = upper.whisker,
                                  lower = lower.hinge, upper = upper.hinge,
                                  fill = e, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Ano",y="Média no ENEM",
                   title = "Boxplot das Médias no ENEM Segundo o Ano") +
              theme(legend.position="none") + scale_x_continuous(breaks = c(2010:2015))
            print(bm)
          }}}}
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebr = readRDS("ebr.rds")
            br = ggplot(ebr, aes(x = e, y = out, middle = median,
                               ymin = lower.whisker, ymax = upper.whisker,
                               lower = lower.hinge, upper = upper.hinge,
                               fill = e, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Ano",y="Média da Redação no ENEM",
                   title = "Boxplot das Médias da Redação no ENEM Segundo o Ano") +
              theme(legend.position="none") + scale_x_continuous(breaks = c(2010:2015))
            print(br)
          }}}}
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebn = readRDS("ebn.rds")
            bn = ggplot(ebn, aes(x = e, y = out, middle = median,
                               ymin = lower.whisker, ymax = upper.whisker,
                               lower = lower.hinge, upper = upper.hinge,
                               fill = e, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Ano",y="Nota em Ciências da Natureza no ENEM",
                   title = "Boxplot das Notas em Ciências da Natureza no ENEM Segundo o Ano") +
              theme(legend.position="none") + scale_x_continuous(breaks = c(2010:2015))
            print(bn)
          }}}}
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebh = readRDS("ebh.rds")
            bh = ggplot(enh, aes(x = e, y = out, middle = median,
                               ymin = lower.whisker, ymax = upper.whisker,
                               lower = lower.hinge, upper = upper.hinge,
                               fill = e, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Ano",y="Nota em Ciências Humanas no ENEM",
                   title = "Boxplot das Notas em Ciências Humanas no ENEM Segundo o Ano") +
              theme(legend.position="none") + scale_x_continuous(breaks = c(2010:2015))
            print(bh)
          }}}}
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebl = readRDS("ebl.rds")
            bl = ggplot(ebl, aes(x = e, y = out, middle = median,
                               ymin = lower.whisker, ymax = upper.whisker,
                               lower = lower.hinge, upper = upper.hinge,
                               fill = e, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Ano",y="Nota em Linguagens e Códigos no ENEM",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM Segundo o Ano") +
              theme(legend.position="none") + scale_x_continuous(breaks = c(2010:2015))
            print(bl)
          }}}}
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebmt = readRDS("ebmt.rds")
            bmt = ggplot(ebmt, aes(x = e, y = out, middle = median,
                               ymin = lower.whisker, ymax = upper.whisker,
                               lower = lower.hinge, upper = upper.hinge,
                               fill = e, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Ano",y="Nota em Matemática no ENEM",
                   title = "Boxplot das Notas em Matemática no ENEM Segundo o Ano") +
              theme(legend.position="none") + scale_x_continuous(breaks = c(2010:2015))
            print(bmt)
          }}}}
    
    ## Por sexo
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebms = readRDS("ebms.rds")
            bms = ggplot(ebms, aes(x = sexo, y = out, middle = median,
                               ymin = lower.whisker, ymax = upper.whisker,
                               lower = lower.hinge, upper = upper.hinge,
                               fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Média no ENEM",
                   title="Boxplot das Médias no ENEM Segundo o Ano e Sexo")+
              theme_bw() + theme(legend.position="none") + facet_grid(~e)
            print(bms)
          }}}}
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebrs = readRDS("ebrs.rds")
            brs = ggplot(ebrs, aes(x = sexo, y = out, middle = median,
                                   ymin = lower.whisker, ymax = upper.whisker,
                                   lower = lower.hinge, upper = upper.hinge,
                                   fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Média da Redação no ENEM",
                   title="Boxplot das Médias da Redação no ENEM Segundo o Ano e Sexo")+
              theme_bw() + theme(legend.position="none") + facet_grid(~e)
            print(brs)
          }}}}
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebns = readRDS("ebns.rds")
            bns = ggplot(ebns, aes(x = sexo, y = out, middle = median,
                                   ymin = lower.whisker, ymax = upper.whisker,
                                   lower = lower.hinge, upper = upper.hinge,
                                   fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Nota em Ciências da Natureza no ENEM",
                   title="Boxplot das Notas em Ciências da Natureza no ENEM Segundo o Ano e Sexo")+
              theme_bw() + theme(legend.position="none") + facet_grid(~e)
            print(bns)
          }}}}
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebhs = readRDS("ebhs.rds")
            bhs = ggplot(ebhs, aes(x = sexo, y = out, middle = median,
                                   ymin = lower.whisker, ymax = upper.whisker,
                                   lower = lower.hinge, upper = upper.hinge,
                                   fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Nota em Ciências Humanas no ENEM",
                   title="Boxplot das Notas em Ciências Humanas no ENEM Segundo o Ano e Sexo")+
              theme_bw() + theme(legend.position="none") + facet_grid(~e)
            print(bhs)
          }}}}
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebls = readRDS("ebls.rds")
            bls = ggplot(ebls, aes(x = sexo, y = out, middle = median,
                                   ymin = lower.whisker, ymax = upper.whisker,
                                   lower = lower.hinge, upper = upper.hinge,
                                   fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Nota em Linguagens e Códigos no ENEM",
                   title="Boxplot das Notas em Linguagens e Códigos no ENEM Segundo o Ano e Sexo")+
              theme_bw() + theme(legend.position="none") + facet_grid(~e)
            print(bls)
          }}}}
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebmts = readRDS("ebmts.rds")
            bmts = ggplot(ebmts, aes(x = sexo, y = out, middle = median,
                                   ymin = lower.whisker, ymax = upper.whisker,
                                   lower = lower.hinge, upper = upper.hinge,
                                   fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Nota em Matemática no ENEM",
                   title="Boxplot das Notas em Matemática no ENEM Segundo o Ano e Sexo")+
              theme_bw() + theme(legend.position="none") + facet_grid(~e)
            print(bmts)
          }}}}
    
    ## por região
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebmr = readRDS("ebmr.rds")
            bmr = ggplot(ebmr, aes(x = reg, y = out, middle = median,
                                   ymin = lower.whisker, ymax = upper.whisker,
                                   lower = lower.hinge, upper = upper.hinge,
                                   fill = reg)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Média no ENEM",
                   title="Boxplot das Médias no ENEM Segundo o Ano e Região")+
              theme_bw() + theme(legend.position="none") + facet_grid(~e)
            print(bmr)
          }}}}
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebrr = readRDS("ebrr.rds")
            brr = ggplot(ebrr, aes(x = reg, y = out, middle = median,
                                   ymin = lower.whisker, ymax = upper.whisker,
                                   lower = lower.hinge, upper = upper.hinge,
                                   fill = reg)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Média da Redação no ENEM",
                   title="Boxplot das Médias da Redação no ENEM Segundo o Ano e Região")+
              theme_bw() + theme(legend.position="none") + facet_grid(~e)
            print(brr)
          }}}}
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebnr = readRDS("ebnr.rds")
            bnr = ggplot(ebnr, aes(x = reg, y = out, middle = median,
                                   ymin = lower.whisker, ymax = upper.whisker,
                                   lower = lower.hinge, upper = upper.hinge,
                                   fill = reg)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Ciências da Natureza no ENEM",
                   title="Boxplot das Notas em Ciências da Natureza no ENEM Segundo o Ano e Região")+
              theme_bw() + theme(legend.position="none") + facet_grid(~e)
            print(bnr)
          }}}}
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebhr = readRDS("ebhr.rds")
            bhr = ggplot(ebhr, aes(x = reg, y = out, middle = median,
                                   ymin = lower.whisker, ymax = upper.whisker,
                                   lower = lower.hinge, upper = upper.hinge,
                                   fill = reg)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Ciências Humanas no ENEM",
                   title="Boxplot das Notas em Ciências Humanas no ENEM Segundo o Ano e Região")+
              theme_bw() + theme(legend.position="none") + facet_grid(~e)
            print(bhr)
          }}}}
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            eblr = readRDS("eblr.rds")
            blr = ggplot(eblr, aes(x = reg, y = out, middle = median,
                                   ymin = lower.whisker, ymax = upper.whisker,
                                   lower = lower.hinge, upper = upper.hinge,
                                   fill = reg)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Linguagens e Códigos no ENEM",
                   title="Boxplot das Notas em Linguagens e Códigos no ENEM Segundo o Ano e Região")+
              theme_bw() + theme(legend.position="none") + facet_grid(~e)
            print(blr)
          }}}}
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            ebmtr = readRDS("ebmtr.rds")
            bmtr = ggplot(ebmtr, aes(x = reg, y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     fill = reg)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Matemática no ENEM",
                   title="Boxplot das Notas em Matemática no ENEM Segundo o Ano e Região")+
              theme_bw() + theme(legend.position="none") + facet_grid(~e)
            print(bmtr)
          }}}}
    
    ## por sexo e região
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebmsr = readRDS("ebmsr.rds")
            bmsr = ggplot(ebmsr, aes(x = sexo, y = out, middle = median,
                                ymin = lower.whisker, ymax = upper.whisker,
                                lower = lower.hinge, upper = upper.hinge,
                                fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Média no ENEM",
                   title="Boxplot das Médias no ENEM Segundo o Ano, Sexo e Região")+
              theme_bw() + theme(legend.position="none") + 
              facet_wrap(e~reg, ncol = 5, strip.position = "right")
            print(bmsr)
          }}}}
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebrsr = readRDS("ebrsr.rds")
            brsr = ggplot(ebrsr, aes(x = sexo, y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     fill = sexo)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Média no ENEM",
                   title="Boxplot das Médias no ENEM Segundo o Ano, Sexo e Região")+
              theme_bw() + theme(legend.position="none") + 
              facet_wrap(e~reg, ncol = 5, strip.position = "right")
            print(brsr)
          }}}}
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebnsr = readRDS("ebnsr.rds")
            bnsr = ggplot(ebnsr, aes(x = sexo, y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Média no ENEM",
                   title="Boxplot das Médias no ENEM Segundo o Ano, Sexo e Região")+
              theme_bw() + theme(legend.position="none") + 
              facet_wrap(e~reg, ncol = 5, strip.position = "right")
            print(bnsr)
          }}}}
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebhsr = readRDS("ebhsr.rds")
            bhsr = ggplot(ebhsr, aes(x = sexo, y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Média no ENEM",
                   title="Boxplot das Médias no ENEM Segundo o Ano, Sexo e Região")+
              theme_bw() + theme(legend.position="none") + 
              facet_wrap(e~reg, ncol = 5, strip.position = "right")
            print(bhsr)
          }}}}
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            eblsr = readRDS("eblsr.rds")
            blsr = ggplot(eblsr, aes(x = sexo, y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Média no ENEM",
                   title="Boxplot das Médias no ENEM Segundo o Ano, Sexo e Região")+
              theme_bw() + theme(legend.position="none") + 
              facet_wrap(e~reg, ncol = 5, strip.position = "right")
            print(blsr)
          }}}}
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebmtsr = readRDS("ebmtsr.rds")
            bmtsr = ggplot(ebmtsr, aes(x = sexo, y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     fill = sexo, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Sexo",y="Média no ENEM",
                   title="Boxplot das Médias no ENEM Segundo o Ano, Sexo e Região")+
              theme_bw() + theme(legend.position="none") + 
              facet_wrap(e~reg, ncol = 5, strip.position = "right")
            print(bmtsr)
          }}}}
    
    # por renda
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebmd = readRDS("ebmd.rds")
            bmd = ggplot(ebmd, aes(x = renda, y = out, middle = median,
                                ymin = lower.whisker, ymax = upper.whisker,
                                lower = lower.hinge, upper = upper.hinge,
                                fill = renda, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Renda",y="Média no ENEM",
                   title="Boxplot das Médias no ENEM Segundo o Ano e Renda",
                   caption="Nota: as legendas variam de ano para ano. Favor consultar nos gráficos com anos individuais")+
              theme_bw() + theme(legend.position="none") + facet_grid(~e)
            print(bmd)
          }}}}
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebrd = readRDS("ebrd.rds")
            brd = ggplot(ebrd, aes(x = renda, y = out, middle = median,
                                ymin = lower.whisker, ymax = upper.whisker,
                                lower = lower.hinge, upper = upper.hinge,
                                fill = renda)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Renda",y="Média no ENEM",
                   title="Boxplot das Médias no ENEM Segundo o Ano e Renda",
                   caption="Nota: as legendas variam de ano para ano. Favor consultar nos gráficos com anos individuais")+
              theme_bw() + theme(legend.position="none") + facet_grid(~e)
            print(brd)
          }}}}
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebnd = readRDS("ebnd.rds")
            bnd = ggplot(ebnd, aes(x = renda, y = out, middle = median,
                                ymin = lower.whisker, ymax = upper.whisker,
                                lower = lower.hinge, upper = upper.hinge,
                                fill = renda, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Renda",y="Média no ENEM",
                   title="Boxplot das Médias no ENEM Segundo o Ano e Renda",
                   caption="Nota: as legendas variam de ano para ano. Favor consultar nos gráficos com anos individuais")+
              theme_bw() + theme(legend.position="none") + facet_grid(~e)
            print(bnd)
          }}}}
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebhd = readRDS("ebhd.rds")
            bhd = ggplot(ebhd, aes(x = renda, y = out, middle = median,
                                ymin = lower.whisker, ymax = upper.whisker,
                                lower = lower.hinge, upper = upper.hinge,
                                fill = renda, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Renda",y="Média no ENEM",
                   title="Boxplot das Médias no ENEM Segundo o Ano e Renda",
                   caption="Nota: as legendas variam de ano para ano. Favor consultar nos gráficos com anos individuais")+
              theme_bw() + theme(legend.position="none") + facet_grid(~e)
            print(bhd)
          }}}}
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebld = readRDS("ebld.rds")
            bld = ggplot(ebld, aes(x = renda, y = out, middle = median,
                                ymin = lower.whisker, ymax = upper.whisker,
                                lower = lower.hinge, upper = upper.hinge,
                                fill = renda, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Renda",y="Média no ENEM",
                   title="Boxplot das Médias no ENEM Segundo o Ano e Renda",
                   caption="Nota: as legendas variam de ano para ano. Favor consultar nos gráficos com anos individuais")+
              theme_bw() + theme(legend.position="none") + facet_grid(~e)
            print(bld)
          }}}}
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            ebmtd = readRDS("ebmtd.rds")
            bmtd = ggplot(ebmtd, aes(x = renda, y = out, middle = median,
                                 ymin = lower.whisker, ymax = upper.whisker,
                                 lower = lower.hinge, upper = upper.hinge,
                                 fill = renda, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Renda",y="Média no ENEM",
                   title="Boxplot das Médias no ENEM Segundo o Ano e Renda",
                   caption="Nota: as legendas variam de ano para ano. Favor consultar nos gráficos com anos individuais")+
              theme_bw() + theme(legend.position="none") + facet_grid(~e)
            print(bmtd)
          }}}}
    
    ## Histogramas
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thm = readRDS("thm.rds")
            hm = ggplot(thm,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=30, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Média no ENEM", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM (Brasil)") +
              theme_bw() + facet_wrap(~ano, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hm)
          }}}}
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thr = readRDS("thr.rds")
            hr = ggplot(thr,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=30, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Média da Redação no ENEM", y="Proporção",
                   title = "Histograma das Proporções das Médias da Redação no ENEM (Brasil)") +
              theme_bw() + facet_wrap(~ano, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hr)
          }}}}
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thn = readRDS("thn.rds")
            hn = ggplot(thn,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=30, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências da Natureza", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências da Natureza no ENEM (Brasil)") +
              theme_bw() + facet_wrap(~ano, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hn)
          }}}}
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thh = readRDS("thh.rds")
            hh = ggplot(thh,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=30, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências Humanas", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências Humanas no ENEM (Brasil)") +
              theme_bw() + facet_wrap(~ano, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hh)
          }}}}
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thl = readRDS("thl.rds")
            hl = ggplot(thl,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=30, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Linguagens e Códigos", y="Proporção",
                   title = "Histograma das Proporções das Notas em Linguagens e Códigos no ENEM (Brasil)") +
              theme_bw() + facet_wrap(~ano, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hl)
          }}}}
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            thmt = readRDS("thmt.rds")
            hmt = ggplot(thmt,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=30, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Matemática", y="Proporção",
                   title = "Histograma das Proporções das Notas em Matemática no ENEM (Brasil)") +
              theme_bw() + facet_wrap(~ano, nrow = 1) +
              theme(axis.text.y=element_text(size = 8))
            print(hmt)
          }}}}
    
    ## por sexo
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thms = readRDS("thms.rds")
            hms = ggplot(thms) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=30, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=30, stat="identity", alpha = 0.5) +
              labs(x="Média no ENEM", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM Segundo o Sexo e Ano") +
              scale_fill_discrete(name="Legenda") + theme_bw() + facet_wrap(~ano, nrow = 1)
            print(hms)
          }}}}
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thrs = readRDS("thrs.rds")
            hrs = ggplot(thrs) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=30, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=30, stat="identity", alpha = 0.5) +
              labs(x="Média da Redação no ENEM", y="Proporção",
                   title = "Histograma das Proporções das Médias da Redação no ENEM Segundo o Sexo e Ano") +
              scale_fill_discrete(name="Legenda") + theme_bw() + facet_wrap(~ano, nrow = 1)
            print(hrs)
          }}}}
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thns = readRDS("thns.rds")
            hns = ggplot(thns) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=30, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=30, stat="identity", alpha = 0.5) +
              labs(x="Nota em Ciências da Natureza no ENEM", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências da Natureza no ENEM Segundo o Sexo e Ano") +
              scale_fill_discrete(name="Legenda") + theme_bw() + facet_wrap(~ano, nrow = 1)
            print(hns)
          }}}}
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thhs = readRDS("thhs.rds")
            hhs = ggplot(thhs) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=30, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=30, stat="identity", alpha = 0.5) +
              labs(x="Nota em Ciências Humanas no ENEM", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências Humanas no ENEM Segundo o Sexo e Ano") +
              scale_fill_discrete(name="Legenda") + theme_bw() + facet_wrap(~ano, nrow = 1)
            print(hhs)
          }}}}
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thls = readRDS("thls.rds")
            hls = ggplot(thls) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=30, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=30, stat="identity", alpha = 0.5) +
              labs(x="Nota em Linguagens e Códigos no ENEM", y="Proporção",
                   title = "Histograma das Proporções das Notas em Linguagens e Códigos no ENEM Segundo o Sexo e Ano") +
              scale_fill_discrete(name="Legenda") + theme_bw() + facet_wrap(~ano, nrow = 1)
            print(hls)
          }}}}
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            thmts = readRDS("thmts.rds")
            hmts = ggplot(thmts) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=30, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=30, stat="identity", alpha = 0.5) +
              labs(x="Nota em Matemática no ENEM", y="Proporção",
                   title = "Histograma das Proporções das Notas em Matemática no ENEM Segundo o Sexo e Ano") +
              scale_fill_discrete(name="Legenda") + theme_bw() + facet_wrap(~ano, nrow = 1)
            print(hmts)
          }}}}
    
    ## por região
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thmr = readRDS("thmr.rds")
            hmr = ggplot(thmr,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=30, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Média no ENEM", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM Segundo a Região e Ano") +
              theme_bw() + facet_wrap(ano~reg, nrow = 6, strip.position = "right") +
              theme(axis.text.y=element_text(size = 8))
            print(hmr)
          }}}}
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thrr = readRDS("thrr.rds")
            hrr = ggplot(thrr,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=30, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Média da Redação no ENEM", y="Proporção",
                   title = "Histograma das Proporções das Médias da Redação no ENEM Segundo a Região e Ano") +
              theme_bw() + facet_wrap(ano~reg, nrow = 6, strip.position = "right") +
              theme(axis.text.y=element_text(size = 8))
            print(hrr)
          }}}}
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thnr = readRDS("thnr.rds")
            hnr = ggplot(thnr,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=30, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências da Natureza no ENEM", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências da Natureza no ENEM Segundo a Região e Ano") +
              theme_bw() + facet_wrap(ano~reg, nrow = 6, strip.position = "right") +
              theme(axis.text.y=element_text(size = 8))
            print(hnr)
          }}}}
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thhr = readRDS("thhr.rds")
            hhr = ggplot(thhr,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=30, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Ciências Humanas no ENEM", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências Humanas no ENEM Segundo a Região e Ano") +
              theme_bw() + facet_wrap(ano~reg, nrow = 6, strip.position = "right") +
              theme(axis.text.y=element_text(size = 8))
            print(hhr)
          }}}}
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thlr = readRDS("thlr.rds")
            hlr = ggplot(thlr,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=30, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Linguagens e Códigos no ENEM", y="Proporção",
                   title = "Histograma das Proporções das Notas em Linguagens e Códigos no ENEM Segundo a Região e Ano") +
              theme_bw() + facet_wrap(ano~reg, nrow = 6, strip.position = "right") +
              theme(axis.text.y=element_text(size = 8))
            print(hlr)
          }}}}
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por região" %in% input$vcategorica){
            rm(list = ls())
            thmtr = readRDS("thmtr.rds")
            hmtr = ggplot(thmtr,aes(x=intervalo, y=prop), alpha=0.5) +
              geom_bar(width=30, stat="identity",colour="aquamarine4", fill="aquamarine4") +
              labs(x="Nota em Matemática no ENEM", y="Proporção",
                   title = "Histograma das Proporções das Notas em Matemática no ENEM Segundo a Região e Ano") +
              theme_bw() + facet_wrap(ano~reg, nrow = 6, strip.position = "right") +
              theme(axis.text.y=element_text(size = 8))
            print(hmtr)
          }}}}
    
    ## por sexo e região
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thmsr = readRDS("thmsr.rds")
            hmsr = ggplot(thmsr) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=30, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=30, stat="identity", alpha = 0.5) +
              labs(x="Média no ENEM", y="Proporção",
                   title = "Histograma das Proporções das Médias no ENEM Segundo o Sexo, Região e Ano") +
              scale_fill_discrete(name="Legenda") + theme_bw() + 
              facet_wrap(ano~reg, ncol = 5, strip.position = "right")
            print(hmsr)
          }}}}
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thrsr = readRDS("thrsr.rds")
            hrsr = ggplot(thrsr) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=30, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=30, stat="identity", alpha = 0.5) +
              labs(x="Média da Redação no ENEM", y="Proporção",
                   title = "Histograma das Proporções das Médias da Redação no ENEM Segundo o Sexo, Região e Ano") +
              scale_fill_discrete(name="Legenda") + theme_bw() + 
              facet_wrap(ano~reg, ncol = 5, strip.position = "right")
            print(hrsr)
          }}}}
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thnsr = readRDS("thnsr.rds")
            hnsr = ggplot(thnsr) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=30, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=30, stat="identity", alpha = 0.5) +
              labs(x="Nota em Ciências da Natureza no ENEM", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências da Natureza no ENEM Segundo o Sexo, Região e Ano") +
              scale_fill_discrete(name="Legenda") + theme_bw() + 
              facet_wrap(ano~reg, ncol = 5, strip.position = "right")
            print(hnsr)
          }}}}
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thhsr = readRDS("thhsr.rds")
            hhsr = ggplot(thhsr) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=30, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=30, stat="identity", alpha = 0.5) +
              labs(x="Nota em Ciências Humanas no ENEM", y="Proporção",
                   title = "Histograma das Proporções das Notas em Ciências Humanas no ENEM Segundo o Sexo, Região e Ano") +
              scale_fill_discrete(name="Legenda") + theme_bw() + 
              facet_wrap(ano~reg, ncol = 5, strip.position = "right")
            print(hhsr)
          }}}}
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thlsr = readRDS("thlsr.rds")
            hlsr = ggplot(thlsr) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=30, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=30, stat="identity", alpha = 0.5) +
              labs(x="Nota em Linguagens e Códigos no ENEM", y="Proporção",
                   title = "Histograma das Proporções das Notas em Linguagens e Códigos no ENEM Segundo o Sexo, Região e Ano") +
              scale_fill_discrete(name="Legenda") + theme_bw() + 
              facet_wrap(ano~reg, ncol = 5, strip.position = "right")
            print(hlsr)
          }}}}
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            thmtsr = readRDS("thmtsr.rds")
            hmtsr = ggplot(thmtsr) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=30, stat="identity", alpha = 0.5) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=30, stat="identity", alpha = 0.5) +
              labs(x="Nota em Matemática no ENEM", y="Proporção",
                   title = "Histograma das Proporções das Notas em Matemática no ENEM Segundo o Sexo, Região e Ano") +
              scale_fill_discrete(name="Legenda") + theme_bw() + 
              facet_wrap(ano~reg, ncol = 5, strip.position = "right")
            print(hmtsr)
          }}}}
    
    ## por renda
    if("Média Geral" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thmd = readRDS("thmd.rds")
            hmd = ggplot(thmd) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=30, stat="identity", alpha = 0.65) +
              labs(x="Renda",y="Média no ENEM",
                   title="Histograma Proporções das Médias no ENEM Segundo o Ano e Renda",
                   caption="Nota: as legendas variam de ano para ano. Favor consultar nos gráficos com anos individuais")+
              theme_bw() + theme(legend.position="none") + facet_grid(~ano)
            print(hmd)
          }}}}
    if("Média da Redação" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thrd = readRDS("thrd.rds")
            hrd = ggplot(thrd) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=30, stat="identity", alpha = 0.65) +
              labs(x="Renda",y="Média da Redação no ENEM",
                   title="Histograma Proporções das Médias da Redação no ENEM Segundo o Ano e Renda",
                   caption="Nota: as legendas variam de ano para ano. Favor consultar nos gráficos com anos individuais")+
              theme_bw() + theme(legend.position="none") + facet_grid(~ano)
            print(hrd)
          }}}}
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thnd = readRDS("thnd.rds")
            hnd = ggplot(thnd) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=30, stat="identity", alpha = 0.65) +
              labs(x="Renda",y="Nota em Ciências da Natureza no ENEM",
                   title="Histograma Proporções das Notas em Ciências da Natureza no ENEM Segundo o Ano e Renda",
                   caption="Nota: as legendas variam de ano para ano. Favor consultar nos gráficos com anos individuais")+
              theme_bw() + theme(legend.position="none") + facet_grid(~ano)
            print(hnd)
          }}}}
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thhd = readRDS("thhd.rds")
            hhd = ggplot(thhd) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=30, stat="identity", alpha = 0.65) +
              labs(x="Renda",y="Nota em Ciências Humanas no ENEM",
                   title="Histograma Proporções das Notas em Ciências Humanas no ENEM Segundo o Ano e Renda",
                   caption="Nota: as legendas variam de ano para ano. Favor consultar nos gráficos com anos individuais")+
              theme_bw() + theme(legend.position="none") + facet_grid(~ano)
            print(hhd)
          }}}}
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thld = readRDS("thld.rds")
            hld = ggplot(thld) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=30, stat="identity", alpha = 0.65) +
              labs(x="Renda",y="Nota em Linguagens e Códigos no ENEM",
                   title="Histograma Proporções das Notas em Linguagens e Códigos no ENEM Segundo o Ano e Renda",
                   caption="Nota: as legendas variam de ano para ano. Favor consultar nos gráficos com anos individuais")+
              theme_bw() + theme(legend.position="none") + facet_grid(~ano)
            print(hld)
          }}}}
    if("Nota em Matemática" %in% input$nota){
      if("Histograma" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por renda" %in% input$vcategorica){
            rm(list = ls())
            thmtd = readRDS("thmtd.rds")
            hmtd = ggplot(thmtd) +
              geom_bar(aes(x=intervalo, y=A, fill="A"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=B, fill="B"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=C, fill="C"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=D, fill="D"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=E, fill="E"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=F, fill="F"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=G, fill="G"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=H, fill="H"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=I, fill="I"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=J, fill="J"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=K, fill="K"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=L, fill="L"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=M, fill="M"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=N, fill="N"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=O, fill="O"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=P, fill="P"),width=30, stat="identity", alpha = 0.65) +
              geom_bar(aes(x=intervalo, y=Q, fill="Q"),width=30, stat="identity", alpha = 0.65) +
              labs(x="Renda",y="Nota em Matemática no ENEM",
                   title="Histograma Proporções das Notas em Matemática no ENEM Segundo o Ano e Renda",
                   caption="Nota: as legendas variam de ano para ano. Favor consultar nos gráficos com anos individuais")+
              theme_bw() + theme(legend.position="none") + facet_grid(~ano)
            print(hmtd)
          }}}}
    
  })
  output$tabela = function(){
    if(2015 %in% input$anos){
      if("Média Geral" %in% input$nota){
        a = read.table("sm15.txt")
        a.html <- kable(a, "html", caption = "Tabela Resumo das Médias do ENEM 2015")
        kable_styling(a.html, "striped")}else{
          if("Média da Redação" %in% input$nota){
            a = read.table("sr15.txt")
            a.html = kable(a, "html", caption = "Tabela Resumo das Médias da Redação do ENEM 2015")
            kable_styling(a.html, "striped")}else{
              if("Nota em Ciências da Natureza" %in% input$nota){
                a = read.table("sn15.txt")
                a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Ciências da Natureza do ENEM 2015")
                kable_styling(a.html, "striped")}else{
                  if("Nota em Ciências Humanas" %in% input$nota){
                    a = read.table("sh15.txt")
                    a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Ciências Humanas do ENEM 2015")
                    kable_styling(a.html, "striped")}else{
                      if("Nota em Linguagens e Códigos" %in% input$nota){
                        a = read.table("sl15.txt")
                        a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Linguagens e Códigos do ENEM 2015")
                        kable_styling(a.html, "striped")}else{
                          if("Nota em Matemática" %in% input$nota){
                            a = read.table("smt15.txt")
                            a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Matemática do ENEM 2015")
                            kable_styling(a.html, "striped")}}}}}}}else{
                              #2014
                              if(2014 %in% input$anos){
                                if("Média Geral" %in% input$nota){
                                  a = read.table("sm14.txt")
                                  a.html <- kable(a, "html", caption = "Tabela Resumo das Médias do ENEM 2014")
                                  kable_styling(a.html, "striped")}else{
                                    if("Média da Redação" %in% input$nota){
                                      a = read.table("sr14.txt")
                                      a.html <- kable(a, "html", caption = "Tabela Resumo das Médias da Redação do ENEM 2014")
                                      kable_styling(a.html, "striped")}else{
                                        if("Nota em Ciências da Natureza" %in% input$nota){
                                          a = read.table("sn14.txt")
                                          a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Ciências da Natureza do ENEM 2014")
                                          kable_styling(a.html, "striped")}else{
                                            if("Nota em Ciências Humanas" %in% input$nota){
                                              a = read.table("sh14.txt")
                                              a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Ciências Humanas do ENEM 2014")
                                              kable_styling(a.html, "striped")}else{
                                                if("Nota em Linguagens e Códigos" %in% input$nota){
                                                  a = read.table("sl14.txt")
                                                  a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Linguagens e Códigos do ENEM 2014")
                                                  kable_styling(a.html, "striped")}else{
                                                    if("Nota em Matemática" %in% input$nota){
                                                      a = read.table("smt14.txt")
                                                      a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Matemática do ENEM 2014")
                                                      kable_styling(a.html, "striped")}}}}}}}else{
                                                        #2013
                                                        if(2013 %in% input$anos){
                                                          if("Média Geral" %in% input$nota){
                                                            a = read.table("sm13.txt")
                                                            a.html <- kable(a, "html", caption = "Tabela Resumo das Médias do ENEM 2013")
                                                            kable_styling(a.html, "striped")}else{
                                                              if("Média da Redação" %in% input$nota){
                                                                a = read.table("sr13.txt")
                                                                a.html <- kable(a, "html", caption = "Tabela Resumo das Médias da Redação do ENEM 2013")
                                                                kable_styling(a.html, "striped")}else{
                                                                  if("Nota em Ciências da Natureza" %in% input$nota){
                                                                    a = read.table("sn13.txt")
                                                                    a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Ciências da Natureza do ENEM 2013")
                                                                    kable_styling(a.html, "striped")}else{
                                                                      if("Nota em Ciências Humanas" %in% input$nota){
                                                                        a = read.table("sh13.txt")
                                                                        a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Ciências Humanas do ENEM 2013")
                                                                        kable_styling(a.html, "striped")}else{
                                                                          if("Nota em Linguagens e Códigos" %in% input$nota){
                                                                            a = read.table("sl13.txt")
                                                                            a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Linguagens e Códigos do ENEM 2013")
                                                                            kable_styling(a.html, "striped")}else{
                                                                              if("Nota em Matemática" %in% input$nota){
                                                                                a = read.table("smt13.txt")
                                                                                a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Matemática do ENEM 2013")
                                                                                kable_styling(a.html, "striped")}}}}}}}else{
                                                                                  #2012
                                                                                  if(2012 %in% input$anos){ 
                                                                                    if("Média Geral" %in% input$nota){
                                                                                      a = read.table("sm12.txt")
                                                                                      a.html <- kable(a, "html", caption = "Tabela Resumo das Médias do ENEM 2012")
                                                                                      kable_styling(a.html, "striped")}else{
                                                                                        if("Média da Redação" %in% input$nota){
                                                                                          a = read.table("sr12.txt")
                                                                                          a.html <- kable(a, "html", caption = "Tabela Resumo das Médias da Redação do ENEM 2012")
                                                                                          kable_styling(a.html, "striped")}else{
                                                                                            if("Nota em Ciências da Natureza" %in% input$nota){
                                                                                              a = read.table("sn12.txt")
                                                                                              a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Ciências da Natureza do ENEM 2012")
                                                                                              kable_styling(a.html, "striped")}else{
                                                                                                if("Nota em Ciências Humanas" %in% input$nota){
                                                                                                  a = read.table("sh12.txt")
                                                                                                  a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Ciências Humanas do ENEM 2012")
                                                                                                  kable_styling(a.html, "striped")}else{
                                                                                                    if("Nota em Linguagens e Códigos" %in% input$nota){
                                                                                                      a = read.table("sl12.txt")
                                                                                                      a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Linguagens e Códigos do ENEM 2012")
                                                                                                      kable_styling(a.html, "striped")}else{
                                                                                                        if("Nota em Matemática" %in% input$nota){
                                                                                                          a = read.table("smt12.txt")
                                                                                                          a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Matemática do ENEM 2012")
                                                                                                          kable_styling(a.html, "striped")}}}}}}}else{
                                                                                                            #2011
                                                                                                            if(2011 %in% input$anos){
                                                                                                              if("Média Geral" %in% input$nota){
                                                                                                                a = read.table("sm11.txt")
                                                                                                                a.html <- kable(a, "html", caption = "Tabela Resumo das Médias do ENEM 2011")
                                                                                                                kable_styling(a.html, "striped")}else{
                                                                                                                  if("Média da Redação" %in% input$nota){
                                                                                                                    a = read.table("sr11.txt")
                                                                                                                    a.html <- kable(a, "html", caption = "Tabela Resumo das Médias da Redação do ENEM 2011")
                                                                                                                    kable_styling(a.html, "striped")}else{
                                                                                                                      if("Nota em Ciências da Natureza" %in% input$nota){
                                                                                                                        a = read.table("sn11.txt")
                                                                                                                        a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Ciências da Natureza do ENEM 2011")
                                                                                                                        kable_styling(a.html, "striped")}else{
                                                                                                                          if("Nota em Ciências Humanas" %in% input$nota){
                                                                                                                            a = read.table("sh11.txt")
                                                                                                                            a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Ciências Humanas do ENEM 2011")
                                                                                                                            kable_styling(a.html, "striped")}else{
                                                                                                                              if("Nota em Linguagens e Códigos" %in% input$nota){
                                                                                                                                a = read.table("sl11.txt")
                                                                                                                                a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Linguagens e Códigos do ENEM 2011")
                                                                                                                                kable_styling(a.html, "striped")}else{
                                                                                                                                  if("Nota em Matemática" %in% input$nota){
                                                                                                                                    a = read.table("smt11.txt")
                                                                                                                                    a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Matemática do ENEM 2011")
                                                                                                                                    kable_styling(a.html, "striped")}}}}}}}else{
                                                                                                                                      #2010
                                                                                                                                      if(2010 %in% input$anos){
                                                                                                                                        if("Média Geral" %in% input$nota){
                                                                                                                                          a = read.table("sm10.txt")
                                                                                                                                          a.html <- kable(a, "html", caption = "Tabela Resumo das Médias do ENEM 2010")
                                                                                                                                          kable_styling(a.html, "striped")}else{
                                                                                                                                            if("Média da Redação" %in% input$nota){
                                                                                                                                              a = read.table("sr10.txt")
                                                                                                                                              a.html <- kable(a, "html", caption = "Tabela Resumo das Médias da Redação do ENEM 2010")
                                                                                                                                              kable_styling(a.html, "striped")}else{
                                                                                                                                                if("Nota em Ciências da Natureza" %in% input$nota){
                                                                                                                                                  a = read.table("sn10.txt")
                                                                                                                                                  a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Ciências da Natureza do ENEM 2010")
                                                                                                                                                  kable_styling(a.html, "striped")}else{
                                                                                                                                                    if("Nota em Ciências Humanas" %in% input$nota){
                                                                                                                                                      a = read.table("sh10.txt")
                                                                                                                                                      a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Ciências Humanas do ENEM 2010")
                                                                                                                                                      kable_styling(a.html, "striped")}else{
                                                                                                                                                        if("Nota em Linguagens e Códigos" %in% input$nota){
                                                                                                                                                          a = read.table("sl10.txt")
                                                                                                                                                          a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Linguagens e Códigos do ENEM 2010")
                                                                                                                                                          kable_styling(a.html, "striped")}else{
                                                                                                                                                            if("Nota em Matemática" %in% input$nota){
                                                                                                                                                              a = read.table("smt10.txt")
                                                                                                                                                              a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Matemática do ENEM 2010")
                                                                                                                                                              kable_styling(a.html, "striped")}}}}}}}else{
                                                                                                                                      #todos os anos
                                                                                                                                      if("Todos os anos" %in% input$anos){
                                                                                                                                        if("Média Geral" %in% input$nota){
                                                                                                                                          a = read.table("sm.txt")
                                                                                                                                          a.html <- kable(a, "html", caption = "Tabela Resumo das Médias do ENEM")
                                                                                                                                          kable_styling(a.html, "striped")}else{
                                                                                                                                            if("Média da Redação" %in% input$nota){
                                                                                                                                              a = read.table("sr.txt")
                                                                                                                                              a.html <- kable(a, "html", caption = "Tabela Resumo das Médias da Redação do ENEM")
                                                                                                                                              kable_styling(a.html, "striped")}else{
                                                                                                                                                if("Nota em Ciências da Natureza" %in% input$nota){
                                                                                                                                                  a = read.table("sn.txt")
                                                                                                                                                  a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Ciências da Natureza do ENEM")
                                                                                                                                                  kable_styling(a.html, "striped")}else{
                                                                                                                                                    if("Nota em Ciências Humanas" %in% input$nota){
                                                                                                                                                      a = read.table("sh.txt")
                                                                                                                                                      a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Ciências Humanas do ENEM")
                                                                                                                                                      kable_styling(a.html, "striped")}else{
                                                                                                                                                        if("Nota em Linguagens e Códigos" %in% input$nota){
                                                                                                                                                          a = read.table("sl.txt")
                                                                                                                                                          a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Linguagens e Códigos do ENEM")
                                                                                                                                                          kable_styling(a.html, "striped")}else{
                                                                                                                                                            if("Nota em Matemática" %in% input$nota){
                                                                                                                                                              a = read.table("smt.txt")
                                                                                                                                                              a.html <- kable(a, "html", caption = "Tabela Resumo das Notas em Matemática do ENEM")
                                                                                                                                                              kable_styling(a.html, "striped")}}}}}}}}}}}}}}
  tab = function(){
    rm(list=ls())
    if(2015 %in% input$anos){
      if("Média Geral" %in% input$nota){
        a = read.table("sm15.txt")}else{
          if("Média da Redação" %in% input$nota){
            a = read.table("sr15.txt")}else{
              if("Nota em Ciências da Natureza" %in% input$nota){
                a = read.table("sn15.txt")}else{
                  if("Nota em Ciências Humanas" %in% input$nota){
                    a = read.table("sh15.txt")}else{
                      if("Nota em Linguagens e Códigos" %in% input$nota){
                        a = read.table("sl15.txt")}else{
                          if("Nota em Matemática" %in% input$nota){
                            a = read.table("smt15.txt")}}}}}}}else{
                              #2014
                              if(2014 %in% input$anos){
                                if("Média Geral" %in% input$nota){
                                  a = read.table("sm14.txt")}else{
                                    if("Média da Redação" %in% input$nota){
                                      a = read.table("mr14.txt")}else{
                                        if("Nota em Ciências da Natureza" %in% input$nota){
                                          a = read.table("sn14.txt")}else{
                                            if("Nota em Ciências Humanas" %in% input$nota){
                                              a = read.table("sh14.txt")}else{
                                                if("Nota em Linguagens e Códigos" %in% input$nota){
                                                  a = read.table("sl14.txt")}else{
                                                    if("Nota em Matemática" %in% input$nota){
                                                      a = read.table("smt14.txt")}}}}}}}else{
                                                        #2013
                                                        if(2013 %in% input$anos){
                                                          if("Média Geral" %in% input$nota){
                                                            a = read.table("sm13.txt")}else{
                                                              if("Média da Redação" %in% input$nota){
                                                                a = read.table("sr13.txt")}else{
                                                                  if("Nota em Ciências da Natureza" %in% input$nota){
                                                                    a = read.table("sn13.txt")}else{
                                                                      if("Nota em Ciências Humanas" %in% input$nota){
                                                                        a = read.table("sh13.txt")}else{
                                                                          if("Nota em Linguagens e Códigos" %in% input$nota){
                                                                            a = read.table("sl13.txt")}else{
                                                                              if("Nota em Matemática" %in% input$nota){
                                                                                a = read.table("smt13.txt")}}}}}}}else{
                                                                                  #2012
                                                                                  if(2012 %in% input$anos){ 
                                                                                    if("Média Geral" %in% input$nota){
                                                                                      a = read.table("sm12.txt")}else{
                                                                                        if("Média da Redação" %in% input$nota){
                                                                                          a = read.table("sr12.txt")}else{
                                                                                            if("Nota em Ciências da Natureza" %in% input$nota){
                                                                                              a = read.table("sn12.txt")}else{
                                                                                                if("Nota em Ciências Humanas" %in% input$nota){
                                                                                                  a = read.table("sh12.txt")}else{
                                                                                                    if("Nota em Linguagens e Códigos" %in% input$nota){
                                                                                                      a = read.table("sl12.txt")}else{
                                                                                                        if("Nota em Matemática" %in% input$nota){
                                                                                                          a = read.table("smt12.txt")}}}}}}}else{
                                                                                                            #2011
                                                                                                            if(2011 %in% input$anos){
                                                                                                              if("Média Geral" %in% input$nota){
                                                                                                                a = read.table("sm11.txt")}else{
                                                                                                                  if("Média da Redação" %in% input$nota){
                                                                                                                    a = read.table("sr11.txt")}else{
                                                                                                                      if("Nota em Ciências da Natureza" %in% input$nota){
                                                                                                                        a = read.table("sn11.txt")}else{
                                                                                                                          if("Nota em Ciências Humanas" %in% input$nota){
                                                                                                                            a = read.table("sh11.txt")}else{
                                                                                                                              if("Nota em Linguagens e Códigos" %in% input$nota){
                                                                                                                                a = read.table("sl11.txt")}else{
                                                                                                                                  if("Nota em Matemática" %in% input$nota){
                                                                                                                                    a = read.table("smt11.txt")}}}}}}}else{
                                                                                                                                      #2010
                                                                                                                                      if(2010 %in% input$anos){
                                                                                                                                        if("Média Geral" %in% input$nota){
                                                                                                                                          a = read.table("sm10.txt")}else{
                                                                                                                                            if("Média da Redação" %in% input$nota){
                                                                                                                                              a = read.table("sr10.txt")}else{
                                                                                                                                                if("Nota em Ciências da Natureza" %in% input$nota){
                                                                                                                                                  a = read.table("sn10.txt")}else{
                                                                                                                                                    if("Nota em Ciências Humanas" %in% input$nota){
                                                                                                                                                      a = read.table("sh10.txt")}else{
                                                                                                                                                        if("Nota em Linguagens e Códigos" %in% input$nota){
                                                                                                                                                          a = read.table("sl10.txt")}else{
                                                                                                                                                            if("Nota em Matemática" %in% input$nota){
                                                                                                                                                              a = read.table("smt10.txt")}}}}}}}
                                                                                                                                      #todos os anos
                                                                                                                                      if("Todos os anos" %in% input$anos){
                                                                                                                                        if("Média Geral" %in% input$nota){
                                                                                                                                          a = read.table("sm.txt")}else{
                                                                                                                                            if("Média da Redação" %in% input$nota){
                                                                                                                                              a = read.table("sr.txt")}else{
                                                                                                                                                if("Nota em Ciências da Natureza" %in% input$nota){
                                                                                                                                                  a = read.table("sn.txt")}else{
                                                                                                                                                    if("Nota em Ciências Humanas" %in% input$nota){
                                                                                                                                                      a = read.table("sh.txt")}else{
                                                                                                                                                        if("Nota em Linguagens e Códigos" %in% input$nota){
                                                                                                                                                          a = read.table("sl.txt")}else{
                                                                                                                                                            if("Nota em Matemática" %in% input$nota){
                                                                                                                                                              a = read.table("smt.txt")}}}}}}}}}}}}
    return(frame)} 
  output$downloadtable = downloadHandler(
    filename = function(){paste0("tabelaResumo", ".csv")},
    content = function(file){
      #frame = read.table("sm15.txt")
      tab()
      write.csv2(frame, file, row.names = F, na = "")}
  )
  }