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
            ebm15 = read.csv("ebm15.csv", header = T)
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
            ebr15 = read.csv("ebr15.csv", header = T)
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
            ebn15 = read.csv("ebn15.csv", header = T)
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
            ebh15 = read.csv("ebh15.csv", header = T)
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
            ebl15 = read.csv("ebl15.csv", header = T)
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
            ebmt15 = read.csv("ebmt15.csv", header = T)
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
            ebms15 = read.csv("ebms15.csv", header = T)
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
            ebrs15 = read.csv("ebrs15.csv", header = T)
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
            ebns15 = read.csv("ebns15.csv", header = T)
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
            ebhs15 = read.csv("ebhs15.csv", header = T)
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
            ebls15 = read.csv("ebls15.csv", header = T)
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
            ebmts15 = read.csv("ebmts15.csv", header = T)
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
            ebmr15 = read.csv("ebmr15.csv", header = T)
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
            ebrr15 = read.csv("ebrr15.csv", header = T)
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
            ebnr15 = read.csv("ebnr15.csv", header = T)
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
            ebhr15 = read.csv("ebhr15.csv", header = T)
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
            eblr15 = read.csv("eblr15.csv", header = T)
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
            ebmtr15 = read.csv("ebmtr15.csv", header = T)
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
            ebmsr15 = read.csv("ebmsr15.csv", header = T)
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
            ebrsr15 = read.csv("ebrsr15.csv", header = T)
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
            ebnsr15 = read.csv("ebnsr15.csv", header = T)
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
            ebhsr15 = read.csv("ebhsr15.csv", header = T)
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
            eblsr15 = read.csv("eblsr15.csv", header = T)
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
            ebmtsr15 = read.csv("ebmtsr15.csv", header = T)
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
            ebmd15 = read.csv("ebmd15.csv", header = T)
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
            ebrd15 = read.csv("ebrd15.csv", header = T)
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
            ebnd15 = read.csv("ebnd15.csv", header = T)
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
            ebhd15 = read.csv("ebhd15.csv", header = T)
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
            ebld15 = read.csv("ebld15.csv", header = T)
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
            ebmtd15 = read.csv("ebmtd15.csv", header = T)
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
            thm15 = read.csv("thm15.csv", header = T)
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
            thr15 = read.csv("thr15.csv", header = T)
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
            thn15 = read.csv("thn15.csv", header = T)
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
            thh15 = read.csv("thh15.csv", header = T)
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
            thl15 = read.csv("thl15.csv", header = T)
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
            thmt15 = read.csv("thmt15.csv", header = T)
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
            thms15 = read.csv("thms15.csv", header = T)
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
            thrs15 = read.csv("thrs15.csv", header = T)
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
            thns15 = read.csv("thns15.csv", header = T)
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
            thhs15 = read.csv("thhs15.csv", header = T)
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
            thls15 = read.csv("thls15.csv", header = T)
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
            thmts15 = read.csv("thmts15.csv", header = T)
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
            thmr15 = read.csv("thmr15.csv", header = T)
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
            thrr15 = read.csv("thrr15.csv", header = T)
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
            thnr15 = read.csv("thnr15.csv", header = T)
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
            thhr15 = read.csv("thhr15.csv", header = T)
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
            thlr15 = read.csv("thlr15.csv", header = T)
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
            thmtr15 = read.csv("thmtr15.csv", header = T)
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
            thmsr15 = read.csv("thmsr15.csv", header = T)
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
            thrsr15 = read.csv("thrsr15.csv", header = T)
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
            thnsr15 = read.csv("thnsr15.csv", header = T)
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
            thhsr15 = read.csv("thhsr15.csv", header = T)
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
            thlsr15 = read.csv("thlsr15.csv", header = T)
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
            thmtsr15 = read.csv("thmtsr15.csv", header = T)
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
          thmd15 = read.csv("thmd15.csv", header = T)
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
          thrd15 = read.csv("thrd15.csv", header = T)
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
          thnd15 = read.csv("thnd15.csv", header = T)
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
          thhd15 = read.csv("thhd15.csv", header = T)
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
          thld15 = read.csv("thld15.csv", header = T)
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
          thmtd15 = read.csv("thmtd15.csv", header = T)
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
            ebm14 = read.csv("ebm14.csv", header = T)
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
            ebr14 = read.csv("ebr14.csv", header = T)
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
            ebn14 = read.csv("ebn14.csv", header = T)
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
            ebh14 = read.csv("ebh14.csv", header = T)
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
            ebl14 = read.csv("ebl14.csv", header = T)
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
            ebmt14 = read.csv("ebmt14.csv", header = T)
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
            ebms14 = read.csv("ebms14.csv", header = T)
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
            ebrs14 = read.csv("ebrs14.csv", header = T)
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
            ebns14 = read.csv("ebns14.csv", header = T)
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
            ebhs14 = read.csv("ebhs14.csv", header = T)
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
            ebls14 = read.csv("ebls14.csv", header = T)
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
            ebmts14 = read.csv("ebmts14.csv", header = T)
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
            ebmr14 = read.csv("ebmr14.csv", header = T)
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
            ebrr14 = read.csv("ebrr14.csv", header = T)
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
            ebnr14 = read.csv("ebnr14.csv", header = T)
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
            ebhr14 = read.csv("ebhr14.csv", header = T)
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
            eblr14 = read.csv("eblr14.csv", header = T)
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
            ebmtr14 = read.csv("ebmtr14.csv", header = T)
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
            ebmsr14 = read.csv("ebmsr14.csv", header = T)
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
            ebrsr14 = read.csv("ebrsr14.csv", header = T)
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
            ebnsr14 = read.csv("ebnsr14.csv", header = T)
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
            ebhsr14 = read.csv("ebhsr14.csv", header = T)
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
            eblsr14 = read.csv("eblsr14.csv", header = T)
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
            ebmtsr14 = read.csv("ebmtsr14.csv", header = T)
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
            ebmd14 = read.csv("ebmd14.csv", header = T)
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
            ebrd14 = read.csv("ebrd14.csv", header = T)
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
            ebnd14 = read.csv("ebnd14.csv", header = T)
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
            ebhd14 = read.csv("ebhd14.csv", header = T)
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
            ebld14 = read.csv("ebld14.csv", header = T)
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
            ebmtd14 = read.csv("ebmtd14.csv", header = T)
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
            thm14 = read.csv("thm14.csv", header = T)
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
            thr14 = read.csv("thr14.csv", header = T)
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
            thn14 = read.csv("thn14.csv", header = T)
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
            thh14 = read.csv("thh14.csv", header = T)
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
            thl14 = read.csv("thl14.csv", header = T)
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
            thmt14 = read.csv("thmt14.csv", header = T)
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
            thms14 = read.csv("thms14.csv", header = T)
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
            thrs14 = read.csv("thrs14.csv", header = T)
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
            thns14 = read.csv("thns14.csv", header = T)
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
            thhs14 = read.csv("thhs14.csv", header = T)
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
            thls14 = read.csv("thls14.csv", header = T)
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
            thmts14 = read.csv("thmts14.csv", header = T)
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
            thmr14 = read.csv("thmr14.csv", header = T)
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
            thrr14 = read.csv("thrr14.csv", header = T)
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
            thnr14 = read.csv("thnr14.csv", header = T)
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
            thhr14 = read.csv("thhr14.csv", header = T)
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
            thlr14 = read.csv("thlr14.csv", header = T)
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
            thmtr14 = read.csv("thmtr14.csv", header = T)
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
            thmsr14 = read.csv("thmsr14.csv", header = T)
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
            thrsr14 = read.csv("thrsr14.csv", header = T)
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
            thnsr14 = read.csv("thnsr14.csv", header = T)
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
            thhsr14 = read.csv("thhsr14.csv", header = T)
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
            thlsr14 = read.csv("thlsr14.csv", header = T)
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
            thmtsr14 = read.csv("thmtsr14.csv", header = T)
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
            thmd14 = read.csv("thmd14.csv", header = T)
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
            thrd14 = read.csv("thrd14.csv", header = T)
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
            thnd14 = read.csv("thnd14.csv", header = T)
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
            thhd14 = read.csv("thhd14.csv", header = T)
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
            thld14 = read.csv("thld14.csv", header = T)
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
            thmtd14 = read.csv("thmtd14.csv", header = T)
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
            ebm13 = read.csv("ebm13.csv", header = T)
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
            ebr13 = read.csv("ebr13.csv", header = T)
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
            ebn13 = read.csv("ebn13.csv", header = T)
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
            ebh13 = read.csv("ebh13.csv", header = T)
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
            ebl13 = read.csv("ebl13.csv", header = T)
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
            ebmt13 = read.csv("ebmt13.csv", header = T)
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
            ebms13 = read.csv("ebms13.csv", header = T)
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
            ebrs13 = read.csv("ebrs13.csv", header = T)
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
            ebns13 = read.csv("ebns13.csv", header = T)
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
            ebhs13 = read.csv("ebhs13.csv", header = T)
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
            ebls13 = read.csv("ebls13.csv", header = T)
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
            ebmts13 = read.csv("ebmts13.csv", header = T)
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
            ebmr13 = read.csv("ebmr13.csv", header = T)
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
            ebrr13 = read.csv("ebrr13.csv", header = T)
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
            ebnr13 = read.csv("ebnr13.csv", header = T)
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
            ebhr13 = read.csv("ebhr13.csv", header = T)
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
            eblr13 = read.csv("eblr13.csv", header = T)
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
            ebmtr13 = read.csv("ebmtr13.csv", header = T)
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
            ebmsr13 = read.csv("ebmsr13.csv", header = T)
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
            ebrsr13 = read.csv("ebrsr13.csv", header = T)
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
            ebnsr13 = read.csv("ebnsr13.csv", header = T)
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
            ebhsr13 = read.csv("ebhsr13.csv", header = T)
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
            eblsr13 = read.csv("eblsr13.csv", header = T)
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
            ebmtsr13 = read.csv("ebmtsr13.csv", header = T)
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
            ebmd13 = read.csv("ebmd13.csv", header = T)
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
            ebrd13 = read.csv("ebrd13.csv", header = T)
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
            ebnd13 = read.csv("ebnd13.csv", header = T)
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
            ebhd13 = read.csv("ebhd13.csv", header = T)
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
            ebld13 = read.csv("ebld13.csv", header = T)
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
            ebmtd13 = read.csv("ebmtd13.csv", header = T)
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
            thm13 = read.csv("thm13.csv", header = T)
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
            thr13 = read.csv("thr13.csv", header = T)
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
            thn13 = read.csv("thn13.csv", header = T)
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
            thh13 = read.csv("thh13.csv", header = T)
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
            thl13 = read.csv("thl13.csv", header = T)
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
            thmt13 = read.csv("thmt13.csv", header = T)
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
            thms13 = read.csv("thms13.csv", header = T)
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
            thrs13 = read.csv("thrs13.csv", header = T)
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
            thns13 = read.csv("thns13.csv", header = T)
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
            thhs13 = read.csv("thhs13.csv", header = T)
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
            thls13 = read.csv("thls13.csv", header = T)
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
            thmts13 = read.csv("thmts13.csv", header = T)
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
            thmr13 = read.csv("thmr13.csv", header = T)
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
            thrr13 = read.csv("thrr13.csv", header = T)
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
            thnr13 = read.csv("thnr13.csv", header = T)
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
            thhr13 = read.csv("thhr13.csv", header = T)
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
            thlr13 = read.csv("thlr13.csv", header = T)
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
            thmtr13 = read.csv("thmtr13.csv", header = T)
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
            thmsr13 = read.csv("thmsr13.csv", header = T)
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
            thrsr13 = read.csv("thrsr13.csv", header = T)
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
            thnsr13 = read.csv("thnsr13.csv", header = T)
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
            thhsr13 = read.csv("thhsr13.csv", header = T)
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
            thlsr13 = read.csv("thlsr13.csv", header = T)
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
            thmtsr13 = read.csv("thmtsr13.csv", header = T)
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
            thmd13 = read.csv("thmd13.csv", header = T)
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
            thrd13 = read.csv("thrd13.csv", header = T)
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
            thnd13 = read.csv("thnd13.csv", header = T)
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
            thhd13 = read.csv("thhd13.csv", header = T)
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
            thld13 = read.csv("thld13.csv", header = T)
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
            thmtd13 = read.csv("thmtd13.csv", header = T)
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
            ebm12 = read.csv("ebm12.csv", header = T)
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
            ebr12 = read.csv("ebr12.csv", header = T)
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
            ebn12 = read.csv("ebn12.csv", header = T)
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
            ebh12 = read.csv("ebh12.csv", header = T)
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
            ebl12 = read.csv("ebl12.csv", header = T)
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
            ebmt12 = read.csv("ebmt12.csv", header = T)
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
            ebms12 = read.csv("ebms12.csv", header = T)
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
            ebrs12 = read.csv("ebrs12.csv", header = T)
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
            ebns12 = read.csv("ebns12.csv", header = T)
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
            ebhs12 = read.csv("ebhs12.csv", header = T)
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
            ebls12 = read.csv("ebls12.csv", header = T)
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
            ebmts12 = read.csv("ebmts12.csv", header = T)
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
            ebmr12 = read.csv("ebmr12.csv", header = T)
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
            ebrr12 = read.csv("ebrr12.csv", header = T)
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
            ebnr12 = read.csv("ebnr12.csv", header = T)
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
            ebhr12 = read.csv("ebhr12.csv", header = T)
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
            eblr12 = read.csv("eblr12.csv", header = T)
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
            ebmtr12 = read.csv("ebmtr12.csv", header = T)
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
            ebmsr12 = read.csv("ebmsr12.csv", header = T)
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
            ebrsr12 = read.csv("ebrsr12.csv", header = T)
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
            ebnsr12 = read.csv("ebnsr12.csv", header = T)
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
            ebhsr12 = read.csv("ebhsr12.csv", header = T)
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
            eblsr12 = read.csv("eblsr12.csv", header = T)
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
            ebmtsr12 = read.csv("ebmtsr12.csv", header = T)
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
            ebmd12 = read.csv("ebmd12.csv", header = T)
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
            ebrd12 = read.csv("ebrd12.csv", header = T)
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
            ebnd12 = read.csv("ebnd12.csv", header = T)
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
            ebhd12 = read.csv("ebhd12.csv", header = T)
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
            ebld12 = read.csv("ebld12.csv", header = T)
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
            ebmtd12 = read.csv("ebmtd12.csv", header = T)
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
            thm12 = read.csv("thm12.csv", header = T)
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
            thr12 = read.csv("thr12.csv", header = T)
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
            thn12 = read.csv("thn12.csv", header = T)
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
            thh12 = read.csv("thh12.csv", header = T)
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
            thl12 = read.csv("thl12.csv", header = T)
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
            thmt12 = read.csv("thmt12.csv", header = T)
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
            thms12 = read.csv("thms12.csv", header = T)
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
            thrs12 = read.csv("thrs12.csv", header = T)
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
            thns12 = read.csv("thns12.csv", header = T)
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
            thhs12 = read.csv("thhs12.csv", header = T)
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
            thls12 = read.csv("thls12.csv", header = T)
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
            thmts12 = read.csv("thmts12.csv", header = T)
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
            thmr12 = read.csv("thmr12.csv", header = T)
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
            thrr12 = read.csv("thrr12.csv", header = T)
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
            thnr12 = read.csv("thnr12.csv", header = T)
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
            thhr12 = read.csv("thhr12.csv", header = T)
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
            thlr12 = read.csv("thlr12.csv", header = T)
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
            thmtr12 = read.csv("thmtr12.csv", header = T)
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
            thmsr12 = read.csv("thmsr12.csv", header = T)
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
            thrsr12 = read.csv("thrsr12.csv", header = T)
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
            thnsr12 = read.csv("thnsr12.csv", header = T)
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
            thhsr12 = read.csv("thhsr12.csv", header = T)
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
            thlsr12 = read.csv("thlsr12.csv", header = T)
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
            thmtsr12 = read.csv("thmtsr12.csv", header = T)
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
            thmd12 = read.csv("thmd12.csv", header = T)
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
            thrd12 = read.csv("thrd12.csv", header = T)
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
            thnd12 = read.csv("thnd12.csv", header = T)
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
            thhd12 = read.csv("thhd12.csv", header = T)
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
            thld12 = read.csv("thld12.csv", header = T)
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
            thmtd12 = read.csv("thmtd12.csv", header = T)
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
            ebm11 = read.csv("ebm11.csv", header = T)
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
            ebr11 = read.csv("ebr11.csv", header = T)
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
            ebn11 = read.csv("ebn11.csv", header = T)
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
            ebh11 = read.csv("ebh11.csv", header = T)
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
            ebl11 = read.csv("ebl11.csv", header = T)
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
            ebmt11 = read.csv("ebmt11.csv", header = T)
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
            ebms11 = read.csv("ebms11.csv", header = T)
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
            ebrs11 = read.csv("ebrs11.csv", header = T)
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
            ebns11 = read.csv("ebns11.csv", header = T)
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
            ebhs11 = read.csv("ebhs11.csv", header = T)
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
            ebls11 = read.csv("ebls11.csv", header = T)
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
            ebmts11 = read.csv("ebmts11.csv", header = T)
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
            ebmr11 = read.csv("ebmr11.csv", header = T)
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
            ebrr11 = read.csv("ebrr11.csv", header = T)
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
            ebnr11 = read.csv("ebnr11.csv", header = T)
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
            ebhr11 = read.csv("ebhr11.csv", header = T)
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
            eblr11 = read.csv("eblr11.csv", header = T)
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
            ebmtr11 = read.csv("ebmtr11.csv", header = T)
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
            ebmsr11 = read.csv("ebmsr11.csv", header = T)
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
            ebrsr11 = read.csv("ebrsr11.csv", header = T)
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
            ebnsr11 = read.csv("ebnsr11.csv", header = T)
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
            ebhsr11 = read.csv("ebhsr11.csv", header = T)
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
            eblsr11 = read.csv("eblsr11.csv", header = T)
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
            ebmtsr11 = read.csv("ebmtsr11.csv", header = T)
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
            ebmd11 = read.csv("ebmd11.csv", header = T)
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
            ebrd11 = read.csv("ebrd11.csv", header = T)
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
            ebnd11 = read.csv("ebnd11.csv", header = T)
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
            ebhd11 = read.csv("ebhd11.csv", header = T)
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
            ebld11 = read.csv("ebld11.csv", header = T)
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
            ebmtd11 = read.csv("ebmtd11.csv", header = T)
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
            thm11 = read.csv("thm11.csv", header = T)
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
            thr11 = read.csv("thr11.csv", header = T)
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
            thn11 = read.csv("thn11.csv", header = T)
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
            thh11 = read.csv("thh11.csv", header = T)
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
            thl11 = read.csv("thl11.csv", header = T)
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
            thmt11 = read.csv("thmt11.csv", header = T)
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
            thms11 = read.csv("thms11.csv", header = T)
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
            thrs11 = read.csv("thrs11.csv", header = T)
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
            thns11 = read.csv("thns11.csv", header = T)
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
            thhs11 = read.csv("thhs11.csv", header = T)
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
            thls11 = read.csv("thls11.csv", header = T)
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
            thmts11 = read.csv("thmts11.csv", header = T)
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
            thmr11 = read.csv("thmr11.csv", header = T)
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
            thrr11 = read.csv("thrr11.csv", header = T)
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
            thnr11 = read.csv("thnr11.csv", header = T)
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
            thhr11 = read.csv("thhr11.csv", header = T)
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
            thlr11 = read.csv("thlr11.csv", header = T)
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
            thmtr11 = read.csv("thmtr11.csv", header = T)
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
            thmsr11 = read.csv("thmsr11.csv", header = T)
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
            thrsr11 = read.csv("thrsr11.csv", header = T)
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
            thnsr11 = read.csv("thnsr11.csv", header = T)
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
            thhsr11 = read.csv("thhsr11.csv", header = T)
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
            thlsr11 = read.csv("thlsr11.csv", header = T)
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
            thmtsr11 = read.csv("thmtsr11.csv", header = T)
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
            thmd11 = read.csv("thmd11.csv", header = T)
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
            thrd11 = read.csv("thrd11.csv", header = T)
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
            thnd11 = read.csv("thnd11.csv", header = T)
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
            thhd11 = read.csv("thhd11.csv", header = T)
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
            thld11 = read.csv("thld11.csv", header = T)
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
            thmtd11 = read.csv("thmtd11.csv", header = T)
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
            ebm10 = read.csv("ebm10.csv", header = T)
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
            ebr10 = read.csv("ebr10.csv", header = T)
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
            ebn10 = read.csv("ebn10.csv", header = T)
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
            ebh10 = read.csv("ebh10.csv", header = T)
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
            ebl10 = read.csv("ebl10.csv", header = T)
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
            ebmt10 = read.csv("ebmt10.csv", header = T)
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
            ebms10 = read.csv("ebms10.csv", header = T)
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
            ebrs10 = read.csv("ebrs10.csv", header = T)
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
            ebns10 = read.csv("ebns10.csv", header = T)
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
            ebhs10 = read.csv("ebhs10.csv", header = T)
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
            ebls10 = read.csv("ebls10.csv", header = T)
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
            ebmts10 = read.csv("ebmts10.csv", header = T)
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
            ebmr10 = read.csv("ebmr10.csv", header = T)
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
            ebrr10 = read.csv("ebrr10.csv", header = T)
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
            ebnr10 = read.csv("ebnr10.csv", header = T)
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
            ebhr10 = read.csv("ebhr10.csv", header = T)
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
            eblr10 = read.csv("eblr10.csv", header = T)
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
            ebmtr10 = read.csv("ebmtr10.csv", header = T)
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
            ebmsr10 = read.csv("ebmsr10.csv", header = T)
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
            ebrsr10 = read.csv("ebrsr10.csv", header = T)
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
            ebnsr10 = read.csv("ebnsr10.csv", header = T)
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
            ebhsr10 = read.csv("ebhsr10.csv", header = T)
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
            eblsr10 = read.csv("eblsr10.csv", header = T)
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
            ebmtsr10 = read.csv("ebmtsr10.csv", header = T)
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
            ebmd10 = read.csv("ebmd10.csv", header = T)
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
            ebrd10 = read.csv("ebrd10.csv", header = T)
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
            ebnd10 = read.csv("ebnd10.csv", header = T)
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
            ebhd10 = read.csv("ebhd10.csv", header = T)
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
            ebld10 = read.csv("ebld10.csv", header = T)
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
            ebmtd10 = read.csv("ebmtd10.csv", header = T)
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
            thm10 = read.csv("thm10.csv", header = T)
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
            thr10 = read.csv("thr10.csv", header = T)
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
            thn10 = read.csv("thn10.csv", header = T)
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
            thh10 = read.csv("thh10.csv", header = T)
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
            thl10 = read.csv("thl10.csv", header = T)
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
            thmt10 = read.csv("thmt10.csv", header = T)
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
            thms10 = read.csv("thms10.csv", header = T)
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
            thrs10 = read.csv("thrs10.csv", header = T)
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
            thns10 = read.csv("thns10.csv", header = T)
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
            thhs10 = read.csv("thhs10.csv", header = T)
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
            thls10 = read.csv("thls10.csv", header = T)
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
            thmts10 = read.csv("thmts10.csv", header = T)
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
            thmr10 = read.csv("thmr10.csv", header = T)
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
            thrr10 = read.csv("thrr10.csv", header = T)
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
            thnr10 = read.csv("thnr10.csv", header = T)
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
            thhr10 = read.csv("thhr10.csv", header = T)
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
            thlr10 = read.csv("thlr10.csv", header = T)
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
            thmtr10 = read.csv("thmtr10.csv", header = T)
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
            thmsr10 = read.csv("thmsr10.csv", header = T)
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
            thrsr10 = read.csv("thrsr10.csv", header = T)
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
            thnsr10 = read.csv("thnsr10.csv", header = T)
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
            thhsr10 = read.csv("thhsr10.csv", header = T)
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
            thlsr10 = read.csv("thlsr10.csv", header = T)
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
            thmtsr10 = read.csv("thmtsr10.csv", header = T)
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
            thmd10 = read.csv("thmd10.csv", header = T)
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
            thrd10 = read.csv("thrd10.csv", header = T)
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
            thnd10 = read.csv("thnd10.csv", header = T)
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
            thhd10 = read.csv("thhd10.csv", header = T)
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
            thld10 = read.csv("thld10.csv", header = T)
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
            thmtd10 = read.csv("thmtd10.csv", header = T)
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
            ebm = read.csv("ebm.csv", header = T)
            bm = ggplot(ebm, aes(x = e, y = out, middle = median,
                                  ymin = lower.whisker, ymax = upper.whisker,
                                  lower = lower.hinge, upper = upper.hinge,
                                  fill = e, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Ano",y="Média no ENEM",
                   title = "Boxplot das Médias no ENEM Segundo o Ano") +
              theme(legend.position="none")
            print(bm)
          }}}}
    if("Média da Redação" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebr = read.csv("ebr.csv", header = T)
            br = ggplot(ebr, aes(x = e, y = out, middle = median,
                               ymin = lower.whisker, ymax = upper.whisker,
                               lower = lower.hinge, upper = upper.hinge,
                               fill = e, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Ano",y="Média da Redação no ENEM",
                   title = "Boxplot das Médias da Redação no ENEM Segundo o Ano") +
              theme(legend.position="none")
            print(br)
          }}}}
    if("Nota em Ciências da Natureza" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebn = read.csv("ebn.csv", header = T)
            bn = ggplot(ebn, aes(x = e, y = out, middle = median,
                               ymin = lower.whisker, ymax = upper.whisker,
                               lower = lower.hinge, upper = upper.hinge,
                               fill = e, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Ano",y="Nota em Ciências da Natureza no ENEM",
                   title = "Boxplot das Notas em Ciências da Natureza no ENEM Segundo o Ano") +
              theme(legend.position="none")
            print(bn)
          }}}}
    if("Nota em Ciências Humanas" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebh = read.csv("ebh.csv", header = T)
            bh = ggplot(enh, aes(x = e, y = out, middle = median,
                               ymin = lower.whisker, ymax = upper.whisker,
                               lower = lower.hinge, upper = upper.hinge,
                               fill = e, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Ano",y="Nota em Ciências Humanas no ENEM",
                   title = "Boxplot das Notas em Ciências Humanas no ENEM Segundo o Ano") +
              theme(legend.position="none")
            print(bh)
          }}}}
    if("Nota em Linguagens e Códigos" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebl = read.csv("ebl.csv", header = T)
            bl = ggplot(ebl, aes(x = e, y = out, middle = median,
                               ymin = lower.whisker, ymax = upper.whisker,
                               lower = lower.hinge, upper = upper.hinge,
                               fill = e, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Ano",y="Nota em Linguagens e Códigos no ENEM",
                   title = "Boxplot das Notas em Linguagens e Códigos no ENEM Segundo o Ano") +
              theme(legend.position="none")
            print(bl)
          }}}}
    if("Nota em Matemática" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Brasil" %in% input$vcategorica){
            rm(list = ls())
            ebmt = read.csv("ebmt.csv", header = T)
            bmt = ggplot(ebmt, aes(x = e, y = out, middle = median,
                               ymin = lower.whisker, ymax = upper.whisker,
                               lower = lower.hinge, upper = upper.hinge,
                               fill = e, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() + theme_bw() +
              labs(x="Ano",y="Nota em Matemática no ENEM",
                   title = "Boxplot das Notas em Matemática no ENEM Segundo o Ano") +
              theme(legend.position="none")
            print(bmt)
          }}}}
    
    ## Por sexo
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo" %in% input$vcategorica){
            rm(list = ls())
            ebms = read.csv("ebms.csv", header = T)
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
            ebrs = read.csv("ebrs.csv", header = T)
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
            ebns = read.csv("ebns.csv", header = T)
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
            ebhs = read.csv("ebhs.csv", header = T)
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
            ebls = read.csv("ebls.csv", header = T)
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
            ebmts = read.csv("ebmts.csv", header = T)
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
            ebmr = read.csv("ebmr.csv", header = T)
            bmr = ggplot(ebmr, aes(x = reg, y = out, middle = median,
                                   ymin = lower.whisker, ymax = upper.whisker,
                                   lower = lower.hinge, upper = upper.hinge,
                                   fill = reg, alpha=0.85)) +
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
            ebrr = read.csv("ebrr.csv", header = T)
            brr = ggplot(ebrr, aes(x = reg, y = out, middle = median,
                                   ymin = lower.whisker, ymax = upper.whisker,
                                   lower = lower.hinge, upper = upper.hinge,
                                   fill = reg, alpha=0.85)) +
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
            ebnr = read.csv("ebnr.csv", header = T)
            bnr = ggplot(ebnr, aes(x = reg, y = out, middle = median,
                                   ymin = lower.whisker, ymax = upper.whisker,
                                   lower = lower.hinge, upper = upper.hinge,
                                   fill = reg, alpha=0.85)) +
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
            ebhr = read.csv("ebhr.csv", header = T)
            bhr = ggplot(ebhr, aes(x = reg, y = out, middle = median,
                                   ymin = lower.whisker, ymax = upper.whisker,
                                   lower = lower.hinge, upper = upper.hinge,
                                   fill = reg, alpha=0.85)) +
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
            eblr = read.csv("eblr.csv", header = T)
            blr = ggplot(eblr, aes(x = reg, y = out, middle = median,
                                   ymin = lower.whisker, ymax = upper.whisker,
                                   lower = lower.hinge, upper = upper.hinge,
                                   fill = reg, alpha=0.85)) +
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
            ebmtr = read.csv("ebmtr.csv", header = T)
            bmtr = ggplot(ebmtr, aes(x = reg, y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     fill = reg, alpha=0.85)) +
              geom_boxplot(stat = "identity") + geom_point() + coord_flip() +
              labs(x="Região",y="Nota em Matemática no ENEM",
                   title="Boxplot das Notas em Matemática no ENEM Segundo o Ano e Região")+
              theme_bw() + theme(legend.position="none") + facet_grid(~e)
            print(bmts)
          }}}}
    
    ## por sexo e região
    if("Média Geral" %in% input$nota){
      if("Boxplot" %in% input$grafico){
        if("Todos os anos" %in% input$anos){
          if("Por sexo e região" %in% input$vcategorica){
            rm(list = ls())
            ebmsr = read.csv("ebmsr.csv", header = T)
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
            ebrsr = read.csv("ebrsr.csv", header = T)
            brsr = ggplot(ebrsr, aes(x = sexo, y = out, middle = median,
                                     ymin = lower.whisker, ymax = upper.whisker,
                                     lower = lower.hinge, upper = upper.hinge,
                                     fill = sexo, alpha=0.85)) +
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
            ebnsr = read.csv("ebnsr.csv", header = T)
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
            ebhsr = read.csv("ebhsr.csv", header = T)
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
            eblsr = read.csv("eblsr.csv", header = T)
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
            ebmtsr = read.csv("ebmtsr.csv", header = T)
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
            ebmd = read.csv("ebmd.csv", header = T)
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
            ebrd = read.csv("ebrd.csv", header = T)
            brd = ggplot(ebrd, aes(x = renda, y = out, middle = median,
                                ymin = lower.whisker, ymax = upper.whisker,
                                lower = lower.hinge, upper = upper.hinge,
                                fill = renda, alpha=0.85)) +
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
            ebnd = read.csv("ebnd.csv", header = T)
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
            ebhd = read.csv("ebhd.csv", header = T)
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
            ebld = read.csv("ebld.csv", header = T)
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
            ebmtd = read.csv("ebmtd.csv", header = T)
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
            thm = read.csv("thm.csv", header = T)
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
            thr = read.csv("thr.csv", header = T)
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
            thn = read.csv("thn.csv", header = T)
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
            thh = read.csv("thh.csv", header = T)
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
            thl = read.csv("thl.csv", header = T)
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
            thmt = read.csv("thmt.csv", header = T)
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
            thms = read.csv("thms.csv", header = T)
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
            thrs = read.csv("thrs.csv", header = T)
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
            thns = read.csv("thns.csv", header = T)
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
            thhs = read.csv("thhs.csv", header = T)
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
            thls = read.csv("thls.csv", header = T)
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
            thmts = read.csv("thmts.csv", header = T)
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
            thmr = read.csv("thmr.csv", header = T)
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
            thrr = read.csv("thrr.csv", header = T)
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
            thnr = read.csv("thnr.csv", header = T)
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
            thhr = read.csv("thhr.csv", header = T)
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
            thlr = read.csv("thlr.csv", header = T)
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
            thmtr = read.csv("thmtr.csv", header = T)
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
            thmsr = read.csv("thmsr.csv", header = T)
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
            thrsr = read.csv("thrsr.csv", header = T)
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
            thnsr = read.csv("thnsr.csv", header = T)
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
            thhsr = read.csv("thhsr.csv", header = T)
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
            thlsr = read.csv("thlsr.csv", header = T)
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
            thmtsr = read.csv("thmtsr.csv", header = T)
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
            thmd = read.csv("thmd.csv", header = T)
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
            thrd = read.csv("thrd.csv", header = T)
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
            thnd = read.csv("thnd.csv", header = T)
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
            thhd = read.csv("thhd.csv", header = T)
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
            thld = read.csv("thld.csv", header = T)
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
            thmtd = read.csv("thmtd.csv", header = T)
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
                                                                                                                                                              kable_styling(a.html, "striped")}}}}}}}
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
                                                                                                                                                              kable_styling(a.html, "striped")}}}}}}}}}}}}}
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