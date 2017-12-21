## O aplicativo
É o resultado do trabalho de conclusão do curso de Estatística de Marylaine Nascimento. 

Tem como objetivo facilitar a visualização dos resultados do ENEM apresentando as médias gerais e as notas por área de conhecimento dos inscritos no ENEM de 2010 a 2015. O aplicativo apresenta as notas através de boxplots e histogramas, acompanhados de tabela com alguns parâmetros. Além disso, as notas poderão ser visualizadas separadas por sexo, região, sexo e região e renda, onde as categorias de renda são as mesmas utilizadas pelo INEP. Para atender o público leigo em estatística o aplicativo conta com uma aba para auxiliar o entendimento e interpretação dos gráficos.

A monografria pode ser acessada neste link: [http://monografias.ufrn.br/jspui/handle/123456789/5427](http://monografias.ufrn.br/jspui/handle/123456789/5427)

Os scripts utilizados estão disponíveis neste link: [https://github.com/Marylaine/Visualiza-o-dos-Resultados-do-ENEM-2010-a-2015-.git](https://github.com/Marylaine/Visualiza-o-dos-Resultados-do-ENEM-2010-a-2015-.git)


## Autores

**Marylaine Nascimento** é graduanda em estatística na UFRN. Possui interesse em visualização de dados, *business intelligence*, *Big Data* e esferas magnéticas. 

Contato: marylainen@gmail.com

**Marcus Nunes** é Professor Adjunto na Universidade Federal do Rio Grande do Norte. É Doutor em Estatística e tem interesse nas áreas de modelos lineares generalizados, *machine learning*, visualização de dados e divulgação científica.

Site: [http://marcusnunes.me/](http://marcusnunes.me/)

### Algumas Observações:

- O Inep atribui nota zero na redação mesmo para os ausentes, exceto  na edição de 2012, isso pode explicar a alta proporção na primeira classe dos histogramas da média da redação.

- Na edição de 2010, o estado de residência do inscrito não era uma pergunta obrigatória. Logo, os inscritos sem identificação na região foram ignorados na geração dos gráficos com o grupo região e o grupo sexo e região. Enquanto que na tabela estes casos constam como **NA** na coluna região.

- Os histogramas por sexo e região mostram pouca diferença na proporção de notas entre os sexos dentro de cada região. Parecia ser um erro no ggplot, mas foi verificado e o resultado é este de fato. (Veja Paradoxo de Simpson)

- Apesar do nosso esforço em acelerar a geração dos gráficos, na opção "Todos os anos" poderá apresentar alguma demora no carregamento, isso devido à maior quantidade de dados, visto que inclui todos os seis anos disponíveis.

- Os histogramas da renda para todos os anos não seguem a mesma legenda, pois o Inep altera a classificação todos os anos.
