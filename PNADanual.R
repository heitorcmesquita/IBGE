install.packages("PNADcIBGE")
install.packages("survey")

library("PNADcIBGE")
library("survey")

library(PNADcIBGE)
library(survey)

#download dados PNADc Anual
PNADc2019 <- get_pnadc(year = 2019, interview = 1)
PNADc2016 <- get_pnadc(year=201, topic=2, labels=TRUE, deflator=TRUE, design=TRUE)
PNADc2019E <- get_pnadc(year=2019, topic=2, labels=TRUE, deflator=TRUE, design=TRUE)

#CAPITAL HUMANO - PEA com Ensino Superior


populacaoE<-svytotal(~V2007,subset(PNADc2019E,Capital=="Município de Maceió (AL)"),na.rm=T)

populacao<-svytotal(~V2007,subset(PNADc2019,Capital=="Município de Maceió (AL)"),na.rm=T)

populacaoE

populacao


populacao1529<-svytotal(~V2007,subset(PNADc2019E,Capital=="Município de Maceió (AL)"),na.rm=T)

populacao1529<-svytotal(~V2007,subset(PNADc2019,Capital=="Município de Maceió (AL)"
                                  &V2009>=15&V2009<=29),na.rm=T)

populacaoE

populacao


nem_nem<-svytotal(~V2007,subset(PNADc2019,Capital=="Município de Maceió (AL)"
                                &V2009>=15&V2009<=29
                                &VD4002=="Pessoas desocupadas"
                                &V3002=="Não"|
                                  Capital=="Município de Maceió (AL)"
                                &V2009>=15&V2009<=29
                                &VD4001=="Pessoas fora da força de trabalho"
                                &V3002=="Não"),na.rm=T)


nem_nem_e<-svytotal(~V2007,subset(PNADc2019E,Capital=="Município de Maceió (AL)"
                                &V2009>=15&V2009<=29
                                &VD4002=="Pessoas desocupadas"
                                &V3002=="Não"|
                                  Capital=="Município de Maceió (AL)"
                                &V2009>=15&V2009<=29
                                &VD4001=="Pessoas fora da força de trabalho"
                                &V3002=="Não"
                                  ),na.rm=T)

nem_nem

nem_nem_e

populacao

populacaoE
pea15estudo<-svyby(~VD3005,~UF,subset(PNADc2019,V2009>=14&VD4002=="Pessoas ocupadas"
                                 |V2009>=14&VD4002=="Pessoas desocupadas"),svytotal,na.rm=T)


write.csv(pea15estudo,file="pea15estudo.csv")

pea<-svyby(~VD4002,~UF,subset(PNADc2019, V2009>=14),svytotal,na.rm=T)

write.csv(pea,file="pea.csv")

#INFRAESTRUTURA - Acesso a Energia Elétrica

dom_energia<-svyby(~S01014,~UF,subset(PNADc2019,V2005=="Pessoa respons?vel pelo domic?lio"),
                   svytotal,na.rm=T)
write.csv(dom_energia,file="dom_energia.csv")

#SUSTENTABILIDADE SOCIAL - Acesso ao Saneamento Básico - Água

dom_agua<-svyby(~S01007, ~UF,subset(PNADc2019,V2005=="Pessoa respons?vel pelo domic?lio"),
                svytotal,na.rm=T)
write.csv(dom_agua,file="dom_agua.csv")

#SUSTENTABILIDADE SOCIAL - Acesso ao Saneamento Básico - Esgoto

dom_esgoto<-svyby(~S01012A, ~UF,subset(PNADc2019,V2005=="Pessoa respons?vel pelo domic?lio"),
                svytotal,na.rm=T)
write.csv(dom_esgoto,file="dom_esgoto.csv")

#SUSTENTABILIDADE SOCIAL - Formalidade no Mercado de Trabalho

merc_total<-svyby(~VD4009,~UF,PNADc2019,svytotal,na.rm=T)

merc_t_c<-svyby(~VD4009,~UF,subset(PNADc2019,VD4012=="Contribuinte"
                               ),svytotal,na.rm=T)

ocupadas<-svyby(~VD4002,~UF,PNADc2019,svytotal,na.rm=T)

write.csv(merc_total,file="merc_total.csv")

write.csv(merc_t_c,file="merc_t_c.csv")

write.csv(ocupadas,file="ocupadas.csv")

#SUSTENTABILIDADE SOCIAL - Inserção Econômica de Jovens


n_estudam_o<-svytotal(~UF,subset(PNADc2019, V2009>=15&V2009<=22
                                 &VD4002=="Pessoas ocupadas"&V3002=="N?o")
              ,na.rm=T)

estudam<-svytotal(~UF,subset(PNADc2019, V2009>=15&V2009<=22
                               &V3002=="Sim"),na.rm=T)


jovens<-svytotal(~UF,subset(PNADc2019, V2009>=15&V2009<=22),na.rm=T)

write.csv(n_estudam_o,file="n_estudam_o.csv")

write.csv(estudam,file="estudam.csv")

write.csv(jovens,file="jovens.csv")

#jovens 15 a 17 Alagoas (suplemento educação - lembrar de fazer download corretamente)

jovens_e18<-svyby(~V3002, ~VD3004,subset(PNADc2018, V2009>=15&V2009<=17&UF=="Alagoas"),svytotal,
              na.rm=T)

jovens_e19<-svyby(~V3002, ~VD3004,subset(PNADc2019, V2009>=15&V2009<=17&UF=="Alagoas"),svytotal,
                  na.rm=T)

write.csv(jovens_e18,file="jovens_e18.csv")
write.csv(jovens_e19,file="jovens_e19.csv")

jovens_t18<-svyby(~V2007,~UF,subset(PNADc2018, V2009>=15&V2009<=17&UF=="Alagoas"),svytotal,
na.rm=T)

jovens_t19<-svyby(~V2007,~UF,subset(PNADc2019, V2009>=15&V2009<=17&UF=="Alagoas"),svytotal,
                  na.rm=T)

EM18<-svyby(~V3003A,~UF,subset(PNADc2018, V2009>=15&V2009<=17&UF=="Alagoas"),svytotal,
                    na.rm=T)

EM19<-svyby(~V3003A,~UF,subset(PNADc2019, V2009>=15&V2009<=17&UF=="Alagoas"),svytotal,
            na.rm=T)

write.csv(EM18,file="EM18.csv")
write.csv(EM19,file="EM19.csv")

#criancas 0 a 5 Alagoas (suplemento educação - lembrar de fazer download corretamente)

criancas_e18<-svyby(~VD3004,~UF,subset(PNADc2018, V2009>=0&V2009<=5&V3002=="Sim"
                                       |V2009>=0&V2009<=5&V3002=="Não"),svytotal,
                  na.rm=T)

criancas_e19<-svyby(~VD3004,~UF,subset(PNADc2019, V2009>=0&V2009<=5&V3002=="Sim"
                                       |V2009>=0&V2009<=5&V3002=="Não"),svytotal,
                    na.rm=T)

write.csv(criancas_e18,file="criancas_e18.csv")
write.csv(criancas_e19,file="criancas_e19.csv")

criancas_t16<-svyby(~V2007,~UF,subset(PNADc2016, V2009>=0&V2009<=5),svytotal,
                    na.rm=T)

criancas_t18<-svyby(~V2007,~UF,subset(PNADc2018, V2009>=0&V2009<=5),svytotal,
                  na.rm=T)

criancas_t19<-svyby(~V2007,~UF,subset(PNADc2019, V2009>=0&V2009<=5),svytotal,
                    na.rm=T)

write.csv(criancas_t16,file="criancas_t16.csv")
write.csv(criancas_t18,file="criancas_t18.csv")
write.csv(criancas_t19,file="criancas_t19.csv")

EI16<-svyby(~V3003A,~UF,subset(PNADc2016, V2009>=0&V2009<=5),svytotal,
            na.rm=T)

EI18<-svyby(~V3003A,~UF,subset(PNADc2018, V2009>=0&V2009<=5),svytotal,
            na.rm=T)

EI19<-svyby(~V3003A,~UF,subset(PNADc2019, V2009>=0&V2009<=5),svytotal,
            na.rm=T)

write.csv(EI16,file="EI16.csv")
write.csv(EI18,file="EI18.csv")
write.csv(EI19,file="EI19.csv")