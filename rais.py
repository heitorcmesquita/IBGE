import basedosdados as bd

# Carregando o dado
df = bd.read_sql('''
                 SELECT 
                     id_municipio, ano, faixa_remun_media_sm, cnae_2, 
                     grau_instrucao_apos_2005, sexo_trabalhador, 
                     faixa_etaria, COUNT(*) count 
                 FROM basedosdados.br_me_rais.microdados_vinculos
                 WHERE ano > 2009
                     and sigla_uf = 'AL'
                 GROUP BY id_municipio, ano, faixa_remun_media_sm, cnae_2, 
                     grau_instrucao_apos_2005, sexo_trabalhador, faixa_etaria
                 ''',
                 billing_project_id='caged-sinc')

df.to_csv("caged.csv", encoding = "iso-8859-1")
