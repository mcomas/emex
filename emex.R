library(xml2)
library(dplyr)
library(stringr)
library(tidyr)

xml.url = 'http://api.idescat.cat/emex/v1/dades.xml'
pobl = read_xml(xml.url)
cols = xml_children( xml_children(pobl) )
ids = xml_attr( cols, attr = 'id')
scheme = xml_attr( cols, attr = 'scheme')
nom = xml_text( cols )


# S'obtenen les dades del servidor d'idescat. 
# IMPORTANT: Es requereix de connecció a Internet
id.info = lapply(ids, function(id){
  print(id)
  xml.url = sprintf('http://api.idescat.cat/emex/v1/dades.xml?id=%s', id)
  rr = NULL
  while(is.null(rr)){
    rr = tryCatch({ read_xml(xml.url) }, error = function(e) NULL )
  }
  print(rr)
  rr
})


# Es llegeixen els valors de tots els camps
get_field = function(fxml, field){
  ptrn = "/fitxes/gg/g/tt/t/ff/f[@id='%s']/v/child::text()"
  res = xml_find_all(fxml, sprintf(ptrn, field) ) %>% xml_text
  if(length(res)>0){
    return(res)
  }else{
    return(as.character(NA))
  }
}
fields = lapply(id.info[scheme == 'mun'], 
                function(id) 
                  xml_find_all(id, '/fitxes/gg/g/tt/t/ff/f/attribute::id') %>% xml_text)
all_fields = Reduce(`union`, fields)

df_all_fields = lapply(id.info[scheme == 'mun'], function(id){
  df_info = xml_find_all(id, "/fitxes/cols/col/child::text()") %>% xml_text %>% as.list %>% 
    setNames(c('mun', 'com', 'ca')) %>% as_tibble()
  df_fields = lapply(all_fields, function(field) get_field(id, field)) %>% 
    setNames(all_fields) %>% 
    as_tibble()
  bind_cols(df_info, df_fields)
}) %>% bind_rows



# Es construeix una taula amb la informació de tots els camps
emex.data = df_all_fields %>% 
  gather(key = field, value = value, -mun, -com, -ca) %>% 
  mutate( value = as.character(value),
          value = str_split_fixed(value, ',', n = 2)[,1],
          value = as.numeric(value)) %>% 
  spread(key=field, value = value)

# i una taula amb la llegenda de codis del diferents camps disponibles.
emex.desc = lapply(id.info[scheme == 'mun'], function(id){
  print(id)
  g.code = xml_find_all(id, '/fitxes/gg/g/attribute::id') %>% xml_text
  g.df = lapply(g.code, function(vg){
    t.code = xml_find_all(id, sprintf("/fitxes/gg/g[@id='%s']/tt/t/attribute::id", vg)) %>% xml_text
    t.df = lapply(t.code, function(vt){
      data_frame('f.code' = xml_find_all(id, sprintf("/fitxes/gg/g[@id='%s']/tt/t[@id='%s']/ff/f/attribute::id", vg, vt)) %>% xml_text,
                 'f.desc' = xml_find_all(id, sprintf("/fitxes/gg/g[@id='%s']/tt/t[@id='%s']/ff/f/c", vg, vt)) %>% xml_text,
                 't.code' = vt)
    })
    t.desc =  rep(xml_find_all(id, sprintf("/fitxes/gg/g[@id='%s']/tt/t/c", vg)) %>% xml_text, sapply(t.df, nrow) )
    t.df = t.df %>% bind_rows
    t.df$t.desc = t.desc
    t.df$g.code = vg
    t.df
  })
  g.desc =  rep(xml_find_all(id, sprintf("/fitxes/gg/g/c")) %>% xml_text, sapply(g.df, nrow) )
  g.df = g.df %>% bind_rows
  g.df$g.desc = g.desc
  g.df
}) %>% bind_rows %>% 
  distinct(g.code, t.code, f.code, .keep_all = TRUE) %>% 
  arrange(g.code, t.code, f.code) %>%
  select(g.code, g.desc, t.code, t.desc, f.code, f.desc)

GIRONA = c("Alt Empordà", "Baix Empordà", "Cerdanya", 
           "Garrotxa", "Gironès", "Pla de l'Estany", "Ripollès",
           "Selva")
BARCELONA = c("Anoia", "Alt Camp", "Alt Penedès", "Bages", "Baix Llobregat",
              "Barcelonès", "Berguedà", "Garraf", "Maresme", "Osona",
              "Vallès Occidental", "Vallès Oriental")
TARRAGONA = c("Baix Penedès", "Baix Ebre", "Baix Camp", "Conca de Barberà",
              "Montsià", "Priorat", "Ribera d'Ebre", "Tarragonès", "Terra Alta")
LLEIDA = c("Alt Urgell", "Alta Ribagorça", "Garrigues", "Noguera",
           "Pallars Jussà", "Pallars Sobirà", "Pla d'Urgell",
           "Segarra", "Segrià", "Solsonès", "Urgell", "Val d'Aran")

vrs = names(emex.data)
vrs = c(vrs[1:2], 'prov', vrs[-(1:2)])

emex.data = emex.data %>%
  mutate(
    prov = NA_character_,
    prov = if_else(com %in% GIRONA, 'Girona', prov),
    prov = if_else(com %in% BARCELONA, 'Barcelona', prov),
    prov = if_else(com %in% TARRAGONA, 'Tarragona', prov),
    prov = if_else(com %in% LLEIDA, 'LLeida', prov)
  ) %>%
  select_(.dots = vrs)

# Guardem les taules creades
save(emex.data, emex.desc, file='emex.RData')
