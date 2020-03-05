agg_cnes_uf <- '

{
  "aggs": {
    "a1": {
      "terms": {
        "field": "uf_SIGLA_UF.keyword",
        "size": 10000
      }
    }
  }
}

'
