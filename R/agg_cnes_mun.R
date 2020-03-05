agg_cnes_mun <- '

{
  "aggs": {
    "a1": {
      "terms": {
        "field": "mun_codigo_adotado",
        "size": 10000
      }
    }
  }
}

'
