agg_cnes_regsaude <- '

{
  "aggs": {
    "a1": {
      "terms": {
        "field": "mun_CSAUDCOD",
        "size": 10000
      }
    }
  }
}

'
