agg_sih_uf <- '

{
  "size": 0,
  "_source": {
    "excludes": []
  },
  "aggs": {
    "a1": {
      "terms": {
        "field": "res_uf_SIGLA_UF",
        "size": 10000
      }
    }
  },
  "stored_fields": [
    "*"
  ],
  "script_fields": {},
  "docvalue_fields": [
    "dt_inter",
    "dt_saida"
  ],
  "query": {
    "bool": {
      "must": [
        {
          "match_all": {}
        }
      ],
      "filter": [],
      "should": [],
      "must_not": []
    }
  }
}

'
