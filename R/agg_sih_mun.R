agg_sih_mun <- '

{
  "size": 0,
  "_source": {
    "excludes": []
  },
  "aggs": {
    "a1": {
      "terms": {
        "field": "res_codigo_adotado",
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
