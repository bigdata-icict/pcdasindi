agg_sim_regsaude <- '

{
  "size": 0,
  "_source": {
    "excludes": []
  },
  "aggs": {
    "a1": {
      "terms": {
        "field": "res_RSAUDCOD",
        "size": 10000
      }
    }
  },
  "stored_fields": [
    "*"
  ],
  "script_fields": {},
  "docvalue_fields": [
    "data_nasc",
    "data_obito"
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
