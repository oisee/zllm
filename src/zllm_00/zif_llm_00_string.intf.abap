interface ZIF_LLM_00_STRING
  public .

 TYPES: string_t TYPE Zif_LLM_00_types=>string_t.
  methods TO_STRING
    returning
      value(RV_) type STRING .
  methods PREDICT_TOKENS
    returning
      value(RV_) type I .
  methods TO_STRING_T
    returning
      value(RT_) type STRING_T .
  methods TO_XSTRING
    returning
      value(RV_) type XSTRING .
endinterface.
