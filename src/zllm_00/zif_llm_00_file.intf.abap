interface ZIF_LLM_00_FILE
  public .


  methods GET_NAME
    returning
      value(RV_) type STRING .
  methods GET_STRING
    returning
      value(RV_) type STRING .
  methods GET_XSTRING
    returning
      value(RV_) type XSTRING .
  methods TO_O_STRING
    returning
      value(RO_) type ref to ZIF_LLM_00_STRING .
endinterface.
