interface ZIF_LLM_00_PAYLOAD_ADAPTER
  public .


  methods INPUT
    importing
      !IT_MSG type ZIF_LLM_00_TYPES=>TT_MESSAGE_IN
      !IV_JSON type SAP_BOOL optional
    returning
      value(RO_) type ref to ZIF_LLM_00_JSON .
  methods OUTPUT
    importing
      !IV_ type STRING
    returning
      value(RV_) type STRING .
endinterface.
