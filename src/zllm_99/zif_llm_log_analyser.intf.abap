interface ZIF_LLM_LOG_ANALYSER
  public .


  types TS_MSG type zif_llm_log=>TS_MSG .
  types TT_MSG type zif_llm_log=>TT_MSG .

  methods GET_WORST_MESSAGE
    returning
      value(RS_) type TS_MSG .
  methods GET_WORST_BAPIRET2
    returning
      value(RS_) type BAPIRET2 .
  methods GET_WORST_MESSAGE_TYPE
    returning
      value(RV_) type TS_MSG-MSGTY .
  methods HAS_ERROR
    returning
      value(RV_) type ABAP_BOOL .
  methods HAS_WARNING_OR_BETTER
    returning
      value(RV_) type ABAP_BOOL .
  methods HAS_SUCCESS_ONLY
    returning
      value(RV_) type ABAP_BOOL .
  methods HAS_WARNING
    returning
      value(RV_) type ABAP_BOOL .
  methods IF_ERROR_SHOW_MOST_SEVERE_AS
    importing
      !IV_TYPE type SY-MSGTY optional .
  methods IF_ANY_SHOW_MOST_SEVERE_AS
    importing
      !IV_TYPE type SY-MSGTY optional .
  methods HAS_ANY
    returning
      value(RV_) type ABAP_BOOL .
endinterface.
