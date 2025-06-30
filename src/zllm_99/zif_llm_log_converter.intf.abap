interface ZIF_LLM_LOG_CONVERTER
  public .


  types TS_MSG type zif_llm_log=>TS_MSG .
  types TT_MSG type zif_llm_log=>TT_MSG .

  methods GET_FLATTEN_TABLE
    returning
      value(RT_) type TT_MSG .
  methods GET_BAPIRET2_TABLE
    returning
      value(RT_) type BAPIRET2_T .
  methods GET_IDOC_STATUS_TABLE
    importing
      !IV_DOCNUM type BDIDOCSTAT-DOCNUM
      !IV_STATUS type BDIDOCSTAT-STATUS
      !IV_APPL_LOG type BDIDOCSTAT-APPL_LOG optional
      !IV_UNAME type BDIDOCSTAT-UNAME default SY-UNAME
    returning
      value(RT_) type BDTIDOCSTA .
*  methods GET_MMPUR_T_MESSAGES_EXT
*    returning
*      value(RT_) type MMPUR_T_MESSAGES_EXT .
  methods GET_IWBEP_MSG_CONTAINER
    importing
      !IV_ERROR_CATEGORY type /IWBEP/IF_MESSAGE_CONTAINER=>TY_ERROR_CATEGORY default /IWBEP/IF_MESSAGE_CONTAINER=>GCS_ERROR_CATEGORY-PROCESSING
      !IV_DETERMINE_LEADING_MSG type /IWBEP/IF_MESSAGE_CONTAINER=>TY_LEADING_MSG_FLAG default /IWBEP/IF_MESSAGE_CONTAINER=>GCS_LEADING_MSG_SEARCH_OPTION-FIRST
      !IV_ENTITY_TYPE type STRING optional
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR optional
      !IV_ADD_TO_RESPONSE_HEADER type /IWBEP/SUP_MC_ADD_TO_RESPONSE default ABAP_TRUE
    returning
      value(RO_) type ref to /IWBEP/IF_MESSAGE_CONTAINER .
endinterface.
