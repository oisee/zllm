CLASS zcl_llm_00_llm_response DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    INTERFACES zif_llm_00_llm_response .
    CLASS-METHODS new
      IMPORTING
        !io_       TYPE REF TO if_http_client   OPTIONAL
        !iv_k      TYPE string                  OPTIONAL
        !iv_v      TYPE string                  OPTIONAL
        !io_cache  TYPE REF TO zif_llm_00_cache OPTIONAL
      RETURNING
        VALUE(ro_) TYPE REF TO zif_llm_00_llm_response .
    CLASS-METHODS _debug
      IMPORTING
        !iv_ TYPE sap_bool
        !io_ TYPE REF TO zif_llm_00_trace OPTIONAL .
  PROTECTED SECTION.
private section.

  data MO_ type ref to IF_HTTP_CLIENT .
  data MV_K type STRING .
  data MV_V type STRING .
  data MV_MSG type STRING .
  data MO_CACHE type ref to ZIF_LLM_00_CACHE .
  class-data GV_DEBUG type SAP_BOOL .
  class-data GO_TRACE type ref to ZIF_LLM_00_TRACE .
  data MV_RETRY_COUNT type I .
  data MV_MAX_RETRIES type I value 15 ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !IO_ type ref to IF_HTTP_CLIENT
      !IV_K type STRING
      !IV_V type STRING
      !IO_CACHE type ref to ZIF_LLM_00_CACHE .
  methods API_RECEIVE
    importing
      !IO_ type ref to IF_HTTP_CLIENT
    returning
      value(RV_) type STRING
    raising
      resumable(ZCX_S) .
  methods RAISE_LAST_ERROR
    importing
      !IO_ type ref to IF_HTTP_CLIENT
    raising
      ZCX_S .
  methods _IN
    importing
      !IV_ type STRING
      !IV_ID type STRING optional .
  methods _OUT
    importing
      !IV_ type STRING
      !IV_ID type STRING optional .
ENDCLASS.



CLASS ZCL_LLM_00_LLM_RESPONSE IMPLEMENTATION.


  METHOD api_receive.

    " Receive the response
    io_->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
    ).

    IF sy-subrc NE 0.
      me->raise_last_error( io_ ).
      RETURN.
    ENDIF.

    " Check the response
    io_->response->get_status(
      IMPORTING
        code   = DATA(lv_code)
        reason = DATA(lv_reason)
    ).
    IF lv_code = 200.
      rv_ = io_->response->get_cdata( ).
      me->_out( rv_ ).
      io_->close( ).
    ELSEIF lv_code = 429. "too many requests. retry ( how many times? )
      IF mv_retry_count >= mv_max_retries. "stop trying, error.
        rv_ = io_->response->get_cdata( ).
        me->_out( rv_ ).
        io_->close( ).
        "me->raise_last_error( io_ ).
        zcx_s=>raise( rv_ ).
        RETURN.
      ENDIF.
      DATA(lv_wait) = zcl_llm=>rand_int( 15 ) + zcl_llm=>rand_int( mv_retry_count * 7 ).
      mv_retry_count = mv_retry_count + 1.
      WAIT UP TO lv_wait SECONDS.
      io_->send( ).
      rv_ = me->api_receive( io_ ).
      "io_->close( ).
    ELSE.
      " Handle error
      rv_ = io_->response->get_cdata( ).
      me->_out( rv_ ).
      io_->close( ).
      "me->raise_last_error( io_ ).
      zcx_s=>raise( rv_ ).
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    mo_      = io_.
    mv_k     = iv_k.
    mv_v     = iv_v.
    mo_cache = io_cache.
    IF iv_k IS NOT INITIAL AND
       mo_cache IS BOUND   AND
       iv_v IS INITIAL.         "bypass cache if the expected value is supplied
      mv_v   = mo_cache->get( iv_k ).
    ENDIF.

    mv_max_retries = zcl_llm_00_tvarvc=>get_integer_by_name(
      iv_name    = 'ZLLM_MAX_RETRIES'
      iv_default = 15
    ).

  ENDMETHOD.


  METHOD new.
    ro_ ?= NEW
      zcl_llm_00_llm_response(
        io_      = io_
        iv_k     = iv_k
        iv_v     = iv_v
        io_cache = io_cache
    ).
  ENDMETHOD.


  METHOD raise_last_error.
    io_->get_last_error(
      IMPORTING
        code           = DATA(lv_subrc)          " Return Value, Return Value After ABAP Statements
        message        = DATA(lv_message)        " Error message
        message_class  = DATA(lv_message_class)  " Application area
        message_number = DATA(lv_message_number) " Message number
    ).
    MESSAGE e001(zllm_00) WITH lv_subrc lv_message lv_message_class lv_message_number INTO mv_msg.
    zcx_s=>raise( sy ).
  ENDMETHOD.


  METHOD zif_llm_00_llm_response~k.
    rv_ = mv_k.
  ENDMETHOD.


  METHOD zif_llm_00_llm_response~v.
    IF mv_v IS NOT INITIAL.
      rv_ = mv_v.
      RETURN.
    ENDIF.
    mv_v = me->api_receive( mo_ ).
    rv_ = mv_v.
    IF mo_cache IS BOUND.
      mo_cache->put(
        k = mv_k
        v = mv_v
      ).
    ENDIF.
  ENDMETHOD.


  METHOD _debug.
    gv_debug = iv_.
    go_trace = io_.
  ENDMETHOD.


  METHOD _in.
    IF gv_debug NE abap_true OR go_trace IS NOT BOUND.
      RETURN.
    ENDIF.
    go_trace->in(
      iv_   = iv_
      iv_id = 'ZCL_LLM_00_LLM_LAZY'
    ).

  ENDMETHOD.


  METHOD _out.
    IF gv_debug NE abap_true OR go_trace IS NOT BOUND.
      RETURN.
    ENDIF.
    go_trace->out(
      iv_   = iv_
      iv_id = 'ZCL_LLM_00_LLM_LAZY'
    ).

  ENDMETHOD.
ENDCLASS.
