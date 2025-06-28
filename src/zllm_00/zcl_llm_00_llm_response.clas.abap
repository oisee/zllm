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
  PRIVATE SECTION.

    DATA mo_ TYPE REF TO if_http_client .
    DATA mv_k TYPE string .
    DATA mv_v TYPE string .
    DATA mv_msg TYPE string .
    DATA mo_cache TYPE REF TO zif_llm_00_cache .

    METHODS constructor
      IMPORTING
        !io_      TYPE REF TO if_http_client
        !iv_k     TYPE string
        !iv_v     TYPE string
        !io_cache TYPE REF TO zif_llm_00_cache .
    METHODS api_receive
      IMPORTING
        !io_       TYPE REF TO if_http_client
      RETURNING
        VALUE(rv_) TYPE string
      RAISING
        RESUMABLE(zcx_s) .
    METHODS raise_last_error
      IMPORTING
        !io_ TYPE REF TO if_http_client
      RAISING
        zcx_s .

    CLASS-DATA gv_debug TYPE sap_bool .
    CLASS-DATA go_trace TYPE REF TO zif_llm_00_trace.

    METHODS _in  IMPORTING iv_   TYPE string
                           iv_id TYPE string OPTIONAL.
    METHODS _out IMPORTING iv_   TYPE string
                           iv_id TYPE string OPTIONAL.
    DATA: mv_retry_count TYPE i.
    DATA: mv_max_retries TYPE i VALUE 12.
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
