CLASS zcl_llm_00_llm_lazy DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_llm_00_types .
    INTERFACES zif_llm_00_llm_lazy .

    CLASS-METHODS new
      IMPORTING
        !is_       TYPE zif_llm_00_types=>ts_env
        !io_cache  TYPE REF TO zif_llm_00_cache OPTIONAL
        !is_lim    TYPE zif_llm_00_llm_lazy=>ts_lim_in OPTIONAL
      RETURNING
        VALUE(ro_) TYPE REF TO zif_llm_00_llm_lazy .
    CLASS-METHODS new_from_file
      IMPORTING
        !io_       TYPE REF TO zif_llm_00_file
        !io_cache  TYPE REF TO zif_llm_00_cache OPTIONAL
        !is_lim    TYPE zif_llm_00_llm_lazy=>ts_lim_in OPTIONAL
      RETURNING
        VALUE(ro_) TYPE REF TO zif_llm_00_llm_lazy .
    CLASS-METHODS _debug
      IMPORTING
        !iv_ TYPE sap_bool
        !io_ TYPE REF TO zif_llm_00_trace OPTIONAL .
    CLASS-METHODS _throttle
      IMPORTING
        !is_lim    TYPE zif_llm_00_llm_lazy=>ts_lim_in
        !iv_llm_id TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_env       TYPE zif_llm_00_types=>ts_env .
    DATA mv_url       TYPE string.
    DATA mv_url_embed TYPE string.
    DATA mv_model     TYPE string.
    DATA ms_          TYPE zif_llm_00_llm_lazy~ts_llm_config.

    METHODS constructor
      IMPORTING
        !is_      TYPE zif_llm_00_types=>ts_env
        !io_cache TYPE REF TO zif_llm_00_cache
        !is_lim   TYPE zif_llm_00_llm_lazy=>ts_lim_in.
    METHODS api_send
      IMPORTING !iv_       TYPE string
                !iv_url    TYPE string
      RETURNING VALUE(ro_) TYPE REF TO if_http_client
      RAISING   zcx_s.

    METHODS _api_receive
      IMPORTING !io_       TYPE REF TO if_http_client
      RETURNING VALUE(rv_) TYPE string
      RAISING   zcx_s.

    METHODS raise_last_error
      IMPORTING io_ TYPE REF TO if_http_client
      RAISING   zcx_s.
    CLASS-DATA: gv_msg TYPE string.

    DATA: mo_cache TYPE REF TO zif_llm_00_cache.

    CLASS-DATA gv_debug TYPE sap_bool .
    CLASS-DATA go_trace TYPE REF TO zif_llm_00_trace.

    METHODS _in  IMPORTING iv_   TYPE string
                           iv_id TYPE string OPTIONAL.
    METHODS _out IMPORTING iv_   TYPE string
                           iv_id TYPE string OPTIONAL.

    DATA: mv_llm_id   TYPE string.
    DATA: mv_llm_hash TYPE hash160.
    DATA: ms_lim      TYPE zif_llm_00_llm_lazy=>ts_lim_in.

    CLASS-DATA: gt_lim TYPE zif_llm_00_llm_lazy=>tt_lim.
ENDCLASS.



CLASS ZCL_LLM_00_LLM_LAZY IMPLEMENTATION.


  METHOD api_send.
    DATA(lr_lim) = REF #( gt_lim[ llm_hash = mv_llm_hash ] OPTIONAL ).
    IF lr_lim IS BOUND.
      ADD 1 TO lr_lim->req_counter.
      IF lr_lim->req_counter >= lr_lim->req_before_pause.
        lr_lim->req_counter = 0.
        WAIT UP TO lr_lim->pause_for SECONDS.
      ENDIF.
    ENDIF.
    cl_http_client=>create_by_url(
      EXPORTING
        url                = iv_url
      IMPORTING
        client             = ro_
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).
    IF sy-subrc NE 0.
      me->raise_last_error( ro_ ).
      RETURN.
    ENDIF.

    " Set the request method and headers
    ro_->request->set_method( if_rest_message=>gc_method_post ).
    ro_->request->set_header_field(
      name  = 'Content-Type'
      value = 'application/json'
    ).
*    ro_->request->set_header_field(
*      name  = 'Authorization'
*      value = |Bearer { ms_env-api_key }|
*    ).
    ro_->request->set_header_field(
      name  = 'api-key'
      value = ms_env-api_key
    ).

    " Set the request body
    ro_->request->set_cdata( iv_ ).

    " Send the request
    me->_in( iv_ ).

    ro_->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
    ).

    IF sy-subrc NE 0.
      me->raise_last_error( ro_ ).
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    mo_cache = io_cache.
    ms_env = is_.

    IF ms_env-api_model IS NOT INITIAL.
      mv_model = ms_env-api_model.
    ENDIF.

    ms_-model_name = mv_model.
    DATA(lv_model_upper) = to_upper( mv_model ).

    ms_-model_type = zcl_llm_00_predictoken=>gc_llm_type-gpt.
    IF lv_model_upper = 'MISTRAL'.
      ms_-model_type = zcl_llm_00_predictoken=>gc_llm_type-mistral.
    ELSEIF lv_model_upper CP 'O1*' OR
           lv_model_upper CP 'O3*' OR
           lv_model_upper CP 'O4*'.
      ms_-model_type = zcl_llm_00_predictoken=>gc_llm_type-gpt_reasoning.
    ENDIF.
*--------------------------------------------------------------------*
    IF ms_-model_type = zcl_llm_00_predictoken=>gc_llm_type-gpt_reasoning.
      mv_url = |{ ms_env-api_url }openai/responses?api-version={ ms_env-api_ver }|.
    ELSE.
      IF ms_env-api_dep IS NOT INITIAL.
        mv_url = |{ ms_env-api_url }openai/deployments/{ ms_env-api_dep }/chat/completions?api-version={ ms_env-api_ver }|.
      ELSE.
        mv_url = COND #( WHEN ms_env-api_url IS INITIAL THEN |https://api.openai.com/v1/chat/completions|
                         ELSE |{ ms_env-api_url }v1/chat/completions| ).
      ENDIF.
    ENDIF.
    IF ms_env-api_dep_embed IS NOT INITIAL.
      mv_url_embed = |{ ms_env-api_url }openai/deployments/{ ms_env-api_dep_embed }/embeddings?api-version={ ms_env-api_ver }|.
    ELSE.
      mv_url_embed = COND #( WHEN ms_env-api_url IS INITIAL THEN |https://api.openai.com/v1/embeddings|
                             ELSE |{ ms_env-api_url }v1/embeddings| ).
    ENDIF.

    ms_-max_token = ms_env-api_max_token.
    ms_-split_limit = ms_env-api_token_split_limit.
*--------------------------------------------------------------------*
    mv_llm_id   = mv_model. "|{ mv_url }-{ mv_model }|.
    mv_llm_hash = zcl_llm=>string_hash( mv_llm_id ).
    IF is_lim-req_before_pause > 0 OR
       is_lim-tok_before_pause > 0.

      IF line_exists( gt_lim[ llm_hash = mv_llm_hash ] ).
        RETURN.
      ENDIF.
      APPEND VALUE #(
        llm_hash         = mv_llm_hash
        llm_id           = mv_llm_id
        req_before_pause = is_lim-req_before_pause
        tok_before_pause = is_lim-tok_before_pause
        pause_for        = is_lim-pause_for
      ) TO gt_lim.
    ENDIF.
  ENDMETHOD.


  METHOD new.
    IF io_cache IS NOT BOUND.
      DATA(lo_cache) = zcl_llm_00_cache=>new( ).
    ELSE.
      lo_cache = io_cache.
    ENDIF.
    ro_ = NEW zcl_llm_00_llm_lazy(
      is_      = CORRESPONDING #( is_ )
      is_lim   = is_lim
      io_cache = lo_cache
    ).
  ENDMETHOD.


  METHOD new_from_file.
    IF io_cache IS NOT BOUND.
      DATA(lo_cache) = zcl_llm_00_cache=>new( ).
    ELSE.
      lo_cache = io_cache.
    ENDIF.
    ro_ = zcl_llm_00_llm_lazy=>new(
      is_      = zcl_llm_00_dotenv=>new( io_ )->get_config( )
      is_lim   = is_lim
      io_cache = lo_cache
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
    MESSAGE e001(zllm_00) WITH lv_subrc lv_message lv_message_class lv_message_number INTO gv_msg.
    zcx_s=>raise( sy ).
  ENDMETHOD.


  METHOD zif_llm_00_llm_lazy~a.
    rv_ = io_->v( ).
  ENDMETHOD.


  METHOD zif_llm_00_llm_lazy~get_config.
    rs_ = ms_.
  ENDMETHOD.


  METHOD zif_llm_00_llm_lazy~q.
    DATA(lv_json) = io_->to_json( ).
    IF iv_k IS INITIAL.
      DATA(lv_k) = lv_json.
    ELSE.
      lv_k = iv_k.
    ENDIF.

    IF mo_cache IS BOUND.
      DATA(lv_v) = mo_cache->get( lv_k ).
      IF lv_v IS NOT INITIAL.
        ro_ = zcl_llm_00_llm_response=>new(
          iv_k     = lv_k
          iv_v     = lv_v
          io_cache = mo_cache
        ).
        RETURN.
      ENDIF.
    ENDIF.

    DATA(lo_http) = me->api_send(
      iv_    = lv_json
      iv_url = mv_url
    ).
    ro_ = zcl_llm_00_llm_response=>new(
      io_      = lo_http
      iv_k     = lv_k
      io_cache = mo_cache
    ).
  ENDMETHOD.


  METHOD _api_receive.
    RETURN.
* api_receive logic is in ZIF_LLM_00_LLM_RESPONSE
    IF 1 = 2.
      DATA(lo_) = zcl_llm_00_llm_response=>new(
*                io_      = io_
*                iv_k     = iv_k
*                iv_v     = iv_v
*                io_cache = io_cache
                  ).
      lo_->v( ).
    ENDIF.

*    " Receive the response
*    io_->receive(
*      EXCEPTIONS
*        http_communication_failure = 1
*        http_invalid_state         = 2
*        http_processing_failed     = 3
*    ).
*
*    IF sy-subrc NE 0.
*      me->raise_last_error( io_ ).
*      RETURN.
*    ENDIF.
*
*    " Check the response
*    io_->response->get_status(
*      IMPORTING
*        code   = DATA(lv_code)
*        reason = DATA(lv_reason)
*    ).
*    IF lv_code = 200.
*      rv_ = io_->response->get_cdata( ).
*    ELSE.
*      " Handle error
*      rv_ = io_->response->get_cdata( ).
*      "me->raise_last_error( io_ ).
*      zcx_s=>raise( rv_ ).
*      RETURN.
*    ENDIF.
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


  METHOD _throttle.

    DATA(lv_llm_hash) = zcl_llm=>string_hash( iv_llm_id ).
    DATA(lr_lim) = REF #( gt_lim[ llm_hash = lv_llm_hash ] OPTIONAL ).
    IF lr_lim IS BOUND.
      lr_lim->* = CORRESPONDING #( is_lim ).
      lr_lim->llm_hash = lv_llm_hash.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    APPEND VALUE #(
      llm_hash = lv_llm_hash
      llm_id   = iv_llm_id
      req_before_pause = is_lim-req_before_pause
      tok_before_pause = is_lim-tok_before_pause
      pause_for        = is_lim-pause_for
    ) TO gt_lim.

  ENDMETHOD.


  METHOD zif_llm_00_llm_lazy~get_payload_adapter.
    ro_ = zcl_llm_00_payload_adapter=>new( me ).
  ENDMETHOD.
ENDCLASS.
