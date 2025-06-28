CLASS zcl_llm_00_llm_lazy_balancer DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_llm_00_types .
    INTERFACES zif_llm_00_llm_lazy .

    TYPES: BEGIN OF ts_pdt,
             model_type TYPE string,
             pdt        TYPE REF TO zcl_llm_00_predictoken,
           END OF ts_pdt.

    TYPES: tt_pdt TYPE STANDARD TABLE OF ts_pdt WITH KEY model_type.

    TYPES tt_llm  TYPE STANDARD TABLE OF REF TO zif_llm_00_llm_lazy WITH DEFAULT KEY.
    TYPES: BEGIN OF ts_llm_group,
             max     TYPE i,
             g       TYPE tt_llm,
             current TYPE i,
             g_count TYPE i,
           END OF ts_llm_group.
    TYPES tt_llm_group TYPE TABLE OF ts_llm_group WITH KEY max.

    CLASS-METHODS new
      IMPORTING
        !it_llm    TYPE tt_llm
      RETURNING
        VALUE(ro_) TYPE REF TO zif_llm_00_llm_lazy .

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA ms_config    TYPE zif_llm_00_llm_lazy~ts_llm_config.
    DATA ms_config_exp TYPE zif_llm_00_llm_lazy~ts_llm_config.
    DATA mt_llm TYPE tt_llm .
    DATA mt_llm_group TYPE tt_llm_group .
    DATA mv_llm_count TYPE i .
    DATA mv_llm_group_count TYPE i .
    DATA mo_llm     TYPE REF TO zif_llm_00_llm_lazy .
    DATA mo_llm_exp TYPE REF TO zif_llm_00_llm_lazy .
    DATA mt_pdt TYPE tt_pdt .
    DATA mo_pdt TYPE REF TO zcl_llm_00_predictoken .

    METHODS constructor
      IMPORTING
        !it_llm TYPE tt_llm .
    METHODS api_receive
      IMPORTING
        !io_       TYPE REF TO if_http_client
      RETURNING
        VALUE(rv_) TYPE string
      RAISING
        zcx_s .
    METHODS raise_last_error
      IMPORTING
        !io_ TYPE REF TO if_http_client
      RAISING
        zcx_s .
    METHODS add_pdt_for_llm
      IMPORTING
        !io_ TYPE REF TO zif_llm_00_llm_lazy .

    METHODS get_next_llm_in_group
      IMPORTING iv_        TYPE i
      RETURNING VALUE(ro_) TYPE REF TO zif_llm_00_llm_lazy .

    CLASS-DATA gv_msg TYPE string.
ENDCLASS.



CLASS ZCL_LLM_00_LLM_LAZY_BALANCER IMPLEMENTATION.


  METHOD add_pdt_for_llm .
    " add instance of predictoken into mt_pdt based on model type
    " Create an instance of the predictoken class
    DATA(lv_model_type) = io_->get_config( )-model_type.
    DATA(lr_pdt) = REF #( mt_pdt[ model_type = lv_model_type ] OPTIONAL ).
    IF lr_pdt IS NOT BOUND.
      DATA(lo_pdt) = zcl_llm_00_predictoken=>new_for_model_type( lv_model_type ).
      APPEND VALUE #(
        model_type = lv_model_type
        pdt = lo_pdt
      ) TO mt_pdt.
    ENDIF.

  ENDMETHOD.


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
    ELSE.
      " Handle error
      rv_ = io_->response->get_cdata( ).
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    mt_llm = it_llm.
    mv_llm_count = lines( mt_llm ).

    " Grouping LLMs by their max tokens into the mt_llm_group
    LOOP AT mt_llm INTO DATA(lo_llm).
      " Check the max tokens of each LLM and group them accordingly
      DATA(lv_max) = lo_llm->get_config( )-max_token.
      DATA(lr_llm_group) = REF #( mt_llm_group[ max = lv_max ] OPTIONAL ).
      IF lr_llm_group IS NOT BOUND.
        APPEND VALUE #(
          max     = lv_max
          g       = VALUE #( ( lo_llm ) )
          current = 1
          g_count = 1
        ) TO mt_llm_group.
      ELSE.
        APPEND lo_llm TO lr_llm_group->g.
        ADD 1 TO lr_llm_group->g_count.
      ENDIF.

      add_pdt_for_llm( lo_llm ).
    ENDLOOP.
    SORT mt_llm_group BY max.

    IF mt_llm_group IS INITIAL.
      RETURN. "?

    ENDIF.

    mv_llm_group_count = lines( mt_llm_group ).

    DATA(ls_llm_group) = mt_llm_group[ mv_llm_group_count ].
    mo_llm     = ls_llm_group-g[ 1 ].
    mo_llm_exp = ls_llm_group-g[ ls_llm_group-g_count ].
    mo_pdt = zcl_llm_00_predictoken=>new( ).
    ms_config    = mo_llm->get_config( ).
    ms_config_exp = mo_llm_exp->get_config( ).

  ENDMETHOD.


  METHOD get_next_llm_in_group.
    " Get the next LLM in the group
    DATA(ls_llm_group) = mt_llm_group[ max = iv_ ].
    DATA(lo_llm) = ls_llm_group-g[ ls_llm_group-current ].
    ro_ = lo_llm.

    ls_llm_group-current = COND #(
      WHEN ls_llm_group-current < ls_llm_group-g_count THEN ls_llm_group-current + 1
      ELSE 1
    ).

  ENDMETHOD.


  METHOD new.
    " Factory method to create an instance of the balancer class
    ro_ ?= NEW zcl_llm_00_llm_lazy_balancer( it_llm ).
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


  METHOD zif_llm_00_llm_lazy~get_config.
    rs_ = ms_config_exp.
  ENDMETHOD.


  METHOD zif_llm_00_llm_lazy~a.
    rv_ = io_->v( ).
  ENDMETHOD.


  METHOD zif_llm_00_llm_lazy~q.
    " Decide which implementation to use based on the threshold
    DATA(lv_json)   = io_->to_json( ).
    DATA(lv_tokens) = mo_pdt->predict( lv_json ).

    LOOP AT mt_llm_group REFERENCE INTO DATA(lr_llm_group).
*      DATA(lv_tokens) = lr_llm_group->pdt ->predict( lv_json ).
      IF lr_llm_group->max < lv_tokens.
        CONTINUE.
      ENDIF.
      DATA(lo_llm) = get_next_llm_in_group( iv_ = lr_llm_group->max ).
      DATA(ls_pdt) = mt_pdt[ model_type = lo_llm->get_config( )-model_type ].
      lv_tokens = ls_pdt-pdt->predict( lv_json ).
      IF lr_llm_group->max < lv_tokens.
        CONTINUE.
      ENDIF.
      EXIT.
    ENDLOOP.
    IF lo_llm IS NOT BOUND.
      lo_llm = mo_llm_exp.
    ENDIF.

    DATA(lv_model_exp) = mo_llm_exp->get_config( )-model_name.
    DATA(lv_model) = lo_llm->get_config( )-model_name.
    REPLACE FIRST OCCURRENCE OF |"model":"{ lv_model_exp }"| IN lv_json WITH |"model":"{ lv_model }"|.
    DATA(lo_json) = zcl_llm_00_json_mock=>new( lv_json ).
    ro_ = lo_llm->q(
      io_  = lo_json
      iv_k = iv_k
    ).

  ENDMETHOD.


  METHOD zif_llm_00_llm_lazy~get_payload_adapter.

    ro_ = zcl_llm_00_payload_adapter=>new( me ).

  ENDMETHOD.
ENDCLASS.
