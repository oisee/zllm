CLASS zcl_llm_00_chat_out DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_llm_00_types.
    INTERFACES zif_llm_00_json .

    ALIASES to_json
      FOR zif_llm_00_json~to_json .

    TYPES ts_ TYPE zif_llm_00_types=>ts_chat_out.

    CLASS-METHODS new
      IMPORTING
        !is_       TYPE ts_
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_llm_00_chat_out
      RAISING
        zcx_s .
    CLASS-METHODS new_from_json
      IMPORTING
                !iv_       TYPE string
      RETURNING
                VALUE(ro_) TYPE REF TO zcl_llm_00_chat_out
      RAISING   zcx_s.
    METHODS get_
      RETURNING
        VALUE(rs_) TYPE ts_ .
    METHODS get_reply
      RETURNING
        VALUE(rv_) TYPE string .
    METHODS is_function_call
      RETURNING
        VALUE(rv_) TYPE sap_bool .
    METHODS get_function
      RETURNING
                VALUE(ro_) TYPE REF TO zif_llm_00_function
      RAISING   zcx_s.
    METHODS get_function_name
      RETURNING
        VALUE(rv_) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA gv_msg TYPE string .

    METHODS constructor
      IMPORTING
        !is_ TYPE ts_
      RAISING
        zcx_s .
    DATA: ms_ TYPE ts_.
ENDCLASS.



CLASS zcl_llm_00_chat_out IMPLEMENTATION.


  METHOD constructor.
    ms_ = is_.
  ENDMETHOD.


  METHOD get_.

    rs_ = ms_.

  ENDMETHOD.


  METHOD get_function.
    IF NOT is_function_call( ).
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    ro_ = zcl_llm_00_function=>new( CONV #( me->get_function_name( ) ) ).

  ENDMETHOD.


  METHOD get_function_name.

    rv_ = VALUE #( ms_-choices[ 1 ]-message-function_call-name OPTIONAL ).

  ENDMETHOD.


  METHOD get_reply.

    rv_ = VALUE #( ms_-choices[ 1 ]-message-content OPTIONAL ).

  ENDMETHOD.


  METHOD is_function_call.

    DATA(lv_) = to_lower( VALUE #( ms_-choices[ 1 ]-finish_reason OPTIONAL ) ).
    rv_ = boolc( lv_ = 'function_call' ).

  ENDMETHOD.


  METHOD new.
    ro_ = NEW #( is_ ).
  ENDMETHOD.


  METHOD new_from_json.
    DATA: ls_ TYPE ts_.
    zcl_llm_00_json=>from_json( EXPORTING json = iv_
                                CHANGING  data = ls_
    ).
    ro_ = NEW #( ls_ ).

    DATA(ls_ch) = VALUE #( ls_-choices[ 1 ] OPTIONAL ).
    IF ls_ch IS INITIAL.
      RETURN.
    ENDIF.
*    IF ls_ch-finish_reason NE 'stop'.
*      MESSAGE w000(zcx_) WITH 'Finish Reason' ls_ch-finish_reason INTO DATA(lv_msg).
*      zcx_s=>raise( sy ).
*      RETURN.
*    ENDIF.
  ENDMETHOD.


  METHOD zif_llm_00_json~to_json.
    rv_ = zcl_llm_00_json=>to_json( ms_ ).
  ENDMETHOD.
ENDCLASS.
