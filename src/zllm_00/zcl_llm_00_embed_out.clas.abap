CLASS zcl_LLM_00_embed_out DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_llm_00_types.
    INTERFACES zif_llm_00_json .
    ALIASES to_json
      FOR zif_llm_00_json~to_json .

    TYPES ts_ TYPE zif_llm_00_types=>ts_embed_out .

    CLASS-METHODS new
      IMPORTING
        !is_       TYPE ts_
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_LLM_00_embed_out
      RAISING
        zcx_s .
    CLASS-METHODS new_from_json
      IMPORTING
        !iv_       TYPE string
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_LLM_00_embed_out .
    METHODS get_
      RETURNING
        VALUE(rs_) TYPE ts_ .
    METHODS get_vector
      RETURNING
        VALUE(rt_) TYPE zif_llm_00_types=>tt_coord .
    METHODS get_vector_q1b
      RETURNING
        VALUE(rx_) TYPE zif_llm_00_types=>ty_v.
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



CLASS ZCL_LLM_00_EMBED_OUT IMPLEMENTATION.


  METHOD constructor.
    ms_ = is_.
  ENDMETHOD.


  METHOD get_.

    rs_ = ms_.

  ENDMETHOD.


  METHOD get_vector.

    rt_ = VALUE #( ms_-data[ 1 ]-embedding OPTIONAL ).

  ENDMETHOD.


  METHOD get_vector_q1b.

    DATA(lt_) = get_vector( ).

    LOOP AT lt_ FROM 1 TO 1536 REFERENCE INTO DATA(lr_).
      IF lr_->* < 0.
        SET BIT sy-tabix OF rx_ TO 1.
      ENDIF.
    ENDLOOP.

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
  ENDMETHOD.


  METHOD zif_llm_00_json~to_json.
    rv_ = zcl_llm_00_json=>to_json( ms_ ).
  ENDMETHOD.
ENDCLASS.
