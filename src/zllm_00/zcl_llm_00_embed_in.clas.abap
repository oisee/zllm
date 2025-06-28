CLASS zcl_llm_00_embed_in DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

*    INTERFACES zif_oai_endpoint.
    INTERFACES zif_llm_00_json.

    ALIASES to_json
      FOR zif_llm_00_json~to_json .


    TYPES: ts_ TYPE ZIF_LLM_00_TYPES=>ts_embed_in.
    CLASS-METHODS new
      IMPORTING
        !is_       TYPE ts_
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_llm_00_embed_in
      RAISING
        zcx_s .
    CLASS-METHODS new_from_json
      IMPORTING
        !iv_       TYPE string
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_llm_00_embed_in.
    METHODS get_
      RETURNING
        VALUE(rs_) TYPE ts_ .
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



CLASS ZCL_LLM_00_EMBED_IN IMPLEMENTATION.


  METHOD constructor.
    ms_ = is_.
    IF ms_-model IS INITIAL.
      ms_-model = 'text-embedding-ada-002'.
    ENDIF.
  ENDMETHOD.


  METHOD get_.

    rs_ = ms_.

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
