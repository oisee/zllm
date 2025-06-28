CLASS zcl_llm_00_llm_lazy_composite DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_llm_00_types .
    INTERFACES zif_llm_00_llm_lazy .

    CLASS-METHODS new
      IMPORTING
        !io_llm       TYPE REF TO zif_llm_00_llm_lazy
        !io_llm_exp   TYPE REF TO zif_llm_00_llm_lazy
        !iv_threshold TYPE i
        !io_cache     TYPE REF TO zif_llm_00_cache OPTIONAL
      RETURNING
        VALUE(ro_)    TYPE REF TO zif_llm_00_llm_lazy .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_llm      TYPE REF TO zif_llm_00_llm_lazy .
    DATA mo_llm_exp  TYPE REF TO zif_llm_00_llm_lazy .             "expensive
    DATA mv_treshold TYPE i .
    DATA mo_predictoken TYPE REF TO zcl_llm_00_predictoken .
    DATA mo_predictoken_exp TYPE REF TO zcl_llm_00_predictoken .

    DATA ms_         TYPE zif_llm_00_llm_lazy~ts_llm_config.
    DATA ms_exp      TYPE zif_llm_00_llm_lazy~ts_llm_config.

    METHODS constructor
      IMPORTING
        !io_llm       TYPE REF TO zif_llm_00_llm_lazy
        !io_llm_exp   TYPE REF TO zif_llm_00_llm_lazy
        !iv_threshold TYPE i
        !io_cache     TYPE REF TO zif_llm_00_cache.
    DATA: mo_cache TYPE REF TO zif_llm_00_cache.
ENDCLASS.



CLASS ZCL_LLM_00_LLM_LAZY_COMPOSITE IMPLEMENTATION.


  METHOD constructor.
    mo_cache = io_cache.
    " Assigning the imported interface references to the private attributes to hold the lightweight and expensive LLM implementations.
    mo_llm      = io_llm.
    mo_llm_exp  = io_llm_exp.
    mv_treshold = iv_threshold.     " the threshold value which might be used to decide between lightweight and expensive operations.

    mo_predictoken     = zcl_llm_00_predictoken=>new_for_llm( mo_llm ).
    mo_predictoken_exp = zcl_llm_00_predictoken=>new_for_llm( mo_llm_exp ).

    ms_    = io_llm->get_config( ).
    ms_exp = io_llm_exp->get_config( ).

  ENDMETHOD.


  METHOD new.
    " Factory method to create an instance of the composite class
    " It encapsulates the creation logic and returns an interface reference
    ro_ = NEW zcl_llm_00_llm_lazy_composite(
      io_llm       = io_llm
      io_llm_exp   = io_llm_exp
      iv_threshold = iv_threshold
      io_cache     = io_cache
    ).
  ENDMETHOD.


  METHOD zif_llm_00_llm_lazy~get_config.
    rs_ = ms_exp.
  ENDMETHOD.


  METHOD zif_llm_00_llm_lazy~a.
    rv_ = io_->v( ).
  ENDMETHOD.


  METHOD zif_llm_00_llm_lazy~q.
    " Decide which implementation to use based on the threshold
    DATA(lv_json) = io_->to_json( ).
    DATA(lv_tokens) = mo_predictoken->predict( lv_json ).

    IF lv_tokens <= mv_treshold .
      DATA(lv_model_exp) = ms_exp-model_name.
      DATA(lv_model)     = ms_-model_name.
      REPLACE FIRST OCCURRENCE OF |"model":"{ lv_model_exp }"| IN lv_json WITH |"model":"{ lv_model }"|.
      DATA(lo_json) = zcl_llm_00_json_mock=>new( lv_json ).
      ro_ = mo_llm->q(
        io_  = lo_json
        iv_k = iv_k
      ).
    ELSE.
      ro_ = mo_llm_exp->q(
        io_  = io_
        iv_k = iv_k
      ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_llm_00_llm_lazy~get_payload_adapter.

    ro_ = zcl_llm_00_payload_adapter=>new( me ).

  ENDMETHOD.
ENDCLASS.
