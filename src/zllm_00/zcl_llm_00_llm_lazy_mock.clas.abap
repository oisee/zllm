CLASS zcl_llm_00_llm_lazy_mock DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_llm_00_types .
    INTERFACES zif_llm_00_llm_lazy .

    CLASS-METHODS new
      IMPORTING
        !is_       TYPE zif_llm_00_types=>ts_env
        !iv_k      TYPE string OPTIONAL
        !iv_v      TYPE string OPTIONAL
        !io_cache  TYPE REF TO zif_llm_00_cache OPTIONAL
      RETURNING
        VALUE(ro_) TYPE REF TO zif_llm_00_llm_lazy .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA ms_          TYPE zif_llm_00_llm_lazy~ts_llm_config.
    DATA ms_env       TYPE zif_llm_00_types=>ts_env .
    DATA:
      mv_k TYPE string,
      mv_v TYPE string.
    METHODS constructor
      IMPORTING
        !is_      TYPE zif_llm_00_types=>ts_env
        !iv_k     TYPE string
        !iv_v     TYPE string
        !io_cache TYPE REF TO zif_llm_00_cache.
    DATA: mo_cache TYPE REF TO zif_llm_00_cache.
ENDCLASS.



CLASS ZCL_LLM_00_LLM_LAZY_MOCK IMPLEMENTATION.


  METHOD constructor.
    mo_cache = io_cache.

    ms_-model_name = ms_env-api_model.
    ms_-model_type = zcl_llm_00_predictoken=>gc_llm_type-gpt.
    IF ms_-model_name = 'MISTRAL'.
      ms_-model_type = zcl_llm_00_predictoken=>gc_llm_type-mistral.
    ENDIF.
    ms_-max_token = ms_env-api_max_token.
    ms_-split_limit = ms_env-api_token_split_limit.

    mv_k = iv_k.
    mv_v = iv_v.
  ENDMETHOD.


  METHOD new.
    ro_ ?= NEW zcl_llm_00_llm_lazy_mock(
      is_      = is_
      iv_k     = iv_k
      iv_v     = iv_v
      io_cache = io_cache
    ).
  ENDMETHOD.


  METHOD zif_llm_00_llm_lazy~a.
    rv_ = '{ }'.
    "do nothing
  ENDMETHOD.


  METHOD zif_llm_00_llm_lazy~get_config.
    rs_ = ms_.
  ENDMETHOD.


  METHOD zif_llm_00_llm_lazy~q.
    "do nothing
    DATA(lv_json) = io_->to_json( ).
    IF iv_k IS INITIAL.
      DATA(lv_k) = lv_json.
    ELSE.
      lv_k = iv_k.
    ENDIF.

    ro_ = zcl_llm_00_llm_response=>new(
     iv_k     = lv_k
     iv_v     = mv_v
     io_cache = mo_cache
    ).
  ENDMETHOD.


  METHOD zif_llm_00_llm_lazy~get_payload_adapter.

    ro_ = zcl_llm_00_payload_adapter=>new( me ).

  ENDMETHOD.
ENDCLASS.
