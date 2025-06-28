INTERFACE zif_llm_00_step_result
  PUBLIC .

  INTERFACES zif_llm_00_string.

  ALIASES:
    to_string FOR zif_llm_00_string~to_string.
  ALIASES
    predict_tokens FOR zif_llm_00_string~predict_tokens.

  METHODS collect
    RETURNING VALUE(rr_) TYPE REF TO data
    RAISING   zcx_s .
  METHODS collect_raw
    RETURNING VALUE(rv_) TYPE string
    RAISING   zcx_s .
  METHODS collect_embed
    RETURNING VALUE(ro_) TYPE REF TO zcl_llm_00_embed_out
    RAISING   zcx_s .
  METHODS is_json RETURNING VALUE(rv_) TYPE sap_bool.

ENDINTERFACE.
