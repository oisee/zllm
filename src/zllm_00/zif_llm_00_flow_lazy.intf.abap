INTERFACE zif_llm_00_flow_lazy
  PUBLIC .

  INTERFACES: zif_llm_00_step_lazy.

  TYPES tt_formula TYPE zif_llm_00_formula_list=>tt_formula .
  TYPES tt_pat TYPE zif_llm_00_pat_list=>tt_pat .
  TYPES tt_step TYPE STANDARD TABLE OF REF TO zif_llm_00_step_lazy .

  METHODS next
    IMPORTING
      !ir_       TYPE REF TO data OPTIONAL
      !io_       TYPE REF TO zif_llm_00_step_result OPTIONAL
    EXPORTING
      !er_       TYPE REF TO data
      !eo_       TYPE REF TO zif_llm_00_step_result
    RETURNING
      VALUE(rv_) TYPE sap_bool
    RAISING
      zcx_s .
  METHODS go
    IMPORTING
      !ir_       TYPE REF TO data OPTIONAL
      !io_       TYPE REF TO zif_llm_00_step_result OPTIONAL
    RETURNING
      VALUE(rr_) TYPE REF TO data
    RAISING
      zcx_s .

  METHODS to_step RETURNING VALUE(ro_) TYPE REF TO zif_llm_00_step_lazy.
ENDINTERFACE.
