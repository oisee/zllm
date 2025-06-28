INTERFACE zif_llm_00_step_lazy
  PUBLIC .

  METHODS exec
    IMPORTING
      !ir_       TYPE REF TO data OPTIONAL
      !io_       TYPE REF TO zif_llm_00_step_result OPTIONAL
        PREFERRED PARAMETER ir_
    RETURNING
      VALUE(rr_) TYPE REF TO data
    RAISING
      zcx_s .
  METHODS start
    IMPORTING
      !ir_       TYPE REF TO data OPTIONAL
      !io_       TYPE REF TO zif_llm_00_step_result OPTIONAL
        PREFERRED PARAMETER ir_
    RETURNING
      VALUE(ro_) TYPE REF TO zif_llm_00_step_result
    RAISING
      zcx_s .
  METHODS collect
    IMPORTING
      !io_       TYPE REF TO zif_llm_00_step_result
    RETURNING
      VALUE(rr_) TYPE REF TO data
    RAISING
      zcx_s .

ENDINTERFACE.
