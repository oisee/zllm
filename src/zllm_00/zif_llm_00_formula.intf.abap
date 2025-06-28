INTERFACE zif_llm_00_formula
  PUBLIC .

  TYPES: BEGIN OF ts_,
           sys TYPE string,
           usr TYPE string,
         END OF ts_.
  METHODS apply
    IMPORTING !ir_       TYPE REF TO data
    RETURNING VALUE(rs_) TYPE ts_ .
  METHODS get_sys  RETURNING VALUE(ro_) TYPE REF TO zif_llm_00_pat.
  METHODS get_usr  RETURNING VALUE(ro_) TYPE REF TO zif_llm_00_pat.
  METHODS get_name RETURNING VALUE(rv_) TYPE string .
ENDINTERFACE.
