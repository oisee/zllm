INTERFACE zif_llm_00_pat
  PUBLIC .


  METHODS apply
    IMPORTING
      !ir_       TYPE REF TO data
      !iv_root   TYPE string OPTIONAL
    RETURNING
      VALUE(rv_) TYPE string .
  METHODS get_name
    RETURNING
      VALUE(rv_) TYPE string .
ENDINTERFACE.
