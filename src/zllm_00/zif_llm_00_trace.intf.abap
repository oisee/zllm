INTERFACE zif_llm_00_trace
  PUBLIC .

  METHODS in IMPORTING iv_   TYPE string
                       iv_id TYPE string OPTIONAL.

  METHODS out IMPORTING iv_   TYPE string
                        iv_id TYPE string OPTIONAL.

ENDINTERFACE.
