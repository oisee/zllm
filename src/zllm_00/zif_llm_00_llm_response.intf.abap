INTERFACE zif_llm_00_llm_response
  PUBLIC .

  METHODS k
    RETURNING
      VALUE(rv_) TYPE string.

  METHODS v
    RETURNING
      VALUE(rv_) TYPE string
      RAISING zcx_s.

ENDINTERFACE.
