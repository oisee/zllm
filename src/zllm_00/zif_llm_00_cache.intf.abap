INTERFACE zif_llm_00_cache
  PUBLIC .

  METHODS put
    IMPORTING
      k TYPE string
      v TYPE string.

  METHODS get
    IMPORTING
      k          TYPE string
    RETURNING
      VALUE(rv_) TYPE string.

  METHODS invalidate
    IMPORTING
      k TYPE string.

  METHODS clear.
  METHODS clear_for_seed
    IMPORTING iv_seed TYPE i.
  METHODS trim.
ENDINTERFACE.
