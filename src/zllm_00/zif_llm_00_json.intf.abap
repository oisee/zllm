INTERFACE zif_llm_00_json
  PUBLIC .


  METHODS to_json
    RETURNING
      VALUE(rv_) TYPE string .
ENDINTERFACE.
