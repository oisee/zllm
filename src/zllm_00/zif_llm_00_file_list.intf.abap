interface ZIF_LLM_00_FILE_LIST
  public .


  types:
    BEGIN OF ts_file,
      name TYPE string,
      file TYPE REF TO zif_llm_00_file,
    END OF ts_file .
  types:
  "TYPES: tt_file TYPE STANDARD TABLE OF REF TO zif_llm_00_file WITH DEFAULT KEY.
    tt_file TYPE STANDARD TABLE OF ts_file WITH KEY name .

  methods GET
    returning
      value(RT_) type TT_FILE .
  methods FILTER
    importing
      !IV_ type STRING
      !IO_ type ref to ZCL_LLM_00_LIST optional
    preferred parameter IV_
    returning
      value(RT_) type TT_FILE .
  methods GET_BY_NAME
    importing
      !IV_ type STRING
    returning
      value(RO_) type ref to ZIF_LLM_00_FILE .

  methods SAVE
    importing
      !IO_ type REF TO ZIF_LLM_00_FILE .
endinterface.
