interface ZIF_LLM_00_CODEC
  public .


  methods ENCODE
    importing
      !IV_ type XSTRING
    returning
      value(RV_) type XSTRING .
  methods DECODE
    importing
      !IV_ type XSTRING
    returning
      value(RV_) type XSTRING .
endinterface.
