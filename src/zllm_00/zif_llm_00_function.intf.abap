interface ZIF_LLM_00_FUNCTION
  public .


  types:
    tt_import_parameter   TYPE STANDARD TABLE OF rsimp    WITH DEFAULT KEY .
  types:
    tt_changing_parameter TYPE STANDARD TABLE OF rscha    WITH DEFAULT KEY .
  types:
    tt_export_parameter   TYPE STANDARD TABLE OF rsexp    WITH DEFAULT KEY .
  types:
    tt_tables_parameter   TYPE STANDARD TABLE OF rstbl    WITH DEFAULT KEY .
  types:
    tt_exception_list     TYPE STANDARD TABLE OF rsexc    WITH DEFAULT KEY .
  types:
    tt_documentation      TYPE STANDARD TABLE OF rsfdo    WITH DEFAULT KEY .
  types:
    tt_source             TYPE STANDARD TABLE OF rssource WITH DEFAULT KEY .
  types:
    BEGIN OF ts_par,
      imp TYPE tt_import_parameter,
      chn TYPE tt_changing_parameter,
      exp TYPE tt_export_parameter,
      tab TYPE tt_tables_parameter,
      exc TYPE tt_exception_list,
      doc TYPE tt_documentation,
      src TYPE tt_source,
    END OF ts_par .

  methods INVOKE
    importing
      !IV_ type STRING optional
    returning
      value(RV_) type STRING
    raising
      ZCX_S .
endinterface.
