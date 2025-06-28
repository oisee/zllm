
CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zif_llm_00_file.  "class under test

    METHODS: setup.
    METHODS: get_string FOR TESTING.
    METHODS: get_xstring FOR TESTING.
    METHODS: new FOR TESTING.
ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.

  METHOD setup.

    mo_cut = zcl_llm_00_file_mock=>new(
               iv_content = 'Content!'
               iv_path    = 'c:\temp\default.txt'
             ).
  ENDMETHOD.



  METHOD get_string.

    DATA rv_ TYPE string.
    rv_ = mo_cut->get_string(  ).
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = rv_                          " Data object with current value
        exp                  = 'Content!'                   " Data object with expected type
    ).

  ENDMETHOD.


  METHOD get_xstring.

    DATA rv_ TYPE xstring.
    rv_ = mo_cut->get_xstring(  ).
  ENDMETHOD.


  METHOD new.

    DATA iv_content TYPE string.
    DATA iv_path TYPE string.
    DATA ro_ TYPE REF TO zif_llm_00_file.

    ro_ = zcl_llm_00_file_mock=>new(
       IV_CONTENT = iv_Content
       IV_PATH = iv_Path
    ).
  ENDMETHOD.




ENDCLASS.
