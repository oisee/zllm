*"* use this source file for your ABAP unit test classes

CLASS lcl_ DEFINITION DEFERRED.

*CLASS zcl_llm_00_json DEFINITION LOCAL FRIENDS.

CLASS lcl_ DEFINITION FOR TESTING RISK LEVEL HARMLESS.
  PRIVATE SECTION .
    METHODS setup.
    METHODS flatten_dref FOR TESTING.
    METHODS flatten_table FOR TESTING.
    "data: mo_cut TYPE REF TO zcl_llm_00_json.
ENDCLASS.

CLASS lcl_ IMPLEMENTATION.
  METHOD setup.
    "mo_cut =
  ENDMETHOD.
  METHOD flatten_dref.
*    DATA lt_ TYPE zif_llm=>.
*    TYPES: tt_tt TYPE STANDARD TABLE OF zif_llm=>tt_gpart_vkont WITH DEFAULT KEY.
*    lt_ = VALUE #(
*      ( gpart = '111' vkont = '122' )
*      ( gpart = '211' vkont = '222' )
*      ( gpart = '311' vkont = '322' )
*      ( gpart = '411' vkont = '422' )
*      ( gpart = '511' vkont = '522' )
*    ).
*
*    DATA(lt_lt) = VALUE tt_tt(
*      ( lt_ )
*      ( lt_ )
*    ).
*
*    DATA(lv_j1) = zcl_llm_00_json=>to_json( lt_lt ).
*
*    DATA(lr_) = zcl_llm_00_json=>flatten_dref( REF #( lt_lt ) ).
*    DATA(lv_j2) = zcl_llm_00_json=>to_json( lr_ ).
*
*    cl_demo_output=>write( lv_j1 ).
*    cl_demo_output=>write( lv_j2 ).
*
*    cl_demo_output=>display( ).

  ENDMETHOD.

  METHOD flatten_table.
*    DATA lt_ TYPE zif_llm=>tt_gpart_vkont.
*    TYPES: tt_tt TYPE STANDARD TABLE OF zif_llm=>tt_gpart_vkont WITH DEFAULT KEY.
*    lt_ = VALUE #(
*      ( gpart = '111' vkont = '122' )
*      ( gpart = '211' vkont = '222' )
*      ( gpart = '311' vkont = '322' )
*      ( gpart = '411' vkont = '422' )
*      ( gpart = '511' vkont = '522' )
*    ).
*
*    DATA(lt_lt) = VALUE tt_tt(
*      ( lt_ )
*      ( lt_ )
*    ).
*
*    DATA(lv_j1) = zcl_llm_00_json=>to_json( lt_lt ).
*
*    DATA(lr_)   = zcl_llm_00_json=>flatten_dref( REF #( lt_lt ) ).
*    DATA(lv_j2) = zcl_llm_00_json=>to_json( lr_ ).
*
*    cl_demo_output=>write( lv_j1 ).
*    cl_demo_output=>write( lv_j2 ).
*
*    cl_demo_output=>display( ).
*
  ENDMETHOD.
ENDCLASS.
