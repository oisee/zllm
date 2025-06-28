CLASS lcl_ DEFINITION DEFERRED.
CLASS ZCL_LLM_00_PAT DEFINITION LOCAL FRIENDS lcl_.

CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
  PRIVATE SECTION.
    types: string_t TYPE zif_llm=>string_t.
    TYPES: BEGIN OF ts_,
             in  TYPE string,
             out TYPE string,
             f1  TYPE string,
             f2  TYPE string,
             f3  TYPE string,
           END OF ts_.

    TYPES: BEGIN OF ts_r,
             r1 TYPE REF TO ts_,
             r2 TYPE REF TO ts_,
           END OF ts_r.

    TYPES: BEGIN OF ts_s,
             s1 TYPE REF TO ts_,
             s2 TYPE REF TO ts_,
           END OF ts_s.

    TYPES: tt_         TYPE STANDARD TABLE OF ts_ WITH KEY in.
    TYPES: tt_r        TYPE STANDARD TABLE OF REF TO ts_    WITH DEFAULT KEY.
    TYPES: tt_r_string TYPE STANDARD TABLE OF REF TO string WITH DEFAULT KEY.

    TYPES: BEGIN OF ts_t,
             caption TYPE string,
             header  TYPE string,
             h1      TYPE string,
             h2      TYPE string,
             h3      TYPE string,
             t       TYPE tt_,
           END OF ts_t.

    TYPES: BEGIN OF ts_t_r,
             caption TYPE string,
             header  TYPE string,
             h1      TYPE string,
             h2      TYPE string,
             h3      TYPE string,
             t       TYPE tt_r,
           END OF ts_t_r.

    TYPES: BEGIN OF ts_t_r_string,
             caption TYPE string,
             header  TYPE string,
             h1      TYPE string,
             t       TYPE tt_r_string,
           END OF ts_t_r_string.

    TYPES: BEGIN OF ts_string_t,
             header TYPE string,
             t      TYPE string_t,
           END OF ts_string_t.

    TYPES: tt_tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    DATA:
      mo_cut TYPE REF TO ZCL_LLM_00_PAT.  "class under test

    METHODS: setup.
    METHODS: apply                 FOR TESTING.
    METHODS: apply_dref            FOR TESTING.
    METHODS: apply_scalar          FOR TESTING.
    METHODS: apply_struct          FOR TESTING.
    METHODS: apply_dref_struct     FOR TESTING.
    METHODS: apply_struct_table    FOR TESTING.
    METHODS: apply_struct_string_t FOR TESTING.
    METHODS: apply_table           FOR TESTING.
    METHODS: apply_string_t        FOR TESTING.
    METHODS: apply_string_t_f      FOR TESTING.
    METHODS: apply_string_t_m      FOR TESTING.
    METHODS: apply_string_t_l      FOR TESTING.
    METHODS: apply_flexi_list_multiline_f FOR TESTING.
    METHODS: apply_flexi_list_multiline_m FOR TESTING.
    METHODS: apply_flexi_list_multiline_l FOR TESTING.
    METHODS: apply_table_md FOR TESTING.
    METHODS: apply_table_of_dref_struct_md FOR TESTING.
    METHODS: apply_table_of_dref_scalar_md FOR TESTING.
    METHODS: new FOR TESTING.
ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.

  METHOD setup.
    DATA iv_        TYPE string.
    DATA iv_prefix  TYPE string.
    DATA iv_postfix TYPE string.

    mo_cut ?= ZCL_LLM_00_PAT=>new(
     `Scalar = {T}` && zif_llm=>n &&
     `Struct = {T-IN}, {T-OUT}` && zif_llm=>n &&
     `DataRef = {T-DR}` &&  zif_llm=>n &&
     `DataRef To Structure = {T-IN} {T-OUT}` &&  zif_llm=>n &&
     ``  &&  zif_llm=>n &&
     ``
    ).
  ENDMETHOD.

  METHOD apply.
    DATA: lv_ TYPE string VALUE 'Scalar String'.

    DATA(lv_act) = mo_cut->apply(
        ir_ = REF #( lv_ )
    ).

    DATA(lv_exp) = `Scalar = Scalar String` && zif_llm=>n &&
     `Struct = {T-IN}, {T-OUT}` &&  zif_llm=>n &&
     `DataRef = {T-DR}` &&  zif_llm=>n &&
     `DataRef To Structure = {T-IN} {T-OUT}` &&  zif_llm=>n &&
     ``  &&  zif_llm=>n &&
     ``.

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act
      exp   = lv_exp          "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.


  METHOD apply_dref.

    DATA: lv_ TYPE string VALUE 'Ref String'.

    DATA(lv_act) = mo_cut->apply(
        ir_     = REF #( lv_ )
        iv_root = 'T-DR'
    ).

    DATA(lv_exp) = `Scalar = {T}` && zif_llm=>n &&
     `Struct = {T-IN}, {T-OUT}` &&  zif_llm=>n &&
     `DataRef = Ref String` &&  zif_llm=>n &&
     `DataRef To Structure = {T-IN} {T-OUT}` &&  zif_llm=>n &&
     ``  &&  zif_llm=>n &&
     ``.

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act
      exp   = lv_exp "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.


  METHOD apply_scalar.

    DATA: lv_ TYPE string VALUE 'Scalar String'.

    DATA(lv_act) = mo_cut->apply(
        ir_ = REF #( lv_ )
    ).

    DATA(lv_exp) = `Scalar = Scalar String` && zif_llm=>n &&
     `Struct = {T-IN}, {T-OUT}` &&  zif_llm=>n &&
     `DataRef = {T-DR}` &&  zif_llm=>n &&
     `DataRef To Structure = {T-IN} {T-OUT}` &&  zif_llm=>n &&
     ``  &&  zif_llm=>n &&
     ``.

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act
      exp   = lv_exp "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.

  METHOD apply_struct.

    DATA(ls_) = VALUE ts_(
      in = `In String`
      out = 'Out CSEQ?'
    ).

    DATA(lv_act) = mo_cut->apply(
        ir_ = REF #( ls_ )
    ).

    DATA(lv_exp) = `Scalar = {T}` && zif_llm=>n &&
     `Struct = In String, Out CSEQ?` &&  zif_llm=>n &&
     `DataRef = {T-DR}` &&  zif_llm=>n &&
     `DataRef To Structure = In String Out CSEQ?` &&  zif_llm=>n &&
     ``  &&  zif_llm=>n &&
     ``.

*    cl_demo_output=>write( 'Act:' ).
*    cl_demo_output=>write( lv_act ).
*    cl_demo_output=>write( 'Exp:' ).
*    cl_demo_output=>write( lv_exp ).
*    cl_demo_output=>display( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act
      exp   = lv_exp "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.

  METHOD apply_dref_struct.

    DATA(ls_1) = VALUE ts_(
      in  = `In String`
      out = `Out String?`
    ).

    DATA(ls_2) = VALUE ts_(
      in  = `2In String`
      out = `2Out String`
    ).
    DATA(ls_) = VALUE ts_r(
      r1 = REF #( ls_1 )
      r2 = REF #( ls_2 )
    ).

    DATA(lo_cut) = ZCL_LLM_00_PAT=>new(
      `DataRef To Structure = {T-R1-IN} {T-R2-OUT}`
    ).

    DATA(lv_act) = lo_cut->apply(
        ir_ = REF #( ls_ )
    ).

    DATA(lv_exp) = `DataRef To Structure = In String 2Out String`.

*    cl_demo_output=>write( 'Act:' ).
*    cl_demo_output=>write( lv_act ).
*    cl_demo_output=>write( 'Exp:' ).
*    cl_demo_output=>write( lv_exp ).
*    cl_demo_output=>display( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act
      exp   = lv_exp "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).
  ENDMETHOD.


  METHOD apply_table.

    DATA(lt_) = VALUE tt_tab(
        ( 5  )
        ( 33  )
    ).

    DATA(lo_cut) = ZCL_LLM_00_PAT=>new(
      `# Title:  `  && zif_llm=>n &&
      `List:  `     && zif_llm=>n &&
      `{T}.`
    ).

    DATA(lv_act) = lo_cut->apply(
        ir_ = REF #( lt_ )
        iv_root = 'T'
    ).

    DATA(lv_exp) =
      `# Title:  `      && zif_llm=>n &&
      `List:  `         && zif_llm=>n &&
      `5.` && zif_llm=>n &&
      `33.`
    .
*    cl_demo_output=>write( 'Act:' ).
*    cl_demo_output=>write( lv_act ).
*    cl_demo_output=>write( 'Exp:' ).
*    cl_demo_output=>write( lv_exp ).
*    cl_demo_output=>display( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act
      exp   = lv_exp "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).

  ENDMETHOD.
  METHOD  apply_flexi_list_multiline_l  .
    DATA(lt_) = VALUE tt_(
         ( f1 = 'F1-1' f2 = 'F2-1' f3 = 'F3-1' )
         ( f1 = 'F1-2' f2 = 'F2-2' f3 = 'F3-2' )
         ( f1 = 'F1-3' f2 = 'F2-3' f3 = 'F3-3' )
         ( f1 = 'F1-4' f2 = 'F2-4' f3 = 'F3-4' )
     ).
    DATA(lo_cut) = ZCL_LLM_00_PAT=>new(
      `# Title:  `   && zif_llm=>n &&
      `FL:  `        && zif_llm=>n &&
      `aa|{T-F1}`    && zif_llm=>n &&
      `   {T-F2}`    && zif_llm=>n &&
      `	  {T-F3}|bb`
    ).

    DATA(lv_act) = lo_cut->apply(
        ir_ = REF #( lt_ )
        iv_root = 'T'
    ).

    DATA(lv_exp) =
      `# Title:  `  && zif_llm=>n &&
      `FL:  `       && zif_llm=>n &&
       `aa|F1-1`    && zif_llm=>n &&
       `   F2-1`    && zif_llm=>n &&
       `   F3-1|  ` && zif_llm=>n &&
       `  |F1-2`    && zif_llm=>n &&
       `   F2-2`    && zif_llm=>n &&
       `   F3-2|  ` && zif_llm=>n &&
       `  |F1-3`    && zif_llm=>n &&
       `   F2-3`    && zif_llm=>n &&
       `   F3-3|  ` && zif_llm=>n &&
       `  |F1-4`    && zif_llm=>n &&
       `   F2-4`    && zif_llm=>n &&
       `   F3-4|bb`
    .

    DATA(lv_act_) = lv_act.
    DATA(lv_exp_) = lv_exp.
    REPLACE ALL OCCURRENCES OF ` ` IN lv_act_ WITH '_'.
    REPLACE ALL OCCURRENCES OF ` ` IN lv_exp_ WITH '_'.

*    cl_demo_output=>write( 'Act:' ).
*    cl_demo_output=>write( lv_act_ ).
*    cl_demo_output=>write( 'Exp:' ).
*    cl_demo_output=>write( lv_exp_ ).
*    cl_demo_output=>display( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act_
      exp   = lv_exp_
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act
      exp   = lv_exp
    ).

  ENDMETHOD.
  METHOD  apply_flexi_list_multiline_f  .
    DATA(lt_) = VALUE tt_(
         ( f1 = 'F1-1' f2 = 'F2-1' f3 = 'F3-1' )
         ( f1 = 'F1-2' f2 = 'F2-2' f3 = 'F3-2' )
         ( f1 = 'F1-3' f2 = 'F2-3' f3 = 'F3-3' )
         ( f1 = 'F1-4' f2 = 'F2-4' f3 = 'F3-4' )
     ).
    DATA(lo_cut) = ZCL_LLM_00_PAT=>new(
      `aa| {T-F1} `    && zif_llm=>n &&
      `    {T-F2} `    && zif_llm=>n &&
      `	   {T-F3} |bb` && zif_llm=>n &&
      `# Footer:  `
    ).

    DATA(lv_act) = lo_cut->apply(
        ir_ = REF #( lt_ )
        iv_root = 'T'
    ).

    DATA(lv_exp) =
      `aa| F1-1 `    && zif_llm=>n &&
      `    F2-1 `    && zif_llm=>n &&
      `    F3-1 |  ` && zif_llm=>n &&
      `  | F1-2 `    && zif_llm=>n &&
      `    F2-2 `    && zif_llm=>n &&
      `    F3-2 |  ` && zif_llm=>n &&
      `  | F1-3 `    && zif_llm=>n &&
      `    F2-3 `    && zif_llm=>n &&
      `    F3-3 |  ` && zif_llm=>n &&
      `  | F1-4 `    && zif_llm=>n &&
      `    F2-4 `    && zif_llm=>n &&
      `    F3-4 |bb` && zif_llm=>n &&
      `# Footer:  `
    .

    DATA(lv_act_) = lv_act.
    DATA(lv_exp_) = lv_exp.
    REPLACE ALL OCCURRENCES OF ` ` IN lv_act_ WITH '_'.
    REPLACE ALL OCCURRENCES OF ` ` IN lv_exp_ WITH '_'.

*    cl_demo_output=>write( 'Act:' ).
*    cl_demo_output=>write( lv_act_ ).
*    cl_demo_output=>write( 'Exp:' ).
*    cl_demo_output=>write( lv_exp_ ).
*    cl_demo_output=>display( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act_
      exp   = lv_exp_
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act
      exp   = lv_exp
    ).

  ENDMETHOD.
  METHOD  apply_flexi_list_multiline_m  .
    DATA(lt_) = VALUE tt_(
         ( f1 = 'F1-1' f2 = 'F2-1' f3 = 'F3-1' )
         ( f1 = 'F1-2' f2 = 'F2-2' f3 = 'F3-2' )
         ( f1 = 'F1-3' f2 = 'F2-3' f3 = 'F3-3' )
         ( f1 = 'F1-4' f2 = 'F2-4' f3 = 'F3-4' )
     ).
    DATA(lo_cut) = ZCL_LLM_00_PAT=>new(
      `# Header:  `  && zif_llm=>n &&
      `aa| {T-F1} `    && zif_llm=>n &&
      `    {T-F2} `    && zif_llm=>n &&
      `	   {T-F3} |bb` && zif_llm=>n &&
      `# Footer:  `
    ).

    DATA(lv_act) = lo_cut->apply(
        ir_ = REF #( lt_ )
        iv_root = 'T'
    ).

    DATA(lv_exp) =
      `# Header:  `  && zif_llm=>n &&
      `aa| F1-1 `    && zif_llm=>n &&
      `    F2-1 `    && zif_llm=>n &&
      `    F3-1 |  ` && zif_llm=>n &&
      `  | F1-2 `    && zif_llm=>n &&
      `    F2-2 `    && zif_llm=>n &&
      `    F3-2 |  ` && zif_llm=>n &&
      `  | F1-3 `    && zif_llm=>n &&
      `    F2-3 `    && zif_llm=>n &&
      `    F3-3 |  ` && zif_llm=>n &&
      `  | F1-4 `    && zif_llm=>n &&
      `    F2-4 `    && zif_llm=>n &&
      `    F3-4 |bb` && zif_llm=>n &&
      `# Footer:  `
    .

    DATA(lv_act_) = lv_act.
    DATA(lv_exp_) = lv_exp.
    REPLACE ALL OCCURRENCES OF ` ` IN lv_act_ WITH '_'.
    REPLACE ALL OCCURRENCES OF ` ` IN lv_exp_ WITH '_'.

*    cl_demo_output=>write( 'Act:' ).
*    cl_demo_output=>write( lv_act_ ).
*    cl_demo_output=>write( 'Exp:' ).
*    cl_demo_output=>write( lv_exp_ ).
*    cl_demo_output=>display( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act_
      exp   = lv_exp_
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act
      exp   = lv_exp
    ).

  ENDMETHOD.

  METHOD  apply_table_md.
    DATA(ls_) = VALUE ts_t(
      caption = 'Caption'
      h1 = 'Header 1'
      h2 = 'Header 2'
      h3 = 'Header 3'
      t = VALUE tt_(
         ( f1 = 'F1-1' f2 = 'F2-1' f3 = 'F3-1' )
         ( f1 = 'F1-2' f2 = 'F2-2' f3 = 'F3-2' )
         ( f1 = 'F1-3' f2 = 'F2-3' f3 = 'F3-3' )
         ( f1 = 'F1-4' f2 = 'F2-4' f3 = 'F3-4' )
     ) ).

    DATA(lo_cut) = ZCL_LLM_00_PAT=>new(
      `# Caption: {S-CAPTION}`  && zif_llm=>n &&
      `a | {S-H1} | {S-H3} | {S-H2} | b` && zif_llm=>n &&
      `aa| {S-T-F1} | {S-T-F3} | {S-T-F2} |bb` && zif_llm=>n &&
      `# Footer:  `
    ).

    DATA(lv_act) = lo_cut->apply(
        ir_ = REF #( ls_ )
        iv_root = 'S'
    ).

    DATA(lv_exp) =
      `# Caption: Caption`  && zif_llm=>n &&
      `a | Header 1 | Header 3 | Header 2 | b` && zif_llm=>n &&
      `aa| F1-1 | F3-1 | F2-1 |bb` && zif_llm=>n &&
      `aa| F1-2 | F3-2 | F2-2 |bb` && zif_llm=>n &&
      `aa| F1-3 | F3-3 | F2-3 |bb` && zif_llm=>n &&
      `aa| F1-4 | F3-4 | F2-4 |bb` && zif_llm=>n &&
      `# Footer:  `
    .

    DATA(lv_act_) = lv_act.
    DATA(lv_exp_) = lv_exp.
    REPLACE ALL OCCURRENCES OF ` ` IN lv_act_ WITH '_'.
    REPLACE ALL OCCURRENCES OF ` ` IN lv_exp_ WITH '_'.

*    cl_demo_output=>write( 'Act:' ).
*    cl_demo_output=>write( lv_act_ ).
*    cl_demo_output=>write( 'Exp:' ).
*    cl_demo_output=>write( lv_exp_ ).
*    cl_demo_output=>display( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act_
      exp   = lv_exp_
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act
      exp   = lv_exp
    ).

  ENDMETHOD.

  METHOD apply_string_t_l.

    DATA(lt_) = VALUE string_t(
        ( `Line1 string_t`  )
        ( `Line2 string_t`  )
    ).

    DATA(lo_cut) = ZCL_LLM_00_PAT=>new(
      `# Title:  `  && zif_llm=>n &&
      `List:  `     && zif_llm=>n &&
      `{T}.`
    ).

    DATA(lv_act) = lo_cut->apply(
        ir_ = REF #( lt_ )
        iv_root = 'T'
    ).

    DATA(lv_exp) =
      `# Title:  `      && zif_llm=>n &&
      `List:  `         && zif_llm=>n &&
      `Line1 string_t.` && zif_llm=>n &&
      `Line2 string_t.`
    .
*    cl_demo_output=>write( 'Act:' ).
*    cl_demo_output=>write( lv_act ).
*    cl_demo_output=>write( 'Exp:' ).
*    cl_demo_output=>write( lv_exp ).
*    cl_demo_output=>display( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act
      exp   = lv_exp "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).

  ENDMETHOD.
  METHOD apply_string_t.

    DATA(lt_) = VALUE string_t(
        ( `Line1 string_t`  )
        ( `Line2 string_t`  )
    ).

    DATA(lo_cut) = ZCL_LLM_00_PAT=>new(
      `{T}.`
    ).

    DATA(lv_act) = lo_cut->apply(
        ir_ = REF #( lt_ )
        iv_root = 'T'
    ).

    DATA(lv_exp) =
      `Line1 string_t.` && zif_llm=>n &&
      `Line2 string_t.`
    .
    cl_demo_output=>write( 'Act:' ).
    cl_demo_output=>write( lv_act ).
    cl_demo_output=>write( 'Exp:' ).
    cl_demo_output=>write( lv_exp ).
    cl_demo_output=>display( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act
      exp   = lv_exp "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).

  ENDMETHOD.

  METHOD apply_string_t_m.

    DATA(lt_) = VALUE string_t(
        ( `Line1 string_t`  )
        ( `Line2 string_t`  )
    ).

    DATA(lo_cut) = ZCL_LLM_00_PAT=>new(
      `# pre` && zif_llm=>n &&
      `{T}`   && zif_llm=>n &&
      `# post`
    ).

    DATA(lv_act) = lo_cut->apply(
        ir_ = REF #( lt_ )
        iv_root = 'T'
    ).

    DATA(lv_exp) =
      `# pre` && zif_llm=>n &&
      `Line1 string_t` && zif_llm=>n &&
      `Line2 string_t` && zif_llm=>n &&
      `# post`
    .
    cl_demo_output=>write( 'Act:' ).
    cl_demo_output=>write( lv_act ).
    cl_demo_output=>write( 'Exp:' ).
    cl_demo_output=>write( lv_exp ).
    cl_demo_output=>display( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act
      exp   = lv_exp "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).

  ENDMETHOD.

  METHOD apply_string_t_f.

    DATA(lt_) = VALUE string_t(
        ( `Line1 string_t`  )
        ( `Line2 string_t`  )
    ).

    DATA(lo_cut) = ZCL_LLM_00_PAT=>new(
      `{T}.` && zif_llm=>n &&
      `post`
    ).

    DATA(lv_act) = lo_cut->apply(
        ir_ = REF #( lt_ )
        iv_root = 'T'
    ).

    DATA(lv_exp) =
      `Line1 string_t.` && zif_llm=>n &&
      `Line2 string_t.` && zif_llm=>n &&
      `post`
    .
*    cl_demo_output=>write( 'Act:' ).
*    cl_demo_output=>write( lv_act ).
*    cl_demo_output=>write( 'Exp:' ).
*    cl_demo_output=>write( lv_exp ).
*    cl_demo_output=>display( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act
      exp   = lv_exp "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).

  ENDMETHOD.


  METHOD apply_struct_string_t.
    DATA(ls_) = VALUE ts_string_t(
      header = 'string_t'
      t      = VALUE string_t(
        ( `Line1 string_t`  )
        ( `Line2 string_t`  )
      )
    ).

    DATA(lo_cut) = ZCL_LLM_00_PAT=>new(
      `List:  `              && zif_llm=>n &&
      `Header: {T-HEADER}`   && zif_llm=>n &&
      `{T-T}.`
    ).

    DATA(lv_act) = lo_cut->apply(
        ir_ = REF #( ls_ )
    ).

    DATA(lv_exp) =
      `List:  `             && zif_llm=>n &&
      `Header: string_t` && zif_llm=>n &&
      `Line1 string_t.`      && zif_llm=>n &&
      `Line2 string_t.`
    .
*    cl_demo_output=>write( 'Act:' ).
*    cl_demo_output=>write( lv_act ).
*    cl_demo_output=>write( 'Exp:' ).
*    cl_demo_output=>write( lv_exp ).
*    cl_demo_output=>display( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act
      exp   = lv_exp "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).

  ENDMETHOD.

  METHOD apply_struct_table.
    DATA(ls_) = VALUE ts_t(
      header = 'List Header'
      t      = VALUE tt_(
        ( in  = `Line1 In` out = `Line1 Out` )
        ( in  = `Line2 In` out = `Line2 Out` )
      )
    ).

    DATA(lo_cut) = ZCL_LLM_00_PAT=>new(
      `List:  `              && zif_llm=>n &&
      `Header: {T-HEADER}`   && zif_llm=>n &&
      `{T-T-IN}, {T-T-OUT}.`
    ).

    DATA(lv_act) = lo_cut->apply(
        ir_ = REF #( ls_ )
    ).

    DATA(lv_exp) =
      `List:  `              && zif_llm=>n &&
      `Header: List Header`  && zif_llm=>n &&
      `Line1 In, Line1 Out.` && zif_llm=>n &&
      `Line2 In, Line2 Out.`
    .
*    cl_demo_output=>write( 'Act:' ).
*    cl_demo_output=>write( lv_act ).
*    cl_demo_output=>write( 'Exp:' ).
*    cl_demo_output=>write( lv_exp ).
*    cl_demo_output=>display( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act
      exp   = lv_exp "<--- please adapt expected value
    " msg   = 'Testing value rv_'
*     level =
    ).

  ENDMETHOD.


  METHOD new.
*
*    DATA: lv_ TYPE string VALUE 'Scalar String'.
*
*    DATA(rv_) = mo_cut->apply(
*        ir_ = REF #( lv_ )
*    ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = rv_
*      exp   = rv_          "<--- please adapt expected value
*    " msg   = 'Testing value rv_'
**     level =
*    ).
  ENDMETHOD.

  METHOD  apply_table_of_dref_struct_md.
    DATA(ls_) = VALUE ts_t_r(
      caption = 'Caption'
      h1 = 'Header 1'
      h2 = 'Header 2'
      h3 = 'Header 3'
    ).

    DATA(ls_1) = VALUE ts_( f1 = 'F1-1' f2 = 'F2-1' f3 = 'F3-1' ).
    DATA(ls_2) = VALUE ts_( f1 = 'F1-2' f2 = 'F2-2' f3 = 'F3-2' ).
    DATA(ls_3) = VALUE ts_( f1 = 'F1-3' f2 = 'F2-3' f3 = 'F3-3' ).
    DATA(ls_4) = VALUE ts_( f1 = 'F1-4' f2 = 'F2-4' f3 = 'F3-4' ).

    APPEND REF #( ls_1 ) TO ls_-t.
    APPEND REF #( ls_2 ) TO ls_-t.
    APPEND REF #( ls_3 ) TO ls_-t.
    APPEND REF #( ls_4 ) TO ls_-t.

    DATA(lo_cut) = ZCL_LLM_00_PAT=>new(
      `# Caption: {S-CAPTION}`  && zif_llm=>n &&
      `a | {S-H1} | {S-H3} | {S-H2} | b` && zif_llm=>n &&
      `aa| {S-T-F1} | {S-T-F3} | {S-T-F2} |bb` && zif_llm=>n &&
      `# Footer:  `
    ).

    DATA(lv_act) = lo_cut->apply(
        ir_ = REF #( ls_ )
        iv_root = 'S'
    ).

    DATA(lv_exp) =
      `# Caption: Caption`  && zif_llm=>n &&
      `a | Header 1 | Header 3 | Header 2 | b` && zif_llm=>n &&
      `aa| F1-1 | F3-1 | F2-1 |bb` && zif_llm=>n &&
      `aa| F1-2 | F3-2 | F2-2 |bb` && zif_llm=>n &&
      `aa| F1-3 | F3-3 | F2-3 |bb` && zif_llm=>n &&
      `aa| F1-4 | F3-4 | F2-4 |bb` && zif_llm=>n &&
      `# Footer:  `
    .

    DATA(lv_act_) = lv_act.
    DATA(lv_exp_) = lv_exp.
    REPLACE ALL OCCURRENCES OF ` ` IN lv_act_ WITH '_'.
    REPLACE ALL OCCURRENCES OF ` ` IN lv_exp_ WITH '_'.

*    cl_demo_output=>write( 'Act:' ).
*    cl_demo_output=>write( lv_act_ ).
*    cl_demo_output=>write( 'Exp:' ).
*    cl_demo_output=>write( lv_exp_ ).
*    cl_demo_output=>display( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act_
      exp   = lv_exp_
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act
      exp   = lv_exp
    ).

  ENDMETHOD.

  METHOD  apply_table_of_dref_scalar_md.
    DATA(ls_) = VALUE ts_t_r_string(
      caption = 'Ref To Scalar'
      h1 = 'Header #'
    ).

    DATA(lv_1) = `Ref To String-1`.
    DATA(lv_2) = `Ref To String-2`.
    DATA(lv_3) = `Ref To String-3`.
    DATA(lv_4) = `Ref To String-4`.

    APPEND REF #( lv_1 ) TO ls_-t.
    APPEND REF #( lv_2 ) TO ls_-t.
    APPEND REF #( lv_3 ) TO ls_-t.
    APPEND REF #( lv_4 ) TO ls_-t.

    DATA(lo_cut) = ZCL_LLM_00_PAT=>new(
      `# Caption: {S-CAPTION}`  && zif_llm=>n &&
      `a | {S-H1} | b` && zif_llm=>n &&
      `aa| {S-T} |bb` && zif_llm=>n &&
      `# Footer:  `
    ).

    DATA(lv_act) = lo_cut->apply(
        ir_ = REF #( ls_ )
        iv_root = 'S'
    ).

    DATA(lv_exp) =
      `# Caption: Ref To Scalar`  && zif_llm=>n &&
      `a | Header # | b` && zif_llm=>n &&
      `aa| Ref To String-1 |bb` && zif_llm=>n &&
      `aa| Ref To String-2 |bb` && zif_llm=>n &&
      `aa| Ref To String-3 |bb` && zif_llm=>n &&
      `aa| Ref To String-4 |bb` && zif_llm=>n &&
      `# Footer:  `
    .

    DATA(lv_act_) = lv_act.
    DATA(lv_exp_) = lv_exp.
    REPLACE ALL OCCURRENCES OF ` ` IN lv_act_ WITH '_'.
    REPLACE ALL OCCURRENCES OF ` ` IN lv_exp_ WITH '_'.

*    cl_demo_output=>write( 'Act:' ).
*    cl_demo_output=>write( lv_act_ ).
*    cl_demo_output=>write( 'Exp:' ).
*    cl_demo_output=>write( lv_exp_ ).
*    cl_demo_output=>display( ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act_
      exp   = lv_exp_
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_act
      exp   = lv_exp
    ).

  ENDMETHOD.


ENDCLASS.
