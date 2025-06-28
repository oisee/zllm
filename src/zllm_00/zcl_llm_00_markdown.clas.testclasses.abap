*"* use this source file for your ABAP unit test classes
*%
*% ABAP Markdown
*% (c) Guilherme Maeda
*% http://abap.ninja
*%
*% For the full license information, view the LICENSE file that was distributed
*% with this source code.
*%

*%
*%  Generated test code for the ZMARKDOWN class
*%
*%  Generated on 2018-03-15 by generate_abapunit_tests.py
*%  Do not change this code manualy!
*%

"!
"! Unit test class for the string template class
"!
CLASS string_tests DEFINITION FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    DATA: o_string TYPE REF TO lcl_string.
    METHODS:
      copying FOR TESTING.
ENDCLASS.                    "string_tests DEFINITION
*!
CLASS string_tests IMPLEMENTATION.
  METHOD copying.
    CREATE OBJECT o_string.
    o_string->data = 'SpongeBob'.

    DATA: lo_new TYPE REF TO lcl_string.
    CREATE OBJECT lo_new.
    lo_new->copy( o_string ).
    cl_aunit_assert=>assert_equals(
      exp = 'SpongeBob'
      act = lo_new->data
    ).
  ENDMETHOD.                    "copying
ENDCLASS.                    "string_tests IMPLEMENTATION


"!
"! Unit test class for the string array template class
"!
CLASS string_array_tests DEFINITION FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    DATA: o_sa TYPE REF TO lcl_string_array.
    METHODS:
      copying FOR TESTING,
      append FOR TESTING,
      append_array FOR TESTING,
      delete FOR TESTING,
      find FOR TESTING.
ENDCLASS.                    "string_array_tests DEFINITION
*!
CLASS string_array_tests IMPLEMENTATION.
  METHOD copying.
    CREATE OBJECT o_sa.
    o_sa->append( 'One' ).
    o_sa->append( 'Two' ).
    o_sa->append( 'Three' ).

    DATA: lo_new  TYPE REF TO lcl_string_array,
          lv_conc TYPE string.
    CREATE OBJECT lo_new.
    lo_new->copy( o_sa ).

    CONCATENATE LINES OF lo_new->data INTO lv_conc.
    cl_aunit_assert=>assert_equals(
      exp = 'OneTwoThree'
      act = lv_conc
    ).
  ENDMETHOD.                    "copying

  METHOD append.
    CREATE OBJECT o_sa.
    o_sa->append( 'One' ).
    o_sa->append( 'Two' ).
    o_sa->append( 'Three' ).

    DATA: lv_conc TYPE string.
    CONCATENATE LINES OF o_sa->data INTO lv_conc.
    cl_aunit_assert=>assert_equals(
      exp = 'OneTwoThree'
      act = lv_conc
    ).
  ENDMETHOD.                    "append

  METHOD append_array.
    CREATE OBJECT o_sa.
    o_sa->append( 'One' ).
    o_sa->append( 'Two' ).
    o_sa->append( 'Three' ).

    DATA: lo_new  TYPE REF TO lcl_string_array,
          lv_conc TYPE string.
    CREATE OBJECT lo_new.
    lo_new->append_array( o_sa ).

    CONCATENATE LINES OF lo_new->data INTO lv_conc.
    cl_aunit_assert=>assert_equals(
      exp = 'OneTwoThree'
      act = lv_conc
    ).
  ENDMETHOD.                    "append_array

  METHOD delete.
    CREATE OBJECT o_sa.
    o_sa->append( 'One' ).
    o_sa->append( 'Two' ).
    o_sa->append( 'Three' ).

    o_sa->delete( 'Two' ).

    DATA: lv_conc TYPE string.
    CONCATENATE LINES OF o_sa->data INTO lv_conc.
    cl_aunit_assert=>assert_equals(
      exp = 'OneThree'
      act = lv_conc
    ).
  ENDMETHOD.                    "delete

  METHOD find.
    CREATE OBJECT o_sa.
    o_sa->append( 'One' ).
    o_sa->append( 'Two' ).
    o_sa->append( 'Three' ).

    DATA: lv_index TYPE i.
    lv_index = o_sa->find( 'Two' ).
    cl_aunit_assert=>assert_equals(
      exp = 2
      act = lv_index
    ).
  ENDMETHOD.                    "find
ENDCLASS.                    "string_array_tests IMPLEMENTATION


"!
"! Unit test class for the hashmap template class
"!
CLASS hashmap_tests DEFINITION FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    DATA: o_hm TYPE REF TO lcl_hashmap.
    METHODS:
      copying FOR TESTING,
      create_string_hashmap FOR TESTING,
      create_array_hashmap FOR TESTING,
      create_hashmap_hashmap FOR TESTING,
      create_4_dimension_hashmap FOR TESTING,
      new FOR TESTING,
      get FOR TESTING,
      set FOR TESTING,
      exists FOR TESTING,
      delete FOR TESTING.
ENDCLASS.                    "hashmap_tests DEFINITION
*!
CLASS hashmap_tests IMPLEMENTATION.
  METHOD copying.
    CREATE OBJECT o_hm
      EXPORTING
        value_type = 'lcl_string'.
    DATA: lo_value TYPE REF TO lcl_string.
    lo_value ?= o_hm->new( 'IdxOne' ).
    lo_value->data = 'ValueOne'.

    DATA: lo_new TYPE REF TO lcl_hashmap.
    CREATE OBJECT lo_new
      EXPORTING
        value_type = 'lcl_string'.
    lo_new->copy( o_hm ).
    lo_value ?= lo_new->get( 'IdxOne' ).
    cl_aunit_assert=>assert_equals(
      exp = 'ValueOne'
      act = lo_value->data
    ).
  ENDMETHOD.                    "copying

  METHOD create_string_hashmap.
    DATA: lo_string TYPE REF TO lcl_string.
    CREATE OBJECT o_hm. "// default
    lo_string ?= o_hm->new( 'IdxOne' ).

    CREATE OBJECT o_hm
      EXPORTING
        value_type = 'lcl_string'.
    lo_string ?= o_hm->new( 'IdxOne' ).
  ENDMETHOD.                    "create_string_hashmap

  METHOD create_array_hashmap.
    DATA: lo_array TYPE REF TO lcl_string_array.
    CREATE OBJECT o_hm
      EXPORTING
        value_type = 'lcl_string_array'.
    lo_array ?= o_hm->new( 'IdxOne' ).
  ENDMETHOD.                    "create_array_hashmap

  METHOD create_hashmap_hashmap.
    DATA: lo_hashmap TYPE REF TO lcl_hashmap.
    CREATE OBJECT o_hm
      EXPORTING
        value_type = 'lcl_hashmap'.
    lo_hashmap ?= o_hm->new( 'IdxOne' ).
  ENDMETHOD.                    "create_hashmap_hashmap

  METHOD create_4_dimension_hashmap.
    DATA: lo_hashmap1 TYPE REF TO lcl_hashmap,
          lo_hashmap2 TYPE REF TO lcl_hashmap,
          lo_hashmap3 TYPE REF TO lcl_hashmap.
    CREATE OBJECT o_hm
      EXPORTING
        value_type = 'lcl_hashmap:lcl_hashmap:lcl_hashmap'.
    lo_hashmap1 ?= o_hm->new( 'IdxOne' ).
    lo_hashmap2 ?= lo_hashmap1->new( 'IdxTwo' ).
    lo_hashmap3 ?= lo_hashmap2->new( 'IdxThree' ).
  ENDMETHOD.                    "create_4_dimension_hashmap

  METHOD new.
    DATA: lo_string TYPE REF TO lcl_string.
    CREATE OBJECT o_hm.
    lo_string ?= o_hm->new( 'IdxOne' ).
    cl_aunit_assert=>assert_not_initial( lo_string ).
    lo_string->data = 'ValueOne'.

    lo_string ?= o_hm->new( 'IdxOne' ).
    cl_aunit_assert=>assert_initial( lo_string ).
  ENDMETHOD.                    "new

  METHOD get.
    DATA: lo_string TYPE REF TO lcl_string.
    CREATE OBJECT o_hm.
    lo_string ?= o_hm->get( 'IdxOne' ).
    cl_aunit_assert=>assert_not_initial( lo_string ).
    lo_string->data = 'ValueOne'.

    lo_string ?= o_hm->get( 'IdxOne' ).
    cl_aunit_assert=>assert_not_initial( lo_string ).
    cl_aunit_assert=>assert_equals(
      exp = 'ValueOne'
      act = lo_string->data
    ).
  ENDMETHOD.                    "get

  METHOD set.
    DATA: lo_str1 TYPE REF TO lcl_string,
          lo_str2 TYPE REF TO lcl_string.
    CREATE OBJECT lo_str1.
    lo_str1->data = 'ValueOne'.

    CREATE OBJECT o_hm.

    lo_str2 ?= o_hm->get( 'IdxOne' ).
    cl_aunit_assert=>assert_not_initial( lo_str2 ).
    cl_aunit_assert=>assert_initial( lo_str2->data ).

    o_hm->set(
      key   = 'IdxOne'
      value = lo_str1
    ).

    lo_str2 ?= o_hm->get( 'IdxOne' ).
    cl_aunit_assert=>assert_not_initial( lo_str2 ).
    cl_aunit_assert=>assert_equals(
      exp = 'ValueOne'
      act = lo_str2->data
    ).
  ENDMETHOD.                    "set

  METHOD exists.
    CREATE OBJECT o_hm.
    DATA: lv_exists TYPE flag.

    o_hm->new( 'IdxOne' ).

    lv_exists = o_hm->exists( 'IdxOne' ).
    cl_aunit_assert=>assert_not_initial( lv_exists ).

    lv_exists = o_hm->exists( 'IdxTwo' ).
    cl_aunit_assert=>assert_initial( lv_exists ).
  ENDMETHOD.                    "exists

  METHOD delete.
    CREATE OBJECT o_hm.
    o_hm->new( 'IdxOne' ).
    o_hm->new( 'IdxTwo' ).
    o_hm->new( 'IdxThree' ).

    DATA: lv_exists TYPE flag.
    lv_exists = o_hm->exists( 'IdxTwo' ).
    cl_aunit_assert=>assert_not_initial( lv_exists ).

    o_hm->delete( 'IdxTwo' ).
    lv_exists = o_hm->exists( 'IdxOne' ).
    cl_aunit_assert=>assert_not_initial( lv_exists ).
    lv_exists = o_hm->exists( 'IdxTwo' ).
    cl_aunit_assert=>assert_initial( lv_exists ).
    lv_exists = o_hm->exists( 'IdxThree' ).
    cl_aunit_assert=>assert_not_initial( lv_exists ).
  ENDMETHOD.                    "delete
ENDCLASS.                    "hashmap_tests IMPLEMENTATION


CLASS markdown_tests DEFINITION CREATE PRIVATE FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short

  PRIVATE SECTION.
    DATA: markdown TYPE REF TO zcl_llm_00_markdown.
    METHODS:
      constructor,
      aesthetic_table FOR TESTING,
      aligned_table FOR TESTING,
      atx_heading FOR TESTING,
      automatic_link FOR TESTING,
      block_level_html FOR TESTING,
      code_block FOR TESTING,
      code_span FOR TESTING,
      compound_blockquote FOR TESTING,
      compound_emphasis FOR TESTING,
      compound_list FOR TESTING,
      deeply_nested_list FOR TESTING,
      em_strong FOR TESTING,
      email FOR TESTING,
      emphasis FOR TESTING,
      escaping FOR TESTING,
      fenced_code_block FOR TESTING,
      horizontal_rule FOR TESTING,
      html_comment FOR TESTING,
      html_entity FOR TESTING,
      image_reference FOR TESTING,
      image_title FOR TESTING,
      implicit_reference FOR TESTING,
      inline_link FOR TESTING,
      inline_link_title FOR TESTING,
      inline_title FOR TESTING,
      lazy_blockquote FOR TESTING,
      lazy_list FOR TESTING,
      line_break FOR TESTING,
      multiline_list_paragraph FOR TESTING,
      multiline_lists FOR TESTING,
      nested_block_level_html FOR TESTING,
      ordered_list FOR TESTING,
      paragraph_list FOR TESTING,
      reference_title FOR TESTING,
      self_closing_html FOR TESTING,
      separated_nested_list FOR TESTING,
      setext_header FOR TESTING,
      simple_blockquote FOR TESTING,
      simple_table FOR TESTING,
      span_level_html FOR TESTING,
      sparse_dense_list FOR TESTING,
      sparse_html FOR TESTING,
      sparse_list FOR TESTING,
      special_characters FOR TESTING,
      strikethrough FOR TESTING,
      strong_em FOR TESTING,
      tab_indented_code_block FOR TESTING,
      table_inline_markdown FOR TESTING,
      text_reference FOR TESTING,
      unordered_list FOR TESTING,
      untidy_table FOR TESTING,
      url_autolinking FOR TESTING,
      whitespace FOR TESTING,
      xss_attribute_encoding FOR TESTING,
      xss_bad_url FOR TESTING,
      xss_text_encoding FOR TESTING.
ENDCLASS.                    "markdown_tests DEFINITION

*#
CLASS markdown_tests IMPLEMENTATION.
  METHOD constructor.
    me->markdown = zcl_llm_00_markdown=>new( ).
  ENDMETHOD.

  METHOD aesthetic_table.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '| header 1 | header 2 |' %_newline '| -------- | -------- |' %_newline
'| cell 1.1 | cell 1.2 |' %_newline '| cell 2.1 | cell 2.2 |' INTO lv_markdown RESPECTING
BLANKS .
    CONCATENATE '<table>' %_newline '<thead>' %_newline '<tr>' %_newline '<th>header 1</th>'
%_newline '<th>header 2</th>' %_newline '</tr>' %_newline '</thead>' %_newline '<tbody>'
%_newline '<tr>' %_newline '<td>cell 1.1</td>' %_newline '<td>cell 1.2</td>' %_newline
'</tr>' %_newline '<tr>' %_newline '<td>cell 2.1</td>' %_newline '<td>cell 2.2</td>'
%_newline '</tr>' %_newline '</tbody>' %_newline '</table>' INTO lv_expected_markup
RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD aligned_table.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '| header 1 | header 2 | header 2 |' %_newline
'| :------- | :------: | -------: |' %_newline '| cell 1.1 | cell 1.2 | cell 1.3 |'
%_newline '| cell 2.1 | cell 2.2 | cell 2.3 |' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<table>' %_newline '<thead>' %_newline '<tr>' %_newline
'<th style="text-align: left;">header 1</th>' %_newline
'<th style="text-align: center;">header 2</th>' %_newline
'<th style="text-align: right;">header 2</th>' %_newline '</tr>' %_newline '</thead>'
%_newline '<tbody>' %_newline '<tr>' %_newline
'<td style="text-align: left;">cell 1.1</td>' %_newline
'<td style="text-align: center;">cell 1.2</td>' %_newline
'<td style="text-align: right;">cell 1.3</td>' %_newline '</tr>' %_newline '<tr>'
%_newline '<td style="text-align: left;">cell 2.1</td>' %_newline
'<td style="text-align: center;">cell 2.2</td>' %_newline
'<td style="text-align: right;">cell 2.3</td>' %_newline '</tr>' %_newline '</tbody>'
%_newline '</table>' INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD atx_heading.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '# h1' %_newline %_newline '## h2' %_newline %_newline '### h3' %_newline
%_newline '#### h4' %_newline %_newline '##### h5' %_newline %_newline '###### h6'
%_newline %_newline '####### not a heading' %_newline %_newline '# closed h1 #' %_newline
%_newline '#' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<h1>h1</h1>' %_newline '<h2>h2</h2>' %_newline '<h3>h3</h3>' %_newline
'<h4>h4</h4>' %_newline '<h5>h5</h5>' %_newline '<h6>h6</h6>' %_newline
'<p>####### not a heading</p>' %_newline '<h1>closed h1</h1>' %_newline '<p>#</p>' INTO
lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD automatic_link.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    lv_markdown = '<http://example.com>'.
    lv_expected_markup = '<p><a href="http://example.com">http://example.com</a></p>'.
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD block_level_html.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '<div>_content_</div>' %_newline %_newline 'paragraph' %_newline %_newline
'<div>' %_newline '  <div class="inner">' %_newline '    _content_' %_newline '  </div>'
%_newline '</div>' %_newline %_newline '<style type="text/css">' %_newline
'  p {color: #789;}' %_newline '</style>' %_newline %_newline '<div>' %_newline
'  <a href="/">home</a></div>' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<div>_content_</div>' %_newline '<p>paragraph</p>' %_newline '<div>'
%_newline '  <div class="inner">' %_newline '    _content_' %_newline '  </div>' %_newline
'</div>' %_newline '<style type="text/css">' %_newline '  p {color: #789;}' %_newline
'</style>' %_newline '<div>' %_newline '  <a href="/">home</a></div>' INTO
lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD code_block.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '    <?php' %_newline %_newline '    $message = ''Hello World!'';' %_newline
'    echo $message;' %_newline %_newline '---' %_newline %_newline '    > not a quote'
%_newline '    - not a list item' %_newline '    [not a reference]: http://foo.com' INTO
lv_markdown RESPECTING BLANKS .
    CONCATENATE '<pre><code>&lt;?php' %_newline %_newline '$message = ''Hello World!'';'
%_newline 'echo $message;</code></pre>' %_newline '<hr />' %_newline
'<pre><code>&gt; not a quote' %_newline '- not a list item' %_newline
'[not a reference]: http://foo.com</code></pre>' INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD code_span.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE 'a `code span`' %_newline %_newline '`this is also a codespan` trailing text'
%_newline %_newline '`and look at this one!`' %_newline %_newline
'single backtick in a code span: `` ` ``' %_newline %_newline
'backtick-delimited string in a code span: `` `foo` ``' %_newline %_newline '`sth `` sth`'
INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<p>a <code>code span</code></p>' %_newline
'<p><code>this is also a codespan</code> trailing text</p>' %_newline
'<p><code>and look at this one!</code></p>' %_newline
'<p>single backtick in a code span: <code>`</code></p>' %_newline
'<p>backtick-delimited string in a code span: <code>`foo`</code></p>' %_newline
'<p><code>sth `` sth</code></p>' INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD compound_blockquote.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '> header' %_newline '> ------' %_newline '>' %_newline '> paragraph'
%_newline '>' %_newline '> - li' %_newline '>' %_newline '> ---' %_newline '>' %_newline
'> paragraph' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<blockquote>' %_newline '<h2>header</h2>' %_newline '<p>paragraph</p>'
%_newline '<ul>' %_newline '<li>li</li>' %_newline '</ul>' %_newline '<hr />' %_newline
'<p>paragraph</p>' %_newline '</blockquote>' INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD compound_emphasis.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '_`code`_ __`code`__' %_newline %_newline '*`code`**`code`**`code`*' %_newline
INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<p><em><code>code</code></em> <strong><code>code</code></strong></p>'
%_newline
'<p><em><code>code</code><strong><code>code</code></strong><code>code</code></em></p>'
INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD compound_list.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '- paragraph' %_newline %_newline '  paragraph' %_newline %_newline
'- paragraph' %_newline %_newline '  > quote' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<ul>' %_newline '<li>' %_newline '<p>paragraph</p>' %_newline
'<p>paragraph</p>' %_newline '</li>' %_newline '<li>' %_newline '<p>paragraph</p>'
%_newline '<blockquote>' %_newline '<p>quote</p>' %_newline '</blockquote>' %_newline
'</li>' %_newline '</ul>' INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD deeply_nested_list.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '- li' %_newline '    - li' %_newline '        - li' %_newline '        - li'
%_newline '    - li' %_newline '- li' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<ul>' %_newline '<li>li' %_newline '<ul>' %_newline '<li>li' %_newline '<ul>'
%_newline '<li>li</li>' %_newline '<li>li</li>' %_newline '</ul></li>' %_newline
'<li>li</li>' %_newline '</ul></li>' %_newline '<li>li</li>' %_newline '</ul>' INTO
lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD em_strong.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '___em strong___' %_newline %_newline '___em strong_ strong__' %_newline
%_newline '__strong _em strong___' %_newline %_newline '__strong _em strong_ strong__'
%_newline %_newline '***em strong***' %_newline %_newline '***em strong* strong**'
%_newline %_newline '**strong *em strong***' %_newline %_newline
'**strong *em strong* strong**' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<p><strong><em>em strong</em></strong></p>' %_newline
'<p><strong><em>em strong</em> strong</strong></p>' %_newline
'<p><strong>strong <em>em strong</em></strong></p>' %_newline
'<p><strong>strong <em>em strong</em> strong</strong></p>' %_newline
'<p><strong><em>em strong</em></strong></p>' %_newline
'<p><strong><em>em strong</em> strong</strong></p>' %_newline
'<p><strong>strong <em>em strong</em></strong></p>' %_newline
'<p><strong>strong <em>em strong</em> strong</strong></p>' INTO lv_expected_markup
RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD email.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE 'my email is <me@example.com>' %_newline %_newline
'html tags shouldn''t start an email autolink <strong>first.last@example.com</strong>'
INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<p>my email is <a href="mailto:me@example.com">me@example.com</a></p>'
%_newline
'<p>html tags shouldn''t start an email autolink <strong>first.last@example.com</strong></p>'
INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD emphasis.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '_underscore_, *asterisk*, _one two_, *three four*, _a_, *b*' %_newline
%_newline '**strong** and *em* and **strong** and *em*' %_newline %_newline '_line'
%_newline 'line' %_newline 'line_' %_newline %_newline 'this_is_not_an_emphasis' %_newline
%_newline 'an empty emphasis __ ** is not an emphasis' %_newline %_newline
'*mixed **double and* single asterisk** spans' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE
'<p><em>underscore</em>, <em>asterisk</em>, <em>one two</em>, <em>three four</em>, <em>a</e'
'm>, <em>b</em></p>' %_newline
'<p><strong>strong</strong> and <em>em</em> and <strong>strong</strong> and <em>em</em></p>'
%_newline '<p><em>line' %_newline 'line' %_newline 'line</em></p>' %_newline
'<p>this_is_not_an_emphasis</p>' %_newline
'<p>an empty emphasis __ ** is not an emphasis</p>' %_newline
'<p>*mixed *<em>double and</em> single asterisk** spans</p>' INTO lv_expected_markup
RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD escaping.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE 'escaped \*emphasis\*.' %_newline %_newline
'`escaped \*emphasis\* in a code span`' %_newline %_newline
'    escaped \*emphasis\* in a code block' %_newline %_newline
'\\ \` \* \_ \{ \} \[ \] \( \) \> \# \+ \- \. \!' %_newline %_newline
'_one\_two_ __one\_two__' %_newline %_newline '*one\*two* **one\*two**' INTO lv_markdown
RESPECTING BLANKS .
    CONCATENATE '<p>escaped *emphasis*.</p>' %_newline
'<p><code>escaped \*emphasis\* in a code span</code></p>' %_newline
'<pre><code>escaped \*emphasis\* in a code block</code></pre>' %_newline
'<p>\ ` * _ { } [ ] ( ) > # + - . !</p>' %_newline
'<p><em>one_two</em> <strong>one_two</strong></p>' %_newline
'<p><em>one*two</em> <strong>one*two</strong></p>' INTO lv_expected_markup RESPECTING
BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD fenced_code_block.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '```' %_newline '<?php' %_newline %_newline
'$message = ''fenced code block'';' %_newline 'echo $message;' %_newline '```' %_newline
%_newline '~~~' %_newline 'tilde' %_newline '~~~' %_newline %_newline '```php' %_newline
'echo ''language identifier'';' %_newline '```' %_newline %_newline '```c#' %_newline
'echo ''language identifier with non words'';' %_newline '```' %_newline %_newline
'```html+php' %_newline '<?php' %_newline 'echo "Hello World";' %_newline '?>' %_newline
'<a href="http://auraphp.com" >Aura Project</a>' %_newline '```' INTO lv_markdown
RESPECTING BLANKS .
    CONCATENATE '<pre><code>&lt;?php' %_newline %_newline '$message = ''fenced code block'';'
%_newline 'echo $message;</code></pre>' %_newline '<pre><code>tilde</code></pre>'
%_newline '<pre><code class="language-php">echo ''language identifier'';</code></pre>'
%_newline
'<pre><code class="language-c#">echo ''language identifier with non words'';</code></pre>'
%_newline '<pre><code class="language-html+php">&lt;?php' %_newline 'echo "Hello World";'
%_newline '?&gt;' %_newline
'&lt;a href="http://auraphp.com" &gt;Aura Project&lt;/a&gt;</code></pre>' INTO
lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD horizontal_rule.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '---' %_newline %_newline '- - -' %_newline %_newline '   - - -' %_newline
%_newline '***' %_newline %_newline '___' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<hr />' %_newline '<hr />' %_newline '<hr />' %_newline '<hr />' %_newline
'<hr />' INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD html_comment.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '<!-- single line -->' %_newline %_newline 'paragraph' %_newline %_newline
'<!-- ' %_newline '  multiline -->' %_newline %_newline 'paragraph' INTO lv_markdown
RESPECTING BLANKS .
    CONCATENATE '<!-- single line -->' %_newline '<p>paragraph</p>' %_newline '<!-- '
%_newline '  multiline -->' %_newline '<p>paragraph</p>' INTO lv_expected_markup
RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD html_entity.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    lv_markdown = '&amp; &copy; &#123;'.
    lv_expected_markup = '<p>&amp; &copy; &#123;</p>'.
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD image_reference.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '![Markdown Logo][image]' %_newline %_newline '[image]: /md.png' %_newline
%_newline '![missing reference]' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<p><img src="/md.png" alt="Markdown Logo" /></p>' %_newline
'<p>![missing reference]</p>' INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD image_title.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '![alt](/md.png "title")' %_newline %_newline '![blank title](/md.png "")'
INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<p><img src="/md.png" alt="alt" title="title" /></p>' %_newline
'<p><img src="/md.png" alt="blank title" title="" /></p>' INTO lv_expected_markup
RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD implicit_reference.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE 'an [implicit] reference link' %_newline %_newline
'[implicit]: http://example.com' %_newline %_newline
'an [implicit][] reference link with an empty link definition' %_newline %_newline
'an [implicit][] reference link followed by [another][]' %_newline %_newline
'[another]: http://cnn.com' %_newline %_newline
'an [explicit][example] reference link with a title' %_newline %_newline
'[example]: http://example.com "Example"' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<p>an <a href="http://example.com">implicit</a> reference link</p>' %_newline
'<p>an <a href="http://example.com">implicit</a> reference link with an empty link definiti'
'on</p>' %_newline
'<p>an <a href="http://example.com">implicit</a> reference link followed by <a href="http:/'
'/cnn.com">another</a></p>' %_newline
'<p>an <a href="http://example.com" title="Example">explicit</a> reference link with a titl'
'e</p>' INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD inline_link.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '[link](http://example.com)' %_newline %_newline
'[link](/url-(parentheses)) with parentheses in URL ' %_newline %_newline
'([link](/index.php)) in parentheses' %_newline %_newline '[`link`](http://example.com)'
%_newline %_newline '[![MD Logo](http://parsedown.org/md.png)](http://example.com)'
%_newline %_newline
'[![MD Logo](http://parsedown.org/md.png) and text](http://example.com)' %_newline
%_newline
'[![MD Logo](data:image/png;base64,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'
'BIWXMAAAsTAAALEwEAmpwYAAAEImlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPHg6eG1wbWV0YSB4bWxuczp4PSJh'
'ZG9iZTpuczptZXRhLyIgeDp4bXB0az0iWE1QIENvcmUgNS40LjAiPgogICA8cmRmOlJERiB4bWxuczpyZGY9Imh0dH'
'A6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPgogICAgICA8cmRmOkRlc2NyaXB0aW9uIHJk'
'ZjphYm91dD0iIgogICAgICAgICAgICB4bWxuczp0aWZmPSJodHRwOi8vbnMuYWRvYmUuY29tL3RpZmYvMS4wLyIKIC'
'AgICAgICAgICAgeG1sbnM6ZXhpZj0iaHR0cDovL25zLmFkb2JlLmNvbS9leGlmLzEuMC8iCiAgICAgICAgICAgIHht'
'bG5zOmRjPSJodHRwOi8vcHVybC5vcmcvZGMvZWxlbWVudHMvMS4xLyIKICAgICAgICAgICAgeG1sbnM6eG1wPSJodH'
'RwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIj4KICAgICAgICAgPHRpZmY6UmVzb2x1dGlvblVuaXQ+MTwvdGlmZjpS'
'ZXNvbHV0aW9uVW5pdD4KICAgICAgICAgPHRpZmY6Q29tcHJlc3Npb24+NTwvdGlmZjpDb21wcmVzc2lvbj4KICAgIC'
'AgICAgPHRpZmY6WFJlc29sdXRpb24+NzI8L3RpZmY6WFJlc29sdXRpb24+CiAgICAgICAgIDx0aWZmOk9yaWVudGF0'
'aW9uPjE8L3RpZmY6T3JpZW50YXRpb24+CiAgICAgICAgIDx0aWZmOllSZXNvbHV0aW9uPjcyPC90aWZmOllSZXNvbH'
'V0aW9uPgogICAgICAgICA8ZXhpZjpQaXhlbFhEaW1lbnNpb24+MzI8L2V4aWY6UGl4ZWxYRGltZW5zaW9uPgogICAg'
'ICAgICA8ZXhpZjpDb2xvclNwYWNlPjE8L2V4aWY6Q29sb3JTcGFjZT4KICAgICAgICAgPGV4aWY6UGl4ZWxZRGltZW'
'5zaW9uPjIwPC9leGlmOlBpeGVsWURpbWVuc2lvbj4KICAgICAgICAgPGRjOnN1YmplY3Q+CiAgICAgICAgICAgIDxy'
'ZGY6QmFnLz4KICAgICAgICAgPC9kYzpzdWJqZWN0PgogICAgICAgICA8eG1wOk1vZGlmeURhdGU+MjAxNS0wNi0xNF'
'QxOTowNjo1OTwveG1wOk1vZGlmeURhdGU+CiAgICAgICAgIDx4bXA6Q3JlYXRvclRvb2w+UGl4ZWxtYXRvciAzLjI8'
'L3htcDpDcmVhdG9yVG9vbD4KICAgICAgPC9yZGY6RGVzY3JpcHRpb24+CiAgIDwvcmRmOlJERj4KPC94OnhtcG1ldG'
'E+Ch7v5WoAAAGgSURBVEgNYywtLbVnYmLqYmBgMANieoJT//79K2MBWr4CaKsEPW2G2mUGspsFZnlnZycjPR1RXl7+'
'H2Q3Ez0txWbXgDsAFAUYABo8YPH////HdXV1LUZWVFZWFsvIyLgIJoYt+pDNwCYP00swBIAWzaysrNSCaQCxgWLTYH'
'xKaawhgGYoJzC7rC4sLDQBiYPYQIoHTQ3ZXGIcADJci42NDeZreGiQbSuSRmKiABb/CUB9IMwAjAKYGIhLESAYAj9/'
'/kwH+t4YaAvM59c4ODiyvn//HotuMzDh9QLFirCIg/I8CPQBE2QxhAkhCYZAf3//d2CJFQpU/h2EQeyGhoYvyIbA2F'
'DDl8H4aPQydMtB8gQdAFLU3t5+DRjsWSAMYoPEcAFOTs5EoNw+NPl9UHE0YQYGglEA09HR0bEAxsZHA0PnFzAqgoBq'
'9gIxKOrOAnEQSBxIYwCiQgBDFwEBYFB/BEaVJ7AQ2wGiQXxcWhhhJRZQ0UBURsSlAVyup4Y4TaKAFIeBouAJUIM0KZ'
'qoqPYpEzBrpQANfEFFQ4k16gXIbgCggnKoJ5DJdwAAAABJRU5ErkJggg==) and text](http://example.com)'
INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<p><a href="http://example.com">link</a></p>' %_newline
'<p><a href="/url-(parentheses)">link</a> with parentheses in URL </p>' %_newline
'<p>(<a href="/index.php">link</a>) in parentheses</p>' %_newline
'<p><a href="http://example.com"><code>link</code></a></p>' %_newline
'<p><a href="http://example.com"><img src="http://parsedown.org/md.png" alt="MD Logo" /></a'
'></p>' %_newline
'<p><a href="http://example.com"><img src="http://parsedown.org/md.png" alt="MD Logo" /> an'
'd text</a></p>' %_newline
'<p><a href="http://example.com"><img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAC'
'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'
'fAn88ngmbWbm3/eE8/syOll+AAAACXBIWXMAAAsTAAALEwEAmpwYAAAEImlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAA'
'AAPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iWE1QIENvcmUgNS40LjAiPgogICA8'
'cmRmOlJERiB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPgogIC'
'AgICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIgogICAgICAgICAgICB4bWxuczp0aWZmPSJodHRwOi8vbnMu'
'YWRvYmUuY29tL3RpZmYvMS4wLyIKICAgICAgICAgICAgeG1sbnM6ZXhpZj0iaHR0cDovL25zLmFkb2JlLmNvbS9leG'
'lmLzEuMC8iCiAgICAgICAgICAgIHhtbG5zOmRjPSJodHRwOi8vcHVybC5vcmcvZGMvZWxlbWVudHMvMS4xLyIKICAg'
'ICAgICAgICAgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIj4KICAgICAgICAgPHRpZmY6Um'
'Vzb2x1dGlvblVuaXQ+MTwvdGlmZjpSZXNvbHV0aW9uVW5pdD4KICAgICAgICAgPHRpZmY6Q29tcHJlc3Npb24+NTwv'
'dGlmZjpDb21wcmVzc2lvbj4KICAgICAgICAgPHRpZmY6WFJlc29sdXRpb24+NzI8L3RpZmY6WFJlc29sdXRpb24+Ci'
'AgICAgICAgIDx0aWZmOk9yaWVudGF0aW9uPjE8L3RpZmY6T3JpZW50YXRpb24+CiAgICAgICAgIDx0aWZmOllSZXNv'
'bHV0aW9uPjcyPC90aWZmOllSZXNvbHV0aW9uPgogICAgICAgICA8ZXhpZjpQaXhlbFhEaW1lbnNpb24+MzI8L2V4aW'
'Y6UGl4ZWxYRGltZW5zaW9uPgogICAgICAgICA8ZXhpZjpDb2xvclNwYWNlPjE8L2V4aWY6Q29sb3JTcGFjZT4KICAg'
'ICAgICAgPGV4aWY6UGl4ZWxZRGltZW5zaW9uPjIwPC9leGlmOlBpeGVsWURpbWVuc2lvbj4KICAgICAgICAgPGRjOn'
'N1YmplY3Q+CiAgICAgICAgICAgIDxyZGY6QmFnLz4KICAgICAgICAgPC9kYzpzdWJqZWN0PgogICAgICAgICA8eG1w'
'Ok1vZGlmeURhdGU+MjAxNS0wNi0xNFQxOTowNjo1OTwveG1wOk1vZGlmeURhdGU+CiAgICAgICAgIDx4bXA6Q3JlYX'
'RvclRvb2w+UGl4ZWxtYXRvciAzLjI8L3htcDpDcmVhdG9yVG9vbD4KICAgICAgPC9yZGY6RGVzY3JpcHRpb24+CiAg'
'IDwvcmRmOlJERj4KPC94OnhtcG1ldGE+Ch7v5WoAAAGgSURBVEgNYywtLbVnYmLqYmBgMANieoJT//79K2MBWr4CaK'
'sEPW2G2mUGspsFZnlnZycjPR1RXl7+H2Q3Ez0txWbXgDsAFAUYABo8YPH////HdXV1LUZWVFZWFsvIyLgIJoYt+pDN'
'wCYP00swBIAWzaysrNSCaQCxgWLTYHxKaawhgGYoJzC7rC4sLDQBiYPYQIoHTQ3ZXGIcADJci42NDeZreGiQbSuSRm'
'KiABb/CUB9IMwAjAKYGIhLESAYAj9//kwH+t4YaAvM59c4ODiyvn//HotuMzDh9QLFirCIg/I8CPQBE2QxhAkhCYZA'
'f3//d2CJFQpU/h2EQeyGhoYvyIbA2FDDl8H4aPQydMtB8gQdAFLU3t5+DRjsWSAMYoPEcAFOTs5EoNw+NPl9UHE0YQ'
'YGglEA09HR0bEAxsZHA0PnFzAqgoBq9gIxKOrOAnEQSBxIYwCiQgBDFwEBYFB/BEaVJ7AQ2wGiQXxcWhhhJRZQ0UBU'
'RsSlAVyup4Y4TaKAFIeBouAJUIM0KZqoqPYpEzBrpQANfEFFQ4k16gXIbgCggnKoJ5DJdwAAAABJRU5ErkJggg==" '
'alt="MD Logo" /> and text</a></p>' INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD inline_link_title.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '[single quotes](http://example.com ''Title'')' %_newline %_newline
'[double quotes](http://example.com "Title")' %_newline %_newline
'[single quotes blank](http://example.com '''')' %_newline %_newline
'[double quotes blank](http://example.com "")' %_newline %_newline
'[space](http://example.com "2 Words")' %_newline %_newline
'[parentheses](http://example.com/url-(parentheses) "Title")' INTO lv_markdown RESPECTING
BLANKS .
    CONCATENATE '<p><a href="http://example.com" title="Title">single quotes</a></p>'
%_newline '<p><a href="http://example.com" title="Title">double quotes</a></p>' %_newline
'<p><a href="http://example.com" title="">single quotes blank</a></p>' %_newline
'<p><a href="http://example.com" title="">double quotes blank</a></p>' %_newline
'<p><a href="http://example.com" title="2 Words">space</a></p>' %_newline
'<p><a href="http://example.com/url-(parentheses)" title="Title">parentheses</a></p>' INTO
lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD inline_title.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE
'[single quotes](http://example.com ''Example'') and [double quotes](http://example.com "Exam'
'ple")' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE
'<p><a href="http://example.com" title="Example">single quotes</a> and <a href="http://exam'
'ple.com" title="Example">double quotes</a></p>' INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD lazy_blockquote.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '> quote' %_newline 'the rest of it' %_newline %_newline '> another paragraph'
%_newline 'the rest of it' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<blockquote>' %_newline '<p>quote' %_newline 'the rest of it</p>' %_newline
'<p>another paragraph' %_newline 'the rest of it</p>' %_newline '</blockquote>' INTO
lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD lazy_list.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '- li' %_newline 'the rest of it' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<ul>' %_newline '<li>li' %_newline 'the rest of it</li>' %_newline '</ul>'
INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD line_break.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE 'line  ' %_newline 'line' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<p>line<br />' %_newline 'line</p>' INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD multiline_list_paragraph.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '- li' %_newline %_newline '  line' %_newline '  line' INTO lv_markdown
RESPECTING BLANKS .
    CONCATENATE '<ul>' %_newline '<li>' %_newline '<p>li</p>' %_newline '<p>line' %_newline
'line</p>' %_newline '</li>' %_newline '</ul>' INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD multiline_lists.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '1. One' %_newline '   First body copy' %_newline %_newline '2. Two' %_newline
'   Last body copy' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<ol>' %_newline '<li>' %_newline '<p>One' %_newline 'First body copy</p>'
%_newline '</li>' %_newline '<li>' %_newline '<p>Two' %_newline 'Last body copy</p>'
%_newline '</li>' %_newline '</ol>' INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD nested_block_level_html.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '<div>' %_newline '_parent_' %_newline '<div>' %_newline '_child_' %_newline
'</div>' %_newline '<pre>' %_newline '_adopted child_' %_newline '</pre>' %_newline
'</div>' %_newline %_newline '_outside_' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<div>' %_newline '_parent_' %_newline '<div>' %_newline '_child_' %_newline
'</div>' %_newline '<pre>' %_newline '_adopted child_' %_newline '</pre>' %_newline
'</div>' %_newline '<p><em>outside</em></p>' INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD ordered_list.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '1. one' %_newline '2. two' %_newline %_newline 'repeating numbers:' %_newline
%_newline '1. one' %_newline '1. two' %_newline %_newline 'large numbers:' %_newline
%_newline '123. one' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<ol>' %_newline '<li>one</li>' %_newline '<li>two</li>' %_newline '</ol>'
%_newline '<p>repeating numbers:</p>' %_newline '<ol>' %_newline '<li>one</li>' %_newline
'<li>two</li>' %_newline '</ol>' %_newline '<p>large numbers:</p>' %_newline
'<ol start="123">' %_newline '<li>one</li>' %_newline '</ol>' INTO lv_expected_markup
RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD paragraph_list.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE 'paragraph' %_newline '- li' %_newline '- li' %_newline %_newline 'paragraph'
%_newline %_newline '   * li' %_newline '   ' %_newline '   * li' INTO lv_markdown
RESPECTING BLANKS .
    CONCATENATE '<p>paragraph</p>' %_newline '<ul>' %_newline '<li>li</li>' %_newline
'<li>li</li>' %_newline '</ul>' %_newline '<p>paragraph</p>' %_newline '<ul>' %_newline
'<li>' %_newline '<p>li</p>' %_newline '</li>' %_newline '<li>' %_newline '<p>li</p>'
%_newline '</li>' %_newline '</ul>' INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD reference_title.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '[double quotes] and [single quotes] and [parentheses]' %_newline %_newline
'[double quotes]: http://example.com "example title"' %_newline
'[single quotes]: http://example.com ''example title''' %_newline
'[parentheses]: http://example.com (example title)' %_newline
'[invalid title]: http://example.com example title' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE
'<p><a href="http://example.com" title="example title">double quotes</a> and <a href="http:'
'//example.com" title="example title">single quotes</a> and <a href="http://example.com" ti'
'tle="example title">parentheses</a></p>' %_newline
'<p>[invalid title]: <a href="http://example.com">http://example.com</a> example title</p>'
INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD self_closing_html.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '<hr>' %_newline 'paragraph' %_newline '<hr/>' %_newline 'paragraph' %_newline
'<hr />' %_newline 'paragraph' %_newline '<hr class="foo" id="bar" />' %_newline
'paragraph' %_newline '<hr class="foo" id="bar"/>' %_newline 'paragraph' %_newline
'<hr class="foo" id="bar" >' %_newline 'paragraph' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<hr>' %_newline '<p>paragraph</p>' %_newline '<hr/>' %_newline
'<p>paragraph</p>' %_newline '<hr />' %_newline '<p>paragraph</p>' %_newline
'<hr class="foo" id="bar" />' %_newline '<p>paragraph</p>' %_newline
'<hr class="foo" id="bar"/>' %_newline '<p>paragraph</p>' %_newline
'<hr class="foo" id="bar" >' %_newline '<p>paragraph</p>' INTO lv_expected_markup
RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD separated_nested_list.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '- li' %_newline %_newline '    - li' %_newline '    - li' INTO lv_markdown
RESPECTING BLANKS .
    CONCATENATE '<ul>' %_newline '<li>' %_newline '<p>li</p>' %_newline '<ul>' %_newline
'<li>li</li>' %_newline '<li>li</li>' %_newline '</ul>' %_newline '</li>' %_newline
'</ul>' INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD setext_header.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE 'h1' %_newline '==' %_newline %_newline 'h2' %_newline '--' %_newline
%_newline 'single character' %_newline '-' %_newline %_newline 'not a header' %_newline
%_newline '------------' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<h1>h1</h1>' %_newline '<h2>h2</h2>' %_newline '<h2>single character</h2>'
%_newline '<p>not a header</p>' %_newline '<hr />' INTO lv_expected_markup RESPECTING
BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD simple_blockquote.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '> quote' %_newline %_newline 'indented:' %_newline '   > quote' %_newline
%_newline 'no space after `>`:' %_newline '>quote' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<blockquote>' %_newline '<p>quote</p>' %_newline '</blockquote>' %_newline
'<p>indented:</p>' %_newline '<blockquote>' %_newline '<p>quote</p>' %_newline
'</blockquote>' %_newline '<p>no space after <code>&gt;</code>:</p>' %_newline
'<blockquote>' %_newline '<p>quote</p>' %_newline '</blockquote>' INTO lv_expected_markup
RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD simple_table.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE 'header 1 | header 2' %_newline '-------- | --------' %_newline
'cell 1.1 | cell 1.2' %_newline 'cell 2.1 | cell 2.2' %_newline %_newline '---' %_newline
%_newline 'header 1 | header 2' %_newline ':------- | --------' %_newline
'cell 1.1 | cell 1.2' %_newline 'cell 2.1 | cell 2.2' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<table>' %_newline '<thead>' %_newline '<tr>' %_newline '<th>header 1</th>'
%_newline '<th>header 2</th>' %_newline '</tr>' %_newline '</thead>' %_newline '<tbody>'
%_newline '<tr>' %_newline '<td>cell 1.1</td>' %_newline '<td>cell 1.2</td>' %_newline
'</tr>' %_newline '<tr>' %_newline '<td>cell 2.1</td>' %_newline '<td>cell 2.2</td>'
%_newline '</tr>' %_newline '</tbody>' %_newline '</table>' %_newline '<hr />' %_newline
'<table>' %_newline '<thead>' %_newline '<tr>' %_newline
'<th style="text-align: left;">header 1</th>' %_newline '<th>header 2</th>' %_newline
'</tr>' %_newline '</thead>' %_newline '<tbody>' %_newline '<tr>' %_newline
'<td style="text-align: left;">cell 1.1</td>' %_newline '<td>cell 1.2</td>' %_newline
'</tr>' %_newline '<tr>' %_newline '<td style="text-align: left;">cell 2.1</td>' %_newline
'<td>cell 2.2</td>' %_newline '</tr>' %_newline '</tbody>' %_newline '</table>' INTO
lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD span_level_html.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE 'an <b>important</b> <a href=''''>link</a>' %_newline %_newline 'broken<br/>'
%_newline 'line' %_newline %_newline '<b>inline tag</b> at the beginning' %_newline
%_newline '<span>http://example.com</span>' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<p>an <b>important</b> <a href=''''>link</a></p>' %_newline '<p>broken<br/>'
%_newline 'line</p>' %_newline '<p><b>inline tag</b> at the beginning</p>' %_newline
'<p><span><a href="http://example.com">http://example.com</a></span></p>' INTO
lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD sparse_dense_list.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '- li' %_newline %_newline '- li' %_newline '- li' INTO lv_markdown RESPECTING
BLANKS .
    CONCATENATE '<ul>' %_newline '<li>' %_newline '<p>li</p>' %_newline '</li>' %_newline
'<li>' %_newline '<p>li</p>' %_newline '</li>' %_newline '<li>' %_newline '<p>li</p>'
%_newline '</li>' %_newline '</ul>' INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD sparse_html.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '<div>' %_newline 'line 1' %_newline %_newline 'line 2' %_newline 'line 3'
%_newline %_newline 'line 4' %_newline '</div>' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<div>' %_newline 'line 1' %_newline %_newline 'line 2' %_newline 'line 3'
%_newline %_newline 'line 4' %_newline '</div>' INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD sparse_list.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '- li' %_newline %_newline '- li' %_newline %_newline '---' %_newline
%_newline '- li' %_newline %_newline '    - indented li' INTO lv_markdown RESPECTING
BLANKS .
    CONCATENATE '<ul>' %_newline '<li>' %_newline '<p>li</p>' %_newline '</li>' %_newline
'<li>' %_newline '<p>li</p>' %_newline '</li>' %_newline '</ul>' %_newline '<hr />'
%_newline '<ul>' %_newline '<li>' %_newline '<p>li</p>' %_newline '<ul>' %_newline
'<li>indented li</li>' %_newline '</ul>' %_newline '</li>' %_newline '</ul>' INTO
lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD special_characters.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE 'AT&T has an ampersand in their name' %_newline %_newline 'this & that'
%_newline %_newline '4 < 5 and 6 > 5' %_newline %_newline
'<http://example.com/autolink?a=1&b=2>' %_newline %_newline
'[inline link](/script?a=1&b=2)' %_newline %_newline '[reference link][1]' %_newline
%_newline '[1]: http://example.com/?a=1&b=2' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<p>AT&amp;T has an ampersand in their name</p>' %_newline
'<p>this &amp; that</p>' %_newline '<p>4 &lt; 5 and 6 &gt; 5</p>' %_newline
'<p><a href="http://example.com/autolink?a=1&amp;b=2">http://example.com/autolink?a=1&amp;b'
'=2</a></p>' %_newline '<p><a href="/script?a=1&amp;b=2">inline link</a></p>' %_newline
'<p><a href="http://example.com/?a=1&amp;b=2">reference link</a></p>' INTO
lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD strikethrough.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '~~strikethrough~~' %_newline %_newline
'here''s ~~one~~ followed by ~~another one~~' %_newline %_newline
'~~ this ~~ is not one neither is ~this~' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<p><del>strikethrough</del></p>' %_newline
'<p>here''s <del>one</del> followed by <del>another one</del></p>' %_newline
'<p>~~ this ~~ is not one neither is ~this~</p>' INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD strong_em.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '*em **strong em***' %_newline %_newline '***strong em** em*' %_newline
%_newline '*em **strong em** em*' %_newline %_newline '_em __strong em___' %_newline
%_newline '___strong em__ em_' %_newline %_newline '_em __strong em__ em_' INTO
lv_markdown RESPECTING BLANKS .
    CONCATENATE '<p><em>em <strong>strong em</strong></em></p>' %_newline
'<p><em><strong>strong em</strong> em</em></p>' %_newline
'<p><em>em <strong>strong em</strong> em</em></p>' %_newline
'<p><em>em <strong>strong em</strong></em></p>' %_newline
'<p><em><strong>strong em</strong> em</em></p>' %_newline
'<p><em>em <strong>strong em</strong> em</em></p>' INTO lv_expected_markup RESPECTING
BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD tab_indented_code_block.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '' %_horizontal_tab '<?php' %_newline '' %_horizontal_tab '' %_newline
'' %_horizontal_tab '$message = ''Hello World!'';' %_newline
'' %_horizontal_tab 'echo $message;' %_newline %_newline
'' %_horizontal_tab 'echo "following a blank line";' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<pre><code>&lt;?php' %_newline %_newline '$message = ''Hello World!'';'
%_newline 'echo $message;' %_newline %_newline
'echo "following a blank line";</code></pre>' INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD table_inline_markdown.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '| _header_ 1   | header 2     |' %_newline '| ------------ | ------------ |'
%_newline '| _cell_ 1.1   | ~~cell~~ 1.2 |' %_newline '| `|` 2.1      | \| 2.2       |'
%_newline '| `\|` 2.1     | [link](/)    |' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<table>' %_newline '<thead>' %_newline '<tr>' %_newline
'<th><em>header</em> 1</th>' %_newline '<th>header 2</th>' %_newline '</tr>' %_newline
'</thead>' %_newline '<tbody>' %_newline '<tr>' %_newline '<td><em>cell</em> 1.1</td>'
%_newline '<td><del>cell</del> 1.2</td>' %_newline '</tr>' %_newline '<tr>' %_newline
'<td><code>|</code> 2.1</td>' %_newline '<td>| 2.2</td>' %_newline '</tr>' %_newline
'<tr>' %_newline '<td><code>\|</code> 2.1</td>' %_newline '<td><a href="/">link</a></td>'
%_newline '</tr>' %_newline '</tbody>' %_newline '</table>' INTO lv_expected_markup
RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD text_reference.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '[reference link][1]' %_newline %_newline '[1]: http://example.com' %_newline
%_newline '[one][website] with a semantic name' %_newline %_newline
'[website]: http://example.com' %_newline %_newline '[one][404] with no definition'
%_newline %_newline '[multiline' %_newline 'one][website] defined on 2 lines' %_newline
%_newline '[one][Label] with a mixed case label and an upper case definition' %_newline
%_newline '[LABEL]: http://example.com' %_newline %_newline '[one]' %_newline
'[1] with the a label on the next line' %_newline %_newline '[`link`][website]' INTO
lv_markdown RESPECTING BLANKS .
    CONCATENATE '<p><a href="http://example.com">reference link</a></p>' %_newline
'<p><a href="http://example.com">one</a> with a semantic name</p>' %_newline
'<p>[one][404] with no definition</p>' %_newline
'<p><a href="http://example.com">multiline' %_newline 'one</a> defined on 2 lines</p>'
%_newline
'<p><a href="http://example.com">one</a> with a mixed case label and an upper case definiti'
'on</p>' %_newline
'<p><a href="http://example.com">one</a> with the a label on the next line</p>' %_newline
'<p><a href="http://example.com"><code>link</code></a></p>' INTO lv_expected_markup
RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD unordered_list.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '- li' %_newline '- li' %_newline %_newline 'mixed markers:' %_newline
%_newline '* li' %_newline '+ li' %_newline '- li' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<ul>' %_newline '<li>li</li>' %_newline '<li>li</li>' %_newline '</ul>'
%_newline '<p>mixed markers:</p>' %_newline '<ul>' %_newline '<li>li</li>' %_newline
'<li>li</li>' %_newline '<li>li</li>' %_newline '</ul>' INTO lv_expected_markup RESPECTING
BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD untidy_table.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '| header 1 | header 2          |' %_newline '| ------------- | ----------- |'
%_newline '| cell 1.1   | cell 1.2 |' %_newline '|    cell 2.1 | cell 2.2     |' INTO
lv_markdown RESPECTING BLANKS .
    CONCATENATE '<table>' %_newline '<thead>' %_newline '<tr>' %_newline '<th>header 1</th>'
%_newline '<th>header 2</th>' %_newline '</tr>' %_newline '</thead>' %_newline '<tbody>'
%_newline '<tr>' %_newline '<td>cell 1.1</td>' %_newline '<td>cell 1.2</td>' %_newline
'</tr>' %_newline '<tr>' %_newline '<td>cell 2.1</td>' %_newline '<td>cell 2.2</td>'
%_newline '</tr>' %_newline '</tbody>' %_newline '</table>' INTO lv_expected_markup
RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD url_autolinking.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE 'an autolink http://example.com' %_newline %_newline
'inside of brackets [http://example.com], inside of braces {http://example.com},  inside of'
' parentheses (http://example.com)' %_newline %_newline
'trailing slash http://example.com/ and http://example.com/path/' INTO lv_markdown
RESPECTING BLANKS .
    CONCATENATE '<p>an autolink <a href="http://example.com">http://example.com</a></p>'
%_newline
'<p>inside of brackets [<a href="http://example.com">http://example.com</a>], inside of bra'
'ces {<a href="http://example.com">http://example.com</a>},  inside of parentheses (<a href'
'="http://example.com">http://example.com</a>)</p>' %_newline
'<p>trailing slash <a href="http://example.com/">http://example.com/</a> and <a href="http:'
'//example.com/path/">http://example.com/path/</a></p>' INTO lv_expected_markup RESPECTING
BLANKS .
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD whitespace.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '    ' %_newline %_newline '    code' %_newline %_newline '    ' INTO
lv_markdown RESPECTING BLANKS .
    lv_expected_markup = '<pre><code>code</code></pre>'.
    me->markdown->set_safe_mode( abap_false ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD xss_attribute_encoding.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '[xss](https://www.example.com")' %_newline %_newline
'![xss](https://www.example.com")' %_newline %_newline '[xss](https://www.example.com'')'
%_newline %_newline '![xss](https://www.example.com'')' %_newline %_newline
'![xss"](https://www.example.com)' %_newline %_newline '![xss''](https://www.example.com)'
INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<p><a href="https://www.example.com&quot;">xss</a></p>' %_newline
'<p><img src="https://www.example.com&quot;" alt="xss" /></p>' %_newline
'<p><a href="https://www.example.com&#039;">xss</a></p>' %_newline
'<p><img src="https://www.example.com&#039;" alt="xss" /></p>' %_newline
'<p><img src="https://www.example.com" alt="xss&quot;" /></p>' %_newline
'<p><img src="https://www.example.com" alt="xss&#039;" /></p>' INTO lv_expected_markup
RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_true ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD xss_bad_url.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '[xss](javascript:alert(1))' %_newline %_newline '[xss]( javascript:alert(1))'
%_newline %_newline '[xss](javascript://alert(1))' %_newline %_newline
'[xss](javascript&colon;alert(1))' %_newline %_newline '![xss](javascript:alert(1))'
%_newline %_newline '![xss]( javascript:alert(1))' %_newline %_newline
'![xss](javascript://alert(1))' %_newline %_newline '![xss](javascript&colon;alert(1))'
%_newline %_newline '[xss](data:text/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==)'
%_newline %_newline '[xss]( data:text/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==)'
%_newline %_newline '[xss](data://text/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==)'
%_newline %_newline
'[xss](data&colon;text/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==)' %_newline
%_newline '![xss](data:text/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==)' %_newline
%_newline '![xss]( data:text/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==)' %_newline
%_newline '![xss](data://text/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==)' %_newline
%_newline '![xss](data&colon;text/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==)' INTO
lv_markdown RESPECTING BLANKS .
    CONCATENATE '<p><a href="javascript%3Aalert(1)">xss</a></p>' %_newline
'<p><a href="javascript%3Aalert(1)">xss</a></p>' %_newline
'<p><a href="javascript%3A//alert(1)">xss</a></p>' %_newline
'<p><a href="javascript&amp;colon;alert(1)">xss</a></p>' %_newline
'<p><img src="javascript%3Aalert(1)" alt="xss" /></p>' %_newline
'<p><img src="javascript%3Aalert(1)" alt="xss" /></p>' %_newline
'<p><img src="javascript%3A//alert(1)" alt="xss" /></p>' %_newline
'<p><img src="javascript&amp;colon;alert(1)" alt="xss" /></p>' %_newline
'<p><a href="data%3Atext/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==">xss</a></p>'
%_newline
'<p><a href="data%3Atext/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==">xss</a></p>'
%_newline
'<p><a href="data%3A//text/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==">xss</a></p>'
%_newline
'<p><a href="data&amp;colon;text/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==">xss</a><'
'/p>' %_newline
'<p><img src="data%3Atext/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==" alt="xss" /></p'
'>' %_newline
'<p><img src="data%3Atext/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==" alt="xss" /></p'
'>' %_newline
'<p><img src="data%3A//text/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==" alt="xss" /><'
'/p>' %_newline
'<p><img src="data&amp;colon;text/html;base64,PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==" alt="xs'
's" /></p>' INTO lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_true ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.


  METHOD xss_text_encoding.
    DATA: lv_markdown        TYPE string,
          lv_expected_markup TYPE string,
          lv_actual_markup   TYPE string.
    CONCATENATE '<script>alert(1)</script>' %_newline %_newline '<script>' %_newline %_newline
'alert(1)' %_newline %_newline '</script>' %_newline %_newline %_newline '<script>'
%_newline 'alert(1)' %_newline '</script>' INTO lv_markdown RESPECTING BLANKS .
    CONCATENATE '<p>&lt;script&gt;alert(1)&lt;/script&gt;</p>' %_newline
'<p>&lt;script&gt;</p>' %_newline '<p>alert(1)</p>' %_newline '<p>&lt;/script&gt;</p>'
%_newline '<p>&lt;script&gt;' %_newline 'alert(1)' %_newline '&lt;/script&gt;</p>' INTO
lv_expected_markup RESPECTING BLANKS .
    me->markdown->set_safe_mode( abap_true ).
    lv_actual_markup = me->markdown->text( lv_markdown ).
    cl_aunit_assert=>assert_equals(
      act = lv_actual_markup
      exp = lv_expected_markup
    ).
  ENDMETHOD.

ENDCLASS.                    "markdown_tests IMPLEMENTATION
