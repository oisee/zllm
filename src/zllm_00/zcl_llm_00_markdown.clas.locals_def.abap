*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
"!
"! Value type interface
"!
interface lif_value_type.
  methods: copy importing source type ref to lif_value_type.
endinterface.                    "lif_value_type

"!
"! String class for use in template objects
"!
class lcl_string definition final.
  public section.
    interfaces lif_value_type.
    data: data type string.
    aliases: copy for lif_value_type~copy.
endclass.                    "lcl_string DEFINITION

"!
"! String array class for use in template objects
"!
class lcl_string_array definition final.
  public section.
    interfaces lif_value_type.
    data: data type table of string.

    methods:
      append importing value type clike,
      append_array importing array type ref to lcl_string_array,
      delete importing value type clike,
      find importing value type clike returning value(index) type i.

    aliases: copy for lif_value_type~copy.
endclass.                    "lcl_array DEFINITION

"!
"! Hashmap template class
"! The key type is `string`, and the value type must be an object.
"!
"! A compound value type may be used, separating the basic type and its subsequent
"!  value type by a colon.
"! Ex: 'lcl_hashmap:lcl_string_array' =&gt; The value type will be lcl_hashmap,
"!     and the value hashmaps' values type will be of type lcl_string_array.
"! Ex: 'lcl_hashmap:lcl_hashmap:lcl_hashmap:lcl_string' =&gt; Recursive composition,
"!     for use of a 4-dimensional hashmap.
"!
class lcl_hashmap definition final.
  public section.
    interfaces lif_value_type.
    types:
    begin of ty_item,
      key type string,
      value type ref to lif_value_type,
    end of ty_item,
    ty_hashmap type hashed table of ty_item with unique key key.

    data: data type ty_hashmap.

    methods:
      constructor importing value(value_type) type clike default 'lcl_string',
      new importing key type clike returning value(value) type ref to lif_value_type,
      exists importing key type clike returning value(exists) type flag,
      get importing key type clike returning value(value) type ref to lif_value_type,
      set importing key type clike value type ref to lif_value_type,
      delete importing key type string.

    aliases: copy for lif_value_type~copy.

  private section.
    data: value_type type string,
          subsequent_hashmap_value_type type string.
endclass.                    "lcl_hashmap DEFINITION
