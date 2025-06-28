*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
*!
class lcl_string implementation.
  method lif_value_type~copy.
    "! Copies the value of the source object to itself
    data: lo_string type ref to lcl_string.
    lo_string ?= source.
    me->data = lo_string->data.
  endmethod.                    "lif_value_type~copy
endclass.                    "lcl_string IMPLEMENTATION


*!
class lcl_string_array implementation.
  method append.
    "! Append a value to the end of the array
    append value to me->data.
  endmethod.                    "append

  method append_array.
    "! Append the items of an array to this array
    field-symbols: <item> type string.
    loop at array->data assigning <item>.
      append( <item> ).
    endloop.
  endmethod.                    "append_array

  method delete.
    "! Deletes a value from the array
    delete me->data where table_line = value.
  endmethod.                    "delete

  method find.
    "! Returns the index of the first occurrence of a value in the array,
    "!  or 0 if not found.
    read table me->data with key table_line = value
      transporting no fields.
    if sy-subrc = 0.
      index = sy-tabix.
    endif.
  endmethod.                    "find

  method lif_value_type~copy.
    "! Copies the value of the source object to itself
    data: lo_sa type ref to lcl_string_array.
    lo_sa ?= source.
    me->data = lo_sa->data.
  endmethod.                    "lif_value_type~copy
endclass.                    "lcl_array IMPLEMENTATION


*!
class lcl_hashmap implementation.
  method constructor.
    "! Hashmap constructor
    "!
    "! @parameter value_type The value part class name. This must be a valid
    "!                       ABAP class name, or a composition of valid ABAP
    "!                       class names separated by a colon.
    if value_type cs ':'.
      find regex '^([^\s:]+)(?::(.+))?$' in value_type
        submatches me->value_type me->subsequent_hashmap_value_type.
      if sy-subrc <> 0.
        me->value_type = value_type.
      endif.
    else.
      me->value_type = value_type.
    endif.
    translate: me->value_type to upper case,
               me->subsequent_hashmap_value_type to upper case.
  endmethod.                    "constructor

  method new.
    "! Adds a new item to the hashmap
    "! The value part in the new item will be created dynamically with
    "! the type passed to the constructor (sorta like a template based hashmap).
    "!
    "! @return The instance of the created item's value part, or empty if the item already exists.
    data: ls_new_item type ty_item.
    field-symbols: <item> type ty_item.
    ls_new_item-key = key.
    insert ls_new_item into table me->data assigning <item>.
    check sy-subrc = 0.

    if me->value_type = 'LCL_HASHMAP' and me->subsequent_hashmap_value_type is not initial.
      create object <item>-value type lcl_hashmap
        exporting value_type = me->subsequent_hashmap_value_type.
    else.
      create object <item>-value type (me->value_type).
    endif.
    value = <item>-value.
  endmethod.                    "new

  method exists.
    "! Checks if a item exists in the hashmap.
    "! @return A flag indicating if the item exists.
    read table me->data with key key = key
      transporting no fields.
    if sy-subrc = 0.
      exists = 'X'.
    endif.
  endmethod.                    "exists

  method get.
    "! Gets an item reference from the hashmap.
    "! If the item is not found, a new item is created, as if using the method new.
    "! @return The reference to the value part of the item.
    field-symbols: <item> type ty_item.
    read table me->data assigning <item>
      with key key = key.
    if sy-subrc = 0.
      value = <item>-value.
    else.
      value = new( key ).
    endif.
  endmethod.                    "get

  method set.
    "! Sets the value of an item in the hashmap.
    "! If the item does not yet exist, an item is created with the passed key/value pair.
    "! If the item already exists, its value is replaced with the passed value.
    data: lo_item type ref to lif_value_type.
    lo_item = get( key ).
    lo_item->copy( value ).
  endmethod.                    "set

  method delete.
    "! Deletes an item from the hashmap.
    delete me->data where key = key.
  endmethod.                    "delete

  method lif_value_type~copy.
    "! Copies the contents of another hashmap to this hashmap
    "!
    "! @parameter hashmap The other (source) hashmap
    data: lo_hashmap type ref to lcl_hashmap,
          lo_value type ref to lif_value_type.
    field-symbols: <item> type ty_item.
    lo_hashmap ?= source.
    loop at lo_hashmap->data assigning <item>.
      lo_value = me->new( <item>-key ).
      lo_value->copy( <item>-value ).
    endloop.
  endmethod.                    "lif_value_type~copy
endclass.                    "lcl_hashmap IMPLEMENTATION
