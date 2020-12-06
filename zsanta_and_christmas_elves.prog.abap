*&---------------------------------------------------------------------*
*& Report ZSANTA_CLAUS_AND_ELVES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsanta_claus_and_elves.

CLASS lcl_elf DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS get_instance
      IMPORTING
        iv_name          TYPE string
      RETURNING
        VALUE(ro_result) TYPE REF TO lcl_elf.

    CLASS-METHODS count_elves
      RETURNING VALUE(rv_result) TYPE i.

    METHODS set_work_to_do
      IMPORTING
        iv_work_to_do TYPE string.

    METHODS constructor.

    METHODS get_work_to_do
      RETURNING
        VALUE(rv_result) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_elves_administration_entry,
             name TYPE string,
             elf TYPE REF TO lcl_elf,
           END OF ty_elves_administration_entry.

    TYPES ty_elves_administration_table TYPE TABLE OF ty_elves_administration_entry WITH KEY name.

    CLASS-DATA mt_elves_administration TYPE ty_elves_administration_table.
    DATA mv_work_to_do TYPE string.
ENDCLASS.

CLASS lcl_elf IMPLEMENTATION.
  METHOD constructor.
   mv_work_to_do = 'nothing'.
  ENDMETHOD.

  METHOD get_instance.
    FIELD-SYMBOLS <elves_administration_entry> TYPE ty_elves_administration_entry.

    ASSIGN mt_elves_administration[ name = iv_name ] TO <elves_administration_entry>.
    IF sy-subrc <> 0.
      DATA(lo_elve) = NEW lcl_elf( ).
      DATA(ls_elves_administration_entry) = VALUE ty_elves_administration_entry( name = iv_name elf = lo_elve ).
      INSERT ls_elves_administration_entry INTO TABLE mt_elves_administration ASSIGNING <elves_administration_entry>.
    ENDIF.

    ro_result = <elves_administration_entry>-elf.
  ENDMETHOD.

  METHOD set_work_to_do.
    mv_work_to_do = iv_work_to_do.
  ENDMETHOD.

  METHOD get_work_to_do.
    rv_result = me->mv_work_to_do.
  ENDMETHOD.

  METHOD count_elves.
    rv_result = lines( mt_elves_administration ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_santa_claus DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_result) TYPE REF TO lcl_santa_claus.

    METHODS send_elf_to_work
      IMPORTING
        iv_name TYPE string
        iv_work TYPE string.

    METHODS check_what_is_elf_doing
      IMPORTING
        iv_name          TYPE string
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS count_elves_under_control
      RETURNING VALUE(rv_result) TYPE i.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-DATA mo_instance TYPE REF TO lcl_santa_claus.
ENDCLASS.

CLASS lcl_santa_claus IMPLEMENTATION.
  METHOD get_instance.
    IF mo_instance IS NOT BOUND.
      mo_instance = NEW lcl_santa_claus( ).
    ENDIF.

    ro_result = mo_instance.
  ENDMETHOD.

  METHOD send_elf_to_work.
    IF iv_name IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_elve) = lcl_elf=>get_instance( iv_name ).
    lo_elve->set_work_to_do( iv_work ).
  ENDMETHOD.

  METHOD check_what_is_elf_doing.
    DATA(lo_elve) = lcl_elf=>get_instance( iv_name ).
    rv_result = lo_elve->get_work_to_do( ).
  ENDMETHOD.

  METHOD count_elves_under_control.
    rv_result = lcl_elf=>count_elves( ).
  ENDMETHOD.
ENDCLASS.


START-OF-SELECTION.
  DATA(lo_santa_claus) = lcl_santa_claus=>get_instance(  ).

  lo_santa_claus->send_elf_to_work( iv_name = 'Herbert' iv_work = 'building toys' ).
  WRITE: / 'Elf Herbert is doing: ', lo_santa_claus->check_what_is_elf_doing( 'Herbert' ).
  WRITE: / 'Elves to take care of: ', lo_santa_claus->count_elves_under_control( ) LEFT-JUSTIFIED.

  lo_santa_claus->send_elf_to_work( iv_name = 'Herbert' iv_work = 'loading christmas sleigh' ).
  WRITE: / 'Elf Herbert is now doing: ', lo_santa_claus->check_what_is_elf_doing( 'Herbert' ).

  WRITE: / 'Elf Paula is doing: ', lo_santa_claus->check_what_is_elf_doing( 'Paula' ).
  WRITE: / 'Elves to take care of: ', lo_santa_claus->count_elves_under_control( ) LEFT-JUSTIFIED.
