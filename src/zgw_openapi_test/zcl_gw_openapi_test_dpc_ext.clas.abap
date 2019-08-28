CLASS zcl_gw_openapi_test_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_zgw_openapi_test_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.

    METHODS usercollection_create_entity
        REDEFINITION .
    METHODS usercollection_delete_entity
        REDEFINITION .
    METHODS usercollection_get_entity
        REDEFINITION .
    METHODS usercollection_get_entityset
        REDEFINITION .
    METHODS usercollection_update_entity
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_GW_OPENAPI_TEST_DPC_EXT IMPLEMENTATION.


  METHOD usercollection_create_entity.

*   Read supplied data and return it as an entity (simulate create)
    io_data_provider->read_entry_data( IMPORTING es_data = er_entity ).

  ENDMETHOD.


  METHOD usercollection_delete_entity.
    DATA: lt_keys          TYPE /iwbep/t_mgw_tech_pairs,
          ls_key           TYPE /iwbep/s_mgw_tech_pair,
          ls_user_addr     TYPE user_addr,
          ls_user_addr_new TYPE user_addr,
          ls_message       TYPE scx_t100key,
          lv_bname         TYPE bname.

*   Read keys from request
    lt_keys = io_tech_request_context->get_keys( ).

*   Read user name from key table
    READ TABLE lt_keys INTO ls_key WITH KEY name = 'BNAME'.
    IF sy-subrc <> 0.
      ls_message-msgid = 'SY'.
      ls_message-msgno = '002'.
      ls_message-attr1 = 'No username provided'.

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid = ls_message.
    ENDIF.

    lv_bname = ls_key-value.

*   Check if user exists
    SELECT SINGLE * FROM user_addr
      INTO ls_user_addr
      WHERE bname = lv_bname.

    IF sy-subrc <> 0.
      ls_message-msgid = 'SY'.
      ls_message-msgno = '002'.
      CONCATENATE `Username ` lv_bname ` doesn't exist` INTO ls_message-attr1.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid = ls_message.
    ENDIF.

  ENDMETHOD.


  METHOD usercollection_get_entity.

*   Read supplied user name
    READ TABLE it_key_tab INTO DATA(ls_key) WITH KEY name = 'Bname'.

*   Read data from database
    SELECT SINGLE * FROM user_addr
      INTO CORRESPONDING FIELDS OF er_entity
      WHERE bname = ls_key-value.

  ENDMETHOD.


  METHOD usercollection_get_entityset.
    DATA ls_user_select_option TYPE /iwbep/s_mgw_select_option.

*   Read select option (user name)
    READ TABLE it_filter_select_options INTO ls_user_select_option WITH KEY property = 'Bname'.

*   Read data from database
    SELECT * FROM user_addr
      INTO CORRESPONDING FIELDS OF TABLE et_entityset
      WHERE bname IN ls_user_select_option-select_options.

  ENDMETHOD.


  METHOD usercollection_update_entity.
    DATA: lt_keys          TYPE /iwbep/t_mgw_tech_pairs,
          ls_key           TYPE /iwbep/s_mgw_tech_pair,
          ls_user_addr     TYPE user_addr,
          ls_user_addr_new TYPE user_addr,
          ls_message       TYPE scx_t100key,
          lv_bname         TYPE bname.

*   Read keys from request
    lt_keys = io_tech_request_context->get_keys( ).

*   Read user name from key table
    READ TABLE lt_keys INTO ls_key WITH KEY name = 'BNAME'.
    IF sy-subrc <> 0.
      ls_message-msgid = 'SY'.
      ls_message-msgno = '002'.
      ls_message-attr1 = 'No username provided'.

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid = ls_message.
    ENDIF.

    lv_bname = ls_key-value.

*   Check if user exists
    SELECT SINGLE * FROM user_addr
      INTO ls_user_addr
      WHERE bname = lv_bname.

    IF sy-subrc <> 0.
      ls_message-msgid = 'SY'.
      ls_message-msgno = '002'.
      CONCATENATE `Username ` lv_bname ` doesn't exist` INTO ls_message-attr1.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid = ls_message.
    ENDIF.

*   Read provided changes into entity (simulate update)
    io_data_provider->read_entry_data( IMPORTING es_data = er_entity ).

  ENDMETHOD.
ENDCLASS.
