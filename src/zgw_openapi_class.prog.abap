*&---------------------------------------------------------------------*
*& Include          ZGW_SERVICE_OVERVIEW_CLASS
*&---------------------------------------------------------------------*
CLASS lcl_screen_handler DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS handle_pbo.
    CLASS-METHODS handle_pai IMPORTING iv_ok_code TYPE syucomm.

    TYPES: BEGIN OF ty_v2_service_s,
             service_name    TYPE	/iwfnd/med_mdl_service_grp_id,
             service_version TYPE /iwfnd/med_mdl_version,
             description     TYPE /iwfnd/med_mdl_srg_description,
           END OF ty_v2_service_s.

    TYPES: BEGIN OF ty_v4_service_s,
             group_id        TYPE	/iwbep/v4_med_group_id,
             repository_id   TYPE /iwbep/v4_med_repository_id,
             service_id      TYPE	/iwbep/v4_med_service_id,
             service_version TYPE /iwbep/v4_med_service_version,
             description     TYPE /iwbep/v4_reg_description,
           END OF ty_v4_service_s.


  PRIVATE SECTION.
    CLASS-METHODS get_v2_data.
    CLASS-METHODS get_v4_data.
    CLASS-METHODS display_alv.

    CLASS-METHODS handle_link_click
                  FOR EVENT link_click OF cl_salv_events_table
      IMPORTING row
                  column.

    CLASS-DATA: gt_v2_data TYPE TABLE OF ty_v2_service_s,
                gt_v4_data TYPE TABLE OF ty_v4_service_s.

ENDCLASS.

CLASS lcl_screen_handler IMPLEMENTATION.

  METHOD handle_pbo.

    LOOP AT SCREEN.
      IF p_v2 = abap_true.
        IF screen-name CS 'S_NAME2' OR screen-name CS 'S_VERS2'.
          screen-invisible = 0.
          screen-active = 1.
        ELSEIF screen-name CS 'S_NAME4' OR screen-name CS 'S_VERS4'
            OR screen-name CS 'S_GRP4' OR screen-name CS 'S_REP4'.
          screen-invisible = 1.
          screen-active = 0.
        ENDIF.
      ELSE.
        IF screen-name CS 'S_NAME2' OR screen-name CS 'S_VERS2'.
          screen-invisible = 1.
          screen-active = 0.
        ELSEIF screen-name CS 'S_NAME4' OR screen-name CS 'S_VERS4'
            OR screen-name CS 'S_GRP4' OR screen-name CS 'S_REP4'.
          screen-invisible = 0.
          screen-active = 1.
        ENDIF.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.

  ENDMETHOD.

  METHOD handle_pai.

    CASE iv_ok_code.
      WHEN 'CRET' OR 'ONLI'.
        IF p_v2 = abap_true.
          get_v2_data( ).
        ELSE.
          get_v4_data( ).
        ENDIF.

        display_alv( ).

      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.

  METHOD get_v2_data.

*   Read service details
    SELECT h~service_name, h~service_version,  t~description
      FROM /iwfnd/i_med_srh AS h
      LEFT OUTER JOIN /iwfnd/i_med_srt AS t ON  h~srv_identifier = t~srv_identifier
                                            AND h~is_active      = t~is_active
                                            AND t~language       = @sy-langu
      INTO CORRESPONDING FIELDS OF TABLE @gt_v2_data
      WHERE h~service_name IN @s_name2
      AND h~service_version IN @s_vers2
      AND h~is_active = 'A'
      ORDER BY service_name ASCENDING.


  ENDMETHOD.

  METHOD get_v4_data.

*   Read service details
    SELECT a~repository_id, a~group_id, a~service_id, s~service_version, t~description
      FROM /iwbep/i_v4_msga AS a
      INNER JOIN /iwbep/i_v4_msrv AS s ON a~service_id = s~service_id
      INNER JOIN /iwfnd/c_v4_msgr AS p ON a~group_id = p~group_id
      LEFT OUTER JOIN /iwbep/i_v4_msrt AS t ON s~service_id = t~service_id
                                            AND s~service_version = t~service_version
                                            AND t~language = @sy-langu
      INTO CORRESPONDING FIELDS OF TABLE @gt_v4_data
      WHERE a~group_id IN @s_grp4
      AND a~service_id IN @s_name4
      AND a~repository_id IN @s_rep4
      AND s~service_version IN @s_vers4
      ORDER BY a~service_id ASCENDING.

  ENDMETHOD.

  METHOD display_alv.
    DATA: lo_column TYPE REF TO cl_salv_column_table.

*   Create ALV object
    IF p_v2 = abap_true.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table   = DATA(lo_alv)
        CHANGING
          t_table        = gt_v2_data ).
    ELSE.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table   = lo_alv
        CHANGING
          t_table        = gt_v4_data ).
    ENDIF.

*   Enable all standard ALV functions
    lo_alv->get_functions( )->set_all( abap_true ).

*   Update field catalog
    DATA(lo_columns) = lo_alv->get_columns( ).

*   Enable hotspot
    IF p_v2 = abap_true.
      lo_column ?= lo_columns->get_column( columnname = 'SERVICE_NAME' ).
      lo_column->set_cell_type(
          value = if_salv_c_cell_type=>hotspot ).
    ELSE.
      lo_column ?= lo_columns->get_column( columnname = 'SERVICE_ID' ).
      lo_column->set_cell_type(
          value = if_salv_c_cell_type=>hotspot ).
    ENDIF.

*   Enable events
    DATA(lo_events) = lo_alv->get_event( ).
    SET HANDLER handle_link_click FOR lo_events.

*   Display ALV
    lo_alv->display( ).

  ENDMETHOD.

  METHOD handle_link_click.

    IF p_v2 = abap_true.
      IF column = 'SERVICE_NAME'.
        READ TABLE gt_v2_data INTO DATA(ls_v2_data) INDEX row.

        zcl_gw_openapi=>launch_bsp(
          EXPORTING
            iv_external_service = ls_v2_data-service_name
            iv_version          = ls_v2_data-service_version
            iv_json             = p_json ).
      ENDIF.

    ELSE.
      IF column = 'SERVICE_ID'.
        READ TABLE gt_v4_data INTO DATA(ls_v4_data) INDEX row.

        zcl_gw_openapi=>launch_bsp(
          EXPORTING
            iv_external_service = CONV /iwfnd/med_mdl_service_grp_id( ls_v4_data-service_id )
            iv_version          = ls_v4_data-service_version
            iv_repository       = ls_v4_data-repository_id
            iv_group_id         = ls_v4_data-group_id
            iv_json             = p_json ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
