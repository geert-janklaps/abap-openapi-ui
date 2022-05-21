*&---------------------------------------------------------------------*
*& Include          ZGW_SERVICE_OVERVIEW_CLASS
*&---------------------------------------------------------------------*
CLASS lcl_screen_handler DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS handle_pbo.
    CLASS-METHODS handle_pai IMPORTING iv_ok_code TYPE syucomm.

    TYPES: BEGIN OF ty_v2_service_s,
             service_name    TYPE /iwfnd/med_mdl_service_grp_id,
             service_version TYPE /iwfnd/med_mdl_version,
             created_by      TYPE cruser,
             created_at      TYPE timestamp,
             created_date    TYPE d,
             created_time    TYPE t,
             changed_by      TYPE chuser,
             changed_at      TYPE timestamp,
             changed_date    TYPE d,
             changed_time    TYPE t,
             description     TYPE /iwfnd/med_mdl_srg_description,
             devclass        TYPE devclass,
           END OF ty_v2_service_s.

    TYPES: BEGIN OF ty_v4_service_s,
             group_id        TYPE /iwbep/v4_med_group_id,
             repository_id   TYPE /iwbep/v4_med_repository_id,
             service_id      TYPE /iwbep/v4_med_service_id,
             service_version TYPE /iwbep/v4_med_service_version,
             description     TYPE /iwbep/v4_reg_description,
             created_by      TYPE cruser,
             created_at      TYPE tzntstmps,
             created_date    TYPE d,
             created_time    TYPE t,
             changed_by      TYPE chuser,
             changed_at      TYPE tzntstmps,
             changed_date    TYPE d,
             changed_time    TYPE t,
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
      CASE p_v2.
        WHEN abap_true.
          IF screen-name CS 'S_NAME2' OR screen-name CS 'S_VERS2'.
            screen-invisible = 0.
            screen-active = 1.
          ELSEIF screen-name CS 'S_NAME4' OR screen-name CS 'S_VERS4'
              OR screen-name CS 'S_GRP4' OR screen-name CS 'S_REP4'.
            screen-invisible = 1.
            screen-active = 0.
          ENDIF.
        WHEN OTHERS.
          IF screen-name CS 'S_NAME2' OR screen-name CS 'S_VERS2'.
            screen-invisible = 1.
            screen-active = 0.
          ELSEIF screen-name CS 'S_NAME4' OR screen-name CS 'S_VERS4'
              OR screen-name CS 'S_GRP4' OR screen-name CS 'S_REP4'.
            screen-invisible = 0.
            screen-active = 1.
          ENDIF.
      ENDCASE.

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
    SELECT h~service_name,
           h~service_version,
           h~created_by,
           h~created_timestmp AS created_at,
           h~changed_by,
           h~changed_timestmp AS changed_at,
           t~description,
           p~devclass
      FROM /iwfnd/i_med_srh AS h
      LEFT OUTER JOIN /iwfnd/i_med_srt AS t ON h~srv_identifier = t~srv_identifier
                                            AND h~is_active      = t~is_active
                                            AND t~language       = @sy-langu
      INNER JOIN tadir AS p ON p~obj_name = h~srv_identifier
      WHERE h~service_name IN @s_name2
        AND h~service_version IN @s_vers2
        AND h~is_active = 'A'
        AND p~pgmid     = 'R3TR'
        AND p~object    = 'IWSG'
      INTO CORRESPONDING FIELDS OF TABLE @gt_v2_data.

    LOOP AT gt_v2_data ASSIGNING FIELD-SYMBOL(<ls_data>).
      CONVERT TIME STAMP <ls_data>-created_at TIME ZONE sy-zonlo
        INTO DATE <ls_data>-created_date TIME <ls_data>-created_time.

      CONVERT TIME STAMP <ls_data>-changed_at TIME ZONE sy-zonlo
        INTO DATE <ls_data>-changed_date TIME <ls_data>-changed_time.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_v4_data.
    DATA: lt_description TYPE RANGE OF /iwbep/v4_reg_description,
          ls_service_key TYPE /iwbep/s_v4_med_service_key.

*   Get service assignments
    TRY.
        /iwfnd/cl_v4_registry_proxy=>find_srv_assignments_by_ranges(
          EXPORTING
            iv_system_alias          = ''
            it_range_group_id        = s_grp4[]
            it_range_repository_id   = s_rep4[]
            it_range_service_id      = s_name4[]
            it_range_service_version = s_vers4[]
            it_range_description     = lt_description
            iv_top                   = 999999
            iv_skip                  = 0
          IMPORTING
            et_srv_assignments_w_txt = DATA(lt_services) ).
      CATCH /iwfnd/cx_gateway.

    ENDTRY.

    MOVE-CORRESPONDING lt_services TO gt_v4_data.

*   Get publishing configuration and published groups
    DATA(lo_publishing_config) = /iwfnd/cl_v4_publishing_config=>get_instance( ).
    DATA(lt_groups) = lo_publishing_config->get_groups_by_id( ).

*   Remove unpublished services and convert timestamps to dates and times for output
    LOOP AT gt_v4_data ASSIGNING FIELD-SYMBOL(<ls_service>).
      DATA(lv_index) = sy-tabix.

      READ TABLE lt_groups WITH KEY group_id = <ls_service>-group_id INTO DATA(ls_group).
      IF sy-subrc = 0.
        <ls_service>-created_by = ls_group-created_by.
        <ls_service>-changed_by = ls_group-changed_by.
        <ls_service>-created_at = ls_group-created_ts.
        <ls_service>-changed_at = ls_group-changed_ts.

        CONVERT TIME STAMP <ls_service>-created_at TIME ZONE sy-zonlo
          INTO DATE <ls_service>-created_date TIME <ls_service>-created_time.

        CONVERT TIME STAMP <ls_service>-changed_at TIME ZONE sy-zonlo
          INTO DATE <ls_service>-changed_date TIME <ls_service>-changed_time.

      ELSE.
        DELETE gt_v4_data INDEX lv_index.
      ENDIF.

    ENDLOOP.

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
    lo_columns->set_optimize( ).

    " Set Zebra Pattern
    lo_alv->get_display_settings( )->set_striped_pattern( 'X' ).

*   Enable hotspot
    IF p_v2 = abap_true.

      lo_column ?= lo_columns->get_column( columnname = 'SERVICE_NAME' ).
      lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

    ELSE.
      lo_column ?= lo_columns->get_column( columnname = 'SERVICE_ID' ).
      lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
    ENDIF.

    lo_column ?= lo_columns->get_column( columnname = 'CREATED_AT' ).
    lo_column->set_technical( ).

    lo_column ?= lo_columns->get_column( columnname = 'CREATED_DATE' ).
    lo_column->set_short_text( 'Created Dt' ).
    lo_column->set_medium_text( 'Created Date' ).
    lo_column->set_long_text( 'Created Date' ).

    lo_column ?= lo_columns->get_column( columnname = 'CREATED_TIME' ).
    lo_column->set_short_text( 'Created Tm' ).
    lo_column->set_medium_text( 'Created Time' ).
    lo_column->set_long_text( 'Created Time' ).

    lo_column ?= lo_columns->get_column( columnname = 'CHANGED_AT' ).
    lo_column->set_technical( ).

    lo_column ?= lo_columns->get_column( columnname = 'CHANGED_DATE' ).
    lo_column->set_short_text( 'Changed Dt' ).
    lo_column->set_medium_text( 'Changed Date' ).
    lo_column->set_long_text( 'Changed Date' ).

    lo_column ?= lo_columns->get_column( columnname = 'CHANGED_TIME' ).
    lo_column->set_short_text( 'Changed Tm' ).
    lo_column->set_medium_text( 'Changed Time' ).
    lo_column->set_long_text( 'Changed Time' ).

*   Enable events
    DATA(lo_events) = lo_alv->get_event( ).
    SET HANDLER handle_link_click FOR lo_events.

*   Display ALV
    lo_alv->display( ).

  ENDMETHOD.

  METHOD handle_link_click.

    IF p_v2 = abap_true AND column = 'SERVICE_NAME'.
      READ TABLE gt_v2_data INTO DATA(ls_v2_data) INDEX row.

      zcl_gw_openapi=>launch_bsp(
          iv_external_service = ls_v2_data-service_name
          iv_version          = ls_v2_data-service_version
          iv_json             = p_json ).

    ELSEIF p_v4 = abap_true AND column = 'SERVICE_ID'.
      READ TABLE gt_v4_data INTO DATA(ls_v4_data) INDEX row.

      zcl_gw_openapi=>launch_bsp(
          iv_external_service = CONV /iwfnd/med_mdl_service_grp_id( ls_v4_data-service_id )
          iv_version          = ls_v4_data-service_version
          iv_repository       = ls_v4_data-repository_id
          iv_group_id         = ls_v4_data-group_id
          iv_json             = p_json ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
