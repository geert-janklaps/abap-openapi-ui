CLASS zcl_gw_openapi_meta_base DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.
  PROTECTED SECTION.

    DATA mv_repository TYPE /iwbep/v4_med_repository_id .
    DATA mv_group_id TYPE /iwbep/v4_med_group_id .
    DATA mv_external_service TYPE /iwfnd/med_mdl_service_grp_id .
    DATA mv_version TYPE /iwfnd/med_mdl_version .
    DATA mv_base_url TYPE string .
    DATA mv_scheme TYPE string .
    DATA mv_host TYPE string .
    DATA mv_path TYPE string .
    DATA mv_description TYPE string .

    METHODS convert_odatav2_to_odatav4
      IMPORTING
        !iv_metadata_v2       TYPE xstring
      RETURNING
        VALUE(rv_metadata_v4) TYPE xstring .
    METHODS convert_odatav4_to_json
      IMPORTING
        !iv_metadata_v4 TYPE xstring
      RETURNING
        VALUE(rv_json)  TYPE xstring .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_GW_OPENAPI_META_BASE IMPLEMENTATION.


  METHOD convert_odatav2_to_odatav4.

*   Convert OData V2 to V4 metadata document
    CALL TRANSFORMATION zgw_odatav2_to_v4
      SOURCE XML iv_metadata_v2
      RESULT XML rv_metadata_v4.

  ENDMETHOD.


  METHOD convert_odatav4_to_json.
    DATA: lb_openapi_badi TYPE REF TO zgw_openapi_badi,
          lt_parameters   TYPE abap_trans_parmbind_tab,
          lv_title        TYPE string,
          lv_version      TYPE string,
          lv_description  TYPE string.

*   Set transformation parameters
    lv_version = me->mv_version.
    SHIFT lv_version LEFT DELETING LEADING '0'.
    lv_version = 'V' && lv_version.

    lv_title = me->mv_external_service.
    lv_description = me->mv_description.

*   Call BADI (Allow modifications to OpenAPI JSON)
    TRY.
        GET BADI lb_openapi_badi.

        CALL BADI lb_openapi_badi->update_openapi_info_attributes
          EXPORTING
            iv_group_id        = me->mv_group_id
            iv_repository_id   = me->mv_repository
            iv_service_id      = me->mv_external_service
            iv_service_version = me->mv_version
          CHANGING
            cv_title           = lv_title
            cv_version         = lv_version
            cv_description     = lv_description.
      CATCH cx_badi_context_error
              cx_badi_filter_error
              cx_badi_initial_context
              cx_badi_multiply_implemented
              cx_badi_not_implemented
              cx_badi_unknown_error.

    ENDTRY.

    lt_parameters = VALUE #( ( name = 'openapi-version' value = '3.0.0' )
                             ( name = 'odata-version' value = '4.0' )
                             ( name = 'scheme' value = me->mv_scheme )
                             ( name = 'host' value = me->mv_host )
                             ( name = 'basePath' value = '/' && me->mv_path )
                             ( name = 'info-version' value = lv_version )
                             ( name = 'info-title' value = lv_title )
                             ( name = 'info-description' value = lv_description )
                             ( name = 'references' value = 'YES' )
                             ( name = 'diagram' value = 'YES' ) ).

*   Convert metadata document to openapi
    CALL TRANSFORMATION zgw_odatav4_to_openapi
      SOURCE XML iv_metadata_v4
      RESULT XML rv_json
      PARAMETERS (lt_parameters).

*   Call BADI (Allow modifications to OpenAPI JSON)
    TRY.
        GET BADI lb_openapi_badi.

        CALL BADI lb_openapi_badi->enhance_openapi_json
          EXPORTING
            iv_group_id        = me->mv_group_id
            iv_repository_id   = me->mv_repository
            iv_service_id      = me->mv_external_service
            iv_service_version = me->mv_version
          CHANGING
            cv_openapi_json    = rv_json.
      CATCH cx_badi_context_error
              cx_badi_filter_error
              cx_badi_initial_context
              cx_badi_multiply_implemented
              cx_badi_not_implemented
              cx_badi_unknown_error.

    ENDTRY.

  ENDMETHOD.
ENDCLASS.
