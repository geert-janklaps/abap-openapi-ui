CLASS zcl_gw_openapi_metadata_v2 DEFINITION
  PUBLIC
  INHERITING FROM zcl_gw_openapi_meta_base
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_gw_openapi_metadata .

    METHODS constructor
      IMPORTING
        !iv_external_service TYPE /iwfnd/med_mdl_service_grp_id
        !iv_version          TYPE /iwfnd/med_mdl_version DEFAULT '0001'
        !iv_base_url         TYPE string .
    CLASS-METHODS factory
      IMPORTING
        !iv_external_service TYPE /iwfnd/med_mdl_service_grp_id
        !iv_version          TYPE /iwfnd/med_mdl_version DEFAULT '0001'
        !iv_base_url         TYPE string
      RETURNING
        VALUE(ri_metadata)   TYPE REF TO zif_gw_openapi_metadata .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS _read_metadata
      RETURNING
        VALUE(rv_metadata) TYPE xstring .
ENDCLASS.



CLASS ZCL_GW_OPENAPI_METADATA_V2 IMPLEMENTATION.


  METHOD constructor.
*   Call super class constructor
    super->constructor( ).

*   Check if service exists
    SELECT SINGLE h~srv_identifier, h~namespace, h~service_name, h~service_version
      FROM /iwfnd/i_med_srh AS h
      INTO @DATA(ls_service)
      WHERE service_name = @iv_external_service
      AND service_version = @iv_version.

*   Store service parameters
    me->mv_external_service = iv_external_service.
    me->mv_version = iv_version.
    me->mv_base_url = iv_base_url.

  ENDMETHOD.


  METHOD factory.

*   Return metadata handler instance
    ri_metadata ?= NEW zcl_gw_openapi_metadata_v2( iv_external_service = iv_external_service
                                                   iv_version = iv_version
                                                   iv_base_url = iv_base_url ).

  ENDMETHOD.


  METHOD zif_gw_openapi_metadata~get_json.
    DATA: lv_openapi_string TYPE string.

    DATA(lv_metadata) = me->_read_metadata( ).
    DATA(lv_metadata_v4) = me->convert_odatav2_to_odatav4( iv_metadata_v2 = lv_metadata ).
    DATA(lv_openapi) = me->convert_odatav4_to_json( iv_metadata_v4 = lv_metadata_v4 ).

*   Convert binary data to string
    DATA(lo_conv) = cl_abap_conv_in_ce=>create(
                        encoding    = 'UTF-8'
                        input       = lv_openapi ).

    lo_conv->read( IMPORTING data = lv_openapi_string ).

*   Add basic authentication to OpenAPI JSON
    "REPLACE ALL OCCURRENCES OF '"components":{' IN lv_openapi_string
    "WITH '"components":{"securitySchemes":{"BasicAuth":{"type":"http","scheme":"basic"}},'.

*   Convert OpenAPI JSON to binary format
    CLEAR lv_openapi.
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = lv_openapi_string
      IMPORTING
        buffer = lv_openapi
      EXCEPTIONS
        failed = 1
        OTHERS = 2.

*   Set exporting parameters
    ev_json = lv_openapi.
    ev_json_string = lv_openapi_string.

  ENDMETHOD.


  METHOD zif_gw_openapi_metadata~get_metadata.

*   Return metadata
    rv_metadata = me->_read_metadata( ).

  ENDMETHOD.


  METHOD _read_metadata.
    DATA: lv_service   TYPE string,
          lv_path(255) TYPE c.

*   Read service details
    SELECT SINGLE h~srv_identifier, h~namespace, h~service_name, h~service_version, t~description
      FROM /iwfnd/i_med_srh AS h
      LEFT OUTER JOIN /iwfnd/i_med_srt AS t ON h~srv_identifier = t~srv_identifier
                                            AND h~is_active      = t~is_active
                                            AND t~language       = @sy-langu
      INTO @DATA(ls_service)
      WHERE service_name = @me->mv_external_service
      AND service_version = @me->mv_version.

*   Store description
    me->mv_description = ls_service-description.

*   Read SICF details
    DATA(lo_icf_access) = /iwfnd/cl_icf_access=>get_icf_access( ).
    DATA(lt_icfdocu) = lo_icf_access->get_icf_docu_for_gw_libs_wo_at( ).

    LOOP AT lt_icfdocu INTO DATA(ls_icfdocu).

*     Get main odata node
      DATA(lv_icf_lib_guid) = lo_icf_access->get_node_guid_wo_at(
                                iv_icf_parent_guid = ls_icfdocu-icfparguid
                                iv_icf_node_name   = CONV icfaltnme( ls_icfdocu-icf_name ) ).

    ENDLOOP.

*   Get OData service URL
    TRY.
        CASE lv_icf_lib_guid.
          WHEN /iwfnd/cl_icf_access=>gcs_icf_node_ids-lib_02.
            DATA(lv_md_url) = /iwfnd/cl_med_utils=>get_meta_data_doc_url_local(
                                  iv_external_service_doc_name = ls_service-service_name
                                  iv_namespace                 = ls_service-namespace
                                  iv_icf_root_node_guid        = lv_icf_lib_guid ).

          WHEN /iwfnd/cl_icf_access=>gcs_icf_node_ids-lib_10.
            lv_md_url = /iwfnd/cl_med_utils=>get_meta_data_doc_url_local(
                            iv_external_service_doc_name = ls_service-service_name
                            iv_namespace                 = ls_service-namespace
                            iv_version                   = ls_service-service_version
                            iv_icf_root_node_guid        = lv_icf_lib_guid ).
        ENDCASE.

      CATCH /iwfnd/cx_med_mdl_access.
    ENDTRY.

*   Remove everything but path from URL
    REPLACE '/?$format=xml' IN lv_md_url WITH ''.
    DATA(lv_md_url_full) = lv_md_url.
    IF lv_md_url IS NOT INITIAL.
      DATA(lv_leng) = strlen( lv_md_url ).
      IF lv_leng > 7 AND ( lv_md_url(7) = 'http://' OR lv_md_url(8) = 'https://' ).
        SEARCH lv_md_url FOR '/sap/opu/'.
        IF sy-subrc = 0.
          lv_md_url = lv_md_url+sy-fdpos.
        ENDIF.
      ENDIF.
    ENDIF.

*   Set service
    lv_service = ls_service-namespace && ls_service-service_name.

*   Get base URL details
    IF me->mv_base_url IS NOT INITIAL.
      DATA(lv_base_url) = me->mv_base_url && lv_md_url.
    ELSE.
      lv_base_url = lv_md_url_full.
    ENDIF.

    SPLIT lv_base_url AT '://' INTO DATA(lv_scheme) DATA(lv_url_without_scheme).
    SPLIT lv_url_without_scheme AT '/' INTO DATA(lv_host) lv_path.

    DATA(lv_length) = strlen( lv_path ) - 1.
    IF lv_path+lv_length(1) = '/'.
      lv_path+lv_length(1) = ''.
    ENDIF.

*   Store scheme, host and path
    me->mv_scheme = lv_scheme.
    me->mv_host = lv_host.
    me->mv_path = lv_path.

*   Initialize NetWeaver Gateway transaction handler
    DATA(lo_transaction_handler) = /iwfnd/cl_transaction_handler=>get_transaction_handler( ).

*   Initialize transaction handler (set metadata access with full documentation)
    lo_transaction_handler->initialize(
        iv_request_id            = ''
        iv_external_srv_grp_name = ls_service-service_name
        iv_version               = ls_service-service_version
        iv_namespace             = ls_service-namespace
        iv_verbose_metadata      = /iwfnd/if_mgw_core_types=>gcs_verbose_metadata-all ).

*   Initialize metadata access
    lo_transaction_handler->set_metadata_access_info(
        iv_load_last_modified_only = abap_false
        iv_is_busi_data_request    = abap_false
        iv_do_cache_handshake      = abap_true ).

*   Load metadata document
    DATA(li_service_factory) = /iwfnd/cl_sodata_svc_factory=>get_svc_factory( ).
    DATA(li_service) = li_service_factory->create_service( iv_name = lv_service ).
    DATA(li_edm) = li_service->get_entity_data_model( ).
    DATA(li_metadata) = li_edm->get_service_metadata( ).

    li_metadata->get_metadata( IMPORTING ev_metadata = rv_metadata ).

  ENDMETHOD.
ENDCLASS.
