CLASS zcl_gw_openapi_metadata_v4 DEFINITION
  PUBLIC
  INHERITING FROM zcl_gw_openapi_meta_base
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_gw_openapi_metadata .

    METHODS constructor
      IMPORTING
        !iv_repository TYPE /iwbep/v4_med_repository_id
        !iv_group_id   TYPE /iwbep/v4_med_group_id
        !iv_service    TYPE /iwbep/v4_med_service_id
        !iv_version    TYPE /iwbep/v4_med_service_version DEFAULT '0001'
        !iv_base_url   TYPE string .
    CLASS-METHODS factory
      IMPORTING
        !iv_repository     TYPE /iwbep/v4_med_repository_id
        !iv_group_id       TYPE /iwbep/v4_med_group_id
        !iv_service        TYPE /iwbep/v4_med_service_id
        !iv_version        TYPE /iwbep/v4_med_service_version DEFAULT '0001'
        !iv_base_url       TYPE string
      RETURNING
        VALUE(ri_metadata) TYPE REF TO zif_gw_openapi_metadata .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS _read_metadata
      RETURNING
        VALUE(rv_metadata) TYPE xstring .
ENDCLASS.



CLASS ZCL_GW_OPENAPI_METADATA_V4 IMPLEMENTATION.


  METHOD constructor.
*   Call super class constructor
    super->constructor( ).

**   Check if service exists
*    SELECT SINGLE a~repository_id, a~group_id, a~service_id, s~service_version
*      FROM /iwbep/i_v4_msga AS a
*      INNER JOIN /iwbep/i_v4_msrv AS s ON a~service_id = s~service_id
*      INNER JOIN /iwfnd/c_v4_msgr AS p ON a~group_id = p~group_id
*      INTO @DATA(ls_service)
*      WHERE a~group_id = @me->mv_group_id
*      AND a~service_id = @me->mv_external_service
*      AND a~repository_id = @me->mv_repository
*      AND s~service_version = @me->mv_version.

*   Store service parameters
    me->mv_repository = iv_repository.
    me->mv_group_id = iv_group_id.
    me->mv_external_service = iv_service.
    me->mv_version = iv_version.
    me->mv_base_url = iv_base_url.

  ENDMETHOD.


  METHOD factory.

*   Return metadata handler instance
    ri_metadata ?= NEW zcl_gw_openapi_metadata_v4(
                          iv_repository = iv_repository
                          iv_group_id   = iv_group_id
                          iv_service    = iv_service
                          iv_version    = iv_version
                          iv_base_url   = iv_base_url ).

  ENDMETHOD.


  METHOD zif_gw_openapi_metadata~get_json.
    DATA: lv_openapi_string TYPE string.

    DATA(lv_metadata) = me->_read_metadata( ).
    DATA(lv_openapi) = me->convert_odatav4_to_json( iv_metadata_v4 = lv_metadata ).

*   Convert binary data to string
    DATA(lo_conv) = cl_abap_conv_in_ce=>create(
                        encoding    = 'UTF-8'
                        input       = lv_openapi ).

    lo_conv->read( IMPORTING data = lv_openapi_string ).

    "REPLACE ALL OCCURRENCES OF ',,' IN lv_openapi_string WITH ''.

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
    DATA: lo_service_factory   TYPE REF TO /iwbep/cl_od_svc_factory,
          lb_openapi_badi      TYPE REF TO zgw_openapi_badi,
          li_service_factory   TYPE REF TO /iwcor/if_od_svc_factory,
          lt_description       TYPE RANGE OF /iwbep/v4_reg_description,
          lt_group_id          TYPE RANGE OF /iwbep/v4_med_group_id,
          lt_repository_id     TYPE RANGE OF /iwbep/v4_med_repository_id,
          lt_service_id        TYPE RANGE OF /iwbep/v4_med_service_id,
          lt_service_version   TYPE RANGE OF /iwbep/v4_med_service_version,
          ls_request_base_info TYPE /iwbep/if_v4_request_info=>ty_s_base_info,
          lv_service           TYPE string,
          lv_path(255)         TYPE c,
          lv_last_modified     TYPE timestamp.

    FIELD-SYMBOLS: <lv_base_url> TYPE string.

*   Read service details
    APPEND VALUE #( sign = 'I' option = 'EQ' low = me->mv_group_id ) TO lt_group_id.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = me->mv_repository ) TO lt_repository_id.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = me->mv_external_service ) TO lt_service_id.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = me->mv_version ) TO lt_service_version.

    TRY.
        /iwfnd/cl_v4_registry_proxy=>find_srv_assignments_by_ranges(
          EXPORTING
            iv_system_alias          = ''
            it_range_group_id        = lt_group_id
            it_range_repository_id   = lt_repository_id
            it_range_service_id      = lt_service_id
            it_range_service_version = lt_service_version
            it_range_description     = lt_description
            iv_top                   = 1
            iv_skip                  = 0
          IMPORTING
            et_srv_assignments_w_txt = DATA(lt_services) ).
      CATCH /iwfnd/cx_gateway.

    ENDTRY.

    READ TABLE lt_services INTO DATA(ls_service) INDEX 1.

*   Store description
    me->mv_description = ls_service-description.

*   Set service base url (gc_uri_icf_path = 7.54, gc_root_url = 7.53 and lower), not available in 7.52
    ASSIGN ('/iwbep/cl_v4_url_util')=>('gc_uri_icf_path') TO <lv_base_url>.
    IF sy-subrc <> 0.
      ASSIGN ('/iwbep/cl_v4_url_util')=>('gc_root_url') TO <lv_base_url>.
    ENDIF.

    IF <lv_base_url> IS ASSIGNED.
      lv_service = <lv_base_url>.
    ELSE.
      lv_service = '/sap/opu/odata4/'.
    ENDIF.

*   Group ID in URL format always starts with namespace, if not set use /SAP/ namespace
    DATA(lv_group_id) = ls_service-group_id.
    IF lv_group_id(1) <> '/'.
      lv_group_id = '/SAP/' && lv_group_id.
    ENDIF.

*   Service ID in URL format always starts with namespace, if not set use /SAP/ namespace
    DATA(lv_service_id) = ls_service-service_id.
    IF lv_service_id(1) <> '/'.
      lv_service_id = '/SAP/' && lv_service_id.
    ENDIF.

    lv_service = lv_service && lv_group_id && '/'
               && ls_service-repository_id && '/'
               && lv_service_id && '/'
               && ls_service-service_version.

    lv_service = to_lower( lv_service ).
    REPLACE ALL OCCURRENCES OF '//' IN lv_service WITH '/'.

*   Get base URL details
    DATA(lv_base_url) = me->mv_base_url && lv_service.

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

*   Initialize OData context
    ls_request_base_info-conditions-if_modified_since = 0.
    ls_request_base_info-conditions-if_unmodified_since = 0.
    ls_request_base_info-http_method = 'GET'.
    ls_request_base_info-host_name = lv_host.
    ls_request_base_info-service_group_id = me->mv_group_id.
    ls_request_base_info-service_key-repository_id = me->mv_repository.
    ls_request_base_info-service_key-service_id = me->mv_external_service.
    ls_request_base_info-service_key-service_version = me->mv_version.

    ls_request_base_info-http_headers = VALUE #( ( name = '~request_method' value = ls_request_base_info-http_method )
                                                 ( name = 'host' value = ls_request_base_info-host_name )
                                                 ( name = 'sap-client' value = sy-mandt )
                                                 ( name = 'sap-language' value = sy-langu ) ).

*   not available in 7.52
    FIELD-SYMBOLS: <fieldvalue> TYPE data.
    ASSIGN COMPONENT 'URI_REQUEST' OF STRUCTURE ls_request_base_info TO <fieldvalue>.
    IF <fieldvalue> IS ASSIGNED.
      <fieldvalue> = lv_service && '$metadata?sap-documentation=all'.
      APPEND INITIAL LINE TO ls_request_base_info-http_headers ASSIGNING FIELD-SYMBOL(<fs_http_header>).
      <fs_http_header>-name = '~request_uri'.
      <fs_http_header>-value = <fieldvalue>.
    ENDIF.


    DATA(li_request_info) = /iwbep/cl_v4s_runtime_factory=>create_request_info( ).
    TRY.
        "li_request_info->init( ls_request_base_info ).
*       Netweaver 7.54+
        CALL METHOD li_request_info->('SET_BASE_INFO')
          EXPORTING
            is_base_info = ls_request_base_info.
      CATCH cx_sy_dyn_call_illegal_method.
        "li_request_info->init( ls_request_base_info ).

*       Netweaver 7.53 and lower
        CALL METHOD li_request_info->('INIT')
          EXPORTING
            is_base_info = ls_request_base_info.
    ENDTRY.

*   Netweaver 7.55+ => cache timestamp of group must be set in request to prevent dump
    DATA(lo_registry) = /iwbep/cl_v4_registry=>get_instance( ).
    TRY.
        CALL METHOD lo_registry->('GET_LAST_MODIFIED_OF_GROUP')
          EXPORTING
            iv_service_group_id = me->mv_group_id
          RECEIVING
            rv_last_modified    = lv_last_modified.

        CALL METHOD li_request_info->('SET_SRV_GROUP_CACHE_TIMESTAMP')
          EXPORTING
            iv_last_modified = lv_last_modified.

      CATCH cx_sy_dyn_call_illegal_method.

    ENDTRY.

    li_request_info->set_lib_request_info( NEW /iwbep/cl_od_request_info( ) ).
    li_request_info->set_operation_kind(
      iv_operation_kind = /iwbep/if_v4_request_info=>gcs_operation_kinds-load_metadata ).

    DATA(lo_context) = NEW /iwcor/cl_od_cntxt( ).
    lo_context->/iwcor/if_od_cntxt~set_object(
        iv_name   = /iwbep/if_od_types=>gc_od_cntx_object_identifier
        io_object = li_request_info ).

*   Load metadata document
    TRY.
        "lo_service_factory ?= /iwbep/cl_od_svc_factory=>get_instance( ).
*       Netweaver 7.53 and lower
        CALL METHOD /iwbep/cl_od_svc_factory=>('GET_INSTANCE')
          RECEIVING
            ro_svc_factory = li_service_factory.

        lo_service_factory ?= li_service_factory.
      CATCH cx_sy_dyn_call_illegal_method.
*       Netweaver 7.54+
        CREATE OBJECT li_service_factory TYPE ('/IWBEP/CL_OD_SVC_FACTORY').
        lo_service_factory ?= li_service_factory.
    ENDTRY.

    lo_service_factory->set_lib_context( io_context = lo_context ).
    DATA(li_service) = lo_service_factory->/iwcor/if_od_svc_factory~create_service( lv_service ).
    DATA(li_edm) = li_service->get_entity_data_model( ).
    DATA(li_metadata) = li_edm->get_service_metadata( ).

    li_metadata->get_metadata( IMPORTING ev_metadata = DATA(lv_metadata) ).

*   Call BADI (Allow modifications to metadata document)
    TRY.
        GET BADI lb_openapi_badi.

        CALL BADI lb_openapi_badi->procces_odata_v4_metadata
          EXPORTING
            iv_group_id        = ls_service-group_id
            iv_repository_id   = ls_service-repository_id
            iv_service_id      = ls_service-service_id
            iv_service_version = ls_service-service_version
            iv_metadata        = lv_metadata
            ii_request_info    = li_request_info
          RECEIVING
            rv_metadata        = rv_metadata.
      CATCH cx_badi_context_error
            cx_badi_filter_error
            cx_badi_initial_context
            cx_badi_multiply_implemented
            cx_badi_not_implemented
            cx_badi_unknown_error.

    ENDTRY.

*   Check if metadata was set by BADI, if initial take default metadata document
    IF rv_metadata IS INITIAL.
      rv_metadata = lv_metadata.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
