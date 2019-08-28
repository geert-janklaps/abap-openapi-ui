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

*   Check if service exists
    SELECT SINGLE a~repository_id, a~group_id, a~service_id, s~service_version
      FROM /iwbep/i_v4_msga AS a
      INNER JOIN /iwbep/i_v4_msrv AS s ON a~service_id = s~service_id
      INNER JOIN /iwfnd/c_v4_msgr AS p ON a~group_id = p~group_id
      INTO @DATA(ls_service)
      WHERE a~group_id = @me->mv_group_id
      AND a~service_id = @me->mv_external_service
      AND a~repository_id = @me->mv_repository
      AND s~service_version = @me->mv_version.

    IF sy-subrc <> 0.
      "Raise exception
    ENDIF.

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
                          iv_base_url   = iv_base_url
                        ).

  ENDMETHOD.


  METHOD zif_gw_openapi_metadata~get_json.
    DATA: lv_openapi_string TYPE string.

    DATA(lv_metadata) = me->_read_metadata( ).
    DATA(lv_openapi) = me->convert_odatav4_to_json( iv_metadata_v4 = lv_metadata ).

*   Convert binary data to string
    DATA(lo_conv) = cl_abap_conv_in_ce=>create(
                        encoding    = 'UTF-8'
                        input       = lv_openapi ).

    lo_conv->read(
      IMPORTING
        data = lv_openapi_string ).

    REPLACE ALL OCCURRENCES OF ',,' IN lv_openapi_string WITH ''.

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
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

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
          ls_request_base_info TYPE /iwbep/if_v4_request_info=>ty_s_base_info,
          lv_service           TYPE string,
          lv_path(255)         TYPE c.

*   Read service details
    SELECT SINGLE a~repository_id, a~group_id, a~service_id, s~service_version, t~description
      FROM /iwbep/i_v4_msga AS a
      INNER JOIN /iwbep/i_v4_msrv AS s ON a~service_id = s~service_id
      INNER JOIN /iwfnd/c_v4_msgr AS p ON a~group_id = p~group_id
      LEFT OUTER JOIN /iwbep/i_v4_msrt AS t ON s~service_id = t~service_id
                                            AND s~service_version = t~service_version
                                            AND t~language = @sy-langu
      INTO @DATA(ls_service)
      WHERE a~group_id = @me->mv_group_id
      AND a~service_id = @me->mv_external_service
      AND a~repository_id = @me->mv_repository
      AND s~service_version = @me->mv_version.

*   Store description
    me->mv_description = ls_service-description.

*   Set service url
    lv_service = /iwbep/cl_v4_url_util=>gc_root_url
               && ls_service-group_id && '/'
               && ls_service-repository_id && '/'
               && ls_service-service_id && '/'
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
    ls_request_base_info-uri_request = lv_service.

    DATA(li_request_info) = /iwbep/cl_v4s_runtime_factory=>create_request_info( ).
    li_request_info->init( ls_request_base_info ).
    li_request_info->set_lib_request_info( NEW /iwbep/cl_od_request_info( ) ).

    DATA(lo_context) = NEW /iwcor/cl_od_cntxt( ).
    lo_context->/iwcor/if_od_cntxt~set_object(
        iv_name   = /iwbep/if_od_types=>gc_od_cntx_object_identifier
        io_object = li_request_info ).

*   Load metadata document
    lo_service_factory ?= /iwbep/cl_od_svc_factory=>get_instance( ).
    lo_service_factory->set_lib_context( io_context = lo_context ).
    DATA(li_service) = lo_service_factory->/iwcor/if_od_svc_factory~create_service( lv_service ).
    DATA(li_edm) = li_service->get_entity_data_model( ).
    DATA(li_metadata) = li_edm->get_service_metadata( ).

    li_metadata->get_metadata(
      IMPORTING
        ev_metadata             = rv_metadata ).

  ENDMETHOD.
ENDCLASS.
