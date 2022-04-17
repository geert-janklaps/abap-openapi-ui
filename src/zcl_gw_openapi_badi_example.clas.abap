CLASS zcl_gw_openapi_badi_example DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES zif_gw_openapi_badi .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_GW_OPENAPI_BADI_EXAMPLE IMPLEMENTATION.


  METHOD zif_gw_openapi_badi~enhance_openapi_json.
    DATA li_value_node TYPE REF TO if_sxml_value_node.

*   Read default metadata document into XML object
    DATA(li_sxml_reader) = cl_sxml_string_reader=>create( iv_openapi_json ).
    DATA(lo_sxml_writer) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

    DO.
      DATA(li_node) = li_sxml_reader->read_next_node( ).
      IF li_node IS INITIAL.
        EXIT.
      ENDIF.

*     DO SOMETHING WITH OPENAPI JSON

      lo_sxml_writer->if_sxml_writer~write_node( li_node ).
    ENDDO.

*   Return OpenAPI JSON in xstring format
    rv_openapi_json = lo_sxml_writer->get_output( ).

  ENDMETHOD.


  METHOD zif_gw_openapi_badi~procces_odata_v2_metadata.
*   Read default metadata document into XML object
    DATA(li_ixml) = cl_ixml=>create( ).
    DATA(li_stream_factory) = li_ixml->create_stream_factory( ).
    DATA(li_stream) = li_stream_factory->create_istream_xstring( iv_metadata ).
    DATA(li_document) = li_ixml->create_document( ).

    DATA(li_parser) = li_ixml->create_parser(
                        document       = li_document
                        istream        = li_stream
                        stream_factory = li_stream_factory
                      ).

    li_parser->parse( ).


*   DO SOMETHING WITH METADATA


*   Create encoding (UTF-8 is required for succesful conversion to openAPI spec!)
    DATA(li_encoding) = li_ixml->create_encoding(
                          byte_order    = if_ixml_encoding=>co_none
                          character_set = 'utf-8' ).

*   Create output stream and set encoding
    DATA(li_ostream) = li_ixml->create_stream_factory( )->create_ostream_xstring( string = rv_metadata ).
    li_ostream->set_encoding( li_encoding ).

*   Convert XML object to binary
    li_ixml->create_renderer( document = li_document
                              ostream  = li_ostream )->render( ).
  ENDMETHOD.


  METHOD zif_gw_openapi_badi~procces_odata_v4_metadata.
    DATA li_model_fw TYPE REF TO /iwbep/if_v4_med_model_fw.

*   Read default metadata document into XML object
    DATA(li_ixml) = cl_ixml=>create( ).
    DATA(li_stream_factory) = li_ixml->create_stream_factory( ).
    DATA(li_stream) = li_stream_factory->create_istream_xstring( iv_metadata ).
    DATA(li_document) = li_ixml->create_document( ).

    DATA(li_parser) = li_ixml->create_parser(
                        document       = li_document
                        istream        = li_stream
                        stream_factory = li_stream_factory
                      ).

    li_parser->parse( ).

*   Get internal gateway framework model (allows access to ABAP data types etc.)
    li_model_fw ?= ii_request_info->get_model_of_current_service( ).
    li_model_fw->get_model_data( IMPORTING es_model_data = DATA(ls_model_info) ).

*   ABAP Data Types available in LS_MODEL_INFO-ENTITY_TYPES[]-PROPERTIES[]-PROVIDER_ABAP_TYPE_INFO

*   DO SOMETHING WITH METADATA

*   Create encoding (UTF-8 is required for succesful conversion to openAPI spec!)
    DATA(li_encoding) = li_ixml->create_encoding(
                          byte_order    = if_ixml_encoding=>co_none
                          character_set = 'utf-8' ).

*   Create output stream and set encoding
    DATA(li_ostream) = li_ixml->create_stream_factory( )->create_ostream_xstring( string = rv_metadata ).
    li_ostream->set_encoding( li_encoding ).

*   Convert XML object to binary
    li_ixml->create_renderer( document = li_document
                              ostream  = li_ostream )->render( ).
  ENDMETHOD.
ENDCLASS.
