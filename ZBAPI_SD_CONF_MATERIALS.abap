*----------------------------------------------------------------------*
***INCLUDE ZSWRE1000_F04.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  JOB_RAW_DATA_CSV_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_RAW_DATA  text
*      -->P_LT_LINE_INFO  text
*----------------------------------------------------------------------*
form job_raw_data_csv_set  tables   lt_jobs structure zswtajobs it_line_info type ty_line_info_t  ct_raw_data type ty_raw_file_t.
  " FORM job_raw_data_csv_set  TABLES   lt_jobs STRUCTURE zswtajobs it_line_info TYPE ty_line_info_t  CHANGING ct_raw_data TYPE ty_raw_file_t.

  types: begin of ty_files_extended,
           job_id          type zswde0020,
           job_context     type zswde0021,
           file_id         type zswde0011,
           has_header_info type zswde0063,
           file_type_id    type zswde0001,
           file_type_level type zswde0034,
         end of  ty_files_extended,
         ty_files_extended_t type standard table of ty_files_extended.


  data: lt_excl_names type ty_comp_name_t with header line.
  ranges: rg_excl_comp for lt_excl_names-name.

  data: begin of itab_words occurs 0,
          word(132).
  data: end of itab_words.

  data:
    lv_len             type i,
    lv_text_len        type i,
    lv_text_max_size   type i value '132',
    lv_string_max_size type i,

    lv_file_type_id    type zswde0001,
    lt_jobs_header     type standard table of zswtajobs,
    lt_jobs_items      type standard table of zswtajob_items,
    lt_files_info      type ty_files_extended_t,
    ls_files_info      type ty_files_extended,
    ls_raw_data        type ty_raw_file.
  data:
    lo_table      type ref to cl_abap_tabledescr,
    lo_struc      type ref to cl_abap_structdescr,
    lo_struc_item type ref to cl_abap_structdescr,
    wa_comp       type abap_compdescr.
  data:
    ls_fileline  type zswtafilelines,
    lt_text      type zswcl0001=>ty_text_t,
    ls_line_info type ty_line_info,
    ls_text      type char255.


  field-symbols:
    <fs_tab>        type any table,
    <fs_tab_item>   type any table,
    <fs_job_id>     type zswde0020,
    <fs_struc>      type any,
    <fs_struc_item> type any,
    <ls_raw_str>    type ty_raw_file,
    <fs_val>        type any.


  lv_file_type_id = 'CSV'.

  select * from zswtajobs into corresponding fields of table lt_jobs_header
  for all entries in lt_jobs
  where job_id = lt_jobs-job_id
  and source_file_type eq lv_file_type_id.

  if not lt_jobs_header[] is initial.

    select * from zswtajob_items into corresponding fields of table lt_jobs_items
    for all entries in lt_jobs_header
    where job_id = lt_jobs_header-job_id.

    select   a~file_id  b~has_header_info  a~file_type_id  b~file_type_level d~job_id d~job_context
    from  zswtafile  as a
    inner join zswta0001 as b
    on a~file_type_id = b~file_type_id
    inner join zswta0002 as c
    on a~file_status_id = c~file_status_id
    inner join  zswtajobs as d
    on a~job_id = d~job_id
    into corresponding fields of table lt_files_info
    for all entries in lt_jobs_header
    where a~job_id = lt_jobs_header-job_id
    and b~file_type_id = lt_jobs_header-source_file_type
    and c~inactive_status ne  'X'.


    perform job_str_excl_comp tables lt_excl_names.
    loop at lt_excl_names.
      rg_excl_comp-sign = 'I'.
      rg_excl_comp-option = 'EQ'.
      rg_excl_comp-low =  lt_excl_names-name.
      append rg_excl_comp.
    endloop.



    assign lt_jobs_header to <fs_tab>.
    assign lt_jobs_items to <fs_tab_item>.

    lo_table ?= cl_abap_datadescr=>describe_by_data( <fs_tab> ).
    lo_struc ?= lo_table->get_table_line_type( ).

    lo_table ?= cl_abap_datadescr=>describe_by_data( <fs_tab_item> ).
    lo_struc_item ?= lo_table->get_table_line_type( ).


    loop at <fs_tab> assigning <fs_struc>.

      assign component 'JOB_ID' of structure  <fs_struc>  to <fs_job_id>.

      read table lt_files_info into ls_files_info
      with key job_id = <fs_job_id>.
      if sy-subrc eq 0.
        move-corresponding ls_files_info to ls_raw_data.
      else.
        exit.
      endif.


      loop at lo_struc->components into wa_comp
        where name not in rg_excl_comp.

        assign component wa_comp-name of structure <fs_struc> to <fs_val>.

        if wa_comp-length gt lv_text_max_size and wa_comp-type_kind eq 'C'.

          lv_len = strlen( <fs_val> ).


          if wa_comp-name cs 'TEXT'.
            lv_string_max_size = lv_text_max_size.
          else.
            lv_string_max_size = 255.
          endif.

          if lv_len gt lv_string_max_size.
            split <fs_val> at space into table itab_words.

            loop at itab_words.

              lv_text_len = strlen( ls_text ) + strlen( itab_words  ) .

              if  lv_text_len lt lv_string_max_size.
                if ls_text is initial.
                  ls_text = itab_words.
                else.
                  concatenate ls_text itab_words into ls_text separated by space.
                endif.

              else.

                ls_raw_data-line_number = ls_raw_data-line_number + 1.
                ls_raw_data-line_text(lv_string_max_size) = ls_text.
                ls_raw_data-line_text+255(255) = wa_comp-name.
                append ls_raw_data to ct_raw_data.
                ls_text = itab_words.
              endif.

            endloop.
          else.
            ls_raw_data-line_number = ls_raw_data-line_number + 1.
            ls_raw_data-line_text(255) = <fs_val>.
            ls_raw_data-line_text+255(255) = wa_comp-name.
            append ls_raw_data to ct_raw_data.
          endif.

        else.
          ls_raw_data-line_number = ls_raw_data-line_number + 1.
          ls_raw_data-line_text(255) = <fs_val>.
          ls_raw_data-line_text+255(255) = wa_comp-name.
          append ls_raw_data to ct_raw_data.
        endif.
      endloop.


      loop at <fs_tab_item> assigning  <fs_struc_item>.

        assign component 'JOB_ID' of structure  <fs_struc_item>  to <fs_job_id>.

        if <fs_job_id> = ls_raw_data-job_id.

          loop at lo_struc_item->components into wa_comp
          where name not in rg_excl_comp.

            assign component wa_comp-name of structure  <fs_struc_item>  to <fs_val>.

            if wa_comp-length gt lv_text_max_size and wa_comp-type_kind eq 'C'.

              lv_len = strlen( <fs_val> ).


              if wa_comp-name cs 'TEXT'.
                lv_string_max_size = lv_text_max_size.
              else.
                lv_string_max_size = 255.
              endif.

              if lv_len gt lv_string_max_size.
                split <fs_val> at space into table itab_words.

                loop at itab_words.

                  lv_text_len = strlen( ls_text ) + strlen( itab_words  ) .

                  if  lv_text_len lt lv_string_max_size.
                    if ls_text is initial.
                      ls_text = itab_words.
                    else.
                      concatenate ls_text itab_words into ls_text separated by space.
                    endif.

                  else.

                    ls_raw_data-line_number = ls_raw_data-line_number + 1.
                    ls_raw_data-line_text(lv_string_max_size) = ls_text.
                    ls_raw_data-line_text+255(255) = wa_comp-name.
                    append ls_raw_data to ct_raw_data.
                    ls_text = itab_words.
                  endif.
                endloop.

              else.
                ls_raw_data-line_number = ls_raw_data-line_number + 1.
                ls_raw_data-line_text(255) = <fs_val>.
                ls_raw_data-line_text+255(255) = wa_comp-name.
                append ls_raw_data to ct_raw_data.
              endif.

            else.
              ls_raw_data-line_number = ls_raw_data-line_number + 1.
              ls_raw_data-line_text(255) = <fs_val>.
              ls_raw_data-line_text+255(255) = wa_comp-name.
              append ls_raw_data to ct_raw_data.
            endif.
          endloop.
        endif.
      endloop.
    endloop.

* TODO next call line identity for each line of raw data

    loop at  ct_raw_data assigning <ls_raw_str>.
      clear: ls_fileline, lt_text[].
      append <ls_raw_str>-line_text to lt_text.

      call method zswcl0001=>line_type_identify
        exporting
          iv_context      = <ls_raw_str>-job_context
          iv_file_type_id = <ls_raw_str>-file_type_id
          iv_file_id      = <ls_raw_str>-file_id
          iv_default_line = space
          ct_text         = lt_text
          iv_update       = space
        importing
          es_fileline     = ls_fileline.

      <ls_raw_str>-line_type_id = ls_fileline-line_type_id.

      read table it_line_info into ls_line_info
      with key line_type_id = ls_fileline-line_type_id.
      if sy-subrc eq 0.
        <ls_raw_str>-src_str_id = ls_line_info-src_str_id.
      endif.


    endloop.
  endif.

  sort ct_raw_data by job_id file_type_level file_id line_number.


endform.                    " JOB_RAW_DATA_CSV_SET
*&---------------------------------------------------------------------*
*&      Form  XML_RAW_DATA_HANDLING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form job_raw_data_xml_set  tables   ct_raw_data type ty_raw_file_t it_line_info type ty_line_info_t.

  types: begin of ty_raw_text,
           text(255) type c,
         end of ty_raw_text.

  data:
*    lt_raw_data     TYPE ty_raw_file_t,
    gcl_xml         type ref to cl_xml_document,
    lt_line_type    type zswcl0001=>ty_text_t,
    ls_line_info    type ty_line_info,
    lv_xml_string   type xstring,
    lt_raw_data_aux type ty_raw_file_t,
    ls_raw_data     type ty_raw_file,
    ls_raw_data_aux type ty_raw_file,
    ls_fileline     type zswtafilelines,
    lt_raw_xml_text type table of ty_raw_text,
    lt_raw_xml_data type swxmlcont,
    lt_xml_data     type table of smum_xmltb,
    ls_xml_data     type smum_xmltb,
    lt_return       type table of bapiret2,
    lt_text         type zswcl0001=>ty_text_t,
    lv_retcode      type sysubrc,
    lv_size         type i.



*  lt_raw_data[] = ct_raw_data[].
*
*  DELETE ct_raw_data WHERE line_type_id EQ 'XML'.
*  DELETE ct_raw_data WHERE file_format EQ 'XML'.

  loop at ct_raw_data into ls_raw_data where file_format eq 'XML'.

    if not gcl_xml is bound.
      create object gcl_xml type cl_xml_document.
    endif.

    ls_raw_data_aux  = ls_raw_data.
    append ls_raw_data-line_text to lt_raw_xml_text.

    at end of file_id ##LOOP_AT_OK.

      call function 'SCMS_TEXT_TO_BINARY'
        importing
          output_length = lv_size
        tables
          text_tab      = lt_raw_xml_text
          binary_tab    = lt_raw_xml_data
        exceptions
          failed        = 1
          others        = 2.
      if sy-subrc eq 0.


        lv_retcode = gcl_xml->create_with_table( table = lt_raw_xml_data size = lv_size ).

        if lv_retcode = 0.
          call method gcl_xml->render_2_xstring
            importing
              retcode = lv_retcode
              stream  = lv_xml_string
              size    = lv_size.
          if lv_retcode = 0.
            clear: lt_xml_data[].
* Convert XML to internal table
            call function 'ZSWFM_SMUM_XML_PARSE'
              exporting
                xml_input = lv_xml_string
              tables
                xml_table = lt_xml_data
                return    = lt_return.

            if lt_xml_data[] is not initial.

              clear ls_raw_data_aux-line_number.

              loop at lt_xml_data into ls_xml_data.
                ls_raw_data_aux-line_text+255(255)  = ls_xml_data-cname.
                ls_raw_data_aux-line_text(255) = ls_xml_data-cvalue.
                ls_raw_data_aux-file_format = space.
                ls_raw_data_aux-line_type_id = space.


                clear: ls_fileline, lt_text[].
                append ls_raw_data_aux-line_text to lt_text.

                call method zswcl0001=>line_type_identify
                  exporting
                    iv_context      = ls_raw_data_aux-job_context
                    iv_file_type_id = ls_raw_data_aux-file_type_id
                    iv_file_id      = ls_raw_data_aux-file_id
                    iv_default_line = 'XML'
                    ct_text         = lt_text
                    iv_update       = space
                  importing
                    es_fileline     = ls_fileline.

                ls_raw_data_aux-line_type_id = ls_fileline-line_type_id.
                ls_raw_data_aux-line_number = ls_raw_data_aux-line_number + 1.
                read table it_line_info into ls_line_info
                with key line_type_id = ls_fileline-line_type_id.
                if sy-subrc eq 0.
                  ls_raw_data_aux-src_str_id = ls_line_info-src_str_id.
                endif.

                append ls_raw_data_aux to lt_raw_data_aux.
              endloop.
              if not lt_raw_data_aux[] is initial.
                append lines of lt_raw_data_aux to ct_raw_data.
                delete ct_raw_data where line_type_id eq 'XML' and file_id eq ls_raw_data-file_id.
                delete ct_raw_data where file_format eq 'XML' and file_id eq ls_raw_data-file_id.
              endif.
            endif.
          endif.
        endif.
      endif.

      clear: lt_raw_xml_data[],
      lt_raw_data_aux[],
      lt_raw_xml_text[],
      lt_text[].
    endat.
  endloop.
endform.                    " XML_RAW_DATA_HANDLING
*&---------------------------------------------------------------------*
*&      Form  JOB_STR_EXCL_COMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_EXCL_NAMES  text
*----------------------------------------------------------------------*
form job_str_excl_comp  tables lt_names type ty_comp_name_t .
  data ls_names type ty_comp_name.

  ls_names-name =  'MANDT'.
  append ls_names to lt_names.
  ls_names-name =  'JOB_ID'.
  append ls_names to lt_names.
  ls_names-name =  'JOB_CONTEXT'.
  append ls_names to lt_names.
  ls_names-name =  'SALES_ORDER'.
  append ls_names to lt_names.
  ls_names-name =  'NPE'.
  append ls_names to lt_names.
  ls_names-name =  'SOURCE_FILE_TYPE'.
  append ls_names to lt_names.
  ls_names-name =  'ACTIVE'.
  append ls_names to lt_names.
  ls_names-name =  'JOB_STATUS'.
  append ls_names to lt_names.
  ls_names-name =  'EXT_CHECK'.
  append ls_names to lt_names.
  ls_names-name =  'SAP_CHECK'.
  append ls_names to lt_names.
  ls_names-name =  'INT_CHECK'.
  append ls_names to lt_names.
  ls_names-name =  'CREATION_DAT'.
  append ls_names to lt_names.
  ls_names-name =  'CREATION_TIM'.
  append ls_names to lt_names.
  ls_names-name =  'CREATION_DAT'.
  append ls_names to lt_names.
endform.                    " JOB_STR_EXCL_COMP
*&---------------------------------------------------------------------*
*&      Form  catalogue_build_from_raw_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form catalogue_build_from_raw_file
using it_raw_data type ty_raw_file_t
      it_src_comp   type ty_src_comp_t
      it_line_info  type ty_line_info_t.
  types:
    begin of ty_line_type,
      linetype(6),
      type_hlevel(6),
      line_hlevel    type char2,
      increment      type posnr_va,
      src_str        type tabname,
      dest_str       type tabname,
      dest_itab_name type tabname,
    end of  ty_line_type .
  types:
  ty_line_type_t type standard table of ty_line_type .

  types: begin of ty_runtime_pos,
           runtime  type zswde0037,
           curr_pos type posnr_va,
         end of ty_runtime_pos,
         ty_runtime_pos_t type standard table of ty_runtime_pos.

  data:
    ls_raw_data      type ty_raw_file,
    lv_field_val     type string,
    startposition    type i,
    o_len            type i,
    blen             type i,
    ls_catalogue     type zswif0001=>ty_value_catalogue,
    lv_curr_pos_itm  type posnr_va,
    lv_curr_pos_sitm type posnr_va,
    ls_src_comp      type ty_src_comp,
    ls_runtime       type ty_runtime_pos,
    lt_runtime       type ty_runtime_pos_t.



  field-symbols :
    <ls_line_type>   type ty_line_info, "ty_line_type
    <ls_line_type_l> type ty_line_info,
    <ls_runtime>     type ty_runtime_pos.


* build lt_runtime.

  loop at it_line_info assigning <ls_line_type>
  where not runtime_level is initial.
    ls_runtime-runtime = <ls_line_type>-runtime_level.
    collect  ls_runtime into lt_runtime.
  endloop.

  unassign   <ls_line_type>.


  loop at it_raw_data into ls_raw_data.


    read table it_line_info assigning <ls_line_type>
    with key line_type_id = ls_raw_data-line_type_id."line_type.
    if sy-subrc eq 0.


      clear: lv_curr_pos_itm , lv_curr_pos_sitm.

      read table lt_runtime assigning <ls_runtime>
      with key runtime = <ls_line_type>-runtime_level.
      if sy-subrc eq 0.
        add <ls_line_type>-item_increment to <ls_runtime>-curr_pos.
        if <ls_line_type>-parent_group_line is initial.
          lv_curr_pos_itm = <ls_runtime>-curr_pos.

          loop at it_line_info assigning <ls_line_type_l>
          where parent_group_line eq <ls_line_type>-main_group_line
          and not parent_group_line is initial.
            read table lt_runtime assigning <ls_runtime>
            with key runtime = <ls_line_type_l>-runtime_level.
            if sy-subrc eq 0.
              clear <ls_runtime>-curr_pos.
            endif.
          endloop.
        else.
          lv_curr_pos_sitm = <ls_runtime>-curr_pos.

          read table it_line_info assigning <ls_line_type_l>
          with key main_group_line = <ls_line_type>-parent_group_line.
          if sy-subrc eq 0.
            read table lt_runtime assigning <ls_runtime>
            with key runtime = <ls_line_type_l>-runtime_level.
            if sy-subrc eq 0.
              lv_curr_pos_itm  = <ls_runtime>-curr_pos.
            endif.
          endif.
        endif.
      endif.

      .
      blen = strlen( ls_raw_data-line_text ).

      clear startposition.

      loop at it_src_comp into ls_src_comp
      where src_str_id eq ls_raw_data-src_str_id.

        startposition = ls_src_comp-start_pos.
        clear lv_field_val.

        o_len = ls_src_comp-fld_length.

        if ( startposition  +  o_len ) gt blen.
          o_len = blen - startposition.
          if  o_len lt 0.
            continue.
          endif.

        endif.

        if o_len eq 0.

        else.
          lv_field_val = ls_raw_data-line_text+startposition(o_len).

          if not ls_src_comp-fld_name cs 'SPACE'.

            clear: ls_catalogue.
            ls_catalogue-job_id = ls_raw_data-job_id.
            ls_catalogue-file_id = ls_raw_data-file_id.
            ls_catalogue-file_type_id = ls_raw_data-file_type_id.
            ls_catalogue-file_sub_id = ls_raw_data-file_sub_id.
            ls_catalogue-file_type_level = ls_raw_data-file_type_level.
            ls_catalogue-file_hdr_rel  = ls_raw_data-has_header_info.
            ls_catalogue-line_number = ls_raw_data-line_number.
            ls_catalogue-src_str_group = ls_src_comp-src_str.
            ls_catalogue-src_str = <ls_line_type>-src_str_id.
            ls_catalogue-job_context = ls_src_comp-job_context.
            ls_catalogue-posnr_p1 = lv_curr_pos_itm .
            ls_catalogue-posnr_p2 = lv_curr_pos_sitm .
            ls_catalogue-kfdna = ls_src_comp-fld_name.

            if ls_src_comp-fldtype eq 'C'.
              ls_catalogue-value = lv_field_val.
            else.
              perform convert_field_value
              using
                    ls_src_comp-fldtype
                    ls_src_comp-fld_length
                    'X' " Value Fields -> 1234.56
                    'X' " Data Value -> YYYYMMDD
              changing
                lv_field_val.

              ls_catalogue-value = lv_field_val.
            endif.
            ls_catalogue-property = ls_src_comp-fld_property.
            condense ls_catalogue-value.
            append ls_catalogue to gt_catalogue.
          endif.
        endif.

      endloop. " structure components
    endif.
  endloop. " raw data
endform.                    "
*&---------------------------------------------------------------------*
*&      Form  catalogue_enrich
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form catalogue_enrich using is_job type zswtajobs.

  data:
    lo_map                         type ref to zswif0001,
    r_classdescr                   type ref to cl_abap_classdescr,
    lv_class                       type seoclsname,
    lv_contx_instance_class_method type seomtdname,
    ls_transf_routines             type zswif0001=>ty_transf_routines,
    lv_dummy                       type string.


  lv_class = zswif0001=>gc_interface_core_class_name.
  lv_contx_instance_class_method = zswif0001=>gc_contx_instance_method_name.

  if not gt_catalogue[] is initial.

    r_classdescr ?= cl_abap_typedescr=>describe_by_name( lv_class ).
    read table r_classdescr->methods with key name =  lv_contx_instance_class_method transporting no fields.
    if sy-subrc eq 0.

      call method (lv_class)=>(lv_contx_instance_class_method)
        exporting
          iv_context = is_job-job_context
          iv_class   = lv_class
        importing
          eo_refobj  = lo_map.

      if lo_map is bound.

        loop at lo_map->gt_transf_routines into ls_transf_routines.

          r_classdescr ?= cl_abap_typedescr=>describe_by_name( ls_transf_routines-class ).
          read table r_classdescr->methods with key name = ls_transf_routines-method transporting no fields.
          if sy-subrc eq 0.
            perform job_class_method_trigger
            using
                  is_job
                  ls_transf_routines-class
                  ls_transf_routines-method
                  changing
                   lv_dummy.
*            CALL METHOD (ls_transf_routines-class)=>(ls_transf_routines-method)
*              EXPORTING
*                iv_class     = lv_class
*                is_job       = is_job
*              CHANGING
*                ct_catalogue = gt_catalogue
*                ct_return    = gt_return.
          endif.
        endloop.
      endif.
    endif.


    sort gt_catalogue by job_id posnr_p1 file_type_level line_number posnr_p2 ascending src_str_group descending kfdna ascending.

  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  BAPI_fill_structures
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form bapi_fill_structures tables lt_catalogue type  zswif0001=>ty_value_catalogue_t using iv_context type zswde0021 iv_vbeln type vbeln iv_sq type char01.

  data:
    lt_mapping_rule       type ty_mapping_rule_t,
    lt_mapping_rule_redux type zswif0001=>ty_mapping_rule_redux_t,
    ls_mapping_rule_redux type zswif0001=>ty_mapping_rule_redux,
    lv_parameter          type conv_parm.

  clear: order_text[].

  perform f_read_mapping_rules tables lt_mapping_rule lt_mapping_rule_redux using iv_context.


*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* PARTNER DATA
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  clear: partner, partner[].
  partner-partn_role = 'AG'.
  move partner-partn_role to lv_parameter.
  perform bapi_build_partner_data tables lt_mapping_rule using  lv_parameter iv_context iv_vbeln.
  clear partner.
  partner-partn_role = 'WE'.
  move partner-partn_role to lv_parameter.
  perform bapi_build_partner_data tables lt_mapping_rule using  lv_parameter iv_context iv_vbeln.

*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* HEADER DATA
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  perform bapi_build_header_data tables lt_mapping_rule using iv_context iv_vbeln iv_sq.

*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* Order Text header DATA
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

* Build header texts

  loop at lt_mapping_rule_redux into ls_mapping_rule_redux where target_str eq lc_bapi_str_texts and ( rule_parameter cs 'H'  or rule_parameter is initial ) .
    perform bapi_build_text tables  lt_mapping_rule using iv_context iv_vbeln '000000' '000000' ls_mapping_rule_redux-rule_parameter.
  endloop.

*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* EXTENSION
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  perform bapi_build_extension_str tables lt_mapping_rule using iv_context iv_vbeln .

*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* ITEM DATA - Build items
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  perform bapi_build_item tables lt_mapping_rule lt_mapping_rule_redux using iv_context iv_vbeln.


endform.
*&---------------------------------------------------------------------*
*&      Form  BAPI_build_header_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MAPPING_RULE  text
*      -->P_IV_CONTEXT  text
*      -->P_LV_VBELN  text
*----------------------------------------------------------------------*
form bapi_build_header_data  tables  lt_mapping_rule type ty_mapping_rule_t using  iv_context iv_vbeln iv_sq.

* Process header bapisdhd1 based on mapping gules

  clear: header, headerx.

  perform f_map_data
  tables lt_mapping_rule
  using header
        headerx
        lc_bapi_str_header_str
        iv_context
        '000000'
        '000000'
        ''
        iv_sq.

  if not iv_vbeln is initial.
    headerx-updateflag = 'U'.

  else.
    headerx-updateflag = 'I'.
  endif.


endform.                    " BAPI_build_header_data
*&---------------------------------------------------------------------*
*&      Form  BAPI_build_partner_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PARTNER_PARTN_ROLE  text
*----------------------------------------------------------------------*
form bapi_build_partner_data tables lt_mapping_rule type ty_mapping_rule_t using iv_partner_role type conv_parm iv_context lv_vbeln.


  perform f_map_data
  tables lt_mapping_rule
  using partner
        ''
        lc_bapi_str_partner_data
        iv_context
        '000000'
        '000000'
        ''
        iv_partner_role.

  if lv_vbeln is initial and not partner is initial.
    append partner.
  endif.


endform.                    " BAPI_build_partner_data
form bapi_build_extension_str tables lt_mapping_rule type ty_mapping_rule_t using iv_context iv_vbeln type vbeln.

  data:   wa_bape_vbak  type bape_vbak,
          wa_bape_vbakx type bape_vbakx.

  data: wa_extensionin type bapiparex,
        lv_struc       type te_struc,
        lv_strucx      type te_struc.


  clear lt_extensionin[].
* Note if more than these current structures to append use fields symbols to loop to all
  lv_struc = 'BAPE_VBAK'.
  lv_strucx = 'BAPE_VBAKX'.


  clear: wa_bape_vbak, wa_bape_vbakx.

  wa_bape_vbak-vbeln = iv_vbeln.
  wa_bape_vbakx-vbeln = iv_vbeln.


  perform f_map_data
  tables lt_mapping_rule
  using wa_bape_vbak
        wa_bape_vbakx
        lc_bapi_str_bape_vbak
        iv_context
        '000000'
        '000000'
        ''
        ''.
  if not wa_bape_vbak is initial.
    wa_extensionin-structure = lv_struc.
    wa_extensionin+30(960)   = wa_bape_vbak ##ENH_OK.
    append wa_extensionin to lt_extensionin.


    wa_extensionin-structure = lv_strucx.
    wa_extensionin+30(960)   = wa_bape_vbakx ##ENH_OK.
    append wa_extensionin to lt_extensionin.
  endif.

endform.
form bapi_build_item tables lt_mapping_rule type ty_mapping_rule_t lt_mapping_rule_redux type zswif0001=>ty_mapping_rule_redux_t
using  iv_context type zswde0021 iv_vbeln type vbeln.

  data: ls_catalogue       type zswif0001=>ty_value_catalogue.
  data: ls_mapping_rule_redux type zswif0001=>ty_mapping_rule_redux.
  data: lv_req_time type tvarv_low, lv_vakey1 type string.
  data: lv_po_item_counter type i.
  data:    ls_vbak   type vbak,
           ls_vbap   type vbap,
           lt_vbepvb type table of vbepvb,
           ls_vbepvb type vbepvb.
  data: lv_matnr type matnr.

  clear: item[], itemx[], item_p2[],lt_schedules_in[],lt_schedules_inx[].
  clear: item, itemx,lt_schedules_in,lt_schedules_inx.
  clear: lt_conditions_in[], lt_conditions_inx[].
  clear: lt_conditions_in , lt_conditions_inx.
  clear: lt_cfgs_ref[],lt_cfgs_inst[],lt_cfgs_part_of[],lt_cfgs_value[].
  clear: lt_cfgs_ref,lt_cfgs_inst,lt_cfgs_part_of,lt_cfgs_value[].
  clear: lv_parent_inst_id.



  loop at gt_catalogue into ls_catalogue where property eq zswif0001=>lc_posnr_property
  and posnr_p2 is initial.

    clear: item , itemx.

    perform f_map_data
    tables lt_mapping_rule
    using
          item
          itemx
          lc_bapi_str_item_str
          iv_context
          ls_catalogue-posnr_p1
          ls_catalogue-posnr_p2
          ''
          ''.

    if item-material is not initial and item-plant is not initial.

      if item-po_itm_no is initial.
        if lv_po_item_counter is initial. lv_po_item_counter = '900000'. endif.
        item-po_itm_no = lv_po_item_counter + '1'.
        lv_po_item_counter = item-po_itm_no.
*      CLEAR itemx-po_itm_no.
      endif.

      if not item-hg_lv_item is initial. " if Main item is missing
        read table item transporting no fields
        with key itm_number = item-hg_lv_item.
        if sy-subrc ne 0.

          call function 'CONVERSION_EXIT_MATN1_OUTPUT'
            exporting
              input  = item-material
            importing
              output = lv_matnr.

          perform add_messages_to_final_return using 'E' c_message_class '057' item-hg_lv_item item-itm_number lv_matnr  '' 'bapi_build_item' item-itm_number '' ''.  "higher level item position &1 missing for  item position &2 material &3
          continue.
        endif.
      endif.


      if not iv_vbeln is initial.
        if ls_vbak is initial.
          call function 'SD_SALES_DOCUMENT_READ'
            exporting
              document_number = iv_vbeln
*             PROCESSING_MODIFICATION          = ' '
*             PROCESSING_BUFFERREAD            = ' '
            importing
              evbak           = ls_vbak.
        endif.

        clear ls_vbap.
        call function 'SD_SALES_ITEM_READ'
          exporting
            item_number         = item-itm_number
          importing
            evbap               = ls_vbap
          exceptions
            item_number_missing = 1
            others              = 2.
        if sy-subrc eq 0 and not ls_vbap is initial.
          itemx-updateflag = 'U'.
        else.
          itemx-updateflag = 'I'. "new item
        endif.

      else.
        itemx-updateflag = 'I'. "new item
      endif.


*EDI1 : Customer Purchase price
      perform bapi_build_item_price_cond tables  lt_mapping_rule using item 'EDI1' iv_context iv_vbeln.

* Build item classification
      perform bapi_build_item_classification tables lt_mapping_rule  using iv_context item.

* Build Item texts
      loop at lt_mapping_rule_redux into ls_mapping_rule_redux where target_str eq lc_bapi_str_texts and rule_parameter cs 'P'.
        perform bapi_build_text tables  lt_mapping_rule using iv_context iv_vbeln item-itm_number '000000' ls_mapping_rule_redux-rule_parameter.
      endloop.

      delete adjacent duplicates from order_text comparing text_id itm_number langu text_line.

* Fill schedule lines

      clear: lt_schedules_in,  lt_schedules_inx.

      call function 'SD_SALES_ITEM_TABLES_READ'
        exporting
          item_number         = item-itm_number
        tables
*         EXKONV              =
          exvbep              = lt_vbepvb
        exceptions
          item_number_missing = 1
          others              = 2.
      if sy-subrc <> 0.
* Implement suitable error handling here
      endif.

      if lv_req_time is initial.
        lv_vakey1 = item-plant.
        lv_req_time =  zcl_harnessing_core=>zif_harnessing_core~constant_values_get( iv_constant_id = gc_schel_req_time_const  iv_vakey1 = lv_vakey1 iv_type = 'P' ).
        if lv_req_time is initial.
          lv_req_time = '000000'.
        endif.
      endif.

      lt_schedules_in-itm_number = item-itm_number.
      lt_schedules_in-sched_line = '0001'.
      lt_schedules_in-req_date   = header-req_date_h .
      lt_schedules_in-req_time   = lv_req_time .
      lt_schedules_in-req_qty    = item-target_qty.
      append lt_schedules_in.

      lt_schedules_inx-updateflag  = 'I'.
      lt_schedules_inx-itm_number  = item-itm_number.
      lt_schedules_inx-sched_line  = lt_schedules_in-sched_line.
      lt_schedules_inx-req_date    = 'X'.
      lt_schedules_inx-req_time    = 'X'.
      lt_schedules_inx-req_qty     = 'X'.
      append lt_schedules_inx.

      if not lt_vbepvb is initial.
        loop at lt_vbepvb into ls_vbepvb
        where vbeln = iv_vbeln
        and posnr = item-itm_number.

          clear: lt_schedules_in,  lt_schedules_inx.

          read table lt_schedules_in transporting no fields with key
          itm_number = item-itm_number
          sched_line = ls_vbepvb-etenr.
          if sy-subrc ne 0.
            lt_schedules_in-itm_number = item-itm_number.
            lt_schedules_in-sched_line = ls_vbepvb-etenr.
            lt_schedules_in-req_date   = ls_vbepvb-edatu.
            lt_schedules_in-req_qty    = ls_vbepvb-wmeng.
            lt_schedules_in-req_time   = ls_vbepvb-ezeit.
            if ls_vbepvb-wmeng gt 0 or ls_vbepvb-bmeng gt 0.
              append lt_schedules_in.
            else.
              continue.
            endif.
          endif.

          read table lt_schedules_inx with key
          itm_number = item-itm_number
          sched_line = ls_vbepvb-etenr.
          if sy-subrc eq 0.
            lt_schedules_inx-updateflag  = 'U'.
            modify lt_schedules_inx index sy-tabix.
          else.
            lt_schedules_inx-updateflag  = 'D'.
            lt_schedules_inx-itm_number  = item-itm_number.
            lt_schedules_inx-sched_line  = ls_vbepvb-etenr.
            lt_schedules_inx-req_date    = 'X'.
            lt_schedules_inx-req_time    = 'X'.
            lt_schedules_inx-req_qty     = 'X'.
            append lt_schedules_inx.
          endif.
        endloop.
      endif.

      append item.
      append itemx.

    endif.
  endloop.
endform.                    " F_BAPI_item_classification
form bapi_build_item_price_cond tables  lt_mapping_rule type ty_mapping_rule_t
using item type bapisditm  iv_cond type kscha iv_context type zswde0021 iv_vbeln type vbeln.

  clear: lt_conditions_in, lt_conditions_inx.

  perform f_map_data
  tables lt_mapping_rule
  using
        lt_conditions_in
        lt_conditions_inx
        lc_bapi_str_item_cond
        iv_context
        item-itm_number
        ''
        ''
        iv_cond.

  if not lt_conditions_in-cond_value is initial and not lt_conditions_in-cond_st_no is initial.
    lt_conditions_in-itm_number = item-itm_number.
    lt_conditions_in-cond_count = lt_conditions_in-cond_count + 1.
    append lt_conditions_in.
    lt_conditions_inx-itm_number = lt_conditions_in-itm_number.
    lt_conditions_inx-cond_count = lt_conditions_in-cond_count.
    lt_conditions_inx-updateflag = 'U'.
    append lt_conditions_inx.
  endif.
endform.
form bapi_build_text tables  lt_mapping_rule type ty_mapping_rule_t
using  iv_context type zswde0021 iv_vbeln type vbeln iv_posnr_p1  type posnr_va iv_posnr_p2 type posnr_va p_rule.

  data:  ls_catalogue       type zswif0001=>ty_value_catalogue.

  loop at gt_catalogue into ls_catalogue
    where posnr_p1 eq iv_posnr_p1
      and posnr_p2 eq iv_posnr_p2
    and property eq  zswif0001=>lc_texts_property.

    clear order_text.

    perform f_map_data
    tables lt_mapping_rule
    using
          order_text
          ''
          lc_bapi_str_texts
          iv_context
          iv_posnr_p1
          iv_posnr_p2
          ls_catalogue-line_number
          p_rule.

    if not order_text-text_id is initial.
      if not iv_vbeln is initial.
        order_text-doc_number = iv_vbeln.
      endif.

      if  order_text-text_line is initial.
        if ls_catalogue-value is initial.
          append order_text.
        endif.
      else.
        append order_text.
      endif.
    endif.
  endloop.


endform.
*&---------------------------------------------------------------------*
*&      Form  _BUILD_ITEM_CLASSIFICATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
form bapi_build_item_classification  tables  lt_mapping_rule type ty_mapping_rule_t
using iv_context type zswde0021 item type bapisditm .

  types: begin of ty_bom_matnr_kdmat_rel,
           root_matnr type matnr,
           werks      type werks_d,
           kdmat      type kdmat,
           idnrk      type idnrk,
         end of ty_bom_matnr_kdmat_rel,
         ty_bom_matnr_kdmat_rel_t type standard table of ty_bom_matnr_kdmat_rel.

  constants:
    lc_capid_pp01 type capid value 'PP01',
    lc_stlan_1    type stlan value '1'.

  data: lv_relevant_p1 type boolean,
        lv_relevant_p2 type boolean.
  data: lv_matnr        type matnr.
  data: lv_string       type string.
  data: ls_catalogue    type zswif0001=>ty_value_catalogue.
  data: lv_bom_sy_subrc        type sysubrc,
        lv_check_date          type sydatum,
        lt_stb                 type standard table of stpox,
        ls_stb                 type stpox,
        ls_mast                type cs01_mastb,
        lt_mast                type cs01_mastb_tab,
        lt_stko                type cs01_stkob_tab,
        lt_stzub               type cs01_stzub_tab,
        lv_stdpd               type stdpd,
        lv_cuobj               type cuobj,
        lv_variant_mat         type flag,
        lt_bom_matnr_kdmat_rel type ty_bom_matnr_kdmat_rel_t,
        ls_bom_matnr_kdmat_rel type ty_bom_matnr_kdmat_rel,
        ls_char_md             type ref to cugen_cstic_master_s,
        lt_conf_values         type ibco2_value_tab,
        ls_conf_values         type ibco2_value_rec.


* item classification reference
* each relevant item increment config_id
  lt_cfgs_ref-posex     = item-po_itm_no.
  lt_cfgs_ref-config_id = lt_cfgs_ref-config_id + '000001'.
  unpack lt_cfgs_ref-config_id to lt_cfgs_ref-config_id.
  lt_cfgs_ref-root_id   = '00000001'.


  clear lt_cfgs_inst-inst_id.
  lt_cfgs_inst-config_id  = lt_cfgs_ref-config_id .
  lt_cfgs_inst-inst_id    = lt_cfgs_inst-inst_id + '00000001'.
  unpack lt_cfgs_inst-inst_id to lt_cfgs_inst-inst_id.
  lt_cfgs_inst-obj_type   =   lc_obj_type.
  lt_cfgs_inst-class_type = lc_class_type.
  lt_cfgs_inst-obj_key    = item-material.
  lt_cfgs_inst-quantity = item-target_qty.
  lt_cfgs_inst-quantity_unit = item-target_qu.
  lt_cfgs_inst-complete = 'T'.
  lt_cfgs_inst-consistent = 'T'.


  perform bapi_build_classification_val using lt_cfgs_inst item-itm_number item-material item-plant '000000' lv_relevant_p1 .


  if not lv_relevant_p1 is initial.
    append lt_cfgs_ref.
    append lt_cfgs_inst.
  else.
    lt_cfgs_ref-config_id = lt_cfgs_ref-config_id - '000001'.
    unpack lt_cfgs_ref-config_id to lt_cfgs_ref-config_id.
    lt_cfgs_inst-inst_id = lt_cfgs_inst-inst_id - '00000001'.
    unpack lt_cfgs_inst-inst_id to lt_cfgs_inst-inst_id.
  endif.

* Sub-items

  lv_parent_inst_id = lt_cfgs_inst-inst_id.

  clear lt_cfgs_part_of.
  loop at gt_catalogue into ls_catalogue where property eq zswif0001=>lc_cable_id_mat_property
  and posnr_p1 = item-itm_number.


    clear:  item_p2 , lv_relevant_p2 .

    lv_check_date = sy-datum. " Change to order request date

* Read Material BOM
    if lt_stb is initial and lv_bom_sy_subrc eq 0.

      clear:ls_mast, lt_mast[].
      call function 'CS_ALT_SELECT_MAT'
        exporting
          capid          = lc_capid_pp01
          datuv          = lv_check_date
          losgr          = item-target_qty
          matnr          = item-material
          stlan          = lc_stlan_1
          werks          = item-plant
        tables
          mastb_wa       = lt_mast
          stkob_wa       = lt_stko
          stzub_wa       = lt_stzub
        exceptions
          alt_not_found  = 1
          bom_not_active = 2
          bom_not_found  = 3
          call_invalid   = 4
          no_alt_found   = 5
          no_bom_found   = 6
          others         = 7.
      if sy-subrc ne 0 or lt_mast[] is initial.
        lv_bom_sy_subrc = sy-subrc.
        perform add_messages_to_final_return using 'E' 'CO' '524'  item-material item-plant '' '' 'build_item_classification' item-itm_number '' ''. " No BOM exists for material & plant &
      else.

* read first alternative until furter logic
        read table lt_mast into ls_mast index 1.

        call function 'CS_BOM_EXPL_MAT_V2'
          exporting
            capid                 = lc_capid_pp01
            datuv                 = lv_check_date
            emeng                 = item-target_qty
            mtnrv                 = item-material
            stlal                 = ls_mast-stlal
            stlan                 = ls_mast-stlan
            werks                 = item-plant
          tables
            stb                   = lt_stb
          exceptions
            alt_not_found         = 1
            call_invalid          = 2
            material_not_found    = 3
            missing_authorization = 4
            no_bom_found          = 5
            no_plant_data         = 6
            no_suitable_bom_found = 7
            conversion_error      = 8
            others                = 9.
        if sy-subrc ne 0.
          lv_bom_sy_subrc = sy-subrc.
          perform add_messages_to_final_return using 'E' 'CO' '524'  item-material item-plant '' '' 'build_item_classification' item-itm_number '' ''. " No BOM exists for material & plant &
        else.

          try.
              ls_char_md = cl_cugen_classdata_service=>get_characteristic( zif_harnessing_core=>gc_kdmat_char_name ).
            catch cx_cugen_error .
          endtry.

          if not ls_char_md is initial.

            loop at lt_stb into ls_stb
            where xloek eq space
            and postp eq 'L'
            and kzkfg eq 'X'.

* Identify BOM component via customer material characteristic
              perform get_material_classification using ls_stb-idnrk ls_stb-werks '' changing lt_conf_values lv_stdpd lv_cuobj lv_variant_mat .


              ls_bom_matnr_kdmat_rel-root_matnr = item-material.
              ls_bom_matnr_kdmat_rel-werks = ls_stb-werks.
              ls_bom_matnr_kdmat_rel-idnrk = ls_stb-idnrk.

              loop at lt_conf_values into ls_conf_values
              where atinn = ls_char_md->atinn.
                ls_bom_matnr_kdmat_rel-kdmat =  ls_conf_values-atwrt.
                append ls_bom_matnr_kdmat_rel to lt_bom_matnr_kdmat_rel.
              endloop.
            endloop.
          endif.
        endif.
      endif.
    endif.

    if not lt_stb[] is initial.

* map BOM item to derive qty
      if not lt_bom_matnr_kdmat_rel[] is initial.

        clear ls_bom_matnr_kdmat_rel.
        read table lt_bom_matnr_kdmat_rel into ls_bom_matnr_kdmat_rel
        with key kdmat = ls_catalogue-value
               root_matnr = item-material
               werks = item-plant.
        if sy-subrc eq 0.

          item_p2-material = ls_bom_matnr_kdmat_rel-idnrk.

          read table lt_stb into ls_stb
          with key idnrk = item_p2-material.
          if sy-subrc eq 0.

            item_p2-target_qty = ls_stb-menge.
            item_p2-target_qu = ls_stb-meins.
            item_p2-plant = item-plant.

            lt_cfgs_inst-config_id  = lt_cfgs_ref-config_id .
            lt_cfgs_inst-inst_id    = lt_cfgs_inst-inst_id + '00000001'.
            unpack lt_cfgs_inst-inst_id to lt_cfgs_inst-inst_id .
            lt_cfgs_inst-obj_type   = lc_obj_type.
            lt_cfgs_inst-class_type = lc_class_type.
            lt_cfgs_inst-obj_key    = item_p2-material.
            lt_cfgs_inst-quantity = item_p2-target_qty.
            lt_cfgs_inst-quantity_unit = item_p2-target_qu.
            lt_cfgs_inst-complete = 'T'.
            lt_cfgs_inst-consistent = 'T'.
*  lt_cfgs_inst-object_guid = lt_cfgs_inst-obj_key.
*  lt_cfgs_inst-persist_id_type = 'G'.


            perform bapi_build_classification_val using lt_cfgs_inst item-itm_number item_p2-material item-plant  ls_catalogue-posnr_p2 lv_relevant_p2.


            lt_cfgs_part_of-config_id = lt_cfgs_inst-config_id.
            lt_cfgs_part_of-parent_id = lv_parent_inst_id.
            lt_cfgs_part_of-inst_id = lt_cfgs_inst-inst_id.
            lt_cfgs_part_of-part_of_no = ls_stb-posnr.
            unpack lt_cfgs_part_of-part_of_no to lt_cfgs_part_of-part_of_no.
            lt_cfgs_part_of-obj_type =  lt_cfgs_inst-obj_type.
            lt_cfgs_part_of-obj_key = lt_cfgs_inst-obj_key.

            if not lv_relevant_p2 is initial.

              append lt_cfgs_part_of.
              append lt_cfgs_inst.


              if  lv_relevant_p1 is initial.  " but not the p2 then still append the cfg ref
                append lt_cfgs_ref.
              endif.

            else.
              lt_cfgs_inst-inst_id = lt_cfgs_inst-inst_id - '00000001'.
              unpack lt_cfgs_inst-inst_id to lt_cfgs_inst-inst_id.
            endif.

          else.
*          PERFORM add_messages_to_final_return USING 'E'  c_message_class '***'  item_p2-material item-material item-plant  item-itm_number 'build_item_classification' item-itm_number '' ''. "
          endif.

* Append posnr2 to item structure as tmp items for validation only
          append item_p2.

        else.

          call function 'CONVERSION_EXIT_MATN1_OUTPUT'
            exporting
              input  = item-material
            importing
              output = lv_matnr.

          concatenate lv_matnr item-plant into lv_string separated by '/'.
          perform add_messages_to_final_return using 'E'  c_message_class '61'  ls_catalogue-value lv_string  item-itm_number ls_catalogue-file_type_id 'build_item_classification' item-itm_number '' ''. "
        endif. " BOM
      endif.
    endif.
  endloop.



endform.                    " _BUILD_ITEM_CLASSIFICATION
*&---------------------------------------------------------------------*
*&      Form  BAPI_build_classification_values
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form bapi_build_classification_val using lt_cfgs_inst type bapicuins iv_posnr_p1 type posnr_va iv_material type matnr iv_plant type werks_d iv_posnr_p2 lv_relevant.

  types:begin of ty_multi_char_sort,
          atnam type atnam,
          count type i,
        end of  ty_multi_char_sort,
        ty_multi_char_sort_t type standard table of ty_multi_char_sort.

  data:
    ls_return              type bapiret2,
    lv_stdpd               type stdpd,
    lv_cuobj               type cuobj,
    lv_variant_mat         type flag,
    lt_conf_values         type ibco2_value_tab,
    ls_conf_values         type ibco2_value_rec,
    lv_objnum              type objnum,
    lv_excl_rules_read     type boolean,
    lt_char_excl           type cuib_atnam_t,
    lv_atnam               type atnam,
    lv_atinn               type atinn,
    lv_value               type atwtb,
    lv_atflv               type atflv,
    ls_char_md             type ref to cugen_cstic_master_s,
    lt_alloc               type tt_bapi1003_alloc_list,
    ls_alloc               type bapi1003_alloc_list,
    lt_characteristics_aux type tt_bapi_char,
    lt_characteristics     type tt_bapi_char,
    ls_characteristics     type bapi_char,
    lt_char_values         type tt_bapi_char_values,
    ls_char_values         type bapi_char_values,
    lt_lines               type tlinetab,
    ls_line                type tline,
    lv_lcount              type numc2,
    lt_multi_char_sort     type ty_multi_char_sort_t,
    ls_multi_char_sort     type ty_multi_char_sort,
    ls_catalogue           type zswif0001=>ty_value_catalogue,
    lv_struc               type te_struc,
    lv_char_number_digits  type anzst,
    wa_bape_vbak           type bape_vbak,
    wa_extensionin         type bapiparex.

  field-symbols: <fs>             type any,
                 <wa_bape_vbak>   type bape_vbak,
                 <wa_extensionin> type any.
  field-symbols <ls_line> type  tline.
  field-symbols <char_val> type bapicuval.


  clear:
   lt_characteristics[],
   lv_cuobj,
   lt_return[],
   lt_alloc[] ,
   lt_characteristics_aux[],
   lv_variant_mat,
*  lt_configuration[],
   lt_conf_values[].


  lv_relevant = 'X'.

  perform get_material_classification using iv_material iv_plant 'X' changing lt_conf_values lv_stdpd lv_cuobj lv_variant_mat.

  if not lv_stdpd is initial.
    lt_cfgs_inst-obj_key = lv_stdpd.
  endif.

  move lt_cfgs_inst-obj_key to lv_objnum.

  call function 'BAPI_OBJCL_GET_KEY_OF_OBJECT'
    exporting
      objectname     = lv_objnum
      objecttable    = lc_objecttable
      classtype      = lc_class_type
    importing
      clobjectkeyout = lv_cuobj
    tables
      return         = lt_return.
  clear lt_return[].
  if not lv_cuobj is initial.
    call function 'BAPI_OBJCL_GETCLASSES_KEY'
      exporting
        clobjectkeyin = lv_cuobj
      tables
        alloclist     = lt_alloc
        return        = lt_return.

    loop at lt_return into ls_return.
      perform add_messages_to_final_return using ls_return-type ls_return-id ls_return-number  iv_material iv_plant '' '' 'BAPI_build_classification_values' item-itm_number '' ''.
    endloop.

    clear lt_return[].

* Material relevant chars
    if not lt_alloc[] is initial.
      loop at lt_alloc into ls_alloc.
        call function 'BAPI_CLASS_GET_CHARACTERISTICS'
          exporting
            classnum        = ls_alloc-classnum
            classtype       = ls_alloc-classtype
          tables
            characteristics = lt_characteristics
            char_values     = lt_char_values.

        append lines of lt_characteristics to lt_characteristics_aux.

      endloop.

      lt_characteristics[] = lt_characteristics_aux[].

    endif.


    if lv_excl_rules_read is initial.
      call method zcl_harnessing_core=>zif_harnessing_core~get_characteristics_exclusion
        importing
          et_char_excl       = lt_char_excl
          ev_excl_rules_read = lv_excl_rules_read.
    endif.


    loop at lt_characteristics into ls_characteristics.

      read table lt_char_excl transporting no fields
      with key table_line = ls_characteristics-name_char.
      if sy-subrc ne 0.
        clear: ls_catalogue, lv_atflv, lt_cfgs_value-value,  lt_multi_char_sort[] , lv_char_number_digits.

        loop at gt_catalogue into ls_catalogue
        where kfdna eq ls_characteristics-name_char
        and posnr_p1 eq iv_posnr_p1.

          lv_char_number_digits = ls_characteristics-number_digits.

          if iv_posnr_p2 ne '000000'.
            check ls_catalogue-posnr_p2 eq iv_posnr_p2.
          endif.

          clear: lt_cfgs_value-value.

          lt_cfgs_value-config_id  = lt_cfgs_inst-config_id.
          lt_cfgs_value-inst_id    = lt_cfgs_inst-inst_id.
          lt_cfgs_value-charc = ls_characteristics-name_char.
          if ls_characteristics-data_type = 'NUM'.

            condense ls_catalogue-value.
            move lt_cfgs_value-charc to lv_atnam.
            move ls_catalogue-value  to lv_value.
            try.
                ls_char_md = cl_cugen_classdata_service=>get_characteristic( lv_atnam ).
              catch cx_cugen_error .
            endtry.


            if not ls_char_md->atinn is initial.

              call function 'COC1_CONV_VALUES_AFTER_INPUT'
                exporting
                  value             = lv_value
                  characteristic_id = ls_char_md->atinn
                importing
                  char_value_float  = lv_atflv
                exceptions
                  char_not_valid    = 1
                  conversion_error  = 2
                  others            = 3.
              if sy-subrc eq 0.
                lt_cfgs_value-value = lv_atflv .
              elseif sy-subrc eq 1.
*                MESSAGE e400(cb) RAISING no_valid_char.
                perform add_messages_to_final_return  using 'E' 'cb' '400' '' '' '' '' 'bapi_build_classification_val' '' '' ''. " Error when assigning characteristic value
              else.
*             Input invalid! -> E message
                perform add_messages_to_final_return  using 'E' 'cb' '611' ls_characteristics-name_char lv_value '' '' 'bapi_build_classification_val' '' '' ''. " Enter allowed value for characteristic & &
              endif.

              condense lt_cfgs_value-value.
              translate lt_cfgs_value-value to upper case.
              append lt_cfgs_value.
            endif.

          else.

            if ls_characteristics-single_value is initial.
              if ls_catalogue-value ca ';'.
                lv_char_number_digits = ls_characteristics-number_digits - 2.
              else.
                lv_char_number_digits = ls_characteristics-number_digits - 3.
              endif.
              clear: ls_line, lt_lines[].
              ls_line-tdline = ls_catalogue-value.
              append ls_line to lt_lines.

              call function 'FORMAT_TEXTLINES'
                exporting
                  linewidth   = lv_char_number_digits
                tables
                  lines       = lt_lines
                exceptions
                  bound_error = 1
                  others      = 2.
              if sy-subrc eq 0.

                loop at lt_lines into ls_line.
                  if ls_line ca ';'.
                    concatenate '''' ls_line-tdline  ''''  into lt_cfgs_value-value.
                  elseif ls_line ca '(' or ls_line ca ')'.
                    replace all occurrences of '(' in ls_line with space.
                    replace all occurrences of ')' in ls_line with space.
                    lt_cfgs_value-value = ls_line-tdline.
                  else.
                    lt_cfgs_value-value = ls_line-tdline.
                  endif.

                  condense lt_cfgs_value-value.
                  translate lt_cfgs_value-value to upper case.
                  append lt_cfgs_value.

                  if ls_characteristics-single_value is initial and not lt_cfgs_value-value cs '''' .
                    ls_multi_char_sort-atnam = lt_cfgs_value-charc.
                    ls_multi_char_sort-count = 1.
                    collect  ls_multi_char_sort into lt_multi_char_sort.
                  endif.
                endloop.
              endif.
            else.
              if ls_catalogue-value ca ';'.
                concatenate '''' ls_catalogue-value  ''''  into ls_catalogue-value.
              endif.

              lt_cfgs_value-value = ls_catalogue-value(lv_char_number_digits). " Hope it does not trunc

              condense lt_cfgs_value-value.
              translate lt_cfgs_value-value to upper case.

              if ls_characteristics-object_table eq 'VBAK'.

                concatenate 'BAPE' ls_characteristics-object_table  into lv_struc separated by '_'.

                read table lt_extensionin assigning <wa_extensionin> with key structure = lv_struc.
                if sy-subrc eq 0.
                  wa_bape_vbak = <wa_extensionin>+30(960) ##ENH_OK.
                  assign component ls_characteristics-table_field of structure wa_bape_vbak to <fs>.
                  if <fs> is assigned.
                    if <fs> eq lt_cfgs_value-value.
                      append lt_cfgs_value.
                    else.
                      if not <fs> is initial.
                        lt_cfgs_value-value = <fs>.
                        append lt_cfgs_value.
                      else.
                        <fs> = lt_cfgs_value-value.
                        <wa_extensionin>+30(960)   = wa_bape_vbak ##ENH_OK.
                        append lt_cfgs_value.
                      endif.
                    endif.
                  endif.
                endif.
              else.
                append lt_cfgs_value.
              endif.
            endif.
          endif.
        endloop.

        " Complement configuration for variant materials if not value derived from catalog data
        if lt_cfgs_value-value is initial and not lv_variant_mat is initial .

          clear lv_atinn.
          try.
              ls_char_md = cl_cugen_classdata_service=>get_characteristic( ls_characteristics-name_char ).
            catch cx_cugen_error .
          endtry.

          move  ls_char_md->atinn to lv_atinn.
          loop at lt_conf_values into ls_conf_values
            where atinn eq lv_atinn.
            lt_cfgs_value-config_id  = lt_cfgs_inst-config_id.
            lt_cfgs_value-inst_id    = lt_cfgs_inst-inst_id.
            lt_cfgs_value-charc = ls_characteristics-name_char.

            if ls_char_md->atfor = 'NUM'.
              lt_cfgs_value-value = ls_conf_values-atflv.
              lt_cfgs_value-value_to = ls_conf_values-atflb.
              append lt_cfgs_value.
            else.
              lt_cfgs_value-value = ls_conf_values-atwrt.
              append lt_cfgs_value.
            endif.
          endloop.
        endif.
      endif.


      if not lt_multi_char_sort[] is initial.

        read table lt_multi_char_sort into ls_multi_char_sort index 1.
        if  ls_multi_char_sort-count gt 1.
          clear lv_lcount.
          loop at lt_cfgs_value assigning <char_val>
            where config_id  = lt_cfgs_inst-config_id
            and   inst_id    = lt_cfgs_inst-inst_id
            and   charc = ls_characteristics-name_char.

            lv_lcount = lv_lcount + 1.
            concatenate  lv_lcount  <char_val>-value  into  <char_val>-value separated by '-'.
          endloop.
        endif.
      endif.  " Add sorting prefix to multi value chart


    endloop. " Loop at characteristics
  else.

* Not classification relevant
    clear lv_relevant.

  endif.
endform.                    " BAPI_build_classification_values
*&---------------------------------------------------------------------*
*&      Form  GET_MATERIAL_CLASSIFICATION
*&---------------------------------------------------------------------*
form get_material_classification
  using  lv_material type matnr
         lv_plant type werks_d
         lv_only_variants type flag
  changing ct_conf_values type ibco2_value_tab
    cv_stdpd  type stdpd
    cv_cuobj type cuobj
    cv_variant_mat type flag .

  data:
    lt_ret           type bapiret2_tab,
    ls_char_md       type ref to cugen_cstic_master_s,
    lv_stdpd         type stdpd,
    lv_cuobj         type cuobj,
    lv_objnum        type objnum,
    lt_configuration type standard table of ibco2_instance_rec2,
    ls_configuration type ibco2_instance_rec2,
    ls_conf_values   type ibco2_value_rec,
    lt_alloc         type tt_bapi1003_alloc_list,
    lt_valueschar    type tt_bapi1003_alloc_values_char,
    ls_valueschar    type bapi1003_alloc_values_char,
    lt_valuescurr    type tt_bapi1003_alloc_values_curr,
    lt_valuesnum     type tt_bapi1003_alloc_values_num,
    ls_valuesnum     type bapi1003_alloc_values_num.

  clear: ct_conf_values[], cv_stdpd, cv_cuobj, cv_variant_mat.

  select single stdpd cuobj from marc into ( lv_stdpd  , lv_cuobj )
  where matnr eq lv_material
  and werks eq lv_plant.
  if sy-subrc eq 0.

    cv_cuobj = lv_cuobj.
    cv_stdpd = lv_stdpd.

    if not lv_stdpd is initial.
      cv_variant_mat = 'X'.
    endif.

    if not lv_only_variants = 'X' and cv_variant_mat = space.

      move lv_material to  lv_objnum.

      call function 'BAPI_OBJCL_GET_KEY_OF_OBJECT'
        exporting
          objectname     = lv_objnum
          objecttable    = lc_objecttable
          classtype      = lc_class_type
        importing
          clobjectkeyout = lv_cuobj
        tables
          return         = lt_ret.
    endif.

    if not lv_cuobj is initial.

      if cv_variant_mat = 'X'.

        call function 'CUCB_GET_CONFIGURATION'
          exporting
            instance                     = lv_cuobj
          importing
            configuration                = lt_configuration
          exceptions
            invalid_input                = 1
            invalid_instance             = 2
            instance_is_a_classification = 3
            others                       = 4.
        if sy-subrc eq 0.
          read table lt_configuration into ls_configuration index 1.
          if sy-subrc eq 0.
            append lines of ls_configuration-values to ct_conf_values.
          endif.
        endif.

      else.


        call function 'BAPI_OBJCL_GETCLASSES_KEY'
          exporting
            clobjectkeyin   = lv_cuobj
            read_valuations = 'X'
          tables
            alloclist       = lt_alloc
            allocvalueschar = lt_valueschar
            allocvaluescurr = lt_valuescurr
            allocvaluesnum  = lt_valuesnum
            return          = lt_ret.

        loop at lt_valueschar into ls_valueschar.

          try.
              ls_char_md = cl_cugen_classdata_service=>get_characteristic( ls_valueschar-charact ).
            catch cx_cugen_error .
          endtry.
          if not  ls_char_md is initial.
            ls_conf_values-atwrt = ls_valueschar-value_neutral.
            ls_conf_values-atinn = ls_char_md->atinn.
            append ls_conf_values to ct_conf_values.
          endif.
        endloop.

        loop at lt_valuesnum into ls_valuesnum .
          try.
              ls_char_md = cl_cugen_classdata_service=>get_characteristic( ls_valueschar-charact ).
            catch cx_cugen_error .
          endtry.
          if not  ls_char_md is initial.
            ls_conf_values-atflv = ls_valuesnum-value_from.
            ls_conf_values-atflb = ls_valuesnum-value_to.
            ls_conf_values-atinn = ls_char_md->atinn.
            append ls_conf_values to ct_conf_values.
          endif.
        endloop.

      endif.
    endif.
  else.



  endif.
endform.                    " GET_MATERIAL_CLASSIFICATION
form f_map_data
tables lt_mapping_rule type ty_mapping_rule_t
using
      iv_targ_str
      iv_targx_str
      iv_targ_str_name
      iv_context
      iv_posnr_p1
      iv_posnr_p2
      iv_line
      iv_rule_parameter.

  data:
    descr_type         type ref to cl_abap_typedescr,
    ls_catalogue       type zswif0001=>ty_value_catalogue,
    ls_mapping_rule    type zswif0001=>ty_mapping_rule,
    lv_line_read_req   type boolean,
    lv_ilen            type i,
    lv_targx_str_exist type boolean,
    r_classdescr       type ref to cl_abap_classdescr,
    lv_olen            type i.

  field-symbols:
    <fs>             type any,
    <fsx>            type any,
    <ls_struct_aux>  type any,
    <ls_structx_aux> type any,
    <ls_targ_str>    type tabname,
    <ls_targx_str>   type tabname.


* describe structure
  describe field iv_targx_str length lv_ilen in character mode.

  if lv_ilen gt 1.
    lv_targx_str_exist = 'X'.
  endif.

  sort lt_mapping_rule by context target_str target_field fld_counter.
  loop at lt_mapping_rule into ls_mapping_rule
  where target_str eq iv_targ_str_name
  and rule_parameter eq iv_rule_parameter
  and context = iv_context.

    clear: lv_line_read_req , ls_catalogue.

    assign component ls_mapping_rule-target_field  of structure iv_targ_str to <fs>.

    if ls_mapping_rule-target_field  = 'TEXT_LINE'. " If more cases add parameter to mapping rule
      lv_line_read_req = 'X'.
    endif.


    if <fs> is assigned.

      if <fs> is initial.
        case ls_mapping_rule-rule_type.

          when 'F'.
            <fs> = ls_mapping_rule-fix_value.
          when 'D' or 'R'.

            if not ls_mapping_rule-src_field is initial.
              if lv_line_read_req is initial.
                read table gt_catalogue into ls_catalogue
                with key
                kfdna = ls_mapping_rule-src_field
                posnr_p1 = iv_posnr_p1
                posnr_p2 = iv_posnr_p2
                src_str_group = ls_mapping_rule-src_str
                job_context = ls_mapping_rule-context.
                if sy-subrc eq 0.
                  if ls_mapping_rule-rule_type eq 'R'.
                    <fs> = ls_mapping_rule-fix_value.
                  else.
                    <fs> = ls_catalogue-value.
                  endif.
                endif.
              endif.
              if not iv_line is initial.
                read table gt_catalogue into ls_catalogue
                with key
                kfdna = ls_mapping_rule-src_field
                posnr_p1 = iv_posnr_p1
                posnr_p2 = iv_posnr_p2
                line_number = iv_line
                src_str_group = ls_mapping_rule-src_str
                job_context = ls_mapping_rule-context.
                if sy-subrc eq 0.
                  if ls_mapping_rule-rule_type eq 'R'.
                    <fs> = ls_mapping_rule-fix_value.
                  else.
                    <fs> = ls_catalogue-value.
                  endif.
                endif.
              endif.
            endif.


          when 'M'.

            if not ls_mapping_rule-class is initial and not ls_mapping_rule-method is initial.
              r_classdescr ?= cl_abap_typedescr=>describe_by_name( ls_mapping_rule-class ).
              read table r_classdescr->methods with key name = ls_mapping_rule-method transporting no fields.
              if sy-subrc eq 0.

                if not ls_mapping_rule-src_field is initial.
                  read table gt_catalogue into ls_catalogue
                  with key
                  kfdna = ls_mapping_rule-src_field
                  posnr_p1 = iv_posnr_p1
                  posnr_p2 = iv_posnr_p2
                  src_str_group = ls_mapping_rule-src_str
                  job_context = ls_mapping_rule-context.

                  if not iv_line is initial.
                    read table gt_catalogue into ls_catalogue
                    with key
                    kfdna = ls_mapping_rule-src_field
                    posnr_p1 = iv_posnr_p1
                    posnr_p2 = iv_posnr_p2
                    line_number = iv_line
                    src_str_group = ls_mapping_rule-src_str
                    job_context = ls_mapping_rule-context.
                  endif.
                endif.

                try.
                    call method (ls_mapping_rule-class)=>(ls_mapping_rule-method)
                      exporting
                        is_catalogue    = ls_catalogue
                        is_mapping_rule = ls_mapping_rule
                      importing
                        ev_value        = <fs>
                      changing
                        ct_catalogue    = gt_catalogue
                        ct_return       = lt_return.

                    append lines of lt_return to gt_return.

                endtry.
              endif.
            else.
              " issue error in case of missing method/class
            endif.
          when others.
        endcase.

        if not ls_mapping_rule-property is initial
            and <fs> is assigned
            and  not <fs> is initial.
          ls_catalogue-posnr_p1 = iv_posnr_p1.
          ls_catalogue-posnr_p2 = iv_posnr_p2.
          ls_catalogue-job_context = iv_context.
          ls_catalogue-kfdna = ls_mapping_rule-property.
          ls_catalogue-property = ls_mapping_rule-property.
          ls_catalogue-value = <fs>.
          append  ls_catalogue to gt_catalogue.
        endif.

        if  ls_mapping_rule-required_field = 'X'.

          if <fs> is initial. " Required field has no value
            perform add_messages_to_final_return in program zswre1000
using 'E' c_message_class '051' ls_mapping_rule-target_field '' iv_posnr_p1  '' 'FORM f_map_data' iv_posnr_p1 '' ''.
          endif.

        endif.

        if not  lv_targx_str_exist eq space.
          assign component ls_mapping_rule-target_field  of structure iv_targx_str to <fsx>.
          if <fsx> is assigned.

            descr_type = cl_abap_typedescr=>describe_by_data( <fsx> ).

            if descr_type->absolute_name eq '\TYPE=BAPIUPDATE' or descr_type->absolute_name eq  '\TYPE=CHAR1'.
              <fsx> = 'X'.
            else.
              <fsx> = <fs>.
            endif.
            unassign <fsx>.
          endif.
        endif.
        unassign <fs>.
      endif.
    endif.
  endloop.

endform.                    " F_MAP_DATA
*&---------------------------------------------------------------------*
*&      Form  F_READ_MAPPING_RULES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_MAPPING_RULE  text
*----------------------------------------------------------------------*
form f_read_mapping_rules
  tables
       lt_mapping_rule type ty_mapping_rule_t
       lt_mapping_rule_redux type zswif0001=>ty_mapping_rule_redux_t
using  iv_context.


  data: ls_mapping_rule       type zswif0001=>ty_mapping_rule,
        ls_mapping_rule_redux type zswif0001=>ty_mapping_rule_redux.

  select * from zswta0012 into table lt_mapping_rule
  where context eq iv_context
  and active eq 'X'
  order by context target_str fld_counter rule_parameter.


  loop at lt_mapping_rule into ls_mapping_rule.
    move-corresponding ls_mapping_rule to ls_mapping_rule_redux.
    collect ls_mapping_rule_redux into lt_mapping_rule_redux.
  endloop.


endform.                    " F_READ_MAPPING_RULES
*&---------------------------------------------------------------------*
*&      Form  COLLECT_SSX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->
*----------------------------------------------------------------------*
form bapi_process_ssx_components
   tables lt_catalogue type zswif0001=>ty_value_catalogue_t
          et_components type ty_ssx_components_t
  using is_job type zswtajobs.


  types:
    begin of ty_ssx_ref_materials,
      ref_matnr type matnr_ku,
      matnr     type matnr,
      hlf       type zswde0113,
      plant     type werks_d,
      cable     type boolean,
    end of ty_ssx_ref_materials,
    ty_ssx_ref_materials_t type standard table of ty_ssx_ref_materials.

  types: begin of ty_posnr_hlf,
           posnr_p1 type posnr_va,
           hlf      type zswde0113,
         end of ty_posnr_hlf,
         ty_posnr_hlf_t type standard table of ty_posnr_hlf.

  data: lv_matnr         type matnr,
        lv_plant         type werks_d,
        lv_hlf           type zswde0113,
        lt_catalogue_aux type zswif0001=>ty_value_catalogue_t,
        ls_catalogue_aux type zswif0001=>ty_value_catalogue,
        ls_catalogue     type zswif0001=>ty_value_catalogue,
        ls_ref_materials type ty_ssx_ref_materials,
        lt_ref_materials type ty_ssx_ref_materials_t,
        ls_components    type ty_ssx_components,
        lt_components    type ty_ssx_components_t,
        lt_posnr_hlf     type ty_posnr_hlf_t,
        ls_posnr_hlf     type ty_posnr_hlf,
        lt_dsconv        type table of zswta_ssx_dsconv,
        ls_dsconv        type zswta_ssx_dsconv,
        lt_ssx_bom       type  table of zswta_ssx_bom,
        ls_ssx_bom       type zswta_ssx_bom.

  data: lt_return  type bapiret2_t.
  data: item_aux              like bapisditm  occurs 0 with header line.
  data: lv_skip_chk           type boolean.
  data: lt_mapping_rule       type ty_mapping_rule_t,
        lt_mapping_rule_redux type zswif0001=>ty_mapping_rule_redux_t.

  data: lv_system_role type cccategory.

  field-symbols: <ls_ref_materials> type ty_ssx_ref_materials,
                 <ls_components>    type ty_ssx_components.

  lt_catalogue_aux[] = lt_catalogue[].
  lt_return[] = gt_return[]. " Logs from f_map_data in this step are ignored

  loop at lt_catalogue into ls_catalogue
  where property eq zswif0001=>lc_ssx_id_number_property
  or property eq zswif0001=>lc_ssx_cable_id_mat_property.

    clear: ls_ref_materials.

    clear lv_skip_chk.

    read table item with key itm_number = ls_catalogue-posnr_p1 .
    if sy-subrc eq 0.
      ls_ref_materials-plant = item-plant.


    else. " Derive plant in case item table is empty

      lv_skip_chk = 'X'.

      if lt_mapping_rule[] is initial.
        perform f_read_mapping_rules tables lt_mapping_rule lt_mapping_rule_redux using is_job-job_context.
      endif.

      perform f_map_data
     tables lt_mapping_rule
     using
           item_aux
           ''
           lc_bapi_str_item_str
           is_job-job_context
           ls_catalogue-posnr_p1
           ls_catalogue-posnr_p2
           ''
           ''.

      ls_ref_materials-plant = item_aux-plant.
    endif.


    ls_ref_materials-ref_matnr = ls_catalogue-value.


    move-corresponding ls_catalogue to ls_components.
    ls_components-job_nr = is_job-job_nr.
    ls_components-posnr = ls_catalogue-posnr_p1.
    ls_components-plant = ls_ref_materials-plant.
    ls_components-ref_matnr = ls_ref_materials-ref_matnr.

    if ls_catalogue-property eq zswif0001=>lc_ssx_cable_id_mat_property.

      ls_ref_materials-cable = 'X'.


      read table lt_catalogue_aux into ls_catalogue_aux
      with key property = zswif0001=>lc_cable_length_property
          file_id = ls_catalogue-file_id
          line_number = ls_catalogue-line_number.
      if sy-subrc eq 0.
        move ls_catalogue_aux-value to ls_components-ssx_sum.
      endif.
    else.
      move 1 to ls_components-ssx_sum.
    endif.


    collect ls_components into lt_components.


* get Hlf from ka nbr
    clear gs_return.
    read table lt_posnr_hlf into ls_posnr_hlf
    with key posnr_p1 = ls_catalogue-posnr_p1.
    if sy-subrc eq 0.
      ls_ref_materials-hlf = ls_posnr_hlf-hlf.
    else.
      read table lt_cfgs_ref with key posex = item-po_itm_no.
      if sy-subrc eq 0.
        read table lt_cfgs_value with key config_id = lt_cfgs_ref-config_id
        charc = zif_harnessing_core=>gc_ka_nbr_char_name.
        if sy-subrc eq 0.
          perform derive_ssx_hlf using zif_harnessing_core=>gc_ssx_ka_vtab lt_cfgs_value-value item-plant ls_ref_materials-hlf.

          if not gs_return is initial.
            gs_return-row = ls_catalogue-posnr_p1.
            append gs_return to lt_return.
          endif.

          ls_posnr_hlf-posnr_p1 = ls_catalogue-posnr_p1.
          ls_posnr_hlf-hlf = ls_ref_materials-hlf.
          append ls_posnr_hlf to lt_posnr_hlf.
        endif.
      else.
        read table lt_catalogue_aux into ls_catalogue_aux
        with key property = zswif0001=>lc_ka_nbr_property
        posnr_p1 = ls_catalogue-posnr_p1.
        if sy-subrc eq 0.
          perform derive_ssx_hlf using zif_harnessing_core=>gc_ssx_ka_vtab ls_catalogue_aux-value item-plant ls_ref_materials-hlf.

          if not gs_return is initial.
            gs_return-row = ls_catalogue-posnr_p1.
            append gs_return to lt_return.
          endif.

          ls_posnr_hlf-posnr_p1 = ls_catalogue-posnr_p1.
          ls_posnr_hlf-hlf = ls_ref_materials-hlf.
          append ls_posnr_hlf to lt_posnr_hlf.
        endif.
      endif.
    endif.

    if not ( lv_skip_chk = 'X' and ls_catalogue-property eq zswif0001=>lc_ssx_cable_id_mat_property ). " skip adding cable if item is null since HLF info is missing
      collect  ls_ref_materials into  lt_ref_materials.
    endif.

  endloop.


  gt_return[] = lt_return[].


  if not lt_ref_materials[] is initial.

    call function 'TR_SYS_PARAMS'
      importing
        system_client_role = lv_system_role.



    select *
    from zswta_ssx_dsconv
    into table lt_dsconv
    for all entries in lt_ref_materials
    where zh_ref_matnr = lt_ref_materials-ref_matnr
    and zh_plant = lt_ref_materials-plant.

    sort lt_ref_materials by plant cable ref_matnr.

    loop at lt_ref_materials assigning <ls_ref_materials>.
      read table lt_dsconv into ls_dsconv
      with key zh_ref_matnr = <ls_ref_materials>-ref_matnr
      zh_plant = <ls_ref_materials>-plant
      zh_hlf = <ls_ref_materials>-hlf.
      if sy-subrc eq 0.
        <ls_ref_materials>-matnr = ls_dsconv-zh_bom_component.
      else.
        read table lt_dsconv into ls_dsconv
        with key zh_ref_matnr = <ls_ref_materials>-ref_matnr
        zh_plant = <ls_ref_materials>-plant
        zh_hlf = space.
        if sy-subrc eq 0.
          <ls_ref_materials>-matnr = ls_dsconv-zh_bom_component.
        endif.
      endif.

      if <ls_ref_materials>-matnr is initial.
        case lv_system_role.

            if  <ls_ref_materials>-hlf is initial and  <ls_ref_materials>-cable = 'X'.
              <ls_ref_materials>-hlf = '?' .
            endif.
            if not  <ls_ref_materials>-cable = 'X'.
              <ls_ref_materials>-hlf = ' ' .
            endif.


          when  'P' or 'T'.
            " Missing entry for ref material &1 plant &2 in view zswtv_ssx_dsconv
            perform add_messages_to_final_return  using 'E' c_message_class '055' <ls_ref_materials>-ref_matnr  ls_ref_materials-plant  <ls_ref_materials>-hlf '' 'bapi_process_ssx_components' '' '' ''.
          when others.
            " Missing entry for ref material &1 plant &2 in view zswtv_ssx_dsconv
            perform add_messages_to_final_return  using 'W' c_message_class '055' <ls_ref_materials>-ref_matnr  ls_ref_materials-plant  <ls_ref_materials>-hlf  '' 'bapi_process_ssx_components' '' '' ''.
        endcase.
      else.
        call function 'CONVERSION_EXIT_MATN1_OUTPUT'
          exporting
            input        = <ls_ref_materials>-matnr
          importing
            output       = lv_matnr
          exceptions
            length_error = 1
            others       = 2.
        if sy-subrc eq 0.
          " SAP material &1 found for ref. customer material &2 Plant &3 HLF = &4
          perform add_messages_to_final_return  using 'I' c_message_class '070' lv_matnr <ls_ref_materials>-ref_matnr  ls_ref_materials-plant  <ls_ref_materials>-hlf  'bapi_process_ssx_components' '' '' ''.
        else.
          perform add_messages_to_final_return  using 'I' c_message_class '070' <ls_ref_materials>-matnr <ls_ref_materials>-ref_matnr  ls_ref_materials-plant  <ls_ref_materials>-hlf  'bapi_process_ssx_components' '' '' ''.
        endif.

      endif.
    endloop.

* Delete previous entries from ZSWTA_SSX_BOM
    delete from zswta_ssx_bom where zh_jobnr eq is_job-job_nr.



* Append new entries to ZSWTA_SSX_BOM
    loop at lt_components assigning <ls_components>.
      read table lt_ref_materials into ls_ref_materials
      with key ref_matnr = <ls_components>-ref_matnr.
      if sy-subrc eq 0.
        ls_ssx_bom-client = sy-mandt.
        ls_ssx_bom-zh_jobnr = <ls_components>-job_nr.
        ls_ssx_bom-zh_plant = <ls_components>-plant.
        ls_ssx_bom-zh_posnr = <ls_components>-posnr.
        ls_ssx_bom-zh_bom_component = ls_ref_materials-matnr.
        ls_ssx_bom-zh_bom_component_qty = <ls_components>-ssx_sum.
        if not ls_ref_materials-matnr is initial.
          <ls_components>-sap_matnr = ls_ref_materials-matnr.
          collect ls_ssx_bom into lt_ssx_bom.
        endif.
      endif.
    endloop.

    if not lt_ssx_bom[] is initial.
      modify zswta_ssx_bom from table lt_ssx_bom.
    endif.

  endif.


  et_components[] = lt_components[].

endform.                    " COLLECT_SSX
*&---------------------------------------------------------------------*
*&      Form  DERIVE_SSX_HLF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VTNAM  text
*      -->P_KA_NBR  text
*      -->P_HLF  text
*----------------------------------------------------------------------*
form derive_ssx_hlf  using    p_vtnam
      p_ka_nbr
      p_plant
      p_hlf.


  data: ls_char_md   type ref to cugen_cstic_master_s,
        lv_atnam     type atnam,
        ls_vt_header type cuvtab,
        lt_query     type standard table of cusl_01,
        lt_match     type standard table of cusl_02,
        ls_query     type cusl_01,
        ls_match     type cusl_02.

  call function 'CUTQ_SELECT_CUVTAB_BY_NAME'
    exporting
      table_name       = p_vtnam
      bypassing_buffer = 'X'
    importing
      table_header     = ls_vt_header
    exceptions
      not_found        = 1
      others           = 2.
  if sy-subrc eq 0.

    lv_atnam = zif_harnessing_core=>gc_ka_nbr_char_name.
    try.
        ls_char_md = cl_cugen_classdata_service=>get_characteristic( lv_atnam ).
      catch cx_cugen_error .
    endtry.
    ls_query-atinn = ls_char_md->atinn.
    ls_query-atwrt = p_ka_nbr.
    ls_query-atfor = ls_char_md->atfor.
    ls_query-atcio = 'I'.
    append ls_query to lt_query.

    lv_atnam = zif_harnessing_core=>gc_plant_char_name.
    try.
        ls_char_md = cl_cugen_classdata_service=>get_characteristic( lv_atnam ).
      catch cx_cugen_error .
    endtry.
    ls_query-atinn = ls_char_md->atinn.
    ls_query-atwrt = p_plant.
    ls_query-atfor = ls_char_md->atfor.
    ls_query-atcio = 'I'.
    append ls_query to lt_query.

    lv_atnam = zif_harnessing_core=>gc_hlf_char_name.
    try.
        ls_char_md = cl_cugen_classdata_service=>get_characteristic( lv_atnam ).
      catch cx_cugen_error .
    endtry.
    ls_query-atinn = ls_char_md->atinn.
    ls_query-atwrt = space.
    ls_query-atfor = space.
    ls_query-atcio = 'O'.
    append ls_query to lt_query.


    call function 'CUTS_TABLE_SELECT_SINGLE'
      exporting
        table                = ls_vt_header-vtint
        date                 = sy-datum
      tables
        query                = lt_query
        match                = lt_match
      exceptions
        not_found            = 1
        no_characteristics   = 2
        no_query             = 3
        no_entry_found       = 4
        mult_entries_found   = 5
        internal_error       = 6
        derivation_undefined = 7
        others               = 8.
    if sy-subrc eq 0 and not lt_match[] is initial.
      read table lt_match into ls_match index 1.
      p_hlf = ls_match-atwrt.
    else.
      perform add_messages_to_final_return using 'E' c_message_class '068' 'KA NBR ' p_ka_nbr p_plant  p_vtnam  '' '' '' ''. " &1 value &2 plant &3 not found in variant table &2
    endif.

  endif.

endform.                    " DERIVE_SSX_HLF
form bapi_ssx_components_validation tables lt_components type ty_ssx_components_t using is_job type zswtajobs.
  data:
    lv_system_role   type cccategory,
    ls_components    type ty_ssx_components,
    lv_key_date      type sttag,
    ls_mbm_sel       type cpsc_mbm_sel_type,
    ls_bom_sel       type cpsc_bom_sel_type,
    lt_bom_ident     type cscl_bom_id_tb_type,
    ls_bom_ident     type cscl_bom_id_type,
    ls_cpsc_matnr    type cpsc_matnr_type,
    ls_cpsc_werks    type cpsc_werks_type,
    ls_cpsc_stlan    type cpsc_stlan_type,
    ls_cpsc_stlty    type cpsc_stlty_type,
    workarea_classes like classes_in_workarea,
    lt_bom_items     type bom_item_tab,
    lv_bomkey        type char14.

  call function 'TR_SYS_PARAMS'
    importing
      system_client_role = lv_system_role.

  if  header-req_date_h lt sy-datum or header-req_date_h is initial.
    lv_key_date = sy-datum.
  else.
    lv_key_date = header-req_date_h.
  endif.


* set valid object types
  workarea_classes-obm_inarea = 'X'.
  workarea_classes-bom_inarea = 'X'.
  workarea_classes-itm_inarea = 'X'.


  loop at item.

    read table lt_components transporting no fields
    with key posnr = item-itm_number
    plant = item-plant.
    if sy-subrc eq 0.

      clear ls_mbm_sel-matnr[].
      ls_cpsc_matnr-sign = 'I'.
      ls_cpsc_matnr-option = 'EQ'.
      ls_cpsc_matnr-low = item-material.
      ls_cpsc_matnr-high = ls_cpsc_matnr-low.
      append ls_cpsc_matnr to ls_mbm_sel-matnr.

      clear ls_mbm_sel-werks[].
      ls_cpsc_werks-sign = 'I'.
      ls_cpsc_werks-option = 'EQ'.
      ls_cpsc_werks-low = item-plant.
      ls_cpsc_werks-high = ls_cpsc_werks-low.
      append ls_cpsc_werks to ls_mbm_sel-werks.

      clear ls_bom_sel-stlty[].
      ls_cpsc_stlty-sign = 'I'.
      ls_cpsc_stlty-option = 'EQ'.
      ls_cpsc_stlty-low = 'M'.
      ls_cpsc_stlty-high = ls_cpsc_stlty-low.
      append ls_cpsc_stlty to ls_bom_sel-stlty.

      clear ls_bom_sel-stlan[].
      ls_cpsc_stlan-sign = 'I'.
      ls_cpsc_stlan-option = 'EQ'.
      ls_cpsc_stlan-low = '1'.
      ls_cpsc_stlan-high = ls_cpsc_stlan-low .
      append ls_cpsc_stlan to ls_bom_sel-stlan.


      call function 'CP_CC_S_LOAD_COMPLEX_BY_BOM'
        exporting
          i_class                        = 'P'
          i_classes_in_workarea          = workarea_classes
          i_cpsc_mbm_sel                 = ls_mbm_sel
          i_cpsc_bom_sel                 = ls_bom_sel
          i_date_from                    = lv_key_date
          i_date_to                      = '99991231'
          i_message_handler              = 'A'
          i_save_protocoll               = ' '
        importing
          e_bom_ident                    = lt_bom_ident
        exceptions
          workarea_not_found             = 1
          class_wrong_type               = 2
          workarea_wrong_type            = 2
          class_in_workarea_inconsistent = 4
          workarea_not_specified         = 5
          bom_not_found                  = 6
          no_selection_criteria          = 7
          invalid_selection_period       = 8
          key_date_required_for_ecm      = 9.
      if sy-subrc eq 0 and not lt_bom_ident[] is initial.



        read table lt_bom_ident  into ls_bom_ident index 1.

        concatenate ls_bom_ident-stlty ls_bom_ident-stlnr ls_bom_ident-stlal into lv_bomkey.
        condense lv_bomkey.

        call function 'CABM_READ_BOM_ITEM'
          exporting
            i_stlty         = ls_bom_ident-stlty
            i_stlnr         = ls_bom_ident-stlnr
            i_stlal         = ls_bom_ident-stlal
            i_date_from     = lv_key_date
          tables
            exp_bom_item    = lt_bom_items
          exceptions
            no_record_found = 1
            others          = 2.
        if sy-subrc eq 0 and not lt_bom_items[] is initial.

          loop at lt_components into ls_components
          where posnr eq item-itm_number
          and job_id eq  is_job-job_id
          and not sap_matnr is initial.

            read table lt_bom_items transporting no fields
            with key idnrk = ls_components-sap_matnr.
            if sy-subrc ne 0.

              call function 'CONVERSION_EXIT_MATN1_OUTPUT'
                exporting
                  input        = ls_components-sap_matnr
                importing
                  output       = ls_components-sap_matnr
                exceptions
                  length_error = 1
                  others       = 2.
              if sy-subrc ne 0.
              endif.

              case lv_system_role.
                when  'P' or 'T'.
                  " SAP Material &1 ( Ref mat. &2 ) plant &3 not in SSX BOM ID &4
                  perform add_messages_to_final_return  using 'E' c_message_class '069' ls_components-sap_matnr ls_components-ref_matnr ls_components-plant lv_bomkey  'bapi_process_ssx_components' '' '' ''.
                when others.
                  " SAP Material &1 ( Ref mat. &2 ) plant &3 not in SSX BOM ID &4
                  perform add_messages_to_final_return  using 'W' c_message_class '069' ls_components-sap_matnr ls_components-ref_matnr  ls_components-plant  lv_bomkey   'bapi_process_ssx_components' '' '' ''.
              endcase.
            endif.

          endloop.

        endif.
      endif.

    endif.
  endloop.

  clear gt_components[].

endform.

form bapi_header_data_validation tables lt_extensionin structure bapiparex using header type bapisdhd1.

  data: wa_extensionin type bapiparex,
        wa_bape_vbak   type bape_vbak,
        l_count        type i.

  read table lt_extensionin into wa_extensionin index 1.
  if sy-subrc eq 0.
    wa_bape_vbak  = wa_extensionin+30(960)  ##ENH_OK.

    if not wa_bape_vbak-zz_ordertype is initial.
      select count(*) into l_count from cawn as a
      inner join cabn as b
      on a~atinn eq b~atinn
      where b~atnam eq zif_harnessing_core=>gc_ordertype_char_name
      and a~atwrt eq wa_bape_vbak-zz_ordertype.

      if  l_count is initial.
        perform add_messages_to_final_return  using 'E' c_message_class '059' wa_bape_vbak-zz_ordertype '' '' '' 'bapi_header_data_validation' '' '' ''.
      endif.
    endif.
  endif.


endform.
form bapi_item_data_validation tables item structure bapisditm using header type bapisdhd1 iv_vbeln type vbeln.

  " Validations ON the bapi structures before BAPPI CALL
  " DONE  -> CHECK IF material/plant IS NOT blocked
  " DONE  -> CHECK IF material IS assembly TO ORDER has BOM AND Routing
  " DONE  ->  Check for each item if if HG_LV_ITEM item exist
  " TODO  ->  Check if vbak extension structure has values for all relevant characteristics that have valuation in the order classification


  data: begin of schedtype,
          fein like rcpse-sched_type value '1',
          rate like rcpse-sched_type value '2',
          grob like rcpse-sched_type value '3',
        end of schedtype.

*  DATA: BEGIN OF tvko_sadr.
*          INCLUDE STRUCTURE sadr.                             "SADR40A
*  DATA: END OF tvko_sadr.

  constants:
    lc_bom_usage_prod type stlan value '1',
* Name of SFC materialview
    con_sfc_matview   type mtcom-kenng value 'MSFCV',
    yx                type c value 'X'.  "Kennzeichen X


  types: begin of ty_mat_plant_val,
           matnr        type matnr,
           plant        type werks_d,
           pstyv        type pstyv,
           lvoma        type lvoma,
           lvowk        type lvowk,
           lvovk        type lvovk,
           mstae        type mstae,
           mstde        type mstde,
           mmsta        type mmsta,
           mmstd        type  mmstd,
           mstav        type mstav,
           mstdv        type mstdv,
           vmsta        type vmsta,
           vmstd        type vmstd,
           mntga        type mntga,
           scprf_chk    type flag,
           has_bom      type flag,
           has_routing  type flag,
           req_conf_prf type flag,
           has_conf_prf type flag,
           return       type bapiret2_tab,
         end of ty_mat_plant_val,
         ty_mat_plant_val_t type standard table of ty_mat_plant_val.

  data:
    ls_mat_plant_val type  ty_mat_plant_val,
    lt_mat_plant_val type  ty_mat_plant_val_t,
    lt_return_val    type bapiret2_tab.

  data:
    lt_vbadr             type standard table of sadrvb,
    ls_vbadr             type sadrvb,
    lt_vbpa              type standard table of vbpavb,
    ls_item              type bapisditm,
    ls_return            type bapiret2,
    ls_mtcom             type mtcom,
    ls_mtcor             type mtcor,
    lv_kund_spras        like sy-langu,
    lv_bedae             type bedae,
    ls_t459k             type t459k,
    ls_t399x             type t399x,
    ls_t003o             type t003o,
    rcpse_ss             type rcpse,
    rcpsl_ss             type rcpsl,
    lv_matnr             type matnr,
    ls_mara              type mara,
    ls_mt61d             type mt61d,
    ls_msfcv             type msfcv,
    ls_maapv             type maapv,
    ls_maapv_h           type maapv,
    lv_mmsta             type mmsta,
    lv_vmsta             type vmsta,
    lv_mmstd             type mmstd,
    lv_mstdv             type vmstd,
    ls_t141wa            type t141,
    ls_wtvms             type tvms,
    lv_pstyv             type pstyv,
    lv_uepst_h           type uepst,
    lv_key_date          type sttag,
    lv_bom_exist         type boolean,
    lv_routing_exist     type boolean,
    lv_plnnr             type plnnr,
    lv_objnum            type objnum,
    lv_scprf_chk         type flag,
    lv_cu_profile_exists type boolean.


  data: ls_tco01 type  tco01.
  data: production_version like plaf-verid.
  data: flg_hell   like rc27x-flg_sel.
  data: flg_au     like rcpse-flg_sel_au.
  data: flg_seldia like rcpse-flg_seldia.
  data: posgr_tmp  like t185-posgr.
  data: sched_type like rcpse-sched_type.
  data: tab_size   like sy-tabix.
  data: l_flg_no_dialog type flag.

  field-symbols:
  <ls_item> type bapisditm.

* Considering that we don't have different dates on schedule line items we can just use the header request date to derive the check date
  if  header-req_date_h lt sy-datum.
    lv_key_date = sy-datum.
  else.
    lv_key_date = header-req_date_h.
  endif.


* Sprache des Auftraggebers nehmen.
* Wenn diese nicht da ist, dann Sprache der Verkaufsorganisation.

  if not iv_vbeln is initial.
    call function 'SD_PARTNER_READ'
      exporting
        f_vbeln  = iv_vbeln
      tables
        i_xvbadr = lt_vbadr
        i_xvbpa  = lt_vbpa.
    read table lt_vbadr into ls_vbadr index 1.
    lv_kund_spras = ls_vbadr-spras.
  else.

    read table partner with key partn_role = 'AG'.
    if not partner-partn_numb is initial.
      select single spras from kna1 into lv_kund_spras where kunnr eq partner-partn_numb.
    endif.
  endif.


*    IF lv_kund_spras = space.
*      lv_kund_spras = tvko_sadr-spras.
*    ENDIF.
  if lv_kund_spras = space.
    lv_kund_spras = sy-langu.
  endif.


  loop at item assigning <ls_item>.

    clear ls_mat_plant_val.
    read table lt_mat_plant_val into ls_mat_plant_val
        with key matnr = <ls_item>-material
                 plant = <ls_item>-plant.
    if sy-subrc ne 0.

      ls_mat_plant_val-matnr =  <ls_item>-material.
      ls_mat_plant_val-plant = <ls_item>-plant.

      clear: lt_return_val[], ls_item, ls_mtcom, ls_mara, ls_mt61d, ls_maapv, ls_maapv_h, lv_uepst_h, lv_pstyv,
      lv_bom_exist, lv_routing_exist, lv_matnr ,rcpse_ss, rcpsl_ss, lv_mmsta, lv_mmstd, lv_vmsta, lv_mstdv, ls_mtcor, lv_scprf_chk.


      call function 'CONVERSION_EXIT_MATN1_OUTPUT'
        exporting
          input  = <ls_item>-material
        importing
          output = lv_matnr.



      move 'MARA' to ls_mtcom-kenng.
      move <ls_item>-material to ls_mtcom-matnr.


      call function 'MATERIAL_READ'
        exporting
          schluessel           = ls_mtcom
        importing
          matdaten             = ls_mara
        tables
          seqmat01             = g_dummy_tab
        exceptions
          account_not_found    = 01
          batch_not_found      = 02
          forecast_not_found   = 03
          lock_on_account      = 04
          lock_on_material     = 05
          lock_on_plant        = 06
          lock_on_sales        = 07
          lock_on_sloc         = 08
          lock_system_error    = 09
          material_not_found   = 10
          plant_not_found      = 11
          sales_not_found      = 12
          sloc_not_found       = 13
          slocnumber_not_found = 14
          sloctype_not_found   = 15
          text_not_found       = 16
          unit_not_found       = 17.
      if sy-subrc eq 0 and not ls_mara is initial.


        if not ls_mara-lvorm is initial.

          perform add_messages_to_final_return
             using 'E' 'V1' '406' lv_matnr '' '' '' 'bapi_item_data_validation' <ls_item>-itm_number '' ''.

          append gs_return to lt_return_val.

        else.
          move 'MT61D' to ls_mtcom-kenng.
          move <ls_item>-material to ls_mtcom-matnr.
          move <ls_item>-plant  to ls_mtcom-werks.

          call function 'MATERIAL_READ'
            exporting
              schluessel           = ls_mtcom
            importing
              matdaten             = ls_mt61d
            tables
              seqmat01             = g_dummy_tab
            exceptions
              account_not_found    = 01
              batch_not_found      = 02
              forecast_not_found   = 03
              lock_on_account      = 04
              lock_on_material     = 05
              lock_on_plant        = 06
              lock_on_sales        = 07
              lock_on_sloc         = 08
              lock_system_error    = 09
              material_not_found   = 10
              plant_not_found      = 11
              sales_not_found      = 12
              sloc_not_found       = 13
              slocnumber_not_found = 14
              sloctype_not_found   = 15
              text_not_found       = 16
              unit_not_found       = 17.
          if sy-subrc eq 0 and not ls_mt61d is initial.


            if not ls_mt61d-lvorm is initial.
              perform add_messages_to_final_return
                using 'E' 'CO' '732' lv_matnr '' '' '' 'bapi_item_data_validation' <ls_item>-itm_number '' ''.
              append gs_return to lt_return_val.
            else.

              move 'MAAPV' to ls_mtcom-kenng.
              move header-sales_org to ls_mtcom-vkorg.
              move header-distr_chan to ls_mtcom-vtweg.
              ls_mtcom-spras = lv_kund_spras.


              call function 'MATERIAL_READ'
                exporting
                  schluessel           = ls_mtcom
                importing
                  matdaten             = ls_maapv
                  return               = ls_mtcor
                tables
                  seqmat01             = g_dummy_tab
                exceptions
                  account_not_found    = 01
                  batch_not_found      = 02
                  forecast_not_found   = 03
                  lock_on_account      = 04
                  lock_on_material     = 05
                  lock_on_plant        = 06
                  lock_on_sales        = 07
                  lock_on_sloc         = 08
                  lock_system_error    = 09
                  material_not_found   = 10
                  plant_not_found      = 11
                  sales_not_found      = 12
                  sloc_not_found       = 13
                  slocnumber_not_found = 14
                  sloctype_not_found   = 15
                  text_not_found       = 16
                  unit_not_found       = 17.
              if sy-subrc eq 0.
                if ls_maapv is initial.
                  ls_maapv-mtpos = ls_mara-mtpos_mara.
                endif.

                if not ls_mtcor-lvorm is initial.
                  perform add_messages_to_final_return
                   using 'E' 'V1' '406' lv_matnr '' '' '' 'bapi_item_data_validation' <ls_item>-itm_number '' ''.
                  append gs_return to lt_return_val.

                else.
* TODO need to determine schedule line before as the logic might depend on the higher level item
* RV_MATERIAL_STATUS_CHECK * PERFORM VBAP_FUELLEN(SAPFV45P).

                  if not <ls_item>-hg_lv_item is initial.


                    read table item into ls_item
                    with key itm_number = <ls_item>-hg_lv_item.
                    if sy-subrc eq 0.
                      if ls_item-item_categ is initial.

                        move 'MAAPV' to ls_mtcom-kenng.
                        move header-sales_org to ls_mtcom-vkorg.
                        move header-distr_chan to ls_mtcom-vtweg.
                        move <ls_item>-material to ls_mtcom-matnr.
                        move <ls_item>-plant  to ls_mtcom-werks.
                        ls_mtcom-spras = 'D'. " change to customer langu


                        call function 'MATERIAL_READ'
                          exporting
                            schluessel           = ls_mtcom
                          importing
                            matdaten             = ls_maapv_h
                          tables
                            seqmat01             = g_dummy_tab
                          exceptions
                            account_not_found    = 01
                            batch_not_found      = 02
                            forecast_not_found   = 03
                            lock_on_account      = 04
                            lock_on_material     = 05
                            lock_on_plant        = 06
                            lock_on_sales        = 07
                            lock_on_sloc         = 08
                            lock_system_error    = 09
                            material_not_found   = 10
                            plant_not_found      = 11
                            sales_not_found      = 12
                            sloc_not_found       = 13
                            slocnumber_not_found = 14
                            sloctype_not_found   = 15
                            text_not_found       = 16
                            unit_not_found       = 17.

                        if sy-subrc eq 0 and not ls_maapv_h is initial.
                          call function 'RV_VBAP_PSTYV_DETERMINE'
                            exporting
                              t184_auart   = header-doc_type
                              t184_mtpos   = ls_maapv_h-mtpos
                              t184_uepst   = space
                              t184_vwpos   = space
                              vbap_pstyv_i = space
                            importing
                              vbap_pstyv   = lv_uepst_h
                            exceptions
                              others       = 1 ##FM_SUBRC_OK.
                        endif.

                      else.
                        lv_uepst_h = ls_item-item_categ.
                      endif.
                    else.
                      perform add_messages_to_final_return
                       using 'E' 'KJ' '143' <ls_item>-hg_lv_item <ls_item>-itm_number '' '' 'bapi_item_data_validation' <ls_item>-itm_number '' ''.
                      append gs_return to lt_return_val.
                    endif.
                  endif.

                  call function 'RV_VBAP_PSTYV_DETERMINE'
                    exporting
                      t184_auart   = header-doc_type
                      t184_mtpos   = ls_maapv-mtpos
                      t184_uepst   = lv_uepst_h
                      t184_vwpos   = space
                      vbap_pstyv_i = space
                    importing
                      vbap_pstyv   = lv_pstyv
                    exceptions
                      others       = 1.
                  if sy-subrc eq 0 and not lv_pstyv is initial.

                    <ls_item>-item_categ = lv_pstyv.


                    call function 'RV_SCHEDULING_TYPE_DETERMINE'
                      exporting
                        f_werks = ls_mt61d-werks
                        f_mtart = ls_mt61d-mtart
                        f_disgr = ls_mt61d-disgr
                        f_strgr = ls_mt61d-strgr
                        f_dismm = ls_mt61d-dismm
                        f_eterl = 'X'
                        f_pstyv = lv_pstyv
                        f_vgtyp = space
                      importing
                        e_bedae = lv_bedae.

                    if not lv_bedae is initial.

                      select single * from t459k into ls_t459k where bedar = lv_bedae.

                      if ls_t459k-mntga ne 0 and not ls_t459k-auart is initial.

                        " * Check if material has work scheduling, Production Scheduling Profile maintained.
                        if ls_mat_plant_val-scprf_chk is initial
                          and ( ls_mara-mtart eq 'ZFEH' or ls_mara-mtart eq 'ZHAL' ). " TODO replace with constant
                          move con_sfc_matview to ls_mtcom-kenng.
                          move <ls_item>-material to ls_mtcom-matnr.
                          move <ls_item>-plant  to ls_mtcom-werks.
                          ls_mtcom-spras = sy-langu.

                          call function 'MATERIAL_READ'
                            exporting
                              schluessel           = ls_mtcom
                            importing
                              matdaten             = ls_msfcv
                            tables
                              seqmat01             = g_dummy_tab
                            exceptions
                              account_not_found    = 01
                              batch_not_found      = 02
                              forecast_not_found   = 03
                              lock_on_account      = 04
                              lock_on_material     = 05
                              lock_on_plant        = 06
                              lock_on_sales        = 07
                              lock_on_sloc         = 08
                              lock_system_error    = 09
                              material_not_found   = 10
                              plant_not_found      = 11
                              sales_not_found      = 12
                              sloc_not_found       = 13
                              slocnumber_not_found = 14
                              sloctype_not_found   = 15
                              text_not_found       = 16
                              unit_not_found       = 17.
                          if  sy-subrc eq 0 and  ls_msfcv-sfcpf is initial and ls_msfcv-beskz = 'E'.
                            perform add_messages_to_final_return
                            using 'E' 'ZSWEC0002' '065' lv_matnr <ls_item>-plant '' '' 'bapi_item_data_validation' <ls_item>-itm_number '' ''. " Production Scheduling Profile missing for material &1 plant &2
                            append gs_return to lt_return_val.
                            lv_scprf_chk  = '0'.
                          elseif  sy-subrc eq 0 and not ls_msfcv-sfcpf is initial and ls_msfcv-beskz = 'E'.
                            lv_scprf_chk  = '1'.
                          endif.
                        endif.
                        " * Check material has BOM and Routing

                        call function 'BAPI_MAT_BOM_EXISTENCE_CHECK'
                          exporting
                            material        = <ls_item>-material
                            plant           = <ls_item>-plant
                            bomusage        = lc_bom_usage_prod
                            valid_from_date = lv_key_date
                          tables
                            return          = lt_return.
                        if lt_return[] is initial.
                          lv_bom_exist = 'X'.
                        else.
                          loop at lt_return into ls_return.
                            if ls_return-id = '29' and ls_return-number = '001'.
                              ls_return-type = 'E'.
                              ls_return-number = '012'.
                              ls_return-message_v1 = lv_matnr.

                              concatenate '/' <ls_item>-plant  '/' lc_bom_usage_prod into ls_return-message_v2.

                            endif.
                            ls_return-parameter = 'bapi_item_data_validation'.
                            ls_return-row = <ls_item>-itm_number.
                            append ls_return to gt_return.
                            append ls_return to lt_return_val.
                          endloop.
                        endif.

* Derive Routing based on order type settings based on PERFORM C_DIA_PLAN_SELEC(LCOSDF1S).

                        if production_version is initial.
                          if flg_hell = space.
                            flg_au = yx.
                          else.
                            flg_au = space.
                          endif.
* bei Fertigungsversion automatisch
                        else.
                          flg_hell = space.
                          flg_au   = yx.
*   Bei Teilumsetzung mit Fertigungsversion muß unabhängig von der
*   Losgröße die Fertigungsversion des Planauftrags genommen werden
*                  IF NOT afpod-tpauf IS INITIAL.
*                    CLEAR amount.
*                  ENDIF.
                        endif.

                        if ls_t003o-auart ne ls_t459k-auart.
                          call function 'READ_T003O'
                            exporting
                              i_auart   = ls_t459k-auart
                            importing
                              e_t003o   = ls_t003o
                            exceptions
                              not_found = 1
                              others    = 2.
                        endif.

                        if sy-subrc eq 0.
                          if not ls_tco01-autyp ne ls_t003o-autyp.

                            call function 'CO_TA_TCO01_READ'
                              exporting
                                autyp    = ls_t003o-autyp
                              importing
                                struct   = ls_tco01
                              exceptions
                                no_entry = 01.
                          endif.

                          if sy-subrc eq 0.

*                    IF header-typkz IS INITIAL AND NOT production_version IS INITIAL.
*                      sched_type = schedtype-fein.
*                    ELSE.
*                      sched_type = header-typkz.
*                    ENDIF.

*   no popup if automatic routing selection in background
                            if l_flg_no_dialog is initial or
                            flg_au is initial.
                              flg_seldia = yx.
                            endif.

                            if ls_t399x-werks ne <ls_item>-plant or ls_t399x-auart ne ls_t459k-auart.
                              call function 'CO_TA_T399X_READ'
                                exporting
                                  auart    = ls_t459k-auart
                                  werks    = <ls_item>-plant
                                importing
                                  struct   = ls_t399x
                                exceptions
                                  no_entry = 01.
                            endif.

                            if sy-subrc eq 0.
                              clear rcpse_ss.
                              move:
                            ls_t399x-cpslid          to rcpse_ss-sel_id,
                            ls_tco01-plnaw           to rcpse_ss-plnaw,
                            ls_t399x-alt_planw       to rcpse_ss-plnaw2,
                            'ORD'                    to rcpse_ss-posgr,
                            iv_vbeln                 to rcpse_ss-vbeln,
                            lv_key_date              to rcpse_ss-sttag,
                            <ls_item>-itm_number     to rcpse_ss-posnr,
                            <ls_item>-material       to rcpse_ss-matnr,
                         <ls_item>-plant          to rcpse_ss-werks_mat,
*                     production_version       TO rcpse_ss-verid,
                         <ls_item>-target_qty     to rcpse_ss-losgr,
                         <ls_item>-target_qu      to rcpse_ss-plnme,
*                     i_pspnr                  TO rcpse_ss-pspnr,
                        space                    to rcpse_ss-sched_type,
                         '1'                     to rcpse_ss-object,
                        flg_hell                 to rcpse_ss-flg_dialog,
                        flg_au                   to rcpse_ss-flg_sel_au.


                              move-corresponding rcpse_ss to rcpsl_ss ##ENH_OK.
                              move flg_seldia to rcpsl_ss-flg_same.

                              rcpsl_ss-panel = '1000'.
                              case ls_t003o-autyp.
                                when auftragstyp-bord.
                                  move '5' to rcpsl_ss-panel+1(1).
                                when auftragstyp-netw.
                                  move '3' to rcpsl_ss-panel+1(1).
                                when others.
                                  move '2' to rcpsl_ss-panel+1(1).
                              endcase.
                              rcpsl_ss-flg_vbeln = 'X'.
                              clear lv_plnnr.
                              call function 'CP_SL_ROUTING_SELECTION'
                                exporting
                                  rcpsl             = rcpsl_ss
                                importing
                                  plnnr_exp         = lv_plnnr
                                  flg_routing_exist = lv_routing_exist.
                              if lv_routing_exist is initial or lv_plnnr is initial.
                                clear lv_routing_exist.
                                perform add_messages_to_final_return
                                 using 'E' 'KZ' '100' lv_matnr '' '' '' 'bapi_item_data_validation' <ls_item>-itm_number '' ''.
                                append gs_return to lt_return_val.
                              endif.
                            endif.
                          endif.
                        endif.
                      endif.

                      " Check if item requires configuration profile
                      if ls_t459k-konfi = '+'.


                        if    ls_mara-kzkfg = 'X' or not ls_mt61d-stdpd is initial.

                          ls_mat_plant_val-req_conf_prf = 'X'.

                          if not ls_mt61d-stdpd is initial.
                            move ls_mt61d-stdpd to lv_objnum.
                          else.
                            move <ls_item>-material to lv_objnum.
                          endif.

                          call function 'CUCO_PROFILE_PREPARE'
                            exporting
                              cuco_object_table        = 'MARA'
                              cuco_object_key          = lv_objnum
                              cuco_date                = lv_key_date
                              cuco_display_only        = 'X'
                            importing
                              cuco_profile_exists      = lv_cu_profile_exists
                            exceptions
                              change_number_conflict   = 1
                              change_number_invalid    = 2
                              change_number_required   = 3
                              enqueue_fail             = 4
                              object_authority_missing = 5
                              object_not_configurable  = 6
                              object_not_found         = 7
                              object_table_unknown     = 8
                              object_work_committed    = 9
                              profile_not_found        = 10
                              others                   = 11.
                          if sy-subrc eq 10.
                            perform add_messages_to_final_return  using 'E' 'CU' '701' lv_objnum '' '' '' 'bapi_item_data_validation' <ls_item>-itm_number '' ''.
                            append gs_return to lt_return_val.
                          elseif sy-subrc eq 0 and lv_cu_profile_exists = 'X'.
                            ls_mat_plant_val-has_conf_prf = lv_cu_profile_exists.
                          else.
                            " CODE other errors
                          endif.

                        endif.
                      endif.


                    endif.
                  endif.


* Check material/plant statuses
* MMSTA Plant-Specific Material Status
* MSTAE Cross-Plant Material Status

                  if not ls_mt61d-mstae is initial or not ls_mt61d-mmsta is initial. " Cross plant status

                    if not ls_mt61d-mstae is initial.
                      lv_mmsta =  ls_mt61d-mstae.
                    else.
                      lv_mmsta =  ls_mt61d-mmsta.
                    endif.

                    if ls_mt61d-mstde is initial. ls_mt61d-mstde = '99991231'. endif. "Date from which the cross-plant material status is valid
                    if ls_mt61d-mmstd is initial. ls_mt61d-mmstd = '99991231'. endif. "Date from which the plant-specific material status is valid

                    if ls_mt61d-mstde gt ls_mt61d-mmstd.
                      lv_mmstd = ls_mt61d-mmstd.
                    else.
                      lv_mmstd = ls_mt61d-mstde.
                    endif.


                    if lv_key_date ge lv_mmstd or lv_mmstd eq '99991231'.
                      call function 'CO_TA_T141_READ'
                        exporting
                          t141_mmsta = lv_mmsta
                        importing
                          t141wa     = ls_t141wa
                        exceptions
                          not_found  = 1
                          others     = 2.
                      if sy-subrc eq 0 and  ls_t141wa-dfako eq 'B' .
                        perform add_messages_to_final_return
                           using 'E' 'CO' '050' lv_matnr lv_mmsta lv_mmstd '' 'bapi_item_data_validation' <ls_item>-itm_number '' ''.
                        append gs_return to lt_return_val.
                      endif.
                    endif.
                  endif.


* Check material/plant/dist. channel statuses
*              mstav cross-distribution-chain material status
*              vmsta distribution-chain-specific material status

                  if not ls_maapv-mstav is initial or not ls_maapv-vmsta is initial.

                    if not ls_maapv-mstav is initial.
                      lv_vmsta =  ls_maapv-mstav.
                    else.
                      lv_vmsta =  ls_maapv-vmsta.
                    endif.

                    if ls_maapv-vmstd is initial. ls_maapv-vmstd = '99991231'. endif. " Date from which distr.-chain-spec. material status is valid
                    if ls_maapv-mstdv is initial. ls_maapv-mstdv = '99991231'. endif. " Date from which the X-distr.-chain material status is valid

                    if ls_maapv-vmstd  gt ls_maapv-mstdv.
                      lv_mstdv = ls_maapv-mstdv .
                    else.
                      lv_mstdv = ls_maapv-vmstd.
                    endif.

                    if lv_key_date ge lv_mstdv or lv_mstdv eq '99991231'.
                      call function 'TVMS_SINGLE_READ'
                        exporting
                          tvms_vmsta = lv_vmsta
                        importing
                          wtvms      = ls_wtvms
                        exceptions
                          not_found  = 1
                          others     = 2.
                      if sy-subrc eq 0 and  ls_wtvms-spvbc eq 'B' .
                        perform add_messages_to_final_return
                             using 'E' 'V1' '028' lv_matnr lv_vmsta lv_mstdv '' 'bapi_item_data_validation' <ls_item>-itm_number '' ''.
                        append gs_return to lt_return_val.
                      endif.
                    endif.
                  endif.
                endif.
              endif.
            endif.
          else.
          endif.
        endif.
      endif.


* collect all material relevant validations
      ls_mat_plant_val-lvoma        = ls_mara-lvorm.
      ls_mat_plant_val-lvowk        = ls_mt61d-lvorm.
      ls_mat_plant_val-lvovk        = ls_mtcor-lvorm.

      ls_mat_plant_val-mstae        = ls_mt61d-mstae.
      ls_mat_plant_val-mstde        = ls_mt61d-mstde.

      ls_mat_plant_val-mmsta        = ls_mt61d-mmsta.
      ls_mat_plant_val-mmstd        = ls_mt61d-mmstd.

      ls_mat_plant_val-mstav        = ls_maapv-mstav.
      ls_mat_plant_val-mstdv        = ls_maapv-mstdv.

      ls_mat_plant_val-vmsta        = ls_maapv-vmsta.
      ls_mat_plant_val-vmstd        = ls_maapv-vmstd.

      ls_mat_plant_val-mntga        = ls_t459k-mntga.
      ls_mat_plant_val-has_bom      = lv_bom_exist.
      ls_mat_plant_val-has_routing  = lv_routing_exist.
      ls_mat_plant_val-scprf_chk    = lv_scprf_chk.
      append lines of lt_return_val to ls_mat_plant_val-return.
      append ls_mat_plant_val to lt_mat_plant_val.

    else.
      loop at ls_mat_plant_val-return into ls_return.
        ls_return-row = <ls_item>-itm_number.
        append ls_return to gt_return.
      endloop.

      <ls_item>-item_categ =  lv_pstyv.

    endif.

    " TO DO ->   CHECK if a collective material has at least one subitem assigned

  endloop.

endform.
form file_repository_access_check using value(iv_constand_id) value(iv_context) value(iv_order_type).

  data:
    lv_result                     type abap_bool,
    lv_ignore_server_chk          type abap_bool,
    wa_bape_vbak                  type bape_vbak,
    wa_extensionin                type bapiparex,
    lt_sel_par_values             type zif_harnessing_core=>ty_sel_par_t,
    ls_sel_par_values             type zswtaconstval,
    lv_target_directory_full_path type string,
    lv_target_directory_path2     type string,
    lv_target_directory_path      type string.


  get parameter id gc_par_exclude_file_server_chk field lv_ignore_server_chk.

  if lv_ignore_server_chk is initial.

    if iv_order_type is initial.
      read table lt_extensionin into wa_extensionin with key structure = 'BAPE_VBAK'.
      if sy-subrc eq 0.
        wa_bape_vbak = wa_extensionin+30(960) ##ENH_OK.
        iv_order_type = wa_bape_vbak-zz_ordertype.
      endif.
    endif.

    perform get_constant_values tables lt_sel_par_values using iv_constand_id iv_context iv_order_type  'P' changing lv_target_directory_path .

    clear lv_target_directory_full_path.


    if lv_target_directory_path is initial and not lt_sel_par_values[] is initial.

      loop at lt_sel_par_values into ls_sel_par_values.
        if sy-tabix eq 1.
          lv_target_directory_path    = ls_sel_par_values-low.
        elseif sy-tabix eq 2.
          lv_target_directory_path2  = ls_sel_par_values-low.
        endif.
      endloop.

      concatenate lv_target_directory_path  lv_target_directory_path2   into lv_target_directory_full_path.

    elseif  not lv_target_directory_path is initial.
      move lv_target_directory_path to lv_target_directory_full_path.
    endif.



    if not lv_target_directory_full_path is initial.
* check access to provided path

      clear : lv_result.
      call method cl_gui_frontend_services=>directory_exist
        exporting
          directory            = lv_target_directory_full_path
        receiving
          result               = lv_result
        exceptions
          cntl_error           = 1
          error_no_gui         = 2
          wrong_parameter      = 3
          not_supported_by_gui = 4
          others               = 5.
      if lv_result ne abap_true.
        perform add_messages_to_final_return using 'E' c_message_class '023'  lv_target_directory_path   '' '' '' 'file_repository_access_check  ' '' '' ''. " Path &1 does not exist. Please ensure the path or mapping is created
      endif.
    endif.
  endif.
endform.

*&---------------------------------------------------------------------*
*&      Form  SALES_ORDER_STATUS_HANDLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sales_order_status_handle using is_vbuk type ty_sales_header_info icsdto type zswde0061.

  data: ls_monitor_header type ty_sales_order_stat.
  field-symbols <gs_monitor_header>.

  move-corresponding  is_vbuk to ls_monitor_header.

  assign ls_monitor_header to <gs_monitor_header>.

* Domain STATV: A = Not yet processed, B = Partially processed, C = Completely processed



* uvall = general incompletion status
* uvvlk = Header incompletion status concerning delivery
* uvfak = Header incompletion status with respect to billing
* uvprs = Document is incomplete with respect to pricing
* uvals = Total incompletion status of all items in general
* uvvls = Total incompletion status of all items: Delivery
* uvfas = Total incompletion status of all items: Billing
* UVALL = ICHIG
* UVVLK = ICHDL
* UVFAK = ICHBI
* UVPRS = ICHPR
* all 4 values = ICHTO
* UVALS = ICSIG
* UVVLS = ICSDL
* UVFAS = ICSBI
* all 3-4 values = ICSTO

* TODO create type Ty_sales_order_stat and <gs_monitor_header> type Ty_sales_order_stat





  field-symbols:
    <head_uvall>  type any,
    <head_uvvlk>  type any,
    <head_uvfak>  type any,
    <head_uvprs>  type any,
    <head_detail> type any,
    <head_uvals>  type any,
    <head_uvvls>  type any,
    <head_uvfas>  type any,
    <head_ichto>  type any,
    <head_ichdl>  type any,
    <head_ichig>  type any,
    <head_ichbi>  type any,
    <head_icsto>  type any,
    <head_icspr>  type any,
    <head_icsdl>  type any,
    <head_icsig>  type any,
    <head_icsbi>  type any,
    <head_icsdto> type any.





  assign component 'UVALL' of structure <gs_monitor_header> to <head_uvall>.
  assign component 'UVVLK' of structure <gs_monitor_header> to <head_uvvlk>.
  assign component 'UVFAK' of structure <gs_monitor_header> to <head_uvfak>.
  assign component 'UVPRS' of structure <gs_monitor_header> to <head_uvprs>.
  assign component 'ITEMS' of structure <gs_monitor_header> to <head_detail>.
  assign component 'UVALS' of structure <gs_monitor_header> to <head_uvals>.
  assign component 'UVVLS' of structure <gs_monitor_header> to <head_uvvls>.
  assign component 'UVFAS' of structure <gs_monitor_header> to <head_uvfas>.
*  ASSIGN COMPONENT 'LFSTK' OF STRUCTURE <gs_monitor_header> TO <head_lfstk>.

  assign component 'ICHTO' of structure <gs_monitor_header> to <head_ichto>.
  assign component 'ICHDL' of structure <gs_monitor_header> to <head_ichdl>.
  assign component 'ICHIG' of structure <gs_monitor_header> to <head_ichig>.
  assign component 'ICHBI' of structure <gs_monitor_header> to <head_ichbi>.
  assign component 'ICSTO' of structure <gs_monitor_header> to <head_icsto>.
  assign component 'ICSPR' of structure <gs_monitor_header> to <head_icspr>.
  assign component 'ICSDL' of structure <gs_monitor_header> to <head_icsdl>.
  assign component 'ICSIG' of structure <gs_monitor_header> to <head_icsig>.
  assign component 'ICSDTO' of structure <gs_monitor_header> to <head_icsdto>.
  assign component 'ICSBI' of structure <gs_monitor_header> to <head_icsbi>.


  if gv_led_green is initial or gv_led_red is initial.
    perform create_led_icons.
  endif.



*     ICHTO: status Total incomplete
  if <head_uvall> = 'C' and
  <head_uvvlk> = 'C' and
  <head_uvfak> = 'C'.
    <head_ichto> = gv_led_green.

  elseif <head_uvall> is not initial and
    <head_uvvlk> is not initial and
    <head_uvfak> is not initial.
    <head_ichto> = gv_led_red.
  endif.


*     ICHDL: delivery status
*  IF <head_uvvlk> = 'C'.
*    <head_ichdl> = gv_led_green.
*  ELSEIF NOT <head_uvvlk> IS INITIAL.
*    <head_ichdl> = gv_led_red.
*  ENDIF.

*     ICHIG: status In General
*  IF <head_uvall> = 'C'.
*    <head_ichig> = gv_led_green.
*  ELSEIF NOT <head_uvall> IS INITIAL.
*    <head_ichig> = gv_led_red.
*  ENDIF.

*     ICHBI: billing status
*  IF <head_uvfak> = 'C'.
*    <head_ichbi> = gv_led_green.
*  ELSEIF NOT <head_uvfak> IS INITIAL.
*    <head_ichbi> = gv_led_red.
*  ENDIF.

*     ICSTO: status Total incomplete
  if <head_uvals> = 'C' and
  <head_uvvls> = 'C' and
  <head_uvfas> = 'C' and
  <head_uvprs> = 'C'.
    <head_icsto> = gv_led_green.

  elseif <head_uvals> is not initial and
    not <head_uvvls> is initial and
    not <head_uvfas> is initial and
    not <head_uvprs> is initial.
    <head_icsto> = gv_led_red.
  endif.

*     ICSPR pricing status
*  IF <head_uvprs> = 'C'.
*    <head_icspr> = gv_led_green.
*
*  ELSEIF <head_uvprs> IS NOT INITIAL.
*    <head_icspr> = gv_led_red.
*
*  ENDIF.

*     ICSDL: delivery status
*  IF <head_uvvls> = 'C'.
*    <head_icsdl> = gv_led_green.
*
*  ELSEIF <head_uvvls> IS NOT INITIAL.
*    <head_icsdl> = gv_led_red.
*
*  ENDIF.

*     ICSIG: status In General
*  IF <head_uvals> = 'C'.
*    <head_icsig> = gv_led_green.
*
*  ELSEIF <head_uvals> IS NOT INITIAL.
*    <head_icsig> = gv_led_red.
*  ENDIF.

*     ICSDTO: status total incomplete of all 7 status
  if <head_ichto> = gv_led_green and
  <head_icsto> = gv_led_green.
    <head_icsdto> = gv_led_green.
    icsdto = icon_led_green.
  else.
    <head_icsdto> = gv_led_red.
    icsdto = icon_led_red.
  endif.

*     ICSBI: billing status
*  IF <head_uvfas> = 'C'.
*    <head_icsbi> = gv_led_green.
*  ELSEIF <head_uvfas> IS NOT INITIAL.
*    <head_icsbi> = gv_led_red.
*  ENDIF.


endform.                    " SALES_ORDER_STATUS_HANDLE
*&---------------------------------------------------------------------*
*&      Form  CALL_BAPI_CREATE_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_VBELN  text
*----------------------------------------------------------------------*
form bapi_order_create_trigger  using  p_test type boolean is_job  type zswtajobs  .

  data :
    lt_job         type table of zswtajobs,
    wa_extensionin type bapiparex,
    wa_bape_vbak   type bape_vbak,
    lv_vbeln       type vbeln,
    lv_vbeln_aux   type vbeln,
    lt_alv_job     type ty_alv_job_editable_t,
    ls_alv_job     type ty_alv_job_editable,
    lv_errors      type boolean,
    lv_bapi_error  type boolean,
    ls_tvakt       type tvakt.
  data: l_text(255) type c.
  field-symbols: <ls_job>   type zswtajobs,
                 <ls_item>  type bapisditm,
                 <ls_itemx> type bapisditmx.

  lv_vbeln = is_job-sales_order.

  clear: v_vbeln, lt_return[].


  if item[] is initial.
    perform add_messages_to_final_return  using 'E' c_message_class '052' is_job-job_id '' '' '' 'bapi_order_create_trigger' '' '' ''.
  endif.



  read table gt_return transporting no fields with key type = 'E'.
  if sy-subrc ne 0.

    if p_test is initial.
      if lv_vbeln is initial.
        concatenate text-t07 ''''
        into l_text separated by space.
      else.
        concatenate text-t09 ''''
        into l_text separated by space.
      endif.
    else.
      concatenate text-t08 ''''
      into l_text separated by space.
    endif.


    if not lv_vbeln is initial.
      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = lv_vbeln
        importing
          output = lv_vbeln_aux.
    endif.

* Reference only works if items are also referenced. So for now keep without references
    if not is_job-quotation_order is initial.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = is_job-quotation_order
        importing
          output = header-ref_doc.

      headerx-ref_doc = 'X'.

      header-refdoc_cat = 'B'. " Quotation
      headerx-refdoc_cat = 'X'.

*items have to be referenced. This considers that quotation matches sales order in full otherwise need to read quotation in order to perform correct assigment

      loop at item assigning <ls_item>.
        <ls_item>-ref_doc = header-ref_doc.
        <ls_item>-ref_doc_it = <ls_item>-itm_number. " vgpos
        <ls_item>-ref_doc_ca = header-refdoc_cat. " VGTYP
      endloop.

      loop at itemx assigning <ls_itemx>.
        <ls_itemx>-ref_doc = 'X'.
        <ls_itemx>-ref_doc_it = 'X'.
        <ls_itemx>-ref_doc_ca = 'X'.
      endloop.
    endif.

    concatenate l_text lv_vbeln_aux ''' ...'
    into l_text.
    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        text = l_text.

    break-point id zh_harnessing_file_processing.

    call function 'BAPI_SALESORDER_CREATEFROMDAT2'
      exporting
        salesdocumentin      = lv_vbeln
        order_header_in      = header
        order_header_inx     = headerx
        testrun              = p_test
*       logic_switch         = 'X'
      importing
        salesdocument        = v_vbeln
      tables
        order_items_in       = item
        order_items_inx      = itemx
        order_partners       = partner
        order_schedules_in   = lt_schedules_in
        order_schedules_inx  = lt_schedules_inx
        order_conditions_in  = lt_conditions_in
        order_conditions_inx = lt_conditions_inx
        order_cfgs_ref       = lt_cfgs_ref
        order_cfgs_inst      = lt_cfgs_inst
        order_cfgs_part_of   = lt_cfgs_part_of
        order_cfgs_value     = lt_cfgs_value
        order_text           = order_text
        extensionin          = lt_extensionin
        return               = lt_return.

    clear l_text.
* Check the return table.
    loop at lt_return where type = 'E' or type = 'A'.
      break-point id zh_harnessing_file_processing .
      lv_bapi_error = 'X'.
      exit.
    endloop.

    append lines of lt_return to gt_return.
  else.
    lv_errors = 'X'.
    break-point id zh_harnessing_file_processing .
  endif.


  if lv_bapi_error = 'X'.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
    call function 'SD_SALES_DOCUMENT_INIT'.
    delete from zswta_ssx_bom where zh_jobnr eq is_job-job_nr. " Delete entries from ZSWTA_SSX_BOM
  else.

    if not v_vbeln is initial and p_test is initial.

      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait   = 'X'
        importing
          return = lt_return.

      read table lt_return with key type = 'E'.
*      IF sy-subrc NE 0.
*
*      ENDIF.
    endif.
  endif.


  if lv_errors = 'X' or lv_bapi_error = 'X'.
    if not  p_test is initial.
      is_job-sap_check = gc_checked_with_errors.
    else.
      is_job-int_check = gc_checked_with_errors.
      is_job-job_status = gc_checked_with_errors.
    endif.
  else.

    if p_test is initial. " Real execution mode
      is_job-int_check = gc_checked_completed.
      is_job-job_status = gc_checked_completed.
      select single * from tvakt into ls_tvakt where spras = sy-langu  and auart = header-doc_type.

      message i311(v1) with ls_tvakt-bezei v_vbeln into l_text.
      perform add_messages_to_final_return
using 'I' 'V1' '311' ls_tvakt-bezei v_vbeln  '' '' 'bapi_order_create_trigger' '' '' ''.
    else.
      is_job-sap_check = gc_checked_completed.
      if is_job-int_check = gc_checked_completed.
        is_job-job_status = gc_checked_completed.
      else.
        is_job-job_status = gc_partially_processed.
      endif.
      message i022(zswec0002) with is_job-job_id into l_text.
      perform add_messages_to_final_return
using 'I' c_message_class '022' is_job-job_id '' '' '' 'bapi_order_create_trigger' '' '' ''.

    endif.

    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        text = l_text.

    is_job-sales_order = v_vbeln.
    is_job-purchase_order = header-purch_no_c.
    read table lt_extensionin into wa_extensionin with key structure = 'BAPE_VBAK'.
    if sy-subrc eq 0.
      wa_bape_vbak = wa_extensionin+30(960) ##ENH_OK.
      is_job-order_type = wa_bape_vbak-zz_ordertype.
    endif.

    read table lt_cfgs_value transporting no fields   " Same for all items
    with key charc =  zif_harnessing_core=>gc_zh_eorder_type_char_name
             value = 'NPE'.
    if sy-subrc eq 0.
      is_job-npe = 'X'.
    else.
      clear is_job-npe.
    endif.

  endif.


  read table gt_job assigning <ls_job>
  with key job_id = is_job-job_id.
  if sy-subrc eq 0.
    <ls_job> = is_job.
  endif.

  read table gt_alv_job assigning <gs_alv_job>
  with key job_id = is_job-job_id.
  if sy-subrc eq 0.
    move-corresponding is_job to <gs_alv_job>.

    append  <gs_alv_job> to lt_alv_job.
    perform job_alv_enrich using lt_alv_job.

    loop at lt_alv_job into ls_alv_job.
      read table gt_alv_job assigning <gs_alv_job>
      with key job_id = ls_alv_job-job_id.
      if sy-subrc eq 0.
        move-corresponding  ls_alv_job to <gs_alv_job>.
      endif.
    endloop.
  endif.


  modify zswtajobs from is_job.
  if sy-subrc = 0.
    commit work.
  else.
    rollback work.
  endif.


  if p_test is initial and lv_errors is initial and lv_bapi_error is initial.
    clear lt_job[].
    append is_job to lt_job.

************************************************************
* Download tech file to tech path
************************************************************

    perform job_download_files tables lt_job.




    read table gt_return transporting no fields with key type = 'E'.
    if sy-subrc eq 0.
      move icon_yellow to <gs_alv_job>-icon_int_check. " icon yellow when problems downloading files to server despites sales order created
    else.
      move icon_green to <gs_alv_job>-icon_int_check.
    endif.

  endif.



  delete gt_return where type eq 'S'.

endform.                    " CALL_BAPI_CREATE_ORDER
form bapi_quotation_create_trigger  using  p_test type boolean is_job  type zswtajobs  .

  data :
    lt_job         type table of zswtajobs,
    wa_extensionin type bapiparex,
    wa_bape_vbak   type bape_vbak,
    lv_vbeln       type vbeln,
    lv_vbeln_aux   type vbeln,
    lt_alv_job     type ty_alv_job_editable_t,
    ls_alv_job     type ty_alv_job_editable,
    lv_errors      type boolean,
    lv_bapi_error  type boolean,
    ls_tvakt       type tvakt.
  data: l_text(255) type c.
  field-symbols: <ls_job>     type zswtajobs.

  lv_vbeln = is_job-quotation_order.

  clear: v_vbeln, lt_return[].


  if item[] is initial.
    perform add_messages_to_final_return  using 'E' c_message_class '052' is_job-job_id '' '' '' 'bapi_order_create_trigger' '' '' ''.
  endif.



  read table gt_return transporting no fields with key type = 'E'.
  if sy-subrc ne 0.

    if p_test is initial.
      if lv_vbeln is initial.
        concatenate text-t19 ''''
        into l_text separated by space.
      else.
        concatenate text-t21 ''''
        into l_text separated by space.
      endif.
    else.
      concatenate text-t20 ''''
      into l_text separated by space.
    endif.


    if not lv_vbeln is initial.
      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = lv_vbeln
        importing
          output = lv_vbeln_aux.
    endif.

    concatenate l_text lv_vbeln_aux ''' ...'
    into l_text.
    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        text = l_text.

    break-point id zh_harnessing_file_processing.


    call function 'BAPI_QUOTATION_CREATEFROMDATA2'
      exporting
        salesdocumentin          = lv_vbeln
        quotation_header_in      = header
        quotation_header_inx     = headerx
        testrun                  = p_test
      importing
        salesdocument            = v_vbeln
      tables
        quotation_items_in       = item
        quotation_items_inx      = itemx
        quotation_partners       = partner
        quotation_schedules_in   = lt_schedules_in
        quotation_schedules_inx  = lt_schedules_inx
        quotation_conditions_in  = lt_conditions_in
        quotation_conditions_inx = lt_conditions_inx
        quotation_cfgs_ref       = lt_cfgs_ref
        quotation_cfgs_inst      = lt_cfgs_inst
        quotation_cfgs_part_of   = lt_cfgs_part_of
        quotation_cfgs_value     = lt_cfgs_value
        quotation_text           = order_text
        extensionin              = lt_extensionin
        return                   = lt_return.


    clear l_text.
* Check the return table.
    loop at lt_return where type = 'E' or type = 'A'.
      break-point id zh_harnessing_file_processing .
      lv_bapi_error = 'X'.
      exit.
    endloop.

    append lines of lt_return to gt_return.
  else.
    lv_errors = 'X'.
    break-point id zh_harnessing_file_processing .
  endif.


  if lv_bapi_error = 'X'.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
    call function 'SD_SALES_DOCUMENT_INIT'.
    delete from zswta_ssx_bom where zh_jobnr eq is_job-job_nr. " Delete entries from ZSWTA_SSX_BOM
  else.

    if not v_vbeln is initial and p_test is initial.

      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait   = 'X'
        importing
          return = lt_return.

      read table lt_return with key type = 'E'.
*      IF sy-subrc NE 0.
*
*      ENDIF.
    endif.
  endif.



  if lv_errors = 'X' or lv_bapi_error = 'X'.
    if not  p_test is initial.
      is_job-quote_check = gc_checked_with_errors.
    else.
      is_job-quote_int = gc_checked_with_errors.
      is_job-job_status = gc_checked_with_errors.
    endif.
  else.

    if p_test is initial. " Real execution mode
      is_job-quote_int = gc_checked_completed.
      if not is_job-int_check = gc_checked_completed.
        is_job-job_status = gc_partially_processed.
      endif.
      select single * from tvakt into ls_tvakt where spras = sy-langu  and auart = header-doc_type.

      message i311(v1) with ls_tvakt-bezei v_vbeln into l_text.
      perform add_messages_to_final_return
      using 'I' 'V1' '311' ls_tvakt-bezei v_vbeln  '' '' 'bapi_order_create_trigger' '' '' ''.
    else.
      is_job-quote_check = gc_checked_completed.
      if is_job-quote_int = gc_checked_completed and not is_job-int_check = gc_checked_completed.
        is_job-job_status = gc_partially_processed.
      endif.
      message i022(zswec0002) with is_job-job_id into l_text.
      perform add_messages_to_final_return
      using 'I' c_message_class '022' is_job-job_id '' '' '' 'bapi_order_create_trigger' '' '' ''.

    endif.


    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        text = l_text.

    is_job-quotation_order = v_vbeln.
    is_job-purchase_order = header-purch_no_c.
    read table lt_extensionin into wa_extensionin with key structure = 'BAPE_VBAK'.
    if sy-subrc eq 0.
      wa_bape_vbak = wa_extensionin+30(960) ##ENH_OK.
      is_job-order_type = wa_bape_vbak-zz_ordertype.
    endif.

    read table lt_cfgs_value transporting no fields   " Same for all items
    with key charc =  zif_harnessing_core=>gc_zh_eorder_type_char_name
    value = 'NPE'.
    if sy-subrc eq 0.
      is_job-npe = 'X'.
    else.
      clear is_job-npe.
    endif.

  endif.


  read table gt_job assigning <ls_job>
  with key job_id = is_job-job_id.
  if sy-subrc eq 0.
    <ls_job> = is_job.
  endif.

  read table gt_alv_job assigning <gs_alv_job>
  with key job_id = is_job-job_id.
  if sy-subrc eq 0.
    move-corresponding is_job to <gs_alv_job>.

    append  <gs_alv_job> to lt_alv_job.
    perform job_alv_enrich using lt_alv_job.

    loop at lt_alv_job into ls_alv_job.
      read table gt_alv_job assigning <gs_alv_job>
      with key job_id = ls_alv_job-job_id.
      if sy-subrc eq 0.
        move-corresponding  ls_alv_job to <gs_alv_job>.
      endif.
    endloop.
  endif.


  modify zswtajobs from is_job.
  if sy-subrc = 0.
    commit work.
  else.
    rollback work.
  endif.


  if p_test is initial and lv_errors is initial and lv_bapi_error is initial.
    clear lt_job[].
    append is_job to lt_job.

************************************************************
* Download tech file to tech path
************************************************************

    perform job_download_files tables lt_job.




    read table gt_return transporting no fields with key type = 'E'.
    if sy-subrc eq 0.
      move icon_yellow to <gs_alv_job>-icon_quote_int. " icon yellow when problems downloading files to server despites sales order created
    else.
      move icon_green to <gs_alv_job>-icon_quote_int.
    endif.

  endif.



  delete gt_return where type eq 'S'.

endform.                    " CALL_BAPI_CREATE_ORDER