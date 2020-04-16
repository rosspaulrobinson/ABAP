class YGTTC_CL_PCMV definition
  public
  final
  create public .

public section.
*"* public components of class YGTTC_CL_PCMV
*"* do not include other source files here!!!

  interfaces IF_RSPC_DELETE .
  interfaces IF_RSPC_EXECUTE .
  interfaces IF_RSPC_GET_LOG .
  interfaces IF_RSPC_MAINTAIN .
  interfaces IF_RSPC_CHECK .
  interfaces IF_RSPC_GET_VARIANT .

  types:
    begin of s_range,
        sign                 type rssign,
        option               type rsoption,
        low                  type rslow,
        high                 type rshigh,
      end of s_range .
  types:
    t_range type table of s_range with non-unique key table_line .
  types:
    begin of s_field,
        fieldnm              type rsfieldnm,
        t_range              type t_range,
      end of s_field .
  types:
    t_field type table of s_field with non-unique key fieldnm .
  types:
    begin of s_filter,
        group                type i,
        t_field              type t_field,
      end of s_filter .
  types:
    t_filter type table of s_filter with non-unique key group .
  types:
    begin of s_alv_filter,
          group                  type numc2,
          fieldnm                type rsfieldnm,
          sign                   type rssign,
          option                 type rsoption,
          low                    type rslow,
          high                   type rshigh,
          vnam                   type rszvnam,
          txtlg                  type rstxtlg,
        end of s_alv_filter .
  types:
    t_alv_filter             type table of s_alv_filter with non-unique key table_line .

  class-methods _SET_ALV_FIELD_CATALOG
    importing
      !I_ALV_USE type STRING
    returning
      value(RT_LVC_FCAT) type LVC_T_FCAT .
  class-methods _FLAT_TO_DEEP_FILTER
    importing
      !IT_ALV_FILTER type T_ALV_FILTER
    returning
      value(RT_FILTER) type T_FILTER .
  class-methods _VALIDATE_DEEP_FILTER
    importing
      !IT_FILTER type T_FILTER
    returning
      value(R_RC) type SYSUBRC .
  class-methods _VALIDATE_FLAT_FILTER
    importing
      !IT_ALV_FILTER type T_ALV_FILTER
    returning
      value(R_RC) type SYSUBRC .
  class-methods _SET_FILTER_IP
    importing
      !I_INFOPACKAGE type RSLOGDPID
      !IT_RSPCVARIANT type RSPC_T_VARIANT .
  class-methods _SET_FILTER_DTP
    importing
      !I_DTP type RSBKDTPNM
      !IT_RSPCVARIANT type RSPC_T_VARIANT .
*"* protected components of class YCL_RSPC_ABAP
*"* do not include other source files here!!!
protected section.
private section.
*"* private components of class YGTTC_CL_PCMV
*"* do not include other source files here!!!
ENDCLASS.



CLASS YGTTC_CL_PCMV IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YGTTC_CL_PCMV=>IF_RSPC_CHECK~CHECK
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_S_PROCESS                    TYPE        RSPC_S_CHAIN
* | [--->] I_T_CHAIN                      TYPE        RSPC_T_CHAIN
* | [--->] I_T_CHAINS                     TYPE        RSPC_T_CHAIN(optional)
* | [<---] E_T_CONFLICTS                  TYPE        RSPC_T_CONFLICTS
* +--------------------------------------------------------------------------------------</SIGNATURE>
method if_rspc_check~check.

  data:
    ls_chain                 type rspc_s_chain,
    ls_chain_tmp             type rspc_s_chain,
    lt_chain                 type rspc_t_chain.

  data:
    ls_conflicts             type rspc_s_conflicts.

  data:
    lo_variant               type ref to cl_rspc_variant.

  data:
    l_type                   type rspc_type,
    l_variant                type rspc_variant,
    l_objvers                type rsobjvers,
    ls_rspcvariantattr       type rspcvariantattr,
    ls_rspcvariant           type rspcvariant,
    lt_rspcvariant           type rspc_t_variant,
    ls_rspcvariantt          type rspcvariantt,
    lt_rspcvariantt          type rspc_t_variantt.

  data:
    l_eventp_start           type btcevtparm.

  data:
    l_infopackage            type bapi6109-infopackage,
    l_with_empty_selfields   type bapi6109-with_empty_selfields.

  data:
    lt_selections            type table of  bapi6109sel,
    lt_infocubes             type table of bapi6109ic,
    lt_third_party_params    type table of bapi6109tcp,
    lt_return                 type table of bapiret2.


  "Create the variant.
  call method cl_rspc_variant=>create
    exporting
      i_type      = 'YGTTC_PCMV'
      i_variant   = i_s_process-variante
    receiving
      r_r_variant = lo_variant
    exceptions
      locked      = 1
      others      = 2.
  if sy-subrc <> 0.
    message id 'YGTBW' type 'I' number '200'
               with 'Variant' i_s_process-variante 'cannot be created.'.
  endif.

  call method lo_variant->get_info
    importing
      e_type              = l_type
      e_variant           = l_variant
      e_objvers           = l_objvers
      e_s_rspcvariantattr = ls_rspcvariantattr
      e_t_rspcvariant     = lt_rspcvariant
      e_s_rspcvariantt    = ls_rspcvariantt
      e_t_rspcvariantt    = lt_rspcvariantt.


  read table lt_rspcvariant into ls_rspcvariant
    with key
      fnam = 'TASK'.
  if sy-subrc <> 0.
    message id 'YGTBW' type 'I' number '200'
               with 'Variant' i_s_process-variante 'is not configured correctly.'.
  endif.

  case ls_rspcvariant-low.
    when 'DELINI'.

      "If the variant is set to delete the Initialisation of a
      "DataSource the variant must immediatly precede a single IP
      "in the Process Chain.

      "Find the variant in the chain.
      read table i_t_chain into ls_chain
        with key
          type = 'YGTTC_PCMV'
          variante = ls_rspcvariant-variante.
      if sy-subrc <> 0.

        ls_conflicts-msgid = 'YGTBW'.
        ls_conflicts-msgty = 'E'.
        ls_conflicts-msgno = '200'.
        ls_conflicts-msgv1 = 'Single successor IP required'.
        append ls_conflicts to e_t_conflicts.

        return.
      endif.

      "Transfer the variant information to the conflicts structure.
      move-corresponding ls_chain to ls_conflicts.

      "Get the finish event.
      if ls_chain-eventp_green is not initial.
        l_eventp_start = ls_chain-eventp_green.
      else.
        l_eventp_start = ls_chain-eventp_red.
      endif.

      "Check there is only one succesor variant.
      lt_chain = i_t_chain.
      delete lt_chain where eventp_start <> l_eventp_start.

      if lines( lt_chain ) <> 1.

        ls_conflicts-msgid = 'YGTBW'.
        ls_conflicts-msgty = 'E'.
        ls_conflicts-msgno = '200'.
        ls_conflicts-msgv1 = 'Single successor IP required'.
        append ls_conflicts to e_t_conflicts.

        return.
      endif.

      "Check the variant is an InfoPackage.
      read table lt_chain into ls_chain index 1.

      if ls_chain-type <> 'LOADING'.

        ls_conflicts-msgid = 'YGTBW'.
        ls_conflicts-msgty = 'E'.
        ls_conflicts-msgno = '200'.
        ls_conflicts-msgv1 = 'Single successor IP required'.
        append ls_conflicts to e_t_conflicts.

        return.
      endif.

      l_infopackage = ls_chain-variante.

      data:
        ls_details type bapi6109,
        ls_scheduling_params type bapi6109btch,
        ls_destinations type bapi6109dest,
        ls_file_params type bapi6109file,
        ls_hie_params type bapi6109hie.

      "Retrieve the IP information.
      call function 'BAPI_IPAK_GETDETAIL'
        exporting
          infopackage          = l_infopackage
*         WITH_EMPTY_SELFIELDS = ' '
        importing
          details              = ls_details
          scheduling_params    = ls_scheduling_params
          destinations         = ls_destinations
          file_params          = ls_file_params
          hie_params           = ls_hie_params
        tables
          selections           = lt_selections
          infocubes            = lt_infocubes
          third_party_params   = lt_third_party_params
          return               = lt_return.

    when 'FILTER'.

*/ The Filter action of the multi-variant has a mandatory single
*  InfoPackage or DTP as a successor variant. Only a green trigger
*  event is supported.

      "Define conflict message.
      ls_conflicts-msgid = 'YGTBW'.
      ls_conflicts-msgty = 'E'.
      ls_conflicts-msgno = '200'.
      ls_conflicts-msgv1 = 'Filter action must have a single IP/DTP successor.'.
      ls_conflicts-msgv2 = 'Only a Green trigger event is supported.'.

      read table i_t_chain into ls_chain
        with key
          type  = i_s_process-type
          variante = i_s_process-variante.
      if sy-subrc = 0.

        move-corresponding ls_chain to ls_conflicts.

        "Check that the finished event is green.
        if ls_chain-eventp_green is initial.
          append ls_conflicts to e_t_conflicts. return.
        endif.

        "Check there is no red finished event.
        if ls_chain-eventp_red is not initial.
          append ls_conflicts to e_t_conflicts. return.
        endif.

        "Collect succesor variants.
        loop at i_t_chain into ls_chain where eventp_start = ls_chain-eventp_green.
          append ls_chain to lt_chain.
        endloop.

        "Check there is only one succesor variant.
        if lines( lt_chain ) <> 1.
          append ls_conflicts to e_t_conflicts. return.
        endif.

        "Get the successor variant.
        read table lt_chain into ls_chain index 1.

        if not ( ls_chain-type = 'LOADING' or ls_chain-type = 'DTP_LOAD' ).
          append ls_conflicts to e_t_conflicts. return.
        endif.
      endif.
    when others.
  endcase.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YGTTC_CL_PCMV=>IF_RSPC_CHECK~GIVE_ALL
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_VARIANT                      TYPE        RSPC_VARIANT
* | [<-()] RETURN                         TYPE        RS_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
method IF_RSPC_CHECK~GIVE_ALL.

  data:
    l_exists                 type rs_bool.

  data:
    lo_variant_ref           type ref to cl_rspc_variant,
    ls_rspcvariant_ref       type rspcvariant,
    lt_rspcvariant_ref       type rspc_t_variant.

  field-symbols:
    <ls_rspcvariant_ref>     type rspcvariant.


  "If the reference variant does not exist, create it.
  call method cl_rspc_variant=>exists
    exporting
      i_type    = 'YGTTC_PCMV'
      i_variant = 'YGTTC_PCMV_REF'
    receiving
      r_exists  = l_exists.
  if l_exists <> 'X'.

    call method cl_rspc_variant=>create
      exporting
        i_type         = 'YGTTC_PCMV'
        i_variant      = 'YGTTC_PCMV_REF'
        i_objvers      = rs_c_objvers-active
        i_no_transport = 'X'
      receiving
        r_r_variant    = lo_variant_ref
      exceptions
        locked         = 1
        others         = 2.
    if sy-subrc <> 0.
      message id 'YGTBW' type 'E' number '200'
                 with 'Unable to create variant YGTTC_PCMV_REF'.
    endif.

    call method lo_variant_ref->save
      exporting
        i_t_rspcvariant = lt_rspcvariant_ref
      exceptions
        failed          = 1
        others          = 2.
    if sy-subrc <> 0.
      message id 'YGTBW' type 'E' number '200'
                 with 'Unable to save variant YGTTC_PCMV_REF'.
    endif.

  endif.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YGTTC_CL_PCMV=>IF_RSPC_DELETE~DELETE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_VARIANT                      TYPE        RSPC_VARIANT
* | [EXC!] FAILED
* +--------------------------------------------------------------------------------------</SIGNATURE>
method IF_RSPC_DELETE~DELETE.

  data:
    lo_variant               type ref to cl_rspc_variant.


  call method cl_rspc_variant=>create
    exporting
      i_type      = 'YGTTC_PCMV'
      i_variant   = i_variant
    receiving
      r_r_variant = lo_variant
    exceptions
      locked      = 1
      others      = 2.
  if sy-subrc <> 0.
    raise failed.
  endif.

  call method lo_variant->delete
    exceptions
      failed = 1
      others = 2.
  if sy-subrc <> 0.
    raise failed.
  endif.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YGTTC_CL_PCMV=>IF_RSPC_EXECUTE~EXECUTE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_VARIANT                      TYPE        RSPC_VARIANT
* | [--->] I_EVENT_START                  TYPE        BTCEVENTID(optional)
* | [--->] I_EVENTP_START                 TYPE        BTCEVTPARM(optional)
* | [--->] I_JOBCOUNT                     TYPE        BTCJOBCNT(optional)
* | [--->] I_BATCHDATE                    TYPE        BTCRELDT(optional)
* | [--->] I_BATCHTIME                    TYPE        BTCRELTM(optional)
* | [--->] I_T_PROCESSLIST                TYPE        RSPC_T_PROCESSLIST(optional)
* | [--->] I_LOGID                        TYPE        RSPC_LOGID(optional)
* | [--->] I_T_VARIABLES                  TYPE        RSPC_T_VARIABLES(optional)
* | [--->] I_SYNCHRONOUS                  TYPE        RSPC_SYNCHRONOUS(optional)
* | [--->] I_SIMULATE                     TYPE        RSPC_SIMULATION(optional)
* | [--->] I_REPAIR                       TYPE        RSPC_REPAIR(optional)
* | [--->] I_BATCHID                      TYPE        RSBTC_BATCH_ID(optional)
* | [<---] E_INSTANCE                     TYPE        RSPC_INSTANCE
* | [<---] E_STATE                        TYPE        RSPC_STATE
* | [<---] E_EVENTNO                      TYPE        RSPC_EVENTNO
* | [<---] E_HOLD                         TYPE        RSPC_HOLD
* +--------------------------------------------------------------------------------------</SIGNATURE>
method if_rspc_execute~execute.
  "  while l_debug_loop is initial. endwhile.

  data:
    l_guid                   type sysuuid_25.

  data:
    ls_processlist           type rspc_s_processlist.

  data:
    lo_variant               type ref to cl_rspc_variant.

  data:
    l_type                   type rspc_type,
    l_variant                type rspc_variant,
    l_objvers                type rsobjvers,
    ls_rspcvariantattr       type rspcvariantattr,
    ls_rspcvariant           type rspcvariant,
    lt_rspcvariant           type rspc_t_variant,
    ls_rspcvariantt          type rspcvariantt,
    lt_rspcvariantt          type rspc_t_variantt.

  data:
    lo_variant_ref           type ref to cl_rspc_variant,
    ls_rspcvariant_ref       type rspcvariant,
    lt_rspcvariant_ref       type rspc_t_variant.

  field-symbols:
    <ls_rspcvariant_ref>     type rspcvariant.

  data:
    l_lnr                    type rslnr.

  data:
    l_tstmp                  type timestampl,
    l_tstmp1                 type timestampl,
    l_tstmp2                 type timestampl,
    l_secs                   type tzntstmpl,
    l_secs_i                 type i.

  data:
    l_eventp_start           type btcevtparm.

  data:
    l_infopackage            type bapi6109-infopackage.

  data:
    ls_details               type bapi6109,
    ls_scheduling_params     type bapi6109btch,
    ls_destinations          type bapi6109dest,
    ls_file_params           type bapi6109file,
    ls_hie_params            type bapi6109hie.

  data:
    lt_selections            type table of bapi6109sel,
    lt_infocubes             type table of bapi6109ic,
    lt_third_party_params    type table of bapi6109tcp,
    lt_return                 type table of bapiret2.

  data:
    ls_ds                    type rsds,
    lt_ds                    type table of rsds.

  data:
    lo_appl_log              type ref to cl_rspc_appl_log,
    ls_msg                   type bal_s_msg,
    lt_msg                   type rs_t_msg.


  "Create a unique id for the log.
  call function 'RSSM_UNIQUE_ID'
    importing
      e_uni_idc25 = l_guid.

  e_instance = `PCMV_` && l_guid.

  "Create an application log to store messages.
  call method cl_rspc_appl_log=>create
    exporting
      i_type           = 'YGTTC_PCMV'
      i_variant        = i_variant
      i_instance       = e_instance
    receiving
      r_r_appl_log     = lo_appl_log
    exceptions
      internal_failure = 1
      others           = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
               with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  "Create the variant.
  call method cl_rspc_variant=>create
    exporting
      i_type      = 'YGTTC_PCMV'
      i_variant   = i_variant
    receiving
      r_r_variant = lo_variant
    exceptions
      locked      = 1
      others      = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
               with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  call method lo_variant->get_info
    importing
      e_type              = l_type
      e_variant           = l_variant
      e_objvers           = l_objvers
      e_s_rspcvariantattr = ls_rspcvariantattr
      e_t_rspcvariant     = lt_rspcvariant
      e_s_rspcvariantt    = ls_rspcvariantt
      e_t_rspcvariantt    = lt_rspcvariantt.

  "Stop the variant by raising an RC8 if requested in the variant.
  read table lt_rspcvariant into ls_rspcvariant
    with key
      fnam = 'STOP'.

  if ls_rspcvariant-low = 'X'.
    e_state = 'R'.
    e_hold = 'X'.

    "Add the messages to the application log.
    call method lo_appl_log->add_message
      exporting
        i_msgty          = 'W'
        i_msgid          = 'YGTTC_PCMV'
        i_msgno          = '002'
      exceptions
        internal_failure = 1
        others           = 2.

    "Save the log.
    call method lo_appl_log->save
      exceptions
        internal_failure = 1
        others           = 2.

    if i_synchronous <> 'X'.
      message id 'YGTTC_PCMV' type 'E' number '002'." `Variant stopped by variant configuration!`
    endif.

    return.
  endif.

  "Get the task type.
  read table lt_rspcvariant into ls_rspcvariant
    with key
      fnam = 'TASK'.
  if sy-subrc <> 0.
    message 'Variant is not configured correctly' type 'I'.
  endif.

  case ls_rspcvariant-low.
    when 'DELINI'.

      "Get the succesor InfoPackage.
      read table i_t_processlist into ls_processlist
        with key
          type = 'YGTTC_PCMV'
          variante = i_variant.
      if sy-subrc <> 0.
        message 'A single DTP must be incldued in the chain.' type 'E'.
      endif.

      "Get the finish event.
      if ls_processlist-eventp_green is not initial.
        l_eventp_start = ls_processlist-eventp_green.
      else.
        l_eventp_start = ls_processlist-eventp_red.
      endif.

      "Check the variant is an InfoPackage.
      read table i_t_processlist into ls_processlist
        with key
          eventp_start = l_eventp_start.

      l_infopackage = ls_processlist-variante.

      "Get the IP information.
      call function 'BAPI_IPAK_GETDETAIL'
        exporting
          infopackage          = l_infopackage
          with_empty_selfields = 'X'
        importing
          details              = ls_details
          scheduling_params    = ls_scheduling_params
          destinations         = ls_destinations
          file_params          = ls_file_params
          hie_params           = ls_hie_params
        tables
          selections           = lt_selections
          infocubes            = lt_infocubes
          third_party_params   = lt_third_party_params
          return               = lt_return.

      ls_ds-datasource = ls_details-datasource.
      append ls_ds to lt_ds.

      call function 'RSS2_DELETE_INITSEL_OLTP'
        exporting
          i_logsys = ls_details-logsys
        tables
          i_t_ds   = lt_ds.

      e_state = 'G'.
      e_hold = ''.

      clear ls_msg.
      ls_msg-msgid = 'YGTBW'.
      ls_msg-msgno = '200'.
      ls_msg-msgty = 'I'.
      ls_msg-msgv1 = |Initialisation deleted for { ls_details-datasource }/{ ls_details-logsys }|.
      append ls_msg to lt_msg.

    when 'FILTER'.

*/ To help ensure the variant has sufficient time to change the
*  filter for the InfoPackage or DTP and start the request a minimum
*  wait time between execution of an IP or DTP of 60 seconds is
*  applied.
*/ This is only really an issue if local chains using the same
*  loading variant are loaded in parallel. The timestamp of the
*  last load of an IP or DTP is stored in the meta-data tables for
*  variant YGTTC_PCMV_REF.

      "Get the succesor InfoPackage/DTP.
      read table i_t_processlist into ls_processlist
        with key
          type = 'YGTTC_PCMV'
          variante = i_variant.
      if sy-subrc <> 0.
        message 'A single DTP must be incldued in the chain.' type 'E'.
      endif.

      "Get the finish event.
      if ls_processlist-eventp_green is not initial.
        l_eventp_start = ls_processlist-eventp_green.
      else.
        l_eventp_start = ls_processlist-eventp_red.
      endif.

      "Check the variant is an InfoPackage/DTP.
      read table i_t_processlist into ls_processlist
        with key
          eventp_start = l_eventp_start.

      l_infopackage = ls_processlist-variante.

      "Get the reference variant and determine if there should be a delay.
      call method cl_rspc_variant=>create
        exporting
          i_type         = 'YGTTC_PCMV'
          i_variant      = 'YGTTC_PCMV_REF'
          i_objvers      = rs_c_objvers-active
          i_no_transport = 'X'
        receiving
          r_r_variant    = lo_variant_ref
        exceptions
          locked         = 1
          others         = 2.
      if sy-subrc <> 0.
        message id 'YGTBW' type 'E' number '200'
                   with 'Unable to create variant YGTTC_PCMV_REF'.
      endif.

      call method lo_variant_ref->get_info
        importing
          e_t_rspcvariant = lt_rspcvariant_ref.

      read table lt_rspcvariant_ref assigning <ls_rspcvariant_ref>
        with key
          fnam = 'LAST_LOAD'
          low = ls_processlist-variante.
      if sy-subrc <> 0.

        sort lt_rspcvariant_ref by lnr descending.
        read table lt_rspcvariant_ref into ls_rspcvariant_ref index 1.
        l_lnr = ls_rspcvariant_ref-lnr + 1.

        append initial line to lt_rspcvariant_ref assigning <ls_rspcvariant_ref>.

        <ls_rspcvariant_ref>-type = 'YGTTC_PCMV'.
        <ls_rspcvariant_ref>-variante = 'YGTTC_PCMV_REF'.
        <ls_rspcvariant_ref>-objvers = rs_c_objvers-active.
        <ls_rspcvariant_ref>-lnr = l_lnr.

        get time stamp field l_tstmp.

        <ls_rspcvariant_ref>-fnam = 'LAST_LOAD'.
        <ls_rspcvariant_ref>-sign = 'I'.
        <ls_rspcvariant_ref>-opt = 'EQ'.
        <ls_rspcvariant_ref>-low = ls_processlist-variante.
        <ls_rspcvariant_ref>-high = l_tstmp.
      else.

        get time stamp field l_tstmp1.
        message id 'YGTBW' type 'S' number '200'
                   with 'Execution time' l_tstmp1.

        l_tstmp2 = <ls_rspcvariant_ref>-high.
        message id 'YGTBW' type 'S' number '200'
                   with 'Last load' l_tstmp2.

        try.

            call method cl_abap_tstmp=>subtract
              exporting
                tstmp1 = l_tstmp1
                tstmp2 = l_tstmp2
              receiving
                r_secs = l_secs.

            l_secs_i = l_secs.

            message id 'YGTBW' type 'I' number '200'
                       with 'Last load' l_secs_i 'seconds ago'.

          catch cx_parameter_invalid_range .
          catch cx_parameter_invalid_type .
        endtry.

        if  l_secs > 0 and l_secs < 60.

          message id 'YGTBW' type 'I' number '200'
                     with 'Wait up to 60 seconds.'.

          l_secs = 60 - l_secs.

          wait up to l_secs seconds.
        endif.

        get time stamp field l_tstmp.
        <ls_rspcvariant_ref>-high = l_tstmp.
      endif.

      call method lo_variant_ref->save
        exporting
          i_t_rspcvariant = lt_rspcvariant_ref
        exceptions
          failed          = 1
          others          = 2.
      if sy-subrc <> 0.
        message id 'YGTBW' type 'E' number '200'
                   with 'Unable to save variant YGTTC_PCMV_REF'.
      endif.

      case ls_processlist-type.
        when 'LOADING'.

*/ The InfoPackage filter is parially set using BAPIs, however since
*  the interface for the BAPI does not support setting OLAP variable or
*  ABAP routine filters these must be manually added.

          call method _set_filter_ip
            exporting
              i_infopackage  = |{ ls_processlist-variante }|
              it_rspcvariant = lt_rspcvariant.

        when 'DTP_LOAD'.

*/ The DTP filter is set based on the same logic as the IP filter, i.e.
*  ABAP routines will not be overwritten, OLAP variables and fixed
*  values will be set.

          call method _set_filter_dtp
            exporting
              i_dtp          = |{ ls_processlist-variante }|
              it_rspcvariant = lt_rspcvariant.

        when others.

          message 'A single IP or DTP must succeed the variant.' type 'E'.

      endcase.

      clear ls_msg.
      ls_msg-msgid = 'YGTBW'.
      ls_msg-msgno = '200'.
      ls_msg-msgty = 'I'.
      ls_msg-msgv1 = |Filter set for IP/DTP|.
      append ls_msg to lt_msg.

      e_state = 'G'.
      e_hold = ''.

    when others.

      message 'Variant has an invalid task type' type 'E'.

  endcase.

  "Add the messages to the application log.
  loop at lt_msg into ls_msg.

    call method lo_appl_log->add_message
      exporting
        i_msgty          = ls_msg-msgty
        i_msgid          = ls_msg-msgid
        i_msgno          = ls_msg-msgno
        i_msgv1          = ls_msg-msgv1
        i_msgv2          = ls_msg-msgv2
        i_msgv3          = ls_msg-msgv3
        i_msgv4          = ls_msg-msgv4
      exceptions
        internal_failure = 1
        others           = 2.
  endloop.

  "Save the log.
  call method lo_appl_log->save
    exceptions
      internal_failure = 1
      others           = 2.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YGTTC_CL_PCMV=>IF_RSPC_EXECUTE~GIVE_CHAIN
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_VARIANT                      TYPE        RSPC_VARIANT
* | [<-()] RETURN                         TYPE        RS_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
method IF_RSPC_EXECUTE~GIVE_CHAIN.

  return = 'X'.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YGTTC_CL_PCMV=>IF_RSPC_GET_LOG~DELETE_LOG
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_VARIANT                      TYPE        RSPC_VARIANT
* | [--->] I_INSTANCE                     TYPE        RSPC_INSTANCE
* | [--->] I_FORCE                        TYPE        RS_BOOL(optional)
* | [EXC!] FAILED
* +--------------------------------------------------------------------------------------</SIGNATURE>
method IF_RSPC_GET_LOG~DELETE_LOG.

  return.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YGTTC_CL_PCMV=>IF_RSPC_GET_LOG~GET_LOG
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_VARIANT                      TYPE        RSPC_VARIANT
* | [--->] I_INSTANCE                     TYPE        RSPC_INSTANCE
* | [<---] E_TITLE                        TYPE        SO_OBJ_DES
* | [<---] E_T_TEXT                       TYPE        RSPC_T_TEXT
* | [<---] E_T_MESSAGES                   TYPE        RS_T_MSG
* +--------------------------------------------------------------------------------------</SIGNATURE>
method IF_RSPC_GET_LOG~GET_LOG.

  data:
    l_s_msg                  type bal_s_msg,
    l_t_msg                  type rs_t_msg.


  call method cl_rspc_appl_log=>display
    exporting
      i_type           = 'YGTTC_PCMV'
      i_variant        = i_variant
      i_instance       = i_instance
      i_no_display     = 'X'
    importing
      e_t_msg          = l_t_msg
    exceptions
      internal_failure = 1
      others           = 2.

  e_t_messages = l_t_msg.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YGTTC_CL_PCMV=>IF_RSPC_GET_VARIANT~EXISTS
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_VARIANT                      TYPE        RSPC_VARIANT
* | [--->] I_OBJVERS                      TYPE        RSOBJVERS
* | [<-()] R_EXISTS                       TYPE        RS_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
method IF_RSPC_GET_VARIANT~EXISTS.

  "Check the variant exists.
  call method cl_rspc_variant=>exists
    exporting
      i_type    = 'YGTTC_PCMV'
      i_variant = i_variant
      i_objvers = i_objvers
    receiving
      r_exists  = r_exists.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YGTTC_CL_PCMV=>IF_RSPC_GET_VARIANT~GET_VARIANT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_TYPE                         TYPE        RSPC_TYPE(optional)
* | [--->] I_VARIANT                      TYPE        RSPC_VARIANT(optional)
* | [--->] I_T_CHAIN                      TYPE        RSPC_T_CHAIN(optional)
* | [--->] I_T_SELECT                     TYPE        RS_T_RSCEDST(optional)
* | [--->] I_OBJVERS                      TYPE        RSOBJVERS (default ='A')
* | [<---] E_VARIANT                      TYPE        RSPC_VARIANT
* | [<---] E_VARIANT_TEXT                 TYPE        RSTXTLG
* | [EXC!] NOTHING_SELECTED
* +--------------------------------------------------------------------------------------</SIGNATURE>
method IF_RSPC_GET_VARIANT~GET_VARIANT.

  types:
    ty_c200                  type c length 200.

  types:
    begin of ty_f4,
      variante               type rspc_variant,
      txtlg                  type rstxtlg,
    end of ty_f4.

  data:
    ls_f4                    type ty_f4,
    lt_f4                    type table of ty_f4.

  data:
    l_sqlwhere               type string.

  data:
    ls_value_tab             type ty_c200,
    lt_value_tab             type table of ty_c200,
    ls_field_tab             type dfies,
    lt_field_tab             type table of dfies,
    ls_return_tab            type ddshretval,
    lt_return_tab            type table of ddshretval.

  data:
    l_longtext               type string,
    lo_root                  type ref to cx_root.


  "Retrieve the existing variants.
  l_sqlwhere = `a~type = 'YGTTC_PCMV'`.

  if i_variant is not initial.

    find '*' in i_variant.
    if sy-subrc = 0.

      replace all occurrences of '*' in i_variant with '%'.
      l_sqlwhere = |{ l_sqlwhere } and a~variante like '{ i_variant }'|.
    else.
      l_sqlwhere = |{ l_sqlwhere } and a~variante = '{ i_variant }'|.
    endif.
  endif.

  try.

      select a~variante txtlg up to 20 rows
        into table lt_f4
        from rspcvariantattr as a
        left outer join rspcvariantt as b
          on a~variante = b~variante
        where (l_sqlwhere)
        group by a~variante txtlg.

    catch cx_sy_dynamic_osql_syntax into lo_root.
      l_longtext = lo_root->get_longtext( ).
    catch cx_sy_dynamic_osql_semantics into lo_root.
      l_longtext = lo_root->get_longtext( ).
  endtry.

  if sy-subrc <> 0.
    message 'No variants exist for this selection' type 'S'.
  endif.

  "Create the F4 dialogue.
  loop at lt_f4 into ls_f4.

    ls_value_tab = ls_f4-variante.
    append ls_value_tab to lt_value_tab.

    ls_value_tab = ls_f4-txtlg.
    append ls_value_tab to lt_value_tab.
  endloop.

  ls_field_tab-tabname = 'RSPCVARIANT'.
  ls_field_tab-fieldname = 'VARIANTE'.
  append ls_field_tab to lt_field_tab.

  ls_field_tab-tabname = 'RSPCVARIANTT'.
  ls_field_tab-fieldname = 'TXTLG'.
  append ls_field_tab to lt_field_tab.

  try.

      call function 'F4IF_INT_TABLE_VALUE_REQUEST'
        exporting
          retfield        = 'VARIANTE'
        tables
          value_tab       = lt_value_tab
          field_tab       = lt_field_tab
          return_tab      = lt_return_tab
        exceptions
          parameter_error = 1
          no_values_found = 2
          others          = 3.
      if sy-subrc <> 0.
        message id 'YGTBW' TYPE 'S' number 200
                    WITH 'Issue in F4 selection.'.
      endif.

    catch  cx_sy_dyn_call_illegal_type into lo_root.
      l_longtext = lo_root->get_longtext( ).
  endtry.

  "Return the selected values.
  loop at lt_return_tab into ls_return_tab.
    e_variant = ls_return_tab-fieldval.
    e_variant_text = ls_return_tab-fieldval.
  endloop.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YGTTC_CL_PCMV=>IF_RSPC_GET_VARIANT~WILDCARD_ENABLED
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RESULT                         TYPE        RS_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
method IF_RSPC_GET_VARIANT~WILDCARD_ENABLED.

  result = 'X'.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YGTTC_CL_PCMV=>IF_RSPC_MAINTAIN~GET_HEADER
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_VARIANT                      TYPE        RSPC_VARIANT
* | [--->] I_OBJVERS                      TYPE        RSOBJVERS (default ='A')
* | [<---] E_VARIANT_TEXT                 TYPE        RSTXTLG
* | [<---] E_S_CHANGED                    TYPE        RSTSTPINT
* | [<---] E_CONTREL                      TYPE        RSCONTREL
* | [<---] E_CONTTIMESTMP                 TYPE        RSCONTTIMESTMP
* +--------------------------------------------------------------------------------------</SIGNATURE>
method IF_RSPC_MAINTAIN~GET_HEADER.

  data:
    lo_variant               type ref to cl_rspc_variant.

  data:
    l_type                   type rspc_type,
    l_variant                type rspc_variant,
    l_objvers                type rsobjvers,
    ls_rspcvariantattr       type rspcvariantattr,
    lt_rspcvariant           type rspc_t_variant,
    ls_rspcvariantt          type rspcvariantt,
    lt_rspcvariantt          type rspc_t_variantt.


  "Create the variant.
  call method cl_rspc_variant=>create
    exporting
      i_type      = 'YGTTC_PCMV'
      i_variant   = i_variant
    receiving
      r_r_variant = lo_variant
    exceptions
      locked      = 1
      others      = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
               with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  call method lo_variant->get_info
    importing
      e_type              = l_type
      e_variant           = l_variant
      e_objvers           = l_objvers
      e_s_rspcvariantattr = ls_rspcvariantattr
      e_t_rspcvariant     = lt_rspcvariant
      e_s_rspcvariantt    = ls_rspcvariantt
      e_t_rspcvariantt    = lt_rspcvariantt.

  e_variant_text = ls_rspcvariantt-txtlg.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YGTTC_CL_PCMV=>IF_RSPC_MAINTAIN~MAINTAIN
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_VARIANT                      TYPE        RSPC_VARIANT(optional)
* | [--->] I_T_CHAIN                      TYPE        RSPC_T_CHAIN(optional)
* | [--->] I_DISPLAY_ONLY                 TYPE        RS_BOOL(optional)
* | [<---] E_VARIANT                      TYPE        RSPC_VARIANT
* | [<---] E_VARIANT_TEXT                 TYPE        RSTXTLG
* +--------------------------------------------------------------------------------------</SIGNATURE>
method IF_RSPC_MAINTAIN~MAINTAIN.

  data:
    lo_variant               type ref to cl_rspc_variant.

  data:
    l_type                   type rspc_type,
    l_variant                type rspc_variant,
    l_objvers                type rsobjvers,
    ls_rspcvariantattr       type rspcvariantattr,
    lt_rspcvariant           type rspc_t_variant,
    ls_rspcvariantt          type rspcvariantt,
    lt_rspcvariantt          type rspc_t_variantt.


  if i_variant is initial.
    message 'Enter valid technical name' type 'I'.
    return.
  endif.

  "Create the variant.
  call method cl_rspc_variant=>create
    exporting
      i_type      = 'YGTTC_PCMV'
      i_variant   = i_variant
    receiving
      r_r_variant = lo_variant
    exceptions
      locked      = 1
      others      = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
               with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  "Go to the maintenance screen.
  try.

      call function 'YGTTC_FM_PCMV'
        changing
          c_o_variant = lo_variant.

    catch cx_sy_dyn_call_param_not_found.
      message 'Invalid parameters' type 'W'. return.
  endtry.

  call method lo_variant->get_info
    importing
      e_type              = l_type
      e_variant           = l_variant
      e_objvers           = l_objvers
      e_s_rspcvariantattr = ls_rspcvariantattr
      e_t_rspcvariant     = lt_rspcvariant
      e_s_rspcvariantt    = ls_rspcvariantt
      e_t_rspcvariantt    = lt_rspcvariantt.

  e_variant = i_variant.
  e_variant_text = ls_rspcvariantt-txtlg.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YGTTC_CL_PCMV=>_FLAT_TO_DEEP_FILTER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_ALV_FILTER                  TYPE        T_ALV_FILTER
* | [<-()] RT_FILTER                      TYPE        T_FILTER
* +--------------------------------------------------------------------------------------</SIGNATURE>
method _flat_to_deep_filter.

  data:
    ls_alv_filter            type s_alv_filter.

  data:
    lt_range                 type t_range,
    lt_field                 type t_field,
    lt_filter                type t_filter.

  field-symbols:
    <ls_range>               type s_range,
    <ls_field>               type s_field,
    <ls_filter>              type s_filter.


  loop at it_alv_filter into ls_alv_filter.

    "Collect the filter groups.
    read table lt_filter assigning <ls_filter>
      with key
        group = ls_alv_filter-group.
    if sy-subrc <> 0.
      append initial line to lt_filter assigning <ls_filter>.
      <ls_filter>-group = ls_alv_filter-group.
    endif.

    "Collect the filter fields assigned to the filter group.
    read table <ls_filter>-t_field assigning <ls_field>
      with key
        fieldnm = ls_alv_filter-fieldnm.
    if sy-subrc <> 0.
      append initial line to <ls_filter>-t_field assigning <ls_field>.
      <ls_field>-fieldnm = ls_alv_filter-fieldnm.
    endif.

    "Collect the filter ranges assigned to the filter fields.
    unassign <ls_range>.
    append initial line to <ls_field>-t_range assigning <ls_range>.

    if <ls_range> is assigned.

      "Either a fixed value or an OLAP variable can be provided as a filter.
      if ls_alv_filter-vnam is not initial.

        <ls_range>-sign   = ''.
        <ls_range>-option = 'VN'. "Variable name.
        <ls_range>-low    = ls_alv_filter-vnam.
        <ls_range>-high   = ''.
      else.

        <ls_range>-sign   = ls_alv_filter-sign.
        <ls_range>-option = ls_alv_filter-option.
        <ls_range>-low    = ls_alv_filter-low.
        <ls_range>-high   = ls_alv_filter-high.
      endif.
    else.
      message id 'YGTBW' type 'E' number '200'
                 with 'Unable to append initial line to <LS_FIELD>-T_RANGE'.
    endif.
  endloop.

  rt_filter = lt_filter.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YGTTC_CL_PCMV=>_SET_ALV_FIELD_CATALOG
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_ALV_USE                      TYPE        STRING
* | [<-()] RT_LVC_FCAT                    TYPE        LVC_T_FCAT
* +--------------------------------------------------------------------------------------</SIGNATURE>
method _set_alv_field_catalog.

  data:
    ls_lvc_fcat              type lvc_s_fcat,
    lt_lvc_fcat              type lvc_t_fcat.


  if i_alv_use = 'GENERATE'.

    "Add a filter group column.
    ls_lvc_fcat-fieldname = 'GROUP'.
    ls_lvc_fcat-coltext   = 'Filter group'.
    ls_lvc_fcat-edit      = 'X'.
    ls_lvc_fcat-rollname  = 'NUMC2'.
    append ls_lvc_fcat to lt_lvc_fcat.
  endif.

  "Add fields for the filter definition.
  clear ls_lvc_fcat.
  ls_lvc_fcat-fieldname = 'FIELDNM'.
  ls_lvc_fcat-coltext   = 'Fieldname'.
  ls_lvc_fcat-rollname  = 'RSFIELDNM'.
  ls_lvc_fcat-edit      = 'X'.
  if i_alv_use = 'GENERATE'.
    ls_lvc_fcat-f4availabl = 'X'.
  endif.
  append ls_lvc_fcat to lt_lvc_fcat.

  clear ls_lvc_fcat.
  ls_lvc_fcat-fieldname = 'SIGN'.
  ls_lvc_fcat-coltext   = 'Sign'.
  ls_lvc_fcat-rollname  = 'RSSIGN'.
  ls_lvc_fcat-edit      = 'X'.
  append ls_lvc_fcat to lt_lvc_fcat.

  clear ls_lvc_fcat.
  ls_lvc_fcat-fieldname = 'OPTION'.
  ls_lvc_fcat-coltext   = 'Option'.
  ls_lvc_fcat-rollname  = 'RSOPTION'.
  ls_lvc_fcat-edit      = 'X'.
  append ls_lvc_fcat to lt_lvc_fcat.

  clear ls_lvc_fcat.
  ls_lvc_fcat-fieldname = 'LOW'.
  ls_lvc_fcat-coltext   = 'Low'.
  ls_lvc_fcat-rollname  = 'RSLOW'.
  ls_lvc_fcat-edit      = 'X'.
  append ls_lvc_fcat to lt_lvc_fcat.

  clear ls_lvc_fcat.
  ls_lvc_fcat-fieldname = 'HIGH'.
  ls_lvc_fcat-coltext   = 'High'.
  ls_lvc_fcat-rollname  = 'RSHIGH'.
  ls_lvc_fcat-edit      = 'X'.
  append ls_lvc_fcat to lt_lvc_fcat.

  "Add alternative OLAP variable entry.
  clear ls_lvc_fcat.
  ls_lvc_fcat-fieldname  = 'VNAM'.
  ls_lvc_fcat-coltext    = 'OLAP variables'.
  ls_lvc_fcat-rollname   = 'RSZVNAM'.
  ls_lvc_fcat-edit       = 'X'.
  ls_lvc_fcat-f4availabl = 'X'.
  ls_lvc_fcat-intlen     = 30.
  ls_lvc_fcat-outputlen  = 30.
  append ls_lvc_fcat to lt_lvc_fcat.

  "Add a text field column for the Local-chain.
  if i_alv_use = 'GENERATE'.

    clear ls_lvc_fcat.
    ls_lvc_fcat-fieldname = 'TXTLG'.
    ls_lvc_fcat-coltext   = 'Local-chain description'.
    ls_lvc_fcat-edit      = 'X'.
    ls_lvc_fcat-rollname  = 'RSTXTLG'.
    ls_lvc_fcat-lowercase = 'X'.
    ls_lvc_fcat-intlen    = 30.
    ls_lvc_fcat-outputlen = 30.
    append ls_lvc_fcat to lt_lvc_fcat.
  endif.

  rt_lvc_fcat = lt_lvc_fcat.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YGTTC_CL_PCMV=>_SET_FILTER_DTP
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DTP                          TYPE        RSBKDTPNM
* | [--->] IT_RSPCVARIANT                 TYPE        RSPC_T_VARIANT
* +--------------------------------------------------------------------------------------</SIGNATURE>
method _set_filter_dtp.

  data:
    lo_rsbk_dtp              type ref to cl_rsbk_dtp,
    lo_rsbc_filter           type ref to cl_rsbc_filter.

  data:
    ls_varseltab             type mch_var_select,
    lt_varseltab_before      type mch_t_var_select,
    lt_varseltab_after       type mch_t_var_select,
    ls_seltab                type rsbk_s_select,
    lt_seltab_before         type mch_t_select,
    lt_seltab_after          type mch_t_select,
    ls_dtprule               type mch_s_sourcecode,
    lt_dtprule               type mch_t_sourcecode,
    ls_selfields             type mch_s_field,
    lt_selfields             type mch_t_field.

  data:
    ls_rspcvariant           type rspcvariant,
    lt_rspcvariant           type rspc_t_variant.

  data:
    lo_root                  type ref to cx_root,
    l_longtext               type string,
    l_subrc                  type sysubrc,
    l_retry_count            type i,
    l_retry_max              type i value 10.


  "Create an instance of the DTP.
  lo_rsbk_dtp = cl_rsbk_dtp=>factory( |{ i_dtp }| ).

  "Get reference to DTP’s filter object.
  lo_rsbc_filter = lo_rsbk_dtp->get_obj_ref_filter( ).

  "Get the filter definition.
  call method lo_rsbc_filter->get_all
    importing
      e_t_varseltab = lt_varseltab_before
      e_t_seltab    = lt_seltab_before
      e_t_dtprule   = lt_dtprule
      e_t_selfields = lt_selfields.

  "Set the DTP to the modified version.
  call method lo_rsbk_dtp->if_rsbk_dtp_maintain~set_objstat
    exporting
      i_objstat = rs_c_objstat-inactive.

  "Define the DTP filter.
  loop at it_rspcvariant into ls_rspcvariant where fnam cp 'RANGE_*'.

    read table lt_varseltab_before transporting no fields
      with key
        field    = ls_rspcvariant-fnam+6
        sel_type = '6'.
    if sy-subrc = 0.
      message id 'YGTBW' type 'E' number '200'
                 with 'It is not possible to change field' ls_rspcvariant-fnam+6 'The field uses an ABAP routine'.
    endif.

    if ls_rspcvariant-opt = 'VN'.

      "Add OLAP variable selections.
      ls_varseltab-field        = ls_rspcvariant-fnam+6.
      ls_varseltab-sel_routine  = ''.
      ls_varseltab-bex_variable = ls_rspcvariant-low.
      ls_varseltab-bex_periv    = ''.
      ls_varseltab-sel_type     = '7'.
      append ls_varseltab to lt_varseltab_after.
    else.

      ls_seltab-field  = ls_rspcvariant-fnam+6.
      ls_seltab-sign   = ls_rspcvariant-sign.
      ls_seltab-option = ls_rspcvariant-opt.
      ls_seltab-low    = ls_rspcvariant-low.
      ls_seltab-high   = ls_rspcvariant-high.
      append ls_seltab to lt_seltab_after.
    endif.
  endloop.

  "Add the ABAP routines to the selection.
  loop at lt_varseltab_before into ls_varseltab where sel_type = '6'.
    append ls_varseltab to lt_varseltab_after.
  endloop.

  "Set the DTP filter.
  call method lo_rsbc_filter->set_all
    exporting
      i_t_varseltab = lt_varseltab_after
      i_t_seltab    = lt_seltab_after
      i_t_dtprule   = lt_dtprule
      i_t_selfields = lt_selfields.

  "Save DTP.
  clear l_retry_count.
  try.

      call method lo_rsbk_dtp->save
        importing
          e_subrc = l_subrc
        exceptions
          others  = 2.
      if l_subrc <> 0.
        raise exception type cx_rs_error_with_message.
      endif.

      message id 'YGTBW' type 'I' number '200' with i_dtp ` saved.`.

    catch cx_rs_error_with_message.

      if l_retry_count < l_retry_max.

        add 1 to l_retry_count.

        message id 'YGTBW' type 'I' number '200' with `Save ` i_dtp ` re-try.`.

        wait up to 10 seconds. retry.
      else.
        message id 'YGTBW' type 'E' number '200' with `Unable to save ` i_dtp.
      endif.
  endtry.

  "Activate DTP.
  clear l_retry_count.
  do.

    call method lo_rsbk_dtp->activate
      importing
        e_subrc = l_subrc.
    if l_subrc = 0.

      message id 'YGTBW' type 'I' number '200' with i_dtp ` activated.`.

      exit.
    endif.

    if l_retry_count < l_retry_max.

      add 1 to l_retry_count.

      message id 'YGTBW' type 'I' number '200' with `Activate ` i_dtp ` re-try.`.

      wait up to 10 seconds. continue.
    else.
      message id 'YGTBW' type 'E' number '200' with `Unable to activate ` i_dtp.
    endif.
  enddo.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YGTTC_CL_PCMV=>_SET_FILTER_IP
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_INFOPACKAGE                  TYPE        RSLOGDPID
* | [--->] IT_RSPCVARIANT                 TYPE        RSPC_T_VARIANT
* +--------------------------------------------------------------------------------------</SIGNATURE>
method _set_filter_ip.

  data:
    ls_details               type bapi6109,
    ls_scheduling_params     type bapi6109btch,
    ls_destinations          type bapi6109dest,
    ls_file_params           type bapi6109file,
    ls_hie_params            type bapi6109hie.

  data:
    ls_selections            type bapi6109sel,
    lt_selections            type table of bapi6109sel,
    lt_infocubes             type table of bapi6109ic,
    lt_third_party_params    type table of bapi6109tcp,
    lt_return                 type table of bapiret2.

  data:
    ls_rsldpsel              type rsldpsel,
    lt_rsldpsel_before       type table of rsldpsel,
    lt_rsldpsel_after        type table of rsldpsel,
    l_lnr                    type rsnrchar.

  field-symbols:
    <ls_rsldpsel>            type rsldpsel.

  data:
    l_type                   type rspc_type,
    l_variant                type rspc_variant,
    l_objvers                type rsobjvers,
    ls_rspcvariantattr       type rspcvariantattr,
    ls_rspcvariant           type rspcvariant,
    lt_rspcvariant           type rspc_t_variant,
    ls_rspcvariantt          type rspcvariantt,
    lt_rspcvariantt          type rspc_t_variantt.

  data:
    l_lock_success           type rs_bool.


  "Get the IP information before the change.
  call function 'BAPI_IPAK_GETDETAIL'
    exporting
      infopackage          = i_infopackage
      with_empty_selfields = 'X'
    importing
      details              = ls_details
      scheduling_params    = ls_scheduling_params
      destinations         = ls_destinations
      file_params          = ls_file_params
      hie_params           = ls_hie_params
    tables
      selections           = lt_selections
      infocubes            = lt_infocubes
      third_party_params   = lt_third_party_params
      return               = lt_return.

  "Get the selections before the change.
  select *
    into table lt_rsldpsel_before
    from rsldpsel
    where logdpid = i_infopackage and
          objvers = 'A' and
          vartyp = '6'.

  "Remove selections for the fields which will be changed filter.
  loop at it_rspcvariant into ls_rspcvariant where fnam(6) = 'RANGE_'.
    delete lt_selections where fieldname = ls_rspcvariant-fnam+6.
  endloop.

  "Move the RSPCVARIANT filter values into the InfoPackage filter format.
  loop at it_rspcvariant into ls_rspcvariant where fnam cp 'RANGE_*'.

    "Do not change ABAP routines.
    read table lt_rsldpsel_before transporting no fields
      with key
        fieldname = ls_rspcvariant-fnam+6
        vartyp = '6'. "ABAP Routine.
    if sy-subrc = 0.
      message id 'YGTBW' type 'E' number '200'
                 with 'It is not possible to change field' ls_rspcvariant-fnam+6 'The field uses an ABAP routine'.
    endif.

    ls_selections-fieldname = ls_rspcvariant-fnam+6.

    if ls_rspcvariant-opt = 'VN'.

      "Add a dummy selection for an OLAP variable, this is replaced later.
      ls_selections-sign      = 'I'.
      ls_selections-opt       = 'EQ'.
      ls_selections-low       = '1'.
      ls_selections-high      = '9'.
    else.

      ls_selections-sign      = ls_rspcvariant-sign.
      ls_selections-opt       = ls_rspcvariant-opt.
      ls_selections-low       = ls_rspcvariant-low.
      ls_selections-high      = ls_rspcvariant-high.
    endif.

    append ls_selections to lt_selections.
  endloop.

  "Add a dummy selection for ABAP routines, this is replaced later.
  loop at lt_rsldpsel_before into ls_rsldpsel where vartyp = '6'.

    ls_selections-fieldname = ls_rsldpsel-fieldname.
    ls_selections-sign      = 'I'.
    ls_selections-opt       = 'EQ'.
    ls_selections-low       = '1'.
    ls_selections-high      = '9'.
    append ls_selections to lt_selections.
  endloop.

  "Update the fixed selections.
  call function 'BAPI_IPAK_CHANGE'
    exporting
      infopackage = i_infopackage
    tables
      selections  = lt_selections
      return      = lt_return.

  "Get the InfoPackage selections after change.
  select *
    into table lt_rsldpsel_after
    from rsldpsel
    where logdpid = i_infopackage and
          objvers = 'A'.

  "Add the OLAP variable selections.
  loop at it_rspcvariant into ls_rspcvariant where fnam cp 'RANGE_*' and opt = 'VN'.

    read table lt_rsldpsel_after assigning <ls_rsldpsel>
      with key
        fieldname = ls_rspcvariant-fnam+6.
    if sy-subrc = 0.

      <ls_rsldpsel>-vartyp       = '7'.
      <ls_rsldpsel>-bex_variable = ls_rspcvariant-low.
      <ls_rsldpsel>-description  = 'YGTTC_PCMV'.

      "Clear dummy values.
      <ls_rsldpsel>-low = ''.
      <ls_rsldpsel>-high = ''.
    else.
      message id 'YGTBW' type 'E' number '200'
                 with 'Dummy record for OLAP variable missing.'.
    endif.
  endloop.

  "Add the ABAP Routine selections.
  loop at lt_rsldpsel_before into ls_rsldpsel where vartyp = '6'.

    read table lt_rsldpsel_after assigning <ls_rsldpsel>
      with key
        fieldname = ls_rsldpsel-fieldname.
    if sy-subrc = 0.

      "Store the line number and return in the step below.
      l_lnr = <ls_rsldpsel>-lnr.

      <ls_rsldpsel> = ls_rsldpsel.
      <ls_rsldpsel>-lnr = l_lnr.

      "Clear dummy values.
      <ls_rsldpsel>-low = ''.
      <ls_rsldpsel>-high = ''.
    else.
      message id 'YGTBW' type 'E' number '200'
                 with 'Dummy record for ABAP routine missing.'.
    endif.
  endloop.

  "Update the RSLDPSEL table.
  loop at lt_rsldpsel_after into ls_rsldpsel.

    clear l_lock_success.

    do 12 times.

      "Lock RSLDPSEL table.
      call function 'ENQUEUE_ERSLDPSEL'
        exporting
          logdpid        = ls_rsldpsel-logdpid
          objvers        = ls_rsldpsel-objvers
          lnr            = ls_rsldpsel-lnr
        exceptions
          foreign_lock   = 1
          system_failure = 2
          others         = 3.
      if sy-subrc <> 0.

        case sy-subrc.
          when 2.
            call function 'DEQUEUE_ALL'.
          when others.

            "Wait a combined maximum of 60 seconds.
            wait up to 5 seconds.
        endcase.
      else.
        l_lock_success = 'X'.
        exit.
      endif.
    enddo.

    "Raise error if lock is not executed.
    if l_lock_success <> 'X'.
      message id 'YGTBW' type 'E' number '200'
                 with 'Unable to lock table RSLDPSEL for' ls_rsldpsel-logdpid ls_rsldpsel-lnr 'after 12 attempts.'.
    endif.

    "Modify RSLDPSEL table.
    modify rsldpsel from ls_rsldpsel.

    "Unlock RSLDPSEL table.
    call function 'DEQUEUE_ERSLDPSEL'
      exporting
        logdpid = ls_rsldpsel-logdpid
        objvers = ls_rsldpsel-objvers
        lnr     = ls_rsldpsel-lnr.
  endloop.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YGTTC_CL_PCMV=>_VALIDATE_DEEP_FILTER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_FILTER                      TYPE        T_FILTER
* | [<-()] R_RC                           TYPE        SYSUBRC
* +--------------------------------------------------------------------------------------</SIGNATURE>
method _validate_deep_filter.

*/ Validating the filter definition when it is in the deep structure
*  makes it easy to validate values across multiple ranges. For
*  example if a variable filter is defined in one row no additional
*  selection is possible for that field. Validating in the deep
*  structure means these errors can easily be identified and an error
*  raised.

  field-symbols:
    <ls_range>               type s_range,
    <ls_field>               type s_field,
    <ls_filter>              type s_filter.

  data:
    l_spool                  type string.


  loop at it_filter assigning <ls_filter>.

    loop at <ls_filter>-t_field assigning <ls_field>.

      read table <ls_field>-t_range assigning <ls_range>
        with key
          option = 'VN'.
      if sy-subrc = 0 and lines( <ls_field>-t_range ) <> 1.


        l_spool = |Change filter for field { <ls_field>-fieldnm }.|.
        message id 'YGTBW' type 'W' number '200' with 'Invalid fixed and variable filter combination.' l_spool.

        r_rc = 4.
        return.
      endif.
    endloop.
  endloop.

  r_rc = 0.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method YGTTC_CL_PCMV=>_VALIDATE_FLAT_FILTER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_ALV_FILTER                  TYPE        T_ALV_FILTER
* | [<-()] R_RC                           TYPE        SYSUBRC
* +--------------------------------------------------------------------------------------</SIGNATURE>
method _validate_flat_filter.

*/ Validating the filter definition when it is in the flat structure
*  makes it easy to validate values across the structure. For example
*  if a fixed and variable filter is used in the same row this can
*  easily be identified and an error raised. This method returns the
*  row and column of any identified error, this is intended to be
*  used to highlight the location of errors in the ALV grid.

  data:
    ls_alv_filter            type s_alv_filter.

  data:
    l_spool                  type string.


  "Validate rows.
  loop at it_alv_filter into ls_alv_filter.

    "The fieldname must be provided.
    if ls_alv_filter-fieldnm is initial.

      l_spool = |Change line { sy-tabix }.|.
      message id 'YGTBW' type 'W' number '200' with 'Missing field name.' l_spool.

      sy-msgv1 = sy-tabix. "Row number.
      sy-msgv2 = 2. "Column number; fieldname.

      r_rc = 4.
      return.
    endif.

    "At least one filter value must be provided, either a fixed or variable filter.
    if ls_alv_filter-vnam is initial and
      ls_alv_filter-sign is initial and
      ls_alv_filter-option is initial and
      ls_alv_filter-low is initial and
      ls_alv_filter-high is initial.

      l_spool = |Change filter for field { ls_alv_filter-fieldnm }.|.
      message id 'YGTBW' type 'W' number '200' with 'Missing filter definition.' l_spool.

      sy-msgv1 = sy-tabix. "Row number.
      sy-msgv2 = ''. "Column number; no column provided.

      r_rc = 4.
      return.
    endif.

    "A fixed and variable filter cannot be combined in the row.
    if ls_alv_filter-vnam is not initial and
      ( ls_alv_filter-sign is not initial or
        ls_alv_filter-option is not initial or
        ls_alv_filter-low is not initial or
        ls_alv_filter-high is not initial
      ).

      l_spool = |Change filter for field { ls_alv_filter-fieldnm }.|.
      message id 'YGTBW' type 'W' number '200' with 'Invalid fixed and variable filter in row.' l_spool.

      sy-msgv1 = sy-tabix. "Row number.
      sy-msgv2 = ''. "Column number; no column provided.

      r_rc = 4.
      return.
    endif.
  endloop.

  r_rc = 0.
endmethod.
ENDCLASS.
