class YGTTC_CL_CBID_PACK definition
  public
  final
  create public .

public section.
*"* public components of class YGTTC_CL_CBID_PACK
*"* do not include other source files here!!!

  type-pools RS .
  types _USEGUI type RS_BOOL .
  types:
    begin of s_macro,
            str                    type string,
            substr                 type string,
            off                    type i,
            len                    type i,
            varnm                  type string,
            varvl                  type string,
          end of s_macro .
  types S_BCHATR type RSDBCHATR .
  types:
    t_bchatr type table of rsdbchatr .
  types FILE_FORMAT type STRING .
  types MAXROWS type I .
  types:
    begin of s_prov,
                  infoprov           type rsinfoprov,
                end of s_prov .
  types:
    t_prov                   type table of s_prov .
  types:
    begin of s_iobj,
                  iobjnm             type rsiobjnm,
                end of s_iobj .
  types:
    t_iobj                   type table of s_iobj .
  types:
    begin of s_cha_map,
                chanm                type rsiobjnm,
                from                 type rsiobjnm,
                to                   type rsiobjnm,
              end of s_cha_map .
  types:
    t_cha_map               type table of s_cha_map .
  types:
    begin of s_val_map,
        iobjnm               type rsiobjnm,
        from                 type rschavl,
        to                   type rschavl,
      end of s_val_map .
  types:
    t_val_map            type table of s_val_map .
  types:
    begin of s_for_all_entries_in,
                iobjnm               type rsiobjnm,
                t_cvc                type ref to data,
                cvc_flag             type rs_bool,
                sqlwhere             type string,
              end of   s_for_all_entries_in .
  types:
    t_for_all_entries_in     type table of s_for_all_entries_in .
  types:
    objnm                    type c length 30 .
  types:
    objtp                    type c length 2 .
  types:
    begin of s_obj_dta,
                  objnm              type c length 30,
                  objtp              type c length 2,
                  t_data             type ref to data,
                  t_file             type stringtab,
                  t_xfile            type xstringtab,
                end of s_obj_dta .
  types:
    t_obj_dta                type table of s_obj_dta .

  data GT_PROV_SRC type T_PROV .
  data GT_PROV_TRG type T_PROV .
  data GT_RSDRI_RANGE type RSDRI_T_RANGE .
  data GT_IOBJ_MA type T_IOBJ .
  data GT_IOBJ_MT type T_IOBJ .
  data G_DIRECTORY type STRING .
  data G_USEGUI type RS_BOOL .
  data G_GET_TD type RS_BOOL .
  data G_GET_MA type RS_BOOL .
  data G_GET_MT type RS_BOOL .
  data G_DELIMITER type C value '|'. "#EC NOTEXT .
  data G_SKIP_PACKAGES type I value 0. "#EC NOTEXT .
  data G_NO_OF_PACKAGES type I value 0. "#EC NOTEXT .
  data G_PACKAGE_SIZE type I value 10000. "#EC NOTEXT .
  data G_MAXROWS type MAXROWS value 10000. "#EC NOTEXT .
  data G_REQUEST type RSREQUNR value 'CBID_'. "#EC NOTEXT .
  data GT_MSG type RS_T_MSG .
  data GT_SPOOL type STRINGTAB .
  data G_RC type SYSUBRC .
  data G_FILE_FORMAT type FILE_FORMAT value 'CSV'. "#EC NOTEXT .
  data G_FILE_PREFIX type CHAR04 .
  data G_FILE_COMPRESS type RS_BOOL value 'X'. "#EC NOTEXT .
  data G_FILE_SEP_ROWS type RS_BOOL value 'X'. "#EC NOTEXT .
  data G_FILE_ROW_SEP type CHAR04 value '000A'. "#EC NOTEXT .
  data GS_LOG type BAL_S_LOG .
  data G_LOG_HANDLE type BALLOGHNDL .
  data G_APPL_LOG type RS_BOOL .
  data G_CURSOR type CURSOR .
  data GT_FOR_ALL_ENTRIES_IN type T_FOR_ALL_ENTRIES_IN .

  methods CONSTRUCTOR
    importing
      !I_T_PROV_SRC type T_PROV
      !I_T_PROV_TRG type T_PROV
      !I_T_RANGE type RSDRI_T_RANGE
      !I_T_IOBJ_MA type T_IOBJ
      !I_T_IOBJ_MT type T_IOBJ
      !I_T_CHA_MAP type T_CHA_MAP
      !I_T_VAL_MAP type T_VAL_MAP
      !I_DIRECTORY type STRING default 'C:\CBID\'
      !I_USEGUI type RS_BOOL default 'X'
      !I_GET_TD type CHAR01
      !I_GET_MA type CHAR01
      !I_GET_MT type CHAR01
      !I_APPL_LOG type RS_BOOL .
  methods IMPORT_WRITE_ALL .
  methods READ_EXPORT_ALL .
  methods IMPORT_WRITE
    importing
      !I_OBJNM type CHAR30
      !I_OBJTP type CHAR_02 default 'TD' .
  methods READ_EXPORT
    importing
      !I_OBJNM type CHAR30
      !I_OBJTP type CHAR_02 default 'TD' .
  methods IMPORT_CSV_PACK
    importing
      !I_OBJNM type CHAR30
      !I_OBJTP type CHAR_02 default 'TD'
      !I_PACKAGE_NUM type I default 1
    exceptions
      FILE_NOT_FOUND .
  methods EXPORT_CSV_PACK
    importing
      !I_OBJNM type CHAR30
      !I_OBJTP type CHAR_02 default 'TD'
      !I_PACKAGE_NUM type I default 1 .
  methods WRITE_TD_PACK
    importing
      !I_INFOPROV_SRC type RSINFOPROV
      !I_INFOPROV_TRG type RSINFOPROV .
  methods READ_TD_PACK
    importing
      !I_INFOPROV type RSINFOPROV
      !I_PACKAGE_NUM type I
    exceptions
      END_OF_DATA
      ERROR .
  methods READ_MD_PACK
    importing
      !I_IOBJNM type RSIOBJNM
      !I_OBJTP type OBJTP
      !I_PACKAGE_NUM type I
    exceptions
      END_OF_DATA
      ERROR .
  methods WRITE_MA_PACK
    importing
      !I_IOBJNM type RSIOBJNM .
  methods WRITE_MT_PACK
    importing
      !I_IOBJNM type RSIOBJNM .
  methods SET_DIRECTORY
    importing
      !I_DIRECTORY type STRING
      !I_USEGUI type RS_BOOL .
  methods SET_PARAMETER
    importing
      !I_PARAMETER type STRING
      !I_VALUE type STRING .
  methods VIEW_SPOOL .
protected section.
*"* protected components of class YGTTC_CL_CBID_PACK
*"* do not include other source files here!!!

  data GT_OBJ_DTA type T_OBJ_DTA .
  data GT_CHA_MAP type T_CHA_MAP .
  data GT_VAL_MAP type T_VAL_MAP .
private section.
*"* private components of class YGTTC_CL_CBID_PACK
*"* do not include other source files here!!!

  types:
    begin of s_map,
        maptp type c length 30,
        mapfr type c length 30,
        mapto type c length 30,
      end of s_map .
  types:
    h_map                    type hashed table of  s_map
                                    with unique key
                                      maptp
                                      mapfr .
  types DOKU_INIT_OBJ_SEL type DOKU .
  types DOKU_INIT_CHA_SEL type DOKU .
  types DOKU_INIT_DTA_TYP type DOKU .
  types DOKU_INIT_FILE type DOKU .
  types DOKU_ACTION type DOKU .
  types DOKU_OPTION type DOKU .
  types DOKU_FAQ type DOKU .

  data G_CCCATEGORY type CCCATEGORY .
  type-pools RSD .
  data GT_DTA type RSD_T_DTA .
  data GT_DTA_IOBJ type RSD_T_DTA_IOBJ .
  data GT_DTA_PRO type RSD_T_DTA_PRO .
  data GT_IOBJ_CMP type RSD_T_IOBJ_CMP .
  data GT_VIOBJ type RSD_T_VIOBJ .
  data GH_VIOBJ type RSD_TH_VIOBJ .
  data GH_MAP type H_MAP .
  data GT_BCHATR type T_BCHATR .
  data GT_IOBJ type T_IOBJ .
  data GT_PROV type T_PROV .
  data G_INFOPROV type RSINFOPROV .
  data GS_MACRO type S_MACRO .
  data G_MESSAGE type STRING .
  data G_S_MSG type RS_S_MSG .
  data GO_RSDRI_INFOPROV type ref to CL_RSDRI_INFOPROV .

  class-methods _IOBJNM_DTELNM
    importing
      !I_IOBJNM type RSIOBJNM
    returning
      value(R_DTELNM) type STRING .
  methods IOBJNM_FIELDNM
    importing
      !I_IOBJNM type RSIOBJNM
    returning
      value(R_FIELDNM) type STRING .
  methods FIELDNM_IOBJNM
    importing
      !I_FIELDNM type RSFIELDNM
    returning
      value(R_IOBJNM) type RSIOBJNM .
  class-methods _CHANM_CHABASNM
    importing
      !I_CHANM type RSCHANM
    returning
      value(R_CHABASNM) type RSCHABASNM .
  class-methods _MSG_SPOOL_RC
    importing
      !I_MSG type ANY
      !I_V1 type ANY
      !I_V2 type ANY
      !I_V3 type ANY
      !I_V4 type ANY
    exporting
      !E_T_MSG type RS_T_MSG
      !E_T_SPOOL type STRINGTAB
      !E_RC type I .
  methods CONVERT_FILE_TO_DATA
    importing
      !I_OBJNM type OBJNM
      !I_OBJTP type OBJTP .
  methods CONVERT_DATA_TO_FILE
    importing
      !I_OBJNM type OBJNM
      !I_OBJTP type OBJTP .
  methods GET_IOBJ_RANGE
    importing
      !I_IOBJNM type RSIOBJNM
    returning
      value(R_T_DATA) type ref to DATA .
  methods CREATE_IOBJNM_CVC
    importing
      !I_IOBJNM type RSIOBJNM .
  methods _ADD_SPOOL
    importing
      !I_MESSAGE type STRING .
ENDCLASS.



CLASS YGTTC_CL_CBID_PACK IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YGTTC_CL_CBID_PACK->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_T_PROV_SRC                   TYPE        T_PROV
* | [--->] I_T_PROV_TRG                   TYPE        T_PROV
* | [--->] I_T_RANGE                      TYPE        RSDRI_T_RANGE
* | [--->] I_T_IOBJ_MA                    TYPE        T_IOBJ
* | [--->] I_T_IOBJ_MT                    TYPE        T_IOBJ
* | [--->] I_T_CHA_MAP                    TYPE        T_CHA_MAP
* | [--->] I_T_VAL_MAP                    TYPE        T_VAL_MAP
* | [--->] I_DIRECTORY                    TYPE        STRING (default ='C:\CBID\')
* | [--->] I_USEGUI                       TYPE        RS_BOOL (default ='X')
* | [--->] I_GET_TD                       TYPE        CHAR01
* | [--->] I_GET_MA                       TYPE        CHAR01
* | [--->] I_GET_MT                       TYPE        CHAR01
* | [--->] I_APPL_LOG                     TYPE        RS_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
method constructor.

  data:
    ls_prov                  type s_prov,
    lt_prov                  type t_prov,
    ls_iobj                  type s_iobj,
    lt_iobj                  type t_iobj.

  data:
    l_cls                  type c length 3.

  data:
    lo_rsd_dta               type ref to cl_rsd_dta,
    ls_dta                   type rsd_s_dta,
    ls_dta_iobj              type rsd_s_dta_iobj,
    lt_dta_iobj              type rsd_t_dta_iobj,
    ls_dta_pro               type rsd_s_dta_pro,
    lt_dta_pro               type rsd_t_dta_pro,
    ls_iobj_cmp              type rsd_s_iobj_cmp,
    lt_iobj_cmp              type rsd_t_iobj_cmp.

  data:
    lo_rsd_iobj              type ref to cl_rsd_iobj,
    ls_viobj                 type rsd_s_viobj,
    lt_viobj                 type rsd_t_viobj,
    ls_cmp                   type rsdo_s_cmp,
    lt_cmp                   type rsdo_t_cmp.

  data:
    l_logsys                 type t000-logsys.

  field-symbols:
    <l_macro_any>            type any.
  "End of variable declaration.


  "Check calling program for production systems.
  select single cccategory
    into g_cccategory
    from t000
    where mandt = sy-mandt.
  if sy-subrc = 0.

    if sy-sysid ='MQ8' or
      sy-sysid ='FR8' or
      sy-sysid ='PR8' or
      sy-sysid ='SR8' or
      sy-sysid ='G13' or
      sy-sysid ='G53' or
      sy-sysid ='G63'.

      clear g_cccategory.
    endif.

    if g_cccategory = 'P'.

      if sy-cprog <> 'YGTTC_CBID_PACK'.
        message 'Not authorised. Try program YGTTC_CBID_PACK.' type 'E'.
      endif.
    endif.
  endif.

  "Hard coded user list.
  case sy-uname.                                           "#EC USER_OK
    when
      'APLOSS'   or
      'BVARA'    or
      'CHANNE2'  or
      'EDEGROO'  or
      'ERODRIG2' or
      'FSPEDIC'  or
      'KCALLIN'  or
      'MTIKKA2'  or
      'MSTANDA'  or
      'INAIRN'   or
      'PBOLLIA'  or
      'PKAKARA'  or
      'PMATHEW'  or
      'PPATEL'   or
      'RGREEN2'  or
      'RROBINS'  or
      'SBUXTON'  or
      'SCOBOS'   or
      'SGIORGI'  or
      'SSOMAY'   or
      'SPERALE'  or
      'TREVESZ'  or
      'TXU1'     or
      'VGUNTIM'  or
      'VTAMMIR'.
    when others.
      message 'No authorisation for program in production.' type 'E'.
  endcase.

  "Validate entry valid combinations.
  if i_get_td = '' and
    ( i_get_ma = '' and i_t_iobj_ma is initial ) and
    ( i_get_mt = '' and i_t_iobj_mt is initial ).

    message 'At least one type of data must be selected i.e. TD, MA, or MT' type 'E'.
  endif.

  if i_get_td = 'X' and i_t_prov_src is initial.
    message 'Provide at least one source InfoProvider' type 'E'.
  endif.

  if i_get_ma = 'X' and i_t_prov_src is initial and i_t_iobj_ma is initial.
    message 'Provide at least one InfoProvider or Characteristic for Attributes' type 'E'.
  endif.

  if i_get_mt = 'X' and i_t_prov_src is initial and i_t_iobj_mt is initial.
    message 'Provide at least one InfoProvider or Characteristic for Texts' type 'E'.
  endif.

  "Set the class-data at the time of the initialisation.
  gt_rsdri_range = i_t_range.

  gt_cha_map = i_t_cha_map.
  gt_val_map = i_t_val_map.

  g_get_td = i_get_td.
  g_get_ma = i_get_ma.
  g_get_mt = i_get_mt.

  "Set the directory provided.
  set_directory( i_directory = i_directory i_usegui = i_usegui ).

  g_appl_log = i_appl_log.

  "Create a valid list of InfoProviders and get the meta-data.
  do 2 times.

    "Check InfoProvider meta-data for source then target InfoProviders.
    case sy-index.
      when 1.
        lt_prov = i_t_prov_src.
        l_cls = 'SRC'.
      when 2.
        lt_prov = i_t_prov_trg.
        l_cls = 'TRG'.
    endcase.

    loop at lt_prov into ls_prov.

      "Check if the InfoProvider has already been processed.
      read table gt_dta into ls_dta
        with key
          infoprov = ls_prov-infoprov.
      if sy-subrc <> 0.

        "Create an instance of the InfoProvider.
        call method cl_rsd_dta=>factory
          exporting
            i_infoprov = ls_prov-infoprov
          receiving
            r_r_dta    = lo_rsd_dta
          exceptions
            not_found  = 1
            others     = 2.
        if sy-subrc <> 0.

          "Add spool message.
          _add_spool( |{ `<W14>` && ls_prov-infoprov && ` is not valid and will be ignored` }| ).
          continue.
        else.

          "Get the meta-data for the InfoProvider.
          call method lo_rsd_dta->if_rsd_dta~dta_get_info
            exporting
              i_with_atr_nav   = rs_c_true
              i_with_meta_iobj = rs_c_true
            importing
              e_s_dta          = ls_dta
              e_t_dta_iobj     = lt_dta_iobj
              e_t_dta_pro      = lt_dta_pro
              e_t_iobj_cmp     = lt_iobj_cmp
            exceptions
              dta_not_found    = 1
              iobj_not_found   = 2
              objvers_invalid  = 3
              others           = 4.
          if sy-subrc <> 0.

            "Add spool message.
            _add_spool( |{ `<W14> Meta-data for ` && ls_prov-infoprov && ` cannot be found` }| ).
            continue.
          endif.
        endif.

        "Move local meta-data to class data.
        append ls_dta to gt_dta.
        append lines of lt_dta_iobj to gt_dta_iobj.
        append lines of lt_dta_pro to gt_dta_pro.
        append lines of lt_iobj_cmp to gt_iobj_cmp.
      endif.

      "Add the InfoProvider to the list of valid InfoProviders.
      case l_cls.
        when 'SRC'.
          if i_get_td = 'X'.
            append ls_prov to gt_prov_src.
          endif.
        when 'TRG'.

          if not ( ls_dta-tlogo = 'CUBE' or ls_dta-tlogo = 'ODSO' ).
            _add_spool( `<W14>` && ls_prov-infoprov && ` is neither an InfoCube nor DSO object` ).
          endif.

          if i_get_td = 'X'.
            append ls_prov to gt_prov_trg.
          endif.
      endcase.
    endloop.
  enddo.

  sort gt_prov.
  delete adjacent duplicates from gt_prov.

  "Create a list of valid Infoobjects and get the meta-data.
  do 2 times.

    "Check InfoObject meta-data for Attributes then Texts.
    case sy-index.
      when 1.

        lt_iobj = i_t_iobj_ma.

        "If the I_GET_MA flag is checked then this indicates that
        "all Characteristics of the InfoProviders should be
        "considered for processing.
        if i_get_ma = 'X'.

          loop at gt_dta_iobj into ls_dta_iobj.

            check ls_dta_iobj-iobjtp = 'CHA' and ls_dta_iobj-atrnavfl = ''.
            append ls_dta_iobj-iobjnm to lt_iobj.
          endloop.
        endif.
        l_cls = 'MA'.
      when 2.

        lt_iobj = i_t_iobj_mt.

        "If the I_GET_MT flag is checked then this indicates that
        "all Characteristics of the InfoProviders should be
        "considered for processing.
        if i_get_mt = 'X'.

          loop at gt_dta_iobj into ls_dta_iobj.

            check ls_dta_iobj-iobjtp = 'CHA' and ls_dta_iobj-atrnavfl = ''.
            append ls_dta_iobj-iobjnm to lt_iobj.
          endloop.
        endif.
        l_cls = 'MT'.
    endcase.

    sort lt_iobj.
    delete adjacent duplicates from lt_iobj.

    loop at lt_iobj into ls_iobj.

      "Check if the InfoProvider has already been processed.
      read table gt_viobj into ls_viobj
        with key
          iobjnm = ls_iobj-iobjnm.
      if sy-subrc <> 0.

        "Create an instance of the InfoObject.
        call method cl_rsd_iobj=>factory
          exporting
            i_iobjnm      = ls_iobj-iobjnm
          receiving
            r_r_iobj      = lo_rsd_iobj
          exceptions
            input_invalid = 1
            others        = 2.
        if sy-subrc <> 0.

          "Add spool message.
          _add_spool( |{ `<W14>` && ls_iobj-iobjnm && ` is not valid and will be ignored` }| ).
          continue.
        endif.

        "Get the meta-data for the InfoObject.
        call method lo_rsd_iobj->get_info
          exporting
            i_objvers = rs_c_objvers-active
          importing
            e_s_viobj = ls_viobj
            e_t_cmp   = lt_cmp
          exceptions
            not_found = 1
            others    = 2.
        if sy-subrc <> 0.

          "Add spool message.
          _add_spool( |{ `<E14>Meta-data for ` && ls_iobj-iobjnm && ` cannot be found` }| ).
          continue.
        endif.

        "Move meta-data for InfoObject to class data.
        append ls_viobj to gt_viobj.

        "Move compounding meta-data to class data.
        if lt_cmp is not initial.

          read table gt_iobj_cmp transporting no fields
            with key
              iobjnm = ls_iobj-iobjnm.
          if sy-subrc <> 0.

            loop at lt_cmp  into ls_cmp.

              move-corresponding ls_cmp to ls_iobj_cmp.
              ls_iobj_cmp-iobjnm  = ls_viobj-iobjnm.
              ls_iobj_cmp-objvers = 'A'.
              append ls_iobj_cmp to gt_iobj_cmp.
            endloop.
          endif.
        endif.
      endif.

      "Add the InfoObject to the list of valid InfoObjects.
      case l_cls.
        when 'MA'.

          "Determine if the InfoObject has Attributes.
          if ls_viobj-attribfl = 'X'.

            append ls_iobj to gt_iobj_ma.
          else.

            read table i_t_iobj_ma transporting no fields
              with key
                iobjnm = ls_iobj-iobjnm.
            if sy-subrc = 0.

              "Add spool message.
              _add_spool( |{ `<W14>` && ls_iobj-iobjnm && ` is marked for Attribute read/write but has no Attributes` }| ).
            endif.
          endif.

        when 'MT'.

          "Determine if the InfoObject has Texts.
          if ls_viobj-txttabfl = '1'.

            append ls_iobj to gt_iobj_mt.
          else.

            read table i_t_iobj_mt transporting no fields
              with key
                iobjnm = ls_iobj-iobjnm.
            if sy-subrc = 0.

              "Add spool message.
              _add_spool( |{ `<W14>` && ls_iobj-iobjnm && ` is marked for Text read/write but has no Texts` }| ).
            endif.
          endif.
        when others.
      endcase.
    endloop.
  enddo.

  "Get all the additional compounded and reference InfoObjects.
  clear lt_iobj.
  append lines of gt_iobj_ma to lt_iobj.
  append lines of gt_iobj_mt to lt_iobj.

  if lt_iobj is not initial.

    select iobjcmp
      appending table lt_iobj
      from rsdiobjcmp
      for all entries in lt_iobj
      where iobjnm = lt_iobj-iobjnm and
            objvers = 'A'.
  endif.

  if lt_iobj is not initial.

    select chabasnm
      appending table lt_iobj
      from rsdcha
      for all entries in lt_iobj
      where chanm = lt_iobj-iobjnm and
            objvers = 'A'.
  endif.

  sort lt_iobj.
  delete adjacent duplicates from lt_iobj.

  loop at lt_iobj into ls_iobj.

    read table gt_viobj transporting no fields
      with key
        iobjnm = ls_iobj-iobjnm.
    if sy-subrc = 0.
      continue.
    endif.

    "Create an instance of the InfoObject.
    call method cl_rsd_iobj=>factory
      exporting
        i_iobjnm      = ls_iobj-iobjnm
      receiving
        r_r_iobj      = lo_rsd_iobj
      exceptions
        input_invalid = 1
        others        = 2.
    if sy-subrc <> 0.

      "Add spool message.
      _add_spool( |{ `<W14>` && ls_iobj-iobjnm && ` is not valid and will be ignored` }| ).
      continue.
    endif.

    "Get the meta-data for the InfoObject.
    call method lo_rsd_iobj->get_info
      exporting
        i_objvers = rs_c_objvers-active
      importing
        e_s_viobj = ls_viobj
        e_t_cmp   = lt_cmp
      exceptions
        not_found = 1
        others    = 2.
    if sy-subrc <> 0.

      "Add spool message.
      _add_spool( |{ `<E14>Meta-data for ` && ls_iobj-iobjnm && ` cannot be found` }| ).
      continue.
    endif.

    "Move meta-data for InfoObject to class data.
    append ls_viobj to gt_viobj.

    "Move compounding meta-data to class data.
    if lt_cmp is not initial.

      read table gt_iobj_cmp transporting no fields
        with key
          iobjnm = ls_iobj-iobjnm.
      if sy-subrc <> 0.

        loop at lt_cmp  into ls_cmp.

          move-corresponding ls_iobj_cmp to ls_cmp.
          ls_iobj_cmp-iobjnm  = ls_viobj-iobjnm.
          ls_iobj_cmp-objvers = 'A'.
          append ls_iobj_cmp to gt_iobj_cmp.
        endloop.
      endif.
    endif.
  endloop.

  if lt_iobj is not initial.

    select *
      into table gt_bchatr
      from rsdbchatr
      for all entries in lt_iobj
      where chabasnm = lt_iobj-iobjnm and
            objvers = 'A'.
  endif.

  "Create hashed table equivelents for performance.
  sort gt_viobj.
  delete adjacent duplicates from gt_viobj comparing iobjnm objvers.
  gh_viobj[] = gt_viobj[].

  "Create the log object.
  if i_appl_log = 'X'.

    if g_log_handle is initial.

      gs_log-object    = 'RSBATCH'.
      gs_log-subobject = 'PROT'.
      try.
          gs_log-extnumber = |{ 'ZCBID_' && cl_system_uuid=>create_uuid_c26_static( ) }|.
          gs_log-extnumber = gs_log-extnumber(30).
        catch cx_uuid_error.
      endtry.
      gs_log-alprog = 'YGTTC_CBID_PACK'.

      call function 'BAL_LOG_CREATE'
        exporting
          i_s_log                 = gs_log
        importing
          e_log_handle            = g_log_handle
        exceptions
          log_header_inconsistent = 1
          others                  = 2.
      if sy-subrc <> 0.
        return.
      endif.
    endif.
  endif.
endmethod.                    "constructor


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YGTTC_CL_CBID_PACK->CONVERT_DATA_TO_FILE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_OBJNM                        TYPE        OBJNM
* | [--->] I_OBJTP                        TYPE        OBJTP
* +--------------------------------------------------------------------------------------</SIGNATURE>
method convert_data_to_file.

  data:
    ls_dta                   type rsd_s_dta.

  data:
    ls_dta_iobj              type rsd_s_dta_iobj.

  data:
    l_csv                    type string,
    lt_csv                   type stringtab.

  data:
    l_dat                    type string,
    lt_dat                   type stringtab.

  data:
    l_off                    type i,
    l_len                    type i.

  data:
    l_text                   type ref to data,
    l_textlen                type i.

  field-symbols:
    <l_text>                 type any.

  data:
    lo_typedescr             type ref to cl_abap_typedescr,
    lo_structdescr           type ref to cl_abap_structdescr,
    lo_tabledescr            type ref to cl_abap_tabledescr,
    lt_component             type abap_component_tab,
    ls_component             type abap_componentdescr.

  field-symbols:
    <ls_component>           type abap_componentdescr.

  data:
    l_data                   type string.

  data:
    l_line_num               type i.

  field-symbols:
    <l_data>                 type any,
    <ls_data>                type any,
    <lt_data>                type standard table,
    <ls_obj_dta>             type s_obj_dta.

  data:
    l_iobjnm                 type rsiobjnm.

  _add_spool( `@5B@ Covert data to file: Object ` && i_objnm && `/` && i_objtp ).

  read table gt_obj_dta assigning <ls_obj_dta>
    with key
      objnm = i_objnm
      objtp = i_objtp.
  if sy-subrc <> 0.
    _add_spool( `@5D@Covert data to file: No data found for ` && i_objnm && `_` && i_objtp ).
    return.
  else.

    assign <ls_obj_dta>-t_data->* to <lt_data>.

    if lines( <lt_data> ) = 0.
      _add_spool( `@5D@Covert data to file: No data found for ` && i_objnm && `_` && i_objtp ).
      clear <ls_obj_dta>-t_file.
      return.
    endif.
  endif.

  case g_file_format.
    when 'CSV'.

      "Get the components of the structure, these will be in the
      "IOBJNM format for InfoCubes and in the FIELDNM format for
      "DSO object and master data objects.
      try.

*<NCE>    lo_tabledescr  ?= cl_abap_typedescr=>describe_by_data_ref( <ls_obj_dta>-t_data ).
          call method cl_abap_typedescr=>describe_by_data_ref
            exporting
              p_data_ref           = <ls_obj_dta>-t_data
            receiving
              p_descr_ref          = lo_typedescr
            exceptions
              reference_is_initial = 1
              others               = 2.
          if sy-subrc <> 0.
            message id sy-msgid type sy-msgty number sy-msgno
                       with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          endif.

          lo_tabledescr ?= lo_typedescr.
*</NCE>
          lo_structdescr ?= lo_tabledescr->get_table_line_type( ).
          lt_component    = lo_structdescr->get_components( ).

        catch:
          cx_sy_struct_comp_name.
      endtry.

      loop at lt_component assigning <ls_component>.
        if i_objtp = 'TD'.

          read table gt_dta into ls_dta
            with key
              infoprov = i_objnm.
          if sy-subrc <> 0.
            return.
          endif.

          case ls_dta-tlogo.
            when 'CUBE'.
              l_iobjnm = <ls_component>-name.
            when 'ODSO'.
              l_iobjnm = fieldnm_iobjnm( |{ <ls_component>-name }| ).
          endcase.

          read table gt_dta_iobj into ls_dta_iobj
            with key
              infoprov = i_objnm
              iobjnm = l_iobjnm.
          if sy-subrc <> 0.
            clear <ls_component>.
            continue.
          endif.

          if ls_dta_iobj-atrnavfl = 'X' or ls_dta_iobj-iobjtp = 'DPA' .
            clear <ls_component>.
            continue.
          endif.
        else.
          l_iobjnm = fieldnm_iobjnm( |{ <ls_component>-name }| ).
        endif.

        l_csv = l_csv && l_iobjnm && `|`.
      endloop.

      "Add the header to the export file.
      l_csv = shift_right( val = l_csv places  = 1 ).
      append l_csv to lt_csv.

      assign <ls_obj_dta>-t_data->* to <lt_data>.

      "Add the lines of the internal table to the file.
      loop at <lt_data> assigning <ls_data>.

        clear l_csv.

        loop at lt_component into ls_component.

          unassign <l_data>.
          assign component ls_component-name of structure <ls_data> to <l_data>.
          if <l_data> is assigned.

            try.

                l_data = <l_data>.

              catch cx_sy_conversion_error.
                exit.
            endtry.

            l_csv = l_csv && l_data &&'|'.
          endif.
        endloop.

        l_csv = shift_right( val = l_csv places  = 1 ).
        append l_csv to lt_csv.
      endloop.

      <ls_obj_dta>-t_file = lt_csv.

    when 'DAT'.

      lo_tabledescr ?= cl_abap_typedescr=>describe_by_data( <lt_data> ).
      lo_structdescr ?= lo_tabledescr->get_table_line_type( ).
      lt_component = lo_structdescr->get_components( ).

      loop at lt_component into ls_component.
        add ls_component-type->length to l_textlen.
      endloop.

      create data l_text type c length l_textlen.
      assign l_text->* to <l_text>.

      loop at <lt_data> assigning <ls_data>.

        l_line_num = sy-tabix.

        clear: l_len, l_off, <l_text>.

        loop at lt_component into ls_component.

          assign component sy-tabix of structure <ls_data> to <l_data>.
          l_len = ls_component-type->length.

          try.

              <l_text>+l_off(l_len) = <l_data>.
            catch cx_sy_conversion_error.
              _add_spool( `@5C@ Convert file to data: Conversion error. Line ` && l_line_num ).
              clear <ls_data>.
              return.
          endtry.

          try. "NCE jibberish
              l_off = l_off + l_len.
            catch cx_sy_arithmetic_overflow.
          endtry.
        endloop.

        l_dat = <l_text>.
        append l_dat to lt_dat.
      endloop.

      <ls_obj_dta>-t_file = lt_dat.

    when others.
      message 'Invalid File Format' type 'E'.
  endcase.
endmethod.                    "convert_data_to_csv


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YGTTC_CL_CBID_PACK->CONVERT_FILE_TO_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_OBJNM                        TYPE        OBJNM
* | [--->] I_OBJTP                        TYPE        OBJTP
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method convert_file_to_data.

    data:
      ls_dta                   type rsd_s_dta,
      ls_viobj                 type rsd_s_viobj.

    field-symbols:
      <ls_obj_dta>             type s_obj_dta.

    data:
      l_structname             type string.

    data:
      ls_data                  type ref to data.

    field-symbols:
      <l_data>                 type any,
      <ls_data>                type any,
      <lt_data>                type standard table.

    data:
      l_header_line            type string,
      l_data_line              type string,
      l_field                  type string,
      lt_field                 type stringtab,
      l_value                  type string,
      lt_value                 type stringtab.

    data:
      l_line_num               type i.

    data:
      lo_structdescr         type ref to cl_abap_structdescr,
      lo_tabledescr          type ref to cl_abap_tabledescr,
      ls_component           type abap_componentdescr,
      lt_component           type abap_component_tab.

    data:
      l_text                 type ref to data,
      l_textlen              type i.

    field-symbols:
      <l_text>               type any.

    data:
      l_off                  type i,
      l_len                  type i.

    data begin of ls_val_map_x.
    data fieldnm           type rsfieldnm.
    include                type s_val_map.
    data end of ls_val_map_x.

    data:
      ls_val_map             type s_val_map,
      lt_val_map_x           like table of ls_val_map_x.
    "End of variable declaration.


    "Get the file data from the internal variable.
    read table gt_obj_dta assigning <ls_obj_dta>
      with key
        objnm = i_objnm
        objtp = i_objtp.
    if sy-subrc <> 0.
      _add_spool( `@5D@ Convert file to data: No file data found for Object: ` && i_objnm && `_` && i_objtp ).
      return.
    else.
      _add_spool( `@5B@ Convert file to data: File to data conversion for Object: ` && i_objnm && `_` && i_objtp ).
    endif.

    "Determine which structure the data should be stored in.
    case i_objtp.
      when 'TD'.
        read table gt_dta into ls_dta
          with key
            infoprov = i_objnm.
        if sy-subrc = 0.

          case ls_dta-tlogo.
            when 'CUBE'.
              l_structname = ls_dta-viewtiobjnm2.
            when 'ODSO'.
              l_structname = ls_dta-viewiobj.
            when others.
              message 'Invalid InfoProvider type' type 'E'.
          endcase.
        endif.
      when 'MA'.
        l_structname = replace( val = _iobjnm_dtelnm( _chanm_chabasnm( i_objnm ) ) sub = '/OI' with = '/P' ).
      when 'MT'.
        l_structname = replace( val = _iobjnm_dtelnm( _chanm_chabasnm( i_objnm ) ) sub = '/OI' with = '/T' ).
    endcase.

    "Create the object to hold the file data.
    try.

        create data ls_data type (l_structname).
        create data <ls_obj_dta>-t_data type table of (l_structname).

      catch cx_sy_create_data_error.
        _add_spool( `@5C@ Convert file to data: Unable to create structure ` && l_structname ).
        return.
    endtry.

    assign ls_data->* to <ls_data>.
    assign <ls_obj_dta>-t_data->* to <lt_data>.

    "Move the lines of the file to the data object.
    case g_file_format.
      when 'CSV'.

        "Get the file header.
        if lines( <ls_obj_dta>-t_file ) >= 1.

          read table <ls_obj_dta>-t_file into l_header_line index 1.

          split l_header_line at g_delimiter into table lt_field.

          "If the target is not an InfoCube then convert the InfoObject names to field names.
          loop at lt_field assigning <l_data>.

            if ls_dta-tlogo <> 'CUBE'.
              <l_data> = iobjnm_fieldnm( |{ <l_data> }| ).
            endif.
          endloop.
        endif.

        "Loop through the file data.
        loop at <ls_obj_dta>-t_file into l_data_line.

          l_line_num = sy-tabix.

          "Skip the header row.
          check sy-tabix <> 1.

          split l_data_line at g_delimiter into table lt_value.

          loop at lt_value into l_value.

            read table lt_field into l_field index sy-tabix.

            assign component l_field of structure <ls_data> to <l_data>.

            try.

                <l_data> = l_value.

              catch cx_sy_conversion_error.
                _add_spool( `@5D@ Convert file to data: Conversion error. Line: ` && l_line_num && ` Field: ` && l_field ).
                clear <ls_data>.
                exit.
            endtry.
          endloop.

          append <ls_data> to <lt_data>.
        endloop.

      when 'DAT'.

        lo_tabledescr ?= cl_abap_typedescr=>describe_by_data( <lt_data> ).
        lo_structdescr ?= lo_tabledescr->get_table_line_type( ).
        lt_component = lo_structdescr->get_components( ).

        loop at lt_component into ls_component.
          add ls_component-type->length to l_textlen.
        endloop.

        create data l_text type c length l_textlen.
        assign l_text->* to <l_text>.

        loop at <ls_obj_dta>-t_file into l_data_line.

          l_line_num = sy-tabix.

          <l_text> = l_data_line.
          clear: l_len, l_off, <ls_data>.

          loop at lt_component into ls_component.

            assign component sy-tabix of structure <ls_data> to <l_data>.
            l_len = ls_component-type->length.

            try.

                <l_data> = <l_text>+l_off(l_len).
              catch cx_sy_conversion_error.
                _add_spool( `@5C@ Convert file to data: Conversion error. Line ` && l_line_num ).
                clear <ls_data>.
                return.
            endtry.

            try. "NCE jibberish
                l_off = l_off + l_len.
              catch cx_sy_arithmetic_overflow.
            endtry.
          endloop.

          append <ls_data> to <lt_data>.
        endloop.

      when others.
        message 'Invalid file format' type 'E'.
    endcase.

    "Update any fields defined in the value mapping table.
    if gt_val_map is not initial.

      loop at gt_val_map into ls_val_map.

        if ls_dta-tlogo = 'CUBE'.
          ls_val_map_x-fieldnm = ls_val_map-iobjnm.
        else.
          ls_val_map_x-fieldnm = iobjnm_fieldnm( ls_val_map-iobjnm ).
        endif.

        assign component ls_val_map_x-fieldnm of structure <ls_data> to <l_data>.
        if sy-subrc = 0.

          move-corresponding ls_val_map to ls_val_map_x.
          append ls_val_map_x to lt_val_map_x.
        endif.
      endloop.

      loop at <lt_data> assigning <ls_data>.

        loop at lt_val_map_x into ls_val_map_x.

          assign component ls_val_map_x-fieldnm of structure <ls_data> to <l_data>.
          if sy-subrc = 0.

            if ls_val_map_x-from is initial or <l_data> = ls_val_map_x-from.
              try.
                  <l_data> = ls_val_map_x-to.
                catch cx_sy_assign_error.
                  return.
              endtry.
            endif.
          endif.
        endloop.
      endloop.
    endif.
  endmethod.                    "convert_file_to_data


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YGTTC_CL_CBID_PACK->CREATE_IOBJNM_CVC
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_IOBJNM                       TYPE        RSIOBJNM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method create_iobjnm_cvc.

    data:
      ls_dta                   type rsd_s_dta.

    data:
      ls_viobj                 type rsd_s_viobj.

    data:
      ls_iobj_cmp              type rsd_s_iobj_cmp.

    data:
      ls_data_cvc              type ref to data,
      lt_data_cvc              type ref to data.

    data:
      lo_typedescr             type ref to cl_abap_typedescr,
      lo_structdescr           type ref to cl_abap_structdescr,
      lt_component             type abap_component_tab,
      ls_component             type abap_componentdescr.

    types:
      begin of s_comp_map,
        td_iobjnm              type rsiobjnm,
        td_fieldnm             type rsfieldnm,
        cvc_iobjnm             type rsiobjnm,
        cvc_fieldnm            type rsfieldnm,
      end of   s_comp_map.

    data:
      ls_comp_map              type s_comp_map,
      lt_comp_map              type table of s_comp_map.

    field-symbols:
      <ls_comp_map>            type s_comp_map.

    field-symbols:
      <lt_data>                type standard table,
      <l_data_td>              type any,
      <ls_data_td>             type any,
      <lt_data_td>             type standard table,
      <l_data_cvc>             type any,
      <ls_data_cvc>            type any,
      <lt_data_cvc>            type standard table.

    field-symbols:
      <ls_for_all_entries_in>  type s_for_all_entries_in.

    data:
      ls_obj_dta               type s_obj_dta.


    "Check if there is meta-data for this object.
    read table gt_viobj into ls_viobj
      with key
        iobjnm = i_iobjnm.
    if sy-subrc <> 0.
      return.
    endif.

    "Add the base Characteristic to the component list.
    call method cl_abap_datadescr=>describe_by_name
      exporting
        p_name         = |{ _iobjnm_dtelnm( _chanm_chabasnm( i_iobjnm ) ) }|
      receiving
        p_descr_ref    = lo_typedescr
      exceptions
        type_not_found = 1
        others         = 2.
    if sy-subrc <> 0.
      return.
    endif.

    ls_component-name = |{ iobjnm_fieldnm( _chanm_chabasnm( i_iobjnm ) ) }|.
    ls_component-type ?= lo_typedescr.
    append ls_component to lt_component.

    ls_comp_map-td_iobjnm = i_iobjnm.
    ls_comp_map-cvc_iobjnm = |{ _chanm_chabasnm( i_iobjnm ) }|.
    ls_comp_map-cvc_fieldnm = ls_component-name.
    append ls_comp_map to lt_comp_map.

    "Get the compound Characteristics if any exists, add these to the component list.
    loop at gt_iobj_cmp into ls_iobj_cmp where iobjnm = i_iobjnm.

      call method cl_abap_datadescr=>describe_by_name
        exporting
          p_name         = |{ _iobjnm_dtelnm( _chanm_chabasnm( ls_iobj_cmp-iobjcmp ) ) }|
        receiving
          p_descr_ref    = lo_typedescr
        exceptions
          type_not_found = 1
          others         = 2.
      if sy-subrc <> 0.
        return.
      endif.

      "All generated data structures are stored with fieldnames _<InfoObject name>.
      ls_component-name = |{ iobjnm_fieldnm( _chanm_chabasnm( ls_iobj_cmp-iobjcmp ) ) }|.
      ls_component-type ?= lo_typedescr.
      append ls_component to lt_component.

      ls_comp_map-td_iobjnm = ls_iobj_cmp-iobjcmp.
      ls_comp_map-cvc_iobjnm = _chanm_chabasnm( ls_iobj_cmp-iobjcmp ).
      ls_comp_map-cvc_fieldnm = ls_component-name.
      append ls_comp_map to lt_comp_map.
    endloop.

    "Create data description from component list, create data
    "references and assign data references to field symbols.
    if lt_component is not initial.

      try.

          call method cl_abap_structdescr=>create
            exporting
              p_components = lt_component
            receiving
              p_result     = lo_structdescr.

        catch cx_sy_struct_creation.
          return.
      endtry.

      create data ls_data_cvc type handle lo_structdescr.
      assign ls_data_cvc->* to <ls_data_cvc>.

      create data lt_data_cvc like table of <ls_data_cvc>.
      assign lt_data_cvc->* to <lt_data_cvc>.
    endif.

    "Loop through the transaction data, create the CVCs and add these to
    "any pre-defined CVCs in the FOR_ALL_ENTRIES_IN variable.
    loop at gt_obj_dta into ls_obj_dta where objtp = 'TD'.

      read table gt_dta into ls_dta
        with key
          infoprov = ls_obj_dta-objnm.
      if sy-subrc <> 0.
        return.
      endif.

      assign ls_obj_dta-t_data->* to <lt_data_td>.

      if sy-subrc <> 0 or <lt_data_td> is initial.
        return.
      endif.

      "Complete the component mapping table.
      loop at lt_comp_map assigning <ls_comp_map>.
        case ls_dta-tlogo.
          when 'CUBE'. <ls_comp_map>-td_fieldnm = <ls_comp_map>-td_iobjnm.
          when 'ODSO'. <ls_comp_map>-td_fieldnm = iobjnm_fieldnm( <ls_comp_map>-td_iobjnm ).
        endcase.
      endloop.

      "Loop through the data and create the CVCs.
      loop at <lt_data_td> assigning <ls_data_td>.

        loop at lt_comp_map into ls_comp_map.

          unassign:
            <l_data_td>,
            <l_data_cvc>.

          assign component ls_comp_map-td_fieldnm of structure <ls_data_td> to <l_data_td>.
          assign component ls_comp_map-cvc_fieldnm  of structure <ls_data_cvc> to <l_data_cvc>.

          if <l_data_td> is assigned and <l_data_cvc> is assigned.
            <l_data_cvc> = <l_data_td>.
          endif.
        endloop.

        append <ls_data_cvc> to <lt_data_cvc>.
      endloop.
    endloop.

    "Add the created CVCs to the variable that contains any CVCs created
    "in previous implementations.
    read table gt_for_all_entries_in assigning <ls_for_all_entries_in>
      with key
        iobjnm = i_iobjnm.
    if sy-subrc <> 0.
      append initial line to gt_for_all_entries_in assigning <ls_for_all_entries_in>.
      <ls_for_all_entries_in>-iobjnm = i_iobjnm.
    endif.

    if <ls_for_all_entries_in>-t_cvc is initial.

      <ls_for_all_entries_in>-t_cvc = lt_data_cvc.
      assign <ls_for_all_entries_in>-t_cvc->* to <lt_data>.
    else.

      assign <ls_for_all_entries_in>-t_cvc->* to <lt_data>.
      append lines of <lt_data_cvc> to <lt_data>.
    endif.

    sort <lt_data>.
    delete adjacent duplicates from <lt_data>.

    _add_spool( `@5B@ Create CVCs: ` && |{ lines( <lt_data> ) }| &&  ` CVCs create for InfoObject ` && i_iobjnm ).
  endmethod.                    "create_iobjnm_cvc


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YGTTC_CL_CBID_PACK->EXPORT_CSV_PACK
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_OBJNM                        TYPE        CHAR30
* | [--->] I_OBJTP                        TYPE        CHAR_02 (default ='TD')
* | [--->] I_PACKAGE_NUM                  TYPE        I (default =1)
* +--------------------------------------------------------------------------------------</SIGNATURE>
method export_csv_pack.

  data:
    l_str                    type string,
    l_strlen                 type i,
    l_xstr                   type xstring,
    lt_xstr                  type table of xstring.

  data:
    l_file                   type string,
    l_xfile                  type string,
    l_filename               type string.

  field-symbols:
    <ls_obj_dta>             type s_obj_dta,
    <lt_data>                type standard table.

  data:
    lo_root                  type ref to cx_root.


  "Retrieve data from the internal variable for the object.
  read table gt_obj_dta assigning <ls_obj_dta>
    with key
      objnm = i_objnm
      objtp = i_objtp.
  if sy-subrc <> 0.
    return.
  endif.

  "Check the data variable is not empty.
  check <ls_obj_dta>-t_data is not initial.

  "Convert the data to file format.
  convert_data_to_file( i_objnm = i_objnm i_objtp = i_objtp ).

  check <ls_obj_dta>-t_file is not initial.

  "Compress if necesary.

  if g_file_compress = 'X'.

    if g_file_sep_rows = 'X'.

      l_str = concat_lines_of( table = <ls_obj_dta>-t_file sep = |{ cl_abap_conv_in_ce=>uccp( g_file_row_sep ) }| ).

      try.
          call method cl_abap_gzip=>compress_text
            exporting
              text_in      = l_str
            importing
              gzip_out     = l_xstr
              gzip_out_len = l_strlen.

        catch cx_parameter_invalid_range into lo_root.
        catch cx_sy_buffer_overflow into lo_root.
        catch cx_sy_conversion_codepage into lo_root.
      endtry.
      if lo_root is not initial.
        return.
      endif.

      append l_xstr to lt_xstr.

    else.

      loop at <ls_obj_dta>-t_file into l_str.

        try.
            call method cl_abap_gzip=>compress_text
              exporting
                text_in      = l_str
              importing
                gzip_out     = l_xstr
                gzip_out_len = l_strlen.

          catch cx_parameter_invalid_range into lo_root.
          catch cx_sy_buffer_overflow into lo_root.
          catch cx_sy_conversion_codepage into lo_root.
        endtry.
        if lo_root is not initial.
          return.
        endif.

        append l_xstr to lt_xstr.
      endloop.
    endif.

    assign lt_xstr to <lt_data>.
  else.
    assign <ls_obj_dta>-t_file to <lt_data>.
  endif.

  "Change the file name to include the package number.
  l_filename = g_directory && g_file_prefix && i_objnm && `_` && i_objtp && `_` && i_package_num && `.TXT`.

  if g_usegui = 'X'.

    call method cl_gui_frontend_services=>gui_download
      exporting
        filename                = l_filename
      changing
        data_tab                = <lt_data>
      exceptions
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        others                  = 24.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
                 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  else.

    open dataset l_filename for output in text mode encoding default.
    if sy-subrc = 0.

      truncate dataset l_filename at position 1.

      if g_file_compress = 'X'.
        loop at <lt_data> into l_xfile.

          transfer l_xfile to l_filename.
        endloop.
      else.

        loop at <ls_obj_dta>-t_file into l_file.

          transfer l_file to l_filename.
        endloop.
      endif.

      close dataset l_filename.
    else.

      message id sy-msgid type sy-msgty number sy-msgno
                 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  endif.

  _add_spool( `@5B@ Export to file: ` && l_filename ).
endmethod.                    "export_csv_pack


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YGTTC_CL_CBID_PACK->FIELDNM_IOBJNM
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_FIELDNM                      TYPE        RSFIELDNM
* | [<-()] R_IOBJNM                       TYPE        RSIOBJNM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method fieldnm_iobjnm.

    data:
      ls_map                   type s_map.

    read table gh_map into ls_map
      with table key
        maptp = 'FIELDNM_IOBJNM'
        mapfr = i_fieldnm.
    if sy-subrc = 0.
      r_iobjnm = ls_map-mapto.
    else.

      select single iobjnm into r_iobjnm from rsdiobj where objvers = 'A' and fieldnm = i_fieldnm.
      if sy-subrc = 0.

        ls_map-maptp = 'FIELDNM_IOBJNM'.
        ls_map-mapfr = i_fieldnm.
        ls_map-mapto = r_iobjnm.
        insert ls_map into table gh_map.
      endif.
    endif.
  endmethod.                    "FIELDNM_IOBJNM


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YGTTC_CL_CBID_PACK->GET_IOBJ_RANGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_IOBJNM                       TYPE        RSIOBJNM
* | [<-()] R_T_DATA                       TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_iobj_range.

    data:
      ls_rsdri_range           type rsdri_s_range,
      ls_data                  type ref to data,
      lt_data                  type ref to data.

    data:
      lo_typedescr             type ref to cl_abap_typedescr,
      lo_structdescr           type ref to cl_abap_structdescr,
      lt_components            type abap_component_tab,
      ls_components            type abap_componentdescr,
      l_dtelnm                 type rollname.

    field-symbols:
      <ls_data>                type any,
      <lt_data>                type standard table.

    field-symbols:
      <l_sign>                 type any,
      <l_option>               type any,
      <l_low>                  type any,
      <l_high>                 type any.


    "Create the range table, sign, option, low and high.
    do 4 times.

      case sy-index.
        when 1.
          l_dtelnm = 'S_SIGN'.
          ls_components-name = 'SIGN'.
        when 2.
          l_dtelnm = 'S_OPTION'.
          ls_components-name = 'OPTION'.
        when 3.
          l_dtelnm = _iobjnm_dtelnm( i_iobjnm ).
          ls_components-name = 'LOW'.
        when 4.
          l_dtelnm = _iobjnm_dtelnm( i_iobjnm ).
          ls_components-name = 'HIGH'.
      endcase.

      call method cl_abap_datadescr=>describe_by_name
        exporting
          p_name         = l_dtelnm
        receiving
          p_descr_ref    = lo_typedescr
        exceptions
          type_not_found = 1
          others         = 2.
      if sy-subrc <> 0.

        message id sy-msgid type sy-msgty number sy-msgno
                   with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.

      ls_components-type ?= lo_typedescr.
      append ls_components to lt_components.
    enddo.

    "Create data description from component list, create data
    "references and assign data references to field symbols.
    if lt_components is not initial.

      try.

          call method cl_abap_structdescr=>create
            exporting
              p_components = lt_components
            receiving
              p_result     = lo_structdescr.

        catch cx_sy_struct_creation.
      endtry.

      create data ls_data type handle lo_structdescr.
      assign ls_data->* to <ls_data>.

      create data lt_data like table of <ls_data>.
      assign lt_data->* to <lt_data>.
    endif.

    read table gt_cha_map transporting no fields
      with key
        from = ''.

    "Fill the range table from the transaction data.
    loop at gt_rsdri_range into ls_rsdri_range where chanm = i_iobjnm.

      clear <ls_data>.

      assign component 'SIGN' of structure <ls_data> to <l_sign>.
      if <l_sign> is assigned.
        <l_sign> = ls_rsdri_range-sign.
      endif.

      assign component 'OPTION' of structure <ls_data> to <l_option>.
      if <l_option> is assigned.
        <l_option> = ls_rsdri_range-compop.
      endif.

      assign component 'LOW' of structure <ls_data> to <l_low>.
      if <l_low> is assigned.
        <l_low> = ls_rsdri_range-low.
      endif.

      assign component 'HIGH' of structure <ls_data> to <l_high>.
      if <l_high> is assigned.
        <l_high> = ls_rsdri_range-high.
      endif.

      append <ls_data> to <lt_data>.
    endloop.

    r_t_data = lt_data.
  endmethod.                    "GET_IOBJ_RANGE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YGTTC_CL_CBID_PACK->IMPORT_CSV_PACK
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_OBJNM                        TYPE        CHAR30
* | [--->] I_OBJTP                        TYPE        CHAR_02 (default ='TD')
* | [--->] I_PACKAGE_NUM                  TYPE        I (default =1)
* | [EXC!] FILE_NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
method import_csv_pack.

  data:
    l_file                   type string,
    l_filename               type string,
    l_result                 type rs_bool.

  data:
    l_prefix                 type string,
    lt_spool                 type stringtab.

  field-symbols:
    <ls_obj_dta>             type s_obj_dta,
    <l_file>                 type any,
    <lt_file>                type standard table.

  field-symbols:
    <l_data>                 type any,
    <lt_data>                type standard table.

  data:
    l_str                    type string,
    lt_str                   type stringtab,
    l_strlen                 type i,
    l_xstr                   type xstring,
    lt_xstr                  type table of xstring.

  data:
    lo_root                  type ref to cx_root.
  "End of variable declaration.

  "Check meta-data exists, this will have been generated in the class
  "constructor, if no meta-data is available then do not process further.
  case i_objtp.
    when 'TD'.

      "Check meta-data exists for InfoProvider.
      read table gt_dta transporting no fields
        with key
          infoprov = i_objnm .
      if sy-subrc <> 0.
        _add_spool( `@5C@ Import file: Meta-data for InfoObject ` && i_objnm && ` not found` ).
        return.
      endif.

    when 'MA'.

      "Check meta-data exists for InfoObject.
      read table gt_viobj transporting no fields
        with key
          iobjnm = i_objnm .
      if sy-subrc <> 0.
        _add_spool( `@5C@ Import file: Meta-data for InfoObject ` && i_objnm && ` not found` ).
        return.
      endif.

    when 'MT'.

      "Check meta-data exists for InfoObject.
      read table gt_viobj transporting no fields
        with key
          iobjnm = i_objnm .
      if sy-subrc <> 0.
        _add_spool( `@5C@ Import file: Meta-data for InfoObject ` && i_objnm && ` not found` ).
        return.
      endif.

    when others.

      message 'Invalid object type' type 'E'.
  endcase.

  "Retrieve or create a new entry in internal data variable for
  "the object. All data is stored and manipulated in this variable.
  read table gt_obj_dta assigning <ls_obj_dta>
    with key
      objnm = i_objnm
      objtp = i_objtp.
  if sy-subrc <> 0.

    append initial line to gt_obj_dta assigning <ls_obj_dta>.

    <ls_obj_dta>-objnm = i_objnm.
    <ls_obj_dta>-objtp = i_objtp.
  endif.

  assign <ls_obj_dta>-t_file to <lt_file>.

  "Assign the data objects to hold either string or xstring files.
  if g_file_compress = 'X'.

    assign l_xstr to <l_data>.
    assign lt_xstr to <lt_data>.
  else.

    assign l_str to <l_data>.
    assign lt_str to <lt_data>.
  endif.

  "Change the file name to include the package number.
  l_filename = g_directory && g_file_prefix && i_objnm && `_` && i_objtp && `_` && i_package_num && '.TXT'.

  if g_usegui = 'X'.

    call method cl_gui_frontend_services=>file_exist
      exporting
        file                 = l_filename
      receiving
        result               = l_result
      exceptions
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        others               = 5.
    if sy-subrc <> 0 or l_result = ''.
      _add_spool( `@5D@ Import file: File not found: ` && l_filename ).
      raise file_not_found.
    endif.

    clear <lt_file>.

    call method cl_gui_frontend_services=>gui_upload
      exporting
        filename                = l_filename
      changing
        data_tab                = <lt_data>
      exceptions
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        others                  = 19.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
                 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  else.

    clear <lt_file>.

    open dataset l_filename for input in text mode encoding default.
    if sy-subrc = 0.
      while sy-subrc = 0.

        read dataset l_filename into l_str.
        <l_data> = l_str.

        if sy-subrc = 0.
          append <l_data> to <lt_data>.
        endif.
      endwhile.
    else.
      _add_spool( `@5D@ Import file: File not found: ` && l_filename ).
      raise file_not_found.
    endif.
  endif.

  "Decompress if necesary.
  if g_file_compress = 'X'.

    if g_file_sep_rows = 'X'.

      read table <lt_data> into l_xstr index 1.

      try.

          call method cl_abap_gzip=>decompress_text
            exporting
              gzip_in      = l_xstr
            importing
              text_out     = l_str
              text_out_len = l_strlen.

        catch cx_parameter_invalid_range into lo_root.
        catch cx_sy_buffer_overflow into lo_root.
        catch cx_sy_conversion_codepage into lo_root.
        catch cx_sy_compression_error into lo_root.
      endtry.
      if lo_root is not initial.
        return.
      endif.

      split l_str at |{ cl_abap_conv_in_ce=>uccp( g_file_row_sep ) }| into table lt_str.
    else.

      loop at <lt_data> into l_xstr.

        try.

            call method cl_abap_gzip=>decompress_text
              exporting
                gzip_in      = l_xstr
              importing
                text_out     = l_str
                text_out_len = l_strlen.

          catch cx_parameter_invalid_range into lo_root.
          catch cx_sy_buffer_overflow into lo_root.
          catch cx_sy_conversion_codepage into lo_root.
          catch cx_sy_compression_error into lo_root.
        endtry.
        if lo_root is not initial.
          return.
        endif.

        append l_str to lt_str.
      endloop.
    endif.

    <lt_file> = lt_str.
  else.
    <lt_file> = <lt_data>.
  endif.

  _add_spool( `@5B@ Import file: ` && |{ lines( <lt_file> ) }| && ` lines imported for object ` && i_objnm && ` / ` && i_objtp ).

  convert_file_to_data( i_objnm = i_objnm i_objtp = i_objtp ).
endmethod.                    "import_csv_pack


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YGTTC_CL_CBID_PACK->IMPORT_WRITE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_OBJNM                        TYPE        CHAR30
* | [--->] I_OBJTP                        TYPE        CHAR_02 (default ='TD')
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method import_write.

    data:
      l_package_num            type i,
      l_infoprov_src           type rsinfoprov,
      l_infoprov_trg           type rsinfoprov.

    field-symbols:
      <ls_obj_dta>             type s_obj_dta.
    "End of variable declaration.


    do.

      l_package_num = sy-index.

      if g_no_of_packages is not initial and l_package_num > g_no_of_packages.
        exit.
      endif.

      "Clear any legacy data from previous packages.
      read table gt_obj_dta assigning <ls_obj_dta>
        with key
          objnm = i_objnm
          objtp = i_objtp.
      if sy-subrc = 0.
        clear <ls_obj_dta>-t_data.
        clear <ls_obj_dta>-t_file.
      endif.

      "Import a package of data to the internal data.
      call method me->import_csv_pack
        exporting
          i_objnm        = i_objnm
          i_objtp        = i_objtp
          i_package_num  = l_package_num
        exceptions
          file_not_found = 1
          others         = 2.
      if sy-subrc <> 0.
        exit.
      endif.

      "Write the data whether it is transaction or master data.
      case i_objtp.
        when 'TD'.

          loop at gt_prov_trg into l_infoprov_trg.

            call method me->write_td_pack
              exporting
                i_infoprov_src = i_objnm
                i_infoprov_trg = l_infoprov_trg.
          endloop.

        when 'MA'.

          call method me->write_ma_pack
            exporting
              i_iobjnm = i_objnm.

        when 'MT'.

          call method me->write_mt_pack
            exporting
              i_iobjnm = i_objnm.

        when others.

          message 'Invalid object type' type 'E'.
      endcase.
    enddo.
  endmethod.                    "import_write


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YGTTC_CL_CBID_PACK->IMPORT_WRITE_ALL
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method import_write_all.

    data:
      l_infoprov          type rsinfoprov,
      l_iobjnm            type rsiobjnm.
    "End of variable declaration.


    "Check the system type, write function in production is not supported.
    if g_cccategory = 'P'.
      msg_spool_rc `I|YGTTC_CBID|001` `` `` `` ``. return.
    endif.

    "Import and Write all transaction data.
    loop at gt_prov_src into l_infoprov.

      call method me->import_write
        exporting
          i_objnm = l_infoprov
          i_objtp = 'TD'.
    endloop.

    "Import and Write all Master data attributes.
    loop at gt_iobj_ma into l_iobjnm.

      call method me->import_write
        exporting
          i_objnm = l_iobjnm
          i_objtp = 'MA'.
    endloop.

    "Import and Write all Master data texts.
    loop at gt_iobj_mt into l_iobjnm.

      call method me->import_write
        exporting
          i_objnm = l_iobjnm
          i_objtp = 'MT'.
    endloop.
  endmethod.                    "IMPORT_WRITE_ALL


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YGTTC_CL_CBID_PACK->IOBJNM_FIELDNM
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_IOBJNM                       TYPE        RSIOBJNM
* | [<-()] R_FIELDNM                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method iobjnm_fieldnm.

    data:
      ls_map                   type s_map.

    read table gh_map into ls_map
      with table key
        maptp = 'IOBJNM_FIELDNM'
        mapfr = i_iobjnm.
    if sy-subrc = 0.
      r_fieldnm = ls_map-mapto.
    else.

      select single fieldnm into r_fieldnm from rsdiobj where iobjnm = i_iobjnm and objvers = 'A'.
      if sy-subrc = 0.

        ls_map-maptp = 'IOBJNM_FIELDNM'.
        ls_map-mapfr = i_iobjnm.
        ls_map-mapto = r_fieldnm.
        insert ls_map into table gh_map.
      endif.
    endif.
  endmethod.                    "IOBJNM_FIELDNM


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YGTTC_CL_CBID_PACK->READ_EXPORT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_OBJNM                        TYPE        CHAR30
* | [--->] I_OBJTP                        TYPE        CHAR_02 (default ='TD')
* +--------------------------------------------------------------------------------------</SIGNATURE>
method read_export.

  data:
    l_package_num            type i,
    l_infoprov_src           type rsinfoprov,
    l_infoprov_trg           type rsinfoprov.

  data:
    ls_iobj                  type s_iobj,
    lt_iobj                  type t_iobj,
    lt_data_cvc              type ref to data.

  field-symbols:
    <ls_obj_dta>             type s_obj_dta.


  do.

    l_package_num = sy-index.

    if g_no_of_packages is not initial and l_package_num > g_no_of_packages.
      exit.
    endif.

    "Clear any legacy data from previous packages.
    read table gt_obj_dta assigning <ls_obj_dta>
      with key
        objnm = i_objnm
        objtp = i_objtp.
    if sy-subrc = 0.
      clear <ls_obj_dta>-t_data.
      clear <ls_obj_dta>-t_file.
    endif.

    "Export the data whether it is transaction or master data.
    case i_objtp.
      when 'TD'.

        call method me->read_td_pack
          exporting
            i_infoprov    = i_objnm
            i_package_num = l_package_num
          exceptions
            end_of_data   = 1
            error         = 2.
        if sy-subrc <> 0.
          exit.
        endif.

      when 'MA' or 'MT'.

        call method me->read_md_pack
          exporting
            i_iobjnm      = i_objnm
            i_objtp       = i_objtp
            i_package_num = l_package_num
          exceptions
            end_of_data   = 1
            error         = 2.
        if sy-subrc <> 0.
          exit.
        endif.

      when others.
        message 'Invalid object type' type 'E'.
    endcase.

    if i_objtp = 'TD'.

      "Create the CVCs based on the transaction data.
      clear lt_iobj.
      append lines of gt_iobj_ma to lt_iobj.
      append lines of gt_iobj_mt to lt_iobj.

      sort lt_iobj.
      delete adjacent duplicates from lt_iobj.

      loop at lt_iobj into ls_iobj.

        call method me->create_iobjnm_cvc
          exporting
            i_iobjnm = ls_iobj-iobjnm.
      endloop.
    endif.

    call method me->export_csv_pack
      exporting
        i_objnm       = i_objnm
        i_objtp       = i_objtp
        i_package_num = l_package_num.
  enddo.

  if i_objtp = 'TD'.

    "Create the CVCs based on the transaction data.
    clear lt_iobj.
    append lines of gt_iobj_ma to lt_iobj.
    append lines of gt_iobj_mt to lt_iobj.

    sort lt_iobj.
    delete adjacent duplicates from lt_iobj.

    loop at lt_iobj into ls_iobj.

      call method me->create_iobjnm_cvc
        exporting
          i_iobjnm = ls_iobj-iobjnm.
    endloop.
  endif.

  if g_no_of_packages is initial or
    ( g_no_of_packages is not initial and
    l_package_num <= g_no_of_packages ).

    call method me->export_csv_pack
      exporting
        i_objnm       = i_objnm
        i_objtp       = i_objtp
        i_package_num = l_package_num.
  endif.
endmethod.                    "READ_EXPORT


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YGTTC_CL_CBID_PACK->READ_EXPORT_ALL
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method read_export_all.

    data:
      l_infoprov               type rsinfoprov,
      l_iobjnm                 type rsiobjnm.


    "Check the system type, write function in production is not supported.
    if g_cccategory = 'P'.
      msg_spool_rc `I|YGTTC_CBID|001` `` `` `` ``. return.
    endif.

    "Import and Write all transaction data.
    loop at gt_prov_src into l_infoprov.

      call method me->read_export
        exporting
          i_objnm = l_infoprov
          i_objtp = 'TD'.
    endloop.

    "Import and Write all Master data attributes.
    loop at gt_iobj_ma into l_iobjnm.

      call method me->read_export
        exporting
          i_objnm = l_iobjnm
          i_objtp = 'MA'.
    endloop.

    "Import and Write all Master data texts.
    loop at gt_iobj_mt into l_iobjnm.

      call method me->read_export
        exporting
          i_objnm = l_iobjnm
          i_objtp = 'MT'.
    endloop.
  endmethod.                    "read_export_all


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YGTTC_CL_CBID_PACK->READ_MD_PACK
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_IOBJNM                       TYPE        RSIOBJNM
* | [--->] I_OBJTP                        TYPE        OBJTP
* | [--->] I_PACKAGE_NUM                  TYPE        I
* | [EXC!] END_OF_DATA
* | [EXC!] ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method read_md_pack.

    data:
      ls_viobj                 type rsd_s_viobj.

    data:
      l_iobjnm                 type rsiobjnm,
      l_iobj_map               type rs_bool,
      ls_cha_map               type s_cha_map.

    data:
      ls_iobj_cmp              type rsd_s_iobj_cmp.

    data:
      ls_for_all_entries_in    type s_for_all_entries_in.

    data:
      lt_range                 type ref to data,
      l_sqlfrom                type string,
      l_sqlwhere               type string.

    field-symbols:
      <lt_cvc>                 type standard table,
      <lt_data>                type standard table,
      <lt_range>               type standard table.

    field-symbols:
      <ls_obj_dta>             type s_obj_dta.
    " ->End of variable declaration.


    "Check there is meta-data for this Characteristic.
    read table gt_viobj into ls_viobj
      with key
        iobjnm = i_iobjnm.
    if sy-subrc <> 0.
      raise end_of_data.
    endif.

    "Check if the Characteristic is a reference characteristic.
    if ls_viobj-iobjnm <> ls_viobj-chabasnm.

      read table gt_viobj into ls_viobj
        with key
          iobjnm = ls_viobj-chabasnm.
      if sy-subrc <> 0.
        raise end_of_data.
      endif.
    endif.

    "Check that a attributes table exists.
    if ls_viobj-attribfl <> 'X'.
      raise end_of_data.
    endif.

    "Check the authorisation to read the object, this is the check performed in RSD1.
    call function 'RSSB_AUTHORITY_ADMWB_MASTERDTA'
      exporting
        i_infoobject         = i_iobjnm
        i_actvt              = '23'
        i_try_display        = rs_c_true
      exceptions
        user_not_authorized  = 1
        iobj_not_found       = 2
        infoobjcat_not_found = 3
        others               = 4.
    if sy-subrc <> 0.
      raise end_of_data.
    endif.

    "Derive the source text table for the Base Characteristic.
    case i_objtp.
      when 'MA'.
        l_sqlfrom = replace( val = _iobjnm_dtelnm( _chanm_chabasnm( ls_viobj-iobjnm ) ) sub = '/OI' with = '/P' ).
      when 'MT'.
        l_sqlfrom = replace( val = _iobjnm_dtelnm( _chanm_chabasnm( ls_viobj-iobjnm ) ) sub = '/OI' with = '/T' ).
    endcase.

    "Retrieve or create an entry in the internal data against the Characteristic.
    read table gt_obj_dta assigning <ls_obj_dta>
      with key
        objnm = i_iobjnm
        objtp = i_objtp.
    if sy-subrc <> 0.

      append initial line to gt_obj_dta assigning <ls_obj_dta>.
      <ls_obj_dta>-objnm = i_iobjnm.
      <ls_obj_dta>-objtp = i_objtp.
    endif.

    "Create the CVC data structure against the Base characteristic.
    create data <ls_obj_dta>-t_data type table of (l_sqlfrom).
    assign <ls_obj_dta>-t_data->* to <lt_data>.

    "Check the InfoObject mapping, this is defined for characteristics such as 0MAT_UNIT.
    read table gt_cha_map into ls_cha_map
      with key
        chanm = i_iobjnm.
    if sy-subrc = 0.
      l_iobjnm = ls_cha_map-from.
      l_iobj_map = 'X'.
    else.
      l_iobjnm = i_iobjnm.
      l_iobj_map = ''.
    endif.

    "Get the CVCs for the mapped or base characteristic.
    read table gt_for_all_entries_in into ls_for_all_entries_in
      with key
        iobjnm = l_iobjnm.
    if sy-subrc <> 0.
      _add_spool( `@5D@ Read MD: CVCs not found for InfoObject ` && i_iobjnm ).
    else.
      assign ls_for_all_entries_in-t_cvc->* to <lt_cvc>.
    endif.

    "Create the SQL statement.
    if l_iobj_map = 'X'.
      l_sqlwhere = iobjnm_fieldnm( _chanm_chabasnm( ls_cha_map-from ) ) && ` = <lt_cvc>-` && iobjnm_fieldnm( _chanm_chabasnm( ls_cha_map-to ) ).
    else.

      l_sqlwhere = iobjnm_fieldnm( _chanm_chabasnm( i_iobjnm ) ) && ` = <lt_cvc>-` && iobjnm_fieldnm( _chanm_chabasnm( i_iobjnm ) ).

      loop at gt_iobj_cmp into ls_iobj_cmp where iobjnm = i_iobjnm.

        l_sqlwhere = l_sqlwhere && ` and ` && iobjnm_fieldnm( _chanm_chabasnm( ls_iobj_cmp-iobjcmp ) ) && ` = <lt_cvc>-` && iobjnm_fieldnm( ls_iobj_cmp-iobjcmp ) .
      endloop.
    endif.

    "Clear the cursor for the first package.
    if i_package_num = 1.
      if g_cursor is not initial.
        close cursor g_cursor.
      endif.
    endif.

    "If there are CVCs then use these for the selection.
    if <lt_cvc> is assigned and <lt_cvc> is not initial.

      "Open the cursor.
      if g_cursor is initial.

        try.

            open cursor with hold g_cursor for
              select *
                from (l_sqlfrom)
                for all entries in <lt_cvc>
                where (l_sqlwhere).

          catch cx_sy_dynamic_osql_syntax.
            raise end_of_data.
          catch cx_sy_dynamic_osql_semantics.
            raise end_of_data.
        endtry.
      endif.
    else.

      "If there are no Characteristic Value Combinations (CVCs) for
      "the Characteristic then use the RSDRI_RANGE_TABLE selections.
      lt_range = get_iobj_range( i_iobjnm ).
      assign lt_range->* to <lt_range>.

      if <lt_range> is initial.
        _add_spool( `@5D@ Read MD: No selection in range table for InfoObject ` && i_iobjnm ).
        raise end_of_data.
      endif.

      "Get the SQL WHERE clause.
      l_sqlwhere = iobjnm_fieldnm( l_iobjnm ) && ` in <lt_range>`.

      "Open the cursor.
      if g_cursor is initial.

        try.

            open cursor with hold g_cursor for
              select *
                from (l_sqlfrom)
                where (l_sqlwhere).

          catch cx_sy_dynamic_osql_syntax.
            raise end_of_data.
          catch cx_sy_dynamic_osql_semantics.
            raise end_of_data.
        endtry.
      endif.
    endif.

    "Fetch the packages of data.
    fetch next cursor g_cursor
      into table <lt_data>
      package size g_package_size.
    if sy-subrc <> 0.

      close cursor g_cursor.
      raise end_of_data.
    endif.

    _add_spool( `@5B@ Read MD: ` && |{ lines( <lt_data> ) }| && ` records read for InfoObject ` && i_iobjnm && ` in package ` && i_package_num ).
  endmethod.                    "read_md_pack


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YGTTC_CL_CBID_PACK->READ_TD_PACK
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_INFOPROV                     TYPE        RSINFOPROV
* | [--->] I_PACKAGE_NUM                  TYPE        I
* | [EXC!] END_OF_DATA
* | [EXC!] ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method read_td_pack.

    data:
      ls_dta                   type rsd_s_dta.

    data:
      ls_dta_pro               type rsd_s_dta_pro.

    data:
      ls_object                type rso_s_tlogo,
      ls_subobject             type rso_s_tlogo,
      lt_subobject             type rssb_t_subobject.

    data:
      lo_typedescr             type ref to cl_abap_typedescr,
      lo_structdescr           type ref to cl_abap_structdescr,
      lt_components            type abap_component_tab,
      ls_components            type abap_componentdescr,
      l_fieldnm                type cl_abap_structdescr=>component-name.

    data:
      lt_datapak               type ref to data,
      lt_data                  type ref to data.

    field-symbols:
      <lt_data_pack>           type standard table,
      <lt_data>                type standard table.

    data:
      ls_sfc                   type rsdri_s_sfc,
      lt_sfc                   type rsdri_t_sfc,
      lh_sfc                   type rsdri_th_sfc,
      ls_sfk                   type rsdri_s_sfk,
      lt_sfk                   type rsdri_t_sfk,
      lh_sfk                   type rsdri_th_sfk,
      ls_range                 type rsdri_s_range,
      lt_range                 type rsdri_t_range,
      l_packagesize            type i,
      l_end_of_data            type rs_bool,
      lt_msg                   type rs_t_msg,
      l_iobjnm                 type rsiobjnm.

    field-symbols:
      <ls_obj_dta>             type s_obj_dta.

    data l_lines               type i.
    " -> end of variable declaration


    "Check the meta-data for the InfoProvider.
    read table gt_dta into ls_dta
      with key
        infoprov = i_infoprov.
    if sy-subrc <> 0.
      _add_spool( `@5C@ Read TD: Meta-data for InfoProvider ` && i_infoprov && ` cannot be found` ).
      raise error.
    endif.

    if i_package_num > 1 and g_usegui = 'X'.
      _add_spool( `@5C@ Read TD: Multiple packages are not possible using the GUI for file transfer` ).
      raise error.
    endif.

    "Check the authorisation to read the object, this is the check performed in LISTCUBE.
    ls_object-tlogo = ls_dta-tlogo.
    ls_object-objnm = ls_dta-infoprov.

    ls_subobject-tlogo = 'AREA'.
    ls_subobject-objnm = ls_dta-infoarea.
    append ls_subobject to lt_subobject.

    call function 'RSSB_AUTHORITY_CHECK'
      exporting
        i_s_object                 = ls_object
        i_t_subobject              = lt_subobject
        i_activity                 = rssb_c_bw_activity-data_display
      exceptions
        only_display_authorization = 1
        no_authority               = 2
        invalid_parameter          = 3
        internal_error             = 4
        query_not_exist            = 5
        others                     = 6.
    if sy-subrc <> 0.
      _add_spool( `@5D@ Read TD: No authorisation to read InfoProv` ).
      raise error.
    endif.

    read table gt_obj_dta assigning <ls_obj_dta>
      with key
        objnm = i_infoprov
        objtp = 'TD'.
    if sy-subrc <> 0.

      append initial line to gt_obj_dta assigning <ls_obj_dta>.
      <ls_obj_dta>-objnm = i_infoprov.
      <ls_obj_dta>-objtp = 'TD'.
    endif.

    "Create the structures for the InfoProvider read.
    case ls_dta-tlogo.
      when 'CUBE'.

        create data lt_data type table of (ls_dta-viewtiobjnm2).
        create data <ls_obj_dta>-t_data type table of (ls_dta-viewtiobjnm2).

      when 'ODSO'.

        create data lt_data type table of (ls_dta-viewiobj).
        create data <ls_obj_dta>-t_data type table of (ls_dta-viewiobj).

      when others.
        message 'Inavlid InfoProvider type' type 'E'.
    endcase.

    assign <ls_obj_dta>-t_data->* to <lt_data>.
    assign lt_data->* to <lt_data_pack>.

    "check the global Read selections are appropriate for the Infoprovider being read
    loop at gt_rsdri_range into ls_range.

      read table gt_dta_iobj transporting no fields
        with key
          infoprov = i_infoprov
          iobjnm   = ls_range-chanm.
      if sy-subrc <> 0.
        _add_spool( `@5D@ Read TD: Selection on characteristic ` && ls_range-chanm && ` is invalid for InfoProvider `  && i_infoprov ).
        continue.
      else.
        append ls_range to lt_range.
      endif.
    endloop.

    "Create the InfoObject to fieldname list.
    loop at gt_dta_pro into ls_dta_pro where infoprov = i_infoprov.

      check ls_dta_pro-iobjtp     <> 'DPA'. "NO tehnical InfoObjects
      check ls_dta_pro-atrnavfl   <> 'X'.   "NO attribute InfoObjects
      check ls_dta_pro-atronlyfl  <> 'X'.   "NO attribute only InfoObjects
      check ls_dta_pro-metaiobjfl <> 'X'.   "NO meta-data objects

      case ls_dta-tlogo.
        when 'CUBE'. l_fieldnm = ls_dta_pro-iobjnm.
        when 'ODSO'. l_fieldnm = iobjnm_fieldnm( ls_dta_pro-iobjnm ).
        when others.
          message 'Invalid InfoProvider type' type 'E'.
      endcase.

      clear: ls_sfk, ls_sfc.
      if ls_dta_pro-iobjtp <> 'KYF'.
        ls_sfc-chanm    = ls_dta_pro-iobjnm.
        ls_sfc-chaalias = l_fieldnm.
        insert ls_sfc into table lh_sfc.
      else.
        ls_sfk-kyfnm    = ls_dta_pro-iobjnm.
        ls_sfk-kyfalias = l_fieldnm.
        insert ls_sfk into table lh_sfk.
      endif.
    endloop.

    "Create an instance of the RSDRI class as a global variable.
    if i_package_num = 1.

      clear: go_rsdri_infoprov.

      try.

          create object go_rsdri_infoprov
            exporting
              i_infoprov    = i_infoprov
            exceptions
              illegal_input = 1
              others        = 2.

        catch cx_sy_create_object_error.

          _add_spool( `@5C@ Read TD: Unable to create instance of CL_RSDRI_INFOPROV` ).
          raise error.
      endtry.
    endif.

    l_packagesize = g_package_size.

    do.

      call method go_rsdri_infoprov->read
        exporting
          i_th_sfc               = lh_sfc
          i_th_sfk               = lh_sfk
          i_t_range              = lt_range
          i_rollup_only          = ''
          i_packagesize          = l_packagesize
          i_caller               = rsdrs_c_caller-datamart "Prevents BIA call
          i_authority_check      = 'R'
          i_currency_conversion  = 'X'
          i_use_db_aggregation   = 'X'
          i_use_aggregates       = 'X'
        importing
          e_t_data               = <lt_data_pack>
          e_end_of_data          = l_end_of_data
          e_t_msg                = lt_msg
        exceptions
          illegal_download       = 1
          illegal_input          = 2
          illegal_input_range    = 3
          illegal_input_sfc      = 4
          illegal_input_sfk      = 5
          illegal_input_tablesel = 6
          illegal_tablename      = 7
          inherited_error        = 8
          no_authorization       = 9
          trans_no_write_mode    = 10
          x_message              = 11
          others                 = 12.
      if sy-subrc <> 0.

        _add_spool( `@5C@ Read TD: Error in InfoProvider read, ` && i_infoprov ).
        clear go_rsdri_infoprov.
        raise end_of_data.
      endif.

      append lines of <lt_data_pack> to <lt_data>.

      if l_end_of_data = 'X'.

        _add_spool( `@5B@ Read TD: ` && |{ lines( <lt_data> ) }| && ` records read for InfoProvider ` && i_infoprov && ` in package ` && i_package_num ).
        clear go_rsdri_infoprov.
        raise end_of_data.
      endif.

      if lines( <lt_data> ) >= g_package_size.
        _add_spool( `@5B@ Read TD: ` && |{ lines( <lt_data> ) }| && ` records read for InfoProvider ` && i_infoprov && ` in package ` && i_package_num ).
        return.
      endif.
    enddo.
  endmethod.                    "read_td_pack


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YGTTC_CL_CBID_PACK->SET_DIRECTORY
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DIRECTORY                    TYPE        STRING
* | [--->] I_USEGUI                       TYPE        RS_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method set_directory.

    data:
      l_directory              type string,
      l_result                 type rs_bool.


    "Check the directory is not initial.
    if i_directory is not initial.
      l_directory = i_directory.
    else.
      message 'Directory is initial' type 'E'.
    endif.

    "If the files are stored on the users local server.
    if i_usegui = 'X'.

      "Check the directory exists.
      call method cl_gui_frontend_services=>directory_exist
        exporting
          directory            = l_directory
        receiving
          result               = l_result
        exceptions
          cntl_error           = 1
          error_no_gui         = 2
          wrong_parameter      = 3
          not_supported_by_gui = 4
          others               = 5.
      if sy-subrc <> 0 or l_result = ''.

        "Support the user in selecting a valid directory.
        call method cl_gui_frontend_services=>directory_browse
          changing
            selected_folder      = l_directory
          exceptions
            cntl_error           = 1
            error_no_gui         = 2
            not_supported_by_gui = 3
            others               = 4.
        if sy-subrc <> 0.
          message id sy-msgid type sy-msgty number sy-msgno
                     with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        endif.
      endif.

      "Ensure the directory string is terminated with the '\' character.
      if not l_directory cp '*\'.
        l_directory = l_directory && '\'.
      endif.

      "Assign the local variable to the global variable.
      g_directory = l_directory.
      g_usegui = i_usegui.
    endif.

    "If the file is stored on the SAP server.
    if i_usegui <> 'X'.

      "Ensure the directory string is terminated with the '/' character.
      if not l_directory cp '*/'.
        l_directory = l_directory && '/'.
      endif.

      "Assign the local variable to the global variable.
      g_directory = l_directory.
      g_usegui = i_usegui.
    endif.
  endmethod.                    "set_directory


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YGTTC_CL_CBID_PACK->SET_PARAMETER
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_PARAMETER                    TYPE        STRING
* | [--->] I_VALUE                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method set_parameter.

    data:
      l_parameter              type string.

    field-symbols:
      <l_parameter>            type any.


    l_parameter = to_upper( |{ 'G_' && i_parameter }| ).

    case l_parameter.
      when 'DIRECTORY'.

        "Do nothing, the directory needs both the G_USEGUI
        "and G_DIRECTORY setting at the same time.

      when 'FILE_FORMAT'.

        "Do nothing, this parameter cannot be changed after
        "the class has been initialised, the setting apply
        "to all data objects.

      when others.

        assign (i_parameter) to <l_parameter>.
        if sy-subrc = 0.
          <l_parameter> = i_value.
        endif.

    endcase.
  endmethod.                    "set_parameter


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YGTTC_CL_CBID_PACK->VIEW_SPOOL
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method view_spool.

    data:
      lo_salv_table              type ref to cl_salv_table,
      lo_salv_functions_list     type ref to cl_salv_functions_list,
      lo_salv_display_settings   type ref to cl_salv_display_settings,
      lo_salv_columns            type ref to cl_salv_columns,
      lo_salv_column             type ref to cl_salv_column,
      lo_columns                 type ref to cl_salv_columns_table,
      lo_column                  type ref to cl_salv_column_table,
      lo_salv_sorts              type ref to  cl_salv_sorts.

    data:
      l_spool                    type string,
      begin of ls_table,
        line                     type c length 256,
      end of ls_table,
      lt_table                   like table of ls_table.


    if gt_spool is initial.
      message 'No spool messages available' type 'S'.
    endif.

    loop at gt_spool into l_spool.

      ls_table-line = l_spool.
      append ls_table to lt_table.
    endloop.

    try.

        call method cl_salv_table=>factory
          importing
            r_salv_table = lo_salv_table
          changing
            t_table      = lt_table.

        call method lo_salv_table->display.

      catch cx_sy_no_handler.
        return.
      catch cx_sy_move_cast_error.
        return.
      catch cx_salv_not_found.
        return.
      catch cx_salv_msg.
        return.
    endtry.
  endmethod.                    "view_spool


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YGTTC_CL_CBID_PACK->WRITE_MA_PACK
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_IOBJNM                       TYPE        RSIOBJNM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method write_ma_pack.

    data:
      ls_obj_dta               type s_obj_dta.

    data:
      ls_viobj                 type rsd_s_viobj.

    data:
      ls_iobj_cmp              type rsd_s_iobj_cmp.

    data:
      ls_attributes            type rsd_s_iobjnm,
      lt_attributes            type table of rsd_s_iobjnm,
      ls_attribute_vls         type rsndi_s_chavl,
      lt_attribute_vls         type table of rsndi_s_chavl,
      ls_messages              type rsndi_s_message,
      lt_messages              type table of rsndi_s_message,
      l_s_msg                  type bal_s_msg.

    data:
      ls_data                  type ref to data,
      lt_data_pack             type ref to data,
      l_fieldnm                type string,
      l_iobjnm                 type rsiobjnm,
      l_attrinm                type rsiobjnm,
      l_lines                  type i.

    data:
      l_kf1                    type string,
      l_kf2                    type string,
      l_kf3                    type string.

    data:
      lt_component             type cl_abap_structdescr=>component_table,
      ls_component             type cl_abap_structdescr=>component,
      lo_structdescr           type ref to cl_abap_structdescr.

    field-symbols:
      <ls_component>           type cl_abap_structdescr=>component.

    data:
      l_no_of_packages         type i,
      l_package_num            type i,
      l_from                   type i,
      l_to                     type i.

    field-symbols:
      <l_data>                 type any,
      <ls_data>                type any,
      <lt_data>                type standard table,
      <lt_data_pack>           type standard table.

    data:
      l_subrc                   type i.
    "End of variable declaration.


    "Check the system type, write function in production is not supported.
    if g_cccategory = 'P'.
      msg_spool_rc `I|YGTTC_CBID|001` `` `` `` ``. return.
    endif.

    "Retrieve the internal data for the InfoObject.
    read table gt_obj_dta into ls_obj_dta
      with key
        objnm = i_iobjnm
        objtp = 'MA'.
    if sy-subrc <> 0.
      return.
    endif.

    "Get the master data table from the class storage.
    assign ls_obj_dta-t_data->* to <lt_data>.

    "Raise message indicating that no data has been found for the InfoObject.
    if <lt_data> is not assigned or <lt_data> is initial.
      return.
    endif.

    "Check if the InfoObject is a reference InfoObject.
    read table gh_viobj into ls_viobj
      with table key
        iobjnm = i_iobjnm
        objvers = 'A'.
    if sy-subrc = 0.
      if ls_viobj-bchreffl = 'X'.
        l_iobjnm = ls_viobj-chabasnm.
      else.
        l_iobjnm = i_iobjnm.
      endif.
    else.
      return.
    endif.

    "Ensure that there are no duplicates in the master data that
    "would have occured due to both an active and modified version
    "of the master data record being extracted.
    loop at gt_iobj_cmp into ls_iobj_cmp where iobjnm = i_iobjnm.

      "Check if the InfoObject is a reference InfoObject.
      read table gh_viobj into ls_viobj
        with table key
          iobjnm = ls_iobj_cmp-iobjcmp
          objvers = 'A'.
      if sy-subrc = 0.
        if ls_viobj-bchreffl = 'X'.
          l_fieldnm = iobjnm_fieldnm( ls_viobj-chabasnm ).
        else.
          l_fieldnm = iobjnm_fieldnm( ls_iobj_cmp-iobjcmp ).
        endif.
      else.
        return.
      endif.

      case abs( ls_iobj_cmp-posit ).
        when 1. l_kf1 = l_fieldnm.
        when 2. l_kf2 = l_fieldnm.
        when 3. l_kf3 = l_fieldnm.
      endcase.
    endloop.

    l_fieldnm = iobjnm_fieldnm( l_iobjnm ).

    "Remove duplicates.
    sort <lt_data> ascending.
    delete adjacent duplicates from <lt_data> comparing (l_fieldnm) (l_kf1) (l_kf2) (l_kf3).

    "Create a structure from the master data table and the table
    "required for packetisation of the data..
    create data ls_data like line of <lt_data>.
    assign ls_data->* to <ls_data>.

    create data lt_data_pack like <lt_data>.
    assign lt_data_pack->* to <lt_data_pack>.

    "Get the component list for the table.
    lo_structdescr ?= cl_abap_typedescr=>describe_by_data( <ls_data> ).
    lt_component    = lo_structdescr->get_components( ).

    "Delete technical fields.
    delete lt_component where name = 'OBJVERS' or name = 'CHANGED'.

    "Work out how many packages the dataset consists off.
    if g_package_size is not initial.

      try.

          l_no_of_packages =  lines( <lt_data> ) div g_package_size + 1.
        catch cx_sy_zerodivide.

      endtry.
    else.
      l_no_of_packages = 1.
    endif.

    do l_no_of_packages times.
      l_package_num = sy-index.

      "Skip some packages as defined in the global variable.
      if g_skip_packages <> 0.
        check l_package_num > g_skip_packages.
      endif.

      "Only process a maximum number of packages.
      if g_no_of_packages <> 0.
        check l_package_num <= g_skip_packages + g_no_of_packages.
      endif.

      "Define the data package subset start and end and create the data pack
      if g_package_size is not initial.

        "Create the temporary package.
        try. "NCE jibberish
            l_to = l_package_num  * g_package_size.
            l_from = l_to - g_package_size + 1.
          catch cx_sy_arithmetic_overflow.
        endtry.

        if l_to > lines( <lt_data> ).
          l_to = lines( <lt_data> ).
        endif.

        clear <lt_data_pack>.
        append lines of <lt_data> from l_from to l_to to <lt_data_pack>.
      else.
        <lt_data_pack> = <lt_data>.
      endif.

      "Clear the attribute tables before filling and each package.
      clear:
        lt_attribute_vls,
        lt_messages.

      "Fill the table indicating the attributes being transfered.
      loop at lt_component into ls_component.

        ls_attributes = fieldnm_iobjnm( |{ ls_component-name }| ).
        append ls_attributes to lt_attributes.
      endloop.

      "Fill the table with the attribute values.
      loop at <lt_data_pack> assigning <ls_data>.

        ls_attribute_vls-record_no = sy-tabix.

        loop at lt_component into ls_component.

          assign component ls_component-name of structure <ls_data> to <l_data>.
          ls_attribute_vls-iobjnm = fieldnm_iobjnm( |{ ls_component-name }| ).

          try.
              ls_attribute_vls-value = <l_data>.
            catch cx_sy_conversion_error.
              continue.
          endtry.

          append ls_attribute_vls to lt_attribute_vls.
        endloop.
      endloop.

      call function 'RSNDI_MD_ATTRIBUTES_UPDATE'
        exporting
          i_iobjnm       = l_iobjnm
        importing
          e_subrc        = l_subrc
        tables
          i_t_attributes = lt_attributes
          i_t_data       = lt_attribute_vls
          e_t_messages   = lt_messages.

      if l_subrc = 0.
        _add_spool( `@5B@ Write MD Attributes: Package ` && l_package_num && ` for InfoObject ` && i_iobjnm ).
      else.

        _add_spool( `@5C@ Write MD Attributes: Package ` && l_package_num && ` for InfoObject ` && i_iobjnm ).
        loop at lt_messages into ls_messages.
          _add_spool( |{ ls_messages-msgtxtp }| ).
        endloop.
      endif.
    enddo.
  endmethod.                    "WRITE_MA_PACK


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YGTTC_CL_CBID_PACK->WRITE_MT_PACK
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_IOBJNM                       TYPE        RSIOBJNM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method write_mt_pack.

    data:
      ls_obj_dta               type s_obj_dta.

    data:
      ls_viobj                 type rsd_s_viobj.

    data:
      ls_iobj_cmp              type rsd_s_iobj_cmp.

    data:
      ls_attribute_vls         type rsndi_s_chavl,
      lt_attribute_vls         type table of rsndi_s_chavl,
      ls_messages              type rsndi_s_message,
      lt_messages              type table of rsndi_s_message,
      l_s_msg                  type bal_s_msg.

    data:
      ls_data                  type ref to data,
      lt_data_pack             type ref to data,
      l_fieldnm                type string,
      l_iobjnm                 type rsiobjnm,
      l_attrinm                type rsiobjnm,
      l_lines                  type i.

    data:
      l_kf1                    type string,
      l_kf2                    type string,
      l_kf3                    type string.

    data:
      l_sqlwhere               type string.

    data:
      lt_component             type cl_abap_structdescr=>component_table,
      ls_component             type cl_abap_structdescr=>component,
      lo_structdescr           type ref to cl_abap_structdescr.

    field-symbols:
      <ls_component>           type cl_abap_structdescr=>component.

    data:
      l_no_of_packages         type i,
      l_package_num            type i,
      l_from                   type i,
      l_to                     type i.

    field-symbols:
      <ls_data>                type any,
      <lt_data>                type standard table,
      <lt_data_pack>           type standard table,
      <l_data>                 type any.

    data:
      l_subrc                   type i.
    "End of variable declaration.


    "Check the system type, write function in production is not supported.
    if g_cccategory = 'P'.
      msg_spool_rc `I|YGTTC_CBID|001` `` `` `` ``. return.
    endif.

    "Retrieve the internal data for the InfoObject.
    read table gt_obj_dta into ls_obj_dta
      with key
        objnm = i_iobjnm
        objtp = 'MT'.
    if sy-subrc <> 0.
      return.
    endif.

    "Get the master data table from the class storage.
    assign ls_obj_dta-t_data->* to <lt_data>.

    "Raise message indicating that no data has been found for the InfoObject.
    if <lt_data> is not assigned or <lt_data> is initial.
      return.
    endif.

    "Check if the InfoObject is a reference InfoObject.
    read table gh_viobj into ls_viobj
      with table key
        iobjnm = i_iobjnm
        objvers = 'A'.
    if sy-subrc = 0.
      if ls_viobj-bchreffl = 'X'.
        l_iobjnm = ls_viobj-chabasnm.
      else.
        l_iobjnm = i_iobjnm.
      endif.
    else.
      return.
    endif.

    if ls_viobj-txttabfl = '1' and ls_viobj-nolangu = ''.
      l_sqlwhere = `langu <> '` && sy-langu && `'`.
      delete <lt_data> where (l_sqlwhere).
    endif.

    "Ensure that there are no duplicates in the master data that
    "would have occured due to both an active and modified version
    "of the master data record being extracted.
    loop at gt_iobj_cmp into ls_iobj_cmp where iobjnm = i_iobjnm.

      "Check if the InfoObject is a reference InfoObject.
      read table gh_viobj into ls_viobj
        with table key
          iobjnm = ls_iobj_cmp-iobjcmp
          objvers = 'A'.
      if sy-subrc = 0.
        if ls_viobj-bchreffl = 'X'.
          l_fieldnm = iobjnm_fieldnm( ls_viobj-chabasnm ).
        else.
          l_fieldnm = iobjnm_fieldnm( ls_iobj_cmp-iobjcmp ).
        endif.
      else.
        return.
      endif.

      case abs( ls_iobj_cmp-posit ).
        when 1. l_kf1 = l_fieldnm.
        when 2. l_kf2 = l_fieldnm.
        when 3. l_kf3 = l_fieldnm.
      endcase.
    endloop.

    l_fieldnm = iobjnm_fieldnm( l_iobjnm ).

    "Remove duplicates.
    sort <lt_data> ascending.
    delete adjacent duplicates from <lt_data> comparing (l_fieldnm) (l_kf1) (l_kf2) (l_kf3).

    "Create a structure from the master data table and the table
    "required for packetisation of the data.
    create data ls_data like line of <lt_data>.
    assign ls_data->* to <ls_data>.

    create data lt_data_pack like <lt_data>.
    assign lt_data_pack->* to <lt_data_pack>.

    "Get the component list for the table.
    lo_structdescr ?= cl_abap_typedescr=>describe_by_data( <ls_data> ).
    lt_component    = lo_structdescr->get_components( ).

    "Delete technical fields.
    delete lt_component where name = 'LANGU'.

    "Work out how many packages the dataset consists off.
    if g_package_size is not initial.

      try.

          l_no_of_packages =  lines( <lt_data> ) div g_package_size + 1.
        catch cx_sy_zerodivide.

      endtry.
    else.
      l_no_of_packages = 1.
    endif.

    do l_no_of_packages times.
      l_package_num = sy-index.

      "Skip some packages as defined in the global variable.
      if g_skip_packages <> 0.
        check l_package_num > g_skip_packages.
      endif.

      "Only process a maximum number of packages.
      if g_no_of_packages <> 0.
        check l_package_num <= g_skip_packages + g_no_of_packages.
      endif.

      "Define the data package subset start and end and create the data pack
      if g_package_size is not initial.

        "Create the temporary package.
        try. "NCE jibberish
            l_to = l_package_num  * g_package_size.
            l_from = l_to - g_package_size + 1.
          catch cx_sy_arithmetic_overflow.
        endtry.

        if l_to > lines( <lt_data> ).
          l_to = lines( <lt_data> ).
        endif.

        clear <lt_data_pack>.
        append lines of <lt_data> from l_from to l_to to <lt_data_pack>.
      else.
        <lt_data_pack> = <lt_data>.
      endif.

      "Clear the attribute tables before filling and each package.
      clear:
        lt_attribute_vls,
        lt_messages.

      loop at <lt_data> assigning <ls_data>.

        ls_attribute_vls-record_no = sy-tabix.

        loop at lt_component into ls_component.

          assign component ls_component-name of structure <ls_data> to <l_data>.
          ls_attribute_vls-iobjnm = fieldnm_iobjnm( |{ ls_component-name }| ).

          try.
              ls_attribute_vls-value = <l_data>.
            catch cx_sy_conversion_error.
              continue.
          endtry.

          append ls_attribute_vls to lt_attribute_vls.
        endloop.
      endloop.

      _add_spool( `@5B@ Write MD Texts: First atempt to load master data.` ).

      call function 'RSNDI_MD_TEXTS_UPDATE'
        exporting
          i_iobjnm     = l_iobjnm
          i_langu      = sy-langu
        importing
          e_subrc      = l_subrc
        tables
          i_t_data     = lt_attribute_vls
          e_t_messages = lt_messages.

      if l_subrc = 0.
        _add_spool( `@5B@ Write MD Texts: Package ` && l_package_num && ` for InfoObject ` && i_iobjnm ).
      else.

        "Output messages.
        loop at lt_messages into ls_messages.
          _add_spool( `@5F@ ` && |{ ls_messages-msgtxtp }| ).
        endloop.

        _add_spool( `@5D@ Write MD Texts: Second atempt to load master data.` ).

        "Delete known errors and try again.
        loop at lt_messages into ls_messages.
          if ls_messages-msgid = 'RSDMD' and ls_messages-msgno = 194.
            delete lt_attribute_vls where record_no = ls_messages-msgv2.
          endif.
        endloop.

        clear lt_messages.

        call function 'RSNDI_MD_TEXTS_UPDATE'
          exporting
            i_iobjnm     = l_iobjnm
            i_langu      = sy-langu
          importing
            e_subrc      = l_subrc
          tables
            i_t_data     = lt_attribute_vls
            e_t_messages = lt_messages.

        if l_subrc = 0.
          _add_spool( `@5B@ Write MD Texts: Package ` && l_package_num && ` for InfoObject ` && i_iobjnm ).
        else.

          _add_spool( `@5C@ Write MD Texts: Package ` && l_package_num && ` for InfoObject ` && i_iobjnm ).

          "Output messages.
          loop at lt_messages into ls_messages.
            _add_spool( `@5F@ ` && |{ ls_messages-msgtxtp }| ).
          endloop.
        endif.
      endif.
    enddo.
  endmethod.                    "WRITE_MT_PACK


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YGTTC_CL_CBID_PACK->WRITE_TD_PACK
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_INFOPROV_SRC                 TYPE        RSINFOPROV
* | [--->] I_INFOPROV_TRG                 TYPE        RSINFOPROV
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method write_td_pack.

    data:
      ls_dta_src               type rsd_s_dta,
      ls_dta_trg               type rsd_s_dta.

    data:
      l_requid                 type rsrequnr,
      l_records                type i,
      lt_data                  type ref to data,
      lt_data_tmp              type ref to data,
      ls_data_pack             type ref to data,
      lt_data_pack             type ref to data.

    data:
      l_no_of_packages       type i,
      l_package_num          type i,
      l_from                 type i,
      l_to                   type i.

    data:
      ls_msg                   type rsdri_s_msg,
      lt_msg                   type rsdri_ts_msg.

    field-symbols:
      <l_data>                 type any,
      <ls_data>                type any,
      <lt_data>                type standard table,
      <lt_data_tmp>            type standard table,
      <l_data_pack>            type any,
      <ls_data_pack>           type any,
      <lt_data_pack>           type standard table.

    data:
      ls_map                   type s_cha_map,
      lt_map                   type t_cha_map.

    data:
      lo_typedescr           type ref to cl_abap_typedescr,
      lo_structdescr         type ref to cl_abap_structdescr,
      lo_tabledescr          type ref to cl_abap_tabledescr,
      lt_component           type abap_component_tab,
      ls_component           type abap_componentdescr.

    data:
      ls_obj_dta        type s_obj_dta.
    "End of variable declaration.


    "check the system type, write function in production is not supported.
    if g_cccategory = 'P'.
      return.
    endif.

    "Check whether data exists for the source InfoProvider.
    read table gt_obj_dta into ls_obj_dta
      with key
        objnm = i_infoprov_src
        objtp = 'TD'.
    if sy-subrc <> 0.
      return.
    endif.

    "Move the global data to the local variable.
    assign ls_obj_dta-t_data->* to <lt_data>.
    if sy-subrc <> 0 or lines( <lt_data> ) = 0.
      return.
    endif.

    create data lt_data_tmp like <lt_data>.
    assign lt_data_tmp->* to <lt_data_tmp>.

    "Get the source InfoProvider type, DSO or InfoCube.
    read table gt_dta into ls_dta_src
      with key
        infoprov = i_infoprov_src.

    "Get the target InfoProvider type, DSO or InfoCube.
    read table gt_dta into ls_dta_trg
      with key
        infoprov = i_infoprov_trg.

    case ls_dta_trg-tlogo.
      when 'CUBE'.

        "Create the target data object for the Write data
        create data ls_data_pack type (ls_dta_trg-viewtiobjnm2).
        create data lt_data_pack type table of (ls_dta_trg-viewtiobjnm2).

      when 'ODSO'.

        "Create the target data object for the Write data
        create data ls_data_pack type (ls_dta_trg-viewiobj).
        create data lt_data_pack type table of (ls_dta_trg-viewiobj).

      when others.
        message 'Invalid InfoProvider type' type 'E'.
    endcase.

    "Assign the target data objects.
    assign ls_data_pack->* to <ls_data_pack>.
    assign lt_data_pack->* to <lt_data_pack>.

    "Work out how many packages the dataset consists off.
    if g_package_size is not initial.

      try.

          l_no_of_packages =  lines( <lt_data> ) div g_package_size.

          if lines( <lt_data> ) mod g_package_size <> 0.
            l_no_of_packages = l_no_of_packages + 1.
          endif.

        catch cx_sy_zerodivide.

      endtry.
    else.
      l_no_of_packages = 1.
    endif.

    do l_no_of_packages times.
      l_package_num = sy-index.

      "Skip some packages as defined in the global variable.
      if g_skip_packages <> 0.
        check l_package_num > g_skip_packages.
      endif.

      "Only process a maximum number of packages.
      if g_no_of_packages <> 0.
        check l_package_num <= g_skip_packages + g_no_of_packages.
      endif.

      "Create the temporary package.
      try. "NCE jibberish
          l_to = l_package_num  * g_package_size.
          l_from = l_to - g_package_size + 1.
        catch cx_sy_arithmetic_overflow.
      endtry.

      if l_to > lines( <lt_data> ).
        l_to = lines( <lt_data> ).
      endif.

      clear <lt_data_tmp>.
      append lines of <lt_data> from l_from to l_to to <lt_data_tmp>.

      "If the source and target structure are the same simply move
      "the data from one structure to the other in packages.
      if i_infoprov_src = i_infoprov_trg.

        <lt_data_pack> = <lt_data_tmp>.
      else.

        if ls_dta_src-tlogo = ls_dta_trg-tlogo.
          loop at <lt_data_tmp> assigning <ls_data>.

            move-corresponding <ls_data> to <ls_data_pack>.
            append <ls_data_pack> to <lt_data_pack>.
          endloop.
        else.

          "Get the components of the source structure.
          lo_tabledescr  ?= cl_abap_typedescr=>describe_by_data( <lt_data_tmp> ).
          lo_structdescr ?= lo_tabledescr->get_table_line_type( ).
          lt_component    = lo_structdescr->get_components( ).

          if ls_dta_src-tlogo = 'CUBE' and ls_dta_trg-tlogo = 'ODSO'.

            "Create the mapping table.
            clear lt_map.
            loop at lt_component into ls_component.

              ls_map-from = ls_component-name.
              ls_map-to = iobjnm_fieldnm( |{ ls_component-name }| ).
              append ls_map to lt_map.
            endloop.
          endif.

          if ls_dta_src-tlogo = 'ODSO' and ls_dta_trg-tlogo = 'CUBE'.

            "Create the mapping table.
            clear lt_map.
            loop at lt_component into ls_component.

              ls_map-from = ls_component-name.
              ls_map-to = fieldnm_iobjnm( |{ ls_component-name }| ).
              append ls_map to lt_map.
            endloop.
          endif.

          "Move data from the source to the target structure.
          loop at <lt_data> assigning <ls_data>.

            loop at lt_map into ls_map.
              unassign:
                <l_data>,
                <l_data_pack>.

              assign component ls_map-from of structure <ls_data> to <l_data>.
              assign component ls_map-to of structure <ls_data_pack> to <l_data_pack>.

              if <l_data> is assigned and <l_data_pack> is assigned.
                <l_data_pack> = <l_data>.
              endif.
            endloop.

            append <ls_data_pack> to <lt_data_pack>.
          endloop.
        endif.
      endif.

      "Write the data to the InfoProviders.
      case ls_dta_trg-tlogo.
        when 'CUBE'.

          "Generate data transfer program.
          call function 'RSDRI_INFOCUBE_GEN_PROGRAM'
            exporting
              i_infocube = i_infoprov_trg
            exceptions
              others     = 1.
          if sy-subrc <> 0.
            _add_spool( `@5C@ Write TD: Unable to generate update program for InfoCube ` && i_infoprov_trg ).
            return.
          endif.

          "Write to the InfoCubes.
          clear lt_msg.
          call function 'RSDRI_CUBE_WRITE_PACKAGE'
            exporting
              i_infocube         = i_infoprov_trg
              i_curr_conversion  = ''
              i_mdata_check      = ''
            importing
              e_requid           = l_requid
              e_records          = l_records
              e_ts_msg           = lt_msg
            changing
              c_t_data           = <lt_data_pack>
            exceptions
              infocube_not_found = 1
              illegal_input      = 2
              rollback_error     = 3
              duplicate_records  = 4
              request_locked     = 5
              not_transactional  = 6
              inherited_error    = 7
              others             = 8.
          if sy-subrc = 0.
            _add_spool( `@5B@ Write TD: Package ` && l_package_num && ` from ` && i_infoprov_src && ` to ` && i_infoprov_trg ).
          else.
            _add_spool( `@5C@ Write TD: Package ` && l_package_num && ` from ` && i_infoprov_src && ` to ` && i_infoprov_trg ).
          endif.

        when 'ODSO'.

          "For write-optimised DSOs populate the REQUEST and RECORD fields.
          "The content of these fields are removed on the read and export.
          if ls_dta_trg-tlogo = 'W'.

            loop at <lt_data_pack> assigning <ls_data_pack>.

              assign component 'REQUEST' of structure <ls_data_pack> to <l_data_pack>.
              if sy-subrc = 0.
                <l_data_pack> = g_request.
              endif.

              assign component 'RECORD' of structure <ls_data_pack> to <l_data_pack>.
              if sy-subrc = 0.
                <l_data_pack> = sy-tabix.
              endif.
            endloop.
          endif.

          "Write the data directly to the Active table of the DSO.
          modify (ls_dta_trg-viewiobj)
            from table <lt_data_pack>.
          if sy-subrc = 0.
            _add_spool( `@5B@ Write TD: Package ` && l_package_num && ` from ` && i_infoprov_src && ` to ` && i_infoprov_trg ).
          else.
            _add_spool( `@5C@ Write TD: Package ` && l_package_num && ` from ` && i_infoprov_src && ` to ` && i_infoprov_trg ).
          endif.
      endcase.
    enddo.
  endmethod.                    "WRITE_TD_PACK


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YGTTC_CL_CBID_PACK->_ADD_SPOOL
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_MESSAGE                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
method _add_spool.

  data:
    l_msgty                type string,
    l_msgid                type string,
    l_msgno                type string,
    l_detlevel             type string,
    l_probclass            type string,
    l_msg_200              type c length 200.

  data:
    l_space                type c length 200,
    l_off                  type i.

  data:
    ls_msg                 type  bal_s_msg.

  data:
    ls_log_handle          type bal_s_logh,
    lt_log_handle          type bal_t_logh,
    lt_new_lognumbers      type bal_t_lgnm.

  data
    l_spool                type string.


  if strlen( i_message ) > 1 and i_message(1) = '<'.

    find '>' in i_message match offset l_off.

    case l_off.
      when 4.
        l_msg_200 = i_message+5.
        l_msgty = i_message+1(1).
        l_msgid = 'YGTTC_CBID'.
        l_msgno = '000'.
        l_detlevel = i_message+2(1).
        l_probclass = i_message+3(1).

      when 7.
        l_msg_200 = i_message+8.
        l_msgty = i_message+1(1).
        l_msgid = 'YGTTC_CBID'.
        l_msgno = i_message+2(3).
        l_detlevel = i_message+5(1).
        l_probclass = i_message+6(1).
      when others.

        l_msg_200 = i_message.
        l_msgid = 'YGTTC_CBID'.
        l_msgno = '000'.
    endcase.

    case l_msgty.
      when 'E'. l_spool = `@5C@ ` && l_msg_200.
      when 'W'. l_spool = `@5D@ ` && l_msg_200.
      when 'I'. l_spool = `@5B@ ` && l_msg_200.
    endcase.
  else.

    l_spool = i_message.
    l_msg_200 = i_message.
    l_msgid = 'YGTTC_CBID'.
    l_msgno = '000'.
  endif.

  if g_appl_log = 'X'.

    if g_log_handle is initial.

      gs_log-object    = 'RSBATCH'.
      gs_log-subobject = 'PROT'.
      try.
          gs_log-extnumber = |{ 'ZCBID_' && cl_system_uuid=>create_uuid_c26_static( ) }|.
          gs_log-extnumber = gs_log-extnumber(30).
        catch cx_uuid_error.
      endtry.
      gs_log-alprog = 'YGTTC_CBID_PACK'.

      call function 'BAL_LOG_CREATE'
        exporting
          i_s_log                 = gs_log
        importing
          e_log_handle            = g_log_handle
        exceptions
          log_header_inconsistent = 1
          others                  = 2.
      if sy-subrc <> 0.
        return.
      endif.
    endif.

    ls_msg-msgty     = l_msgty.
    ls_msg-msgid     = l_msgid.
    ls_msg-msgno     = l_msgno.
    ls_msg-msgv1     = l_msg_200(50).
    ls_msg-msgv2     = l_msg_200+50(50).
    ls_msg-msgv3     = l_msg_200+100(50).
    ls_msg-msgv4     = l_msg_200+150(50).
    ls_msg-detlevel  = l_detlevel.
    ls_msg-probclass = l_probclass.

    call function 'BAL_LOG_MSG_ADD'
      exporting
        i_log_handle     = g_log_handle
        i_s_msg          = ls_msg
      exceptions
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        others           = 4.
    if sy-subrc <> 0.
      return.
    endif.

    ls_log_handle-sign   = 'I'.
    ls_log_handle-option = 'EQ'.
    ls_log_handle-low    = g_log_handle.
    append ls_log_handle to lt_log_handle.

    call function 'BAL_DB_SAVE'
      exporting
        i_save_all       = 'X'
      importing
        e_new_lognumbers = lt_new_lognumbers
      exceptions
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        others           = 4.
    if sy-subrc <> 0.
      return.
    endif.
  endif.

  append l_spool to gt_spool.
endmethod.                    "_ADD_SPOOL


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method YGTTC_CL_CBID_PACK=>_CHANM_CHABASNM
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_CHANM                        TYPE        RSCHANM
* | [<-()] R_CHABASNM                     TYPE        RSCHABASNM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method _chanm_chabasnm.
    select single chabasnm into r_chabasnm from rsdcha where chanm = i_chanm and objvers = 'A'.
  endmethod.                    "_chanm_chabasnm


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method YGTTC_CL_CBID_PACK=>_IOBJNM_DTELNM
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_IOBJNM                       TYPE        RSIOBJNM
* | [<-()] R_DTELNM                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method _iobjnm_dtelnm.

    data:
      l_iobjnm            type rsd_s_iobj-iobjnm,
      l_dtelnm            type rollname.

    l_iobjnm = i_iobjnm.

    call function 'RSD_DTELNM_GET_FOR_IOBJ'
      exporting
        i_iobjnm   = l_iobjnm
      importing
        e_dtelnm   = l_dtelnm
      exceptions
        name_error = 1
        others     = 2.
    if sy-subrc <> 0.
      return.
    endif.

    r_dtelnm = l_dtelnm.

  endmethod.                    "_iobjnm_dtelnm


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method YGTTC_CL_CBID_PACK=>_MSG_SPOOL_RC
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_MSG                          TYPE        ANY
* | [--->] I_V1                           TYPE        ANY
* | [--->] I_V2                           TYPE        ANY
* | [--->] I_V3                           TYPE        ANY
* | [--->] I_V4                           TYPE        ANY
* | [<---] E_T_MSG                        TYPE        RS_T_MSG
* | [<---] E_T_SPOOL                      TYPE        STRINGTAB
* | [<---] E_RC                           TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method _msg_spool_rc.

    data:
      l_msg               type string,
      l_s_msg             type bal_s_msg,
      l_t_msg             type rs_t_msg,
      l_v1                type string,
      l_v2                type string,
      l_v3                type string,
      l_v4                type string,
      l_fieldvl           type string,
      l_t_fieldvls        type stringtab,
      l_spool             type string,
      l_msg_text          type gsugi_pa-rcmessage.

    l_msg = i_msg.

    split  l_msg at '|' into table  l_t_fieldvls.

    loop at  l_t_fieldvls into  l_fieldvl.

      case sy-tabix.
        when 1.
          l_s_msg-msgty = l_fieldvl.
        when 2.
          l_s_msg-msgid = l_fieldvl.
        when 3.
          l_s_msg-msgno = l_fieldvl.
        when others.
          exit.
      endcase.
    endloop.

    l_s_msg-msgv1 = i_v1.
    l_s_msg-msgv2 = i_v2.
    l_s_msg-msgv3 = i_v3.
    l_s_msg-msgv4 = i_v4.

    append l_s_msg to l_t_msg.
    append lines of l_t_msg to e_t_msg.

    call function 'SPDA_CONVERT_MESSAGE_TO_TEXT'
      exporting
        msg_class  = l_s_msg-msgid
        msg_number = l_s_msg-msgno
        msg_var1   = l_s_msg-msgv1
        msg_var2   = l_s_msg-msgv2
        msg_var3   = l_s_msg-msgv3
        msg_var4   = l_s_msg-msgv4
      importing
        msg_text   = l_msg_text.

    case l_s_msg-msgty.
      when 'W'.
        e_rc = 4.
        l_spool = '@09@'.
      when 'E'.
        e_rc = 8.
        l_spool = '@0A@'.
      when others.
        e_rc = 0.
        l_spool = '@08@'.
    endcase.

    l_spool = l_spool && l_msg_text.
    append l_spool to e_t_spool.
  endmethod.                    "_MSG_SPOOL_RC
ENDCLASS.
