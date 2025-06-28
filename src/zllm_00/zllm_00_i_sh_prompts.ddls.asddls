@AbapCatalog.sqlViewName: 'ZLLM00ISHPRO00'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help Bin'
@Metadata.ignorePropagatedAnnotations: true
define view ZLLM_00_I_SH_PROMPTS
  as select from ZLLM_00_I_SH_PROMPTS_01
{
      //key bin,
      //key name,
      //substring( name, 1, len ) as  prompt_name,
      replace( name, '.USR.MD', '' ) as name,
      name as file_name,
      v,
      ts,
      cdate
      //bin
}
where bin_user = $session.user and name like '%.USR.MD'
