@AbapCatalog.sqlViewName: 'ZLLM00ISHPRO01'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help Bin'
@Metadata.ignorePropagatedAnnotations: true
define view ZLLM_00_I_SH_PROMPTS_01 as select from zllm_00_bin
{
    key bin,
    key name,
    v,
    ts,
    cdate,
    substring( bin, 7, 23) as  bin_user,
    length( name) as len
    //bin
} where bin like '$ZRAY_%'  

