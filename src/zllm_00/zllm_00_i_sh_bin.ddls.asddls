@AbapCatalog.sqlViewName: 'ZLLM00ISHBIN00'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Search Help Bin'
@Metadata.ignorePropagatedAnnotations: true
define view ZLLM_00_I_SH_BIN as select from zllm_00_bin
{
   bin 
} group by bin 
