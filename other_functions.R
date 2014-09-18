name_cleanup = function(x) {
  x[x=="Robert Griffin III"] = "Robert Griffin"
  x = gsub(" D/ST\u00A0D/ST","",x)
  return(x)
}
