{ withControl ?  false , callCabal2nixWithOptions , lib, ... }: callCabal2nixWithOptions 
  "eventlog-live-otelcol" 
  (lib.cleanSource ../../eventlog-live-otelcol) 
  (lib.optionalString withControl "-fcontrol") 
  { }
