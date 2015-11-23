{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "json-schema";
    version = "0.2.2";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/json-schema/-/json-schema-0.2.2.tgz";
      sha1 = "50354f19f603917c695f70b85afa77c3b0f23506";
    };
    deps = [];
    meta = {
      description = "JSON Schema validation and specifications";
      keywords = [ "json" "schema" ];
    };
  }