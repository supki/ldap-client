{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "ncp";
    version = "0.6.0";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/ncp/-/ncp-0.6.0.tgz";
      sha1 = "df8ce021e262be21b52feb3d3e5cfaab12491f0d";
    };
    deps = [];
    meta = {
      homepage = "https://github.com/AvianFlu/ncp";
      description = "Asynchronous recursive file copy utility.";
      keywords = [ "cli" "copy" ];
    };
  }