{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "dtrace-provider";
    version = "0.6.0";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/dtrace-provider/-/dtrace-provider-0.6.0.tgz";
      sha1 = "0b078d5517937d873101452d9146737557b75e51";
    };
    deps = with nodePackages; [
      nan_2-1-0
    ];
    meta = {
      homepage = "https://github.com/chrisa/node-dtrace-provider#readme";
      description = "Native DTrace providers for node.js applications";
      keywords = [ "dtrace" ];
    };
  }