{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "dtrace-provider";
    version = "0.2.8";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/dtrace-provider/-/dtrace-provider-0.2.8.tgz";
      sha1 = "e243f19219aa95fbf0d8f2ffb07f5bd64e94fe20";
    };
    deps = [];
    meta = {
      homepage = "https://github.com/chrisa/node-dtrace-provider#readme";
      description = "Native DTrace providers for node.js applications";
      keywords = [ "dtrace" ];
    };
  }