{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "assert-plus";
    version = "0.1.5";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/assert-plus/-/assert-plus-0.1.5.tgz";
      sha1 = "ee74009413002d84cec7219c6ac811812e723160";
    };
    deps = [];
    devDependencies = [];
    meta = {
      description = "Extra assertions on top of node's assert module";
    };
  }