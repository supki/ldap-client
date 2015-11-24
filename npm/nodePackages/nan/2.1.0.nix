{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "nan";
    version = "2.1.0";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/nan/-/nan-2.1.0.tgz";
      sha1 = "020a7ccedc63fdee85f85967d5607849e74abbe8";
    };
    deps = [];
    meta = {
      homepage = "https://github.com/nodejs/nan#readme";
      description = "Native Abstractions for Node.js: C++ header for Node 0.8 -> 4 compatibility";
    };
  }