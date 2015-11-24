{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "dashdash";
    version = "1.10.1";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/dashdash/-/dashdash-1.10.1.tgz";
      sha1 = "0abf1af89a8f5129a81f18c2b35b21df22622f60";
    };
    deps = with nodePackages; [
      assert-plus_0-1-5
    ];
    meta = {
      homepage = "https://github.com/trentm/node-dashdash";
      description = "A light, featureful and explicit option parsing library.";
      keywords = [
        "option"
        "parser"
        "parsing"
        "cli"
        "command"
        "args"
      ];
    };
  }