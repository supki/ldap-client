{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "once";
    version = "1.3.0";
    src = pkgs.fetchurl {
      url = "http://registry.npmjs.org/once/-/once-1.3.0.tgz";
      sha1 = "151af86bfc1f08c4b9f07d06ab250ffcbeb56581";
    };
    deps = [];
    meta = {
      description = "Run a function exactly one time";
      keywords = [
        "once"
        "function"
        "one"
        "single"
      ];
    };
  }